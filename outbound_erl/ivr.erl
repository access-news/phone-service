-module(ivr).
-behaviour(gen_statem).

-export(
    [ start/1
    , start_link/1

    % gen_statem callbacks
    , init/1
    , callback_mode/0
    , handle_event/4
    , terminate/3
    ]).

-define(FS_NODE, 'freeswitch@tr2').

% Replaced this with a `main_category()` function at one point but had to revert because needed to use this in guards when handling DTMFs, and that will not work.
-define(CONTENT_ROOT, "/home/toraritte/clones/phone-service/content-root/").
% -define(CATEGORIES, {category, 0, ?CONTENT_ROOT}).
-define(CATEGORIES, {category, 0, "Main category"}).
% erl -eval 'cover:compile_directory("./outbound_erl").' -eval '{lofa, freeswitch@tr2} ! register_event_handler.' -run filog -run user_db -sname access_news -setcookie OldTimeRadio
% a new DTMF signal arrives while we are collecting digits. According to [the `gen_statem` section in Design Principles](from https://erlang.org/doc/design_principles/statem.html#cancelling-a-time-out) (and whoever wrote it was not a fan of proper punctuation): "_When a time-out is started any running time-out of the same type; state_timeout, {timeout, Name} or timeout, is cancelled, that is, the time-out is restarted with the new time._"  The [man pages](https://erlang.org/doc/man/gen_statem.html#ghlink-type-generic_timeout) are clearer: "_Setting a [generic] timer with the same `Name` while it is running will restart it with the new time-out value. Therefore it is possible to cancel a specific time-out by setting it to infinity._"
-define(DEMO_TIMEOUT, 300000). % 5 min
-define(INTERDIGIT_TIMEOUT, 2000).
% -define(INACTIVITY_WARNING_TIMEOUT, 600000). % 10 min
% -define(INACTIVITY_TIMEOUT, 720000). % 12 min

% -define(DTMF_DIGIT_TIMEOUT, 5000). % TODO ???

start_link(Ref) ->
    {ok, Pid} = gen_statem:start_link(?MODULE, [], []),
    {Ref, Pid}.

start(Ref) ->
    {ok, Pid} = gen_statem:start(?MODULE, [], []),
    {Ref, Pid}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_statem callbacks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

% State transition cheat sheat {{- {{-
% ============================

%             init
%               |
%  _____________V______________________
% /                                    \
% | {incoming_call, unregistered}      |
% \____________________________________/
%               |
%               |<--- "MOD_ERL_EVENT_MASSAGE"
%               |
%  _____________V______________________
% /                                    \
% | {incoming_call, unregistered}      |
% \____________________________________/
%               |
%               |<--- "INCOMING_CALL"
%               |
%               V
% *------------------------------------*
% | {main_menu, CallerStatus}          |
% *------------------------------------*

% }}- }}-
init(_Args) -> % {{-
    %% Set up logging to file. {{- {{-
    % filog:add_process_handler(?MODULE, Ref),
    % filog:process_handler_filter(?MODULE, Ref),
    %% }}- }}-
    % current /etc/freeswitch/dialplan/default.xml {{-
    % to start the gen_statem process on incoming calls

    % <?xml version="1.0" encoding="utf-8"?>                                                                                                                                                                                                                                          <include>                                                                                                                                                                                                                                                                         <context name="default">

    %     <extension name="SignalWire CONNECTORS incoming call">
    %     <!-- the number you assigned in your dashboard -->
    %     <condition field="destination_number" expression="^(\+19162510217)$">

    %         <!-- <action application="set" data="outside_call=true"/> -->
    %         <!-- <action application="export" data="RFC2822_DATE=${strftime(%a, %d %b %Y %T %z)}"/> -->
    %         <!-- <action application="answer"/> -->
    %         <!-- <action application="lua" data="main.lua"/> -->
    %         <!-- <action application="erlang" data="call_control:start access_news@tr2"/> -->

    %         <action application="set" data="playback_terminators=none"/>
    %         <action application="erlang" data="ivr:start access_news@tr2"/>

    %     </condition>
    %     </extension>
    % </context>
    % </include>
    % }}-

    % TODO launch content.erl gen_server
    %      AND GET RID OF FILOG LOGGING there!
    %      AND IT CANNOT HAVE content AS REGISTERED NAME ANYMORE!

    logger:debug("==========================="),
    logger:debug("==========================="),
    logger:debug("==========================="),

    % TODO Why was this necessary?
    process_flag(trap_exit, true),

    % TODO which approach is better: gen_server, or just module?
    % PROP each ivr gen_statem will get a copy of the content graph from the content gen_server during init, and the `content` will take care of keeping it up to date, and parsing during server startup (so it should start earlier than we could receive calls)
    % pipe(
    %   [ gen_server:call(content, {get, graph})
    %   , fun deserialize/1
    %   , (curry(fun erlang:put/2))(content_graph)
    %   ]
    % ),

    Data =
        #{ received_digits => []
         % ,       anchor => ""
         % , current_content => {}
         ,  playback_offset => 0
         % ,  playback_offset => -1
         ,  auth_status => unregistered % | registered
         % , menu_history => [] % used as a stack
         , current_content => none
         % Why the map? Needed a data structure that can also hold info whether playback has been stopped or not (here: `is_stopped` flag). THE STOPPED FLAG IS IMPORTANT: had the false assumptions that simple checking whether PlaybackName =:= CurrentState, but the behaviour should be different when the playback stops naturally, or by a warning that will keep the same state. Without a "stopped" bit there is no way to know how to proceed (e.g., ?CATEGORIES is stopped by a warning, warning starts playing, CHANNEL_EXECUTE_COMPLETE comes in with ?CATEGORIES, but if we simple repeat, than the the warning is stopped immediately.)
         % Could have used a proplist instead as a stack but (1) lookup is less convenient (more on that below), (2) less explicit (see below).
         ,    playbacks => []
         % ,   prev_state => init
         % There should be only one playback running at any time, and, a corollary, each state should only have one active playback associated in this map. T
         % , playback_stopped => false
         % ,  playback_ids => #{}
         },
    % When to push history {{-
    % ====================================================
    % Push history when leaving a category other than `?CATEGORIES`, `greeting`, and `incoming_call`.
    % quick example:
    %                                                                                   (the only other option from main_menu menu would be ?CATEGORIES
    % `main_menu` state is special in that it is not added to the history
    % TODO add flowchart
    % }}-
    % Why not worry about `main_menu` (and states beneath)
    % showing up in history? {{-
    % ====================================================
    % Because the only way out of `main_menu` is by going forward to `?CATEGORIES` or going back/up, which always pops the history. Inside `main_menu`, whenever an option is chosen, `main_menu` gets pushed to the history, but the only way out of submenus is to go back, which will pop it.
    % }}-
    %    State
    % This is kind of a cheat, becuase FreeSWITH ESL outbound mode only start the Erlang process when there is an incoming call, but because of the digraph-driven nature of this state machine, it is better to disambiguate between the states of `init` and `incoming_call`
    {ok, init, Data}.
% }}-

callback_mode() ->
    [handle_event_function, state_enter].
    % handle_event_function.

terminate(Reason, State, Data) ->
    logger:debug(#{ self() => ["TERMINATE (normal-ish)", #{ data => Data, reason => Reason, state => State }]}).
     % filog:process_log(debug, #{ from => ["TERMINATE", #{ reason => Reason, state => State, data => Data }]}),
     % filog:remove_process_handler(?MODULE).

% ENTER
% handle_event(enter, OldState, State, Data) % {{-
% when State =:= collect_digits
% ->
%     {keep_state, Data};

handle_event(enter, OldState, State, Data) -> %

    NewData =
        % ( composeFlipped(
        f:pipe(
          [ Data
          , fun stop_playback/1
          % , fun(Data) -> comfort_noise(), Data end
          , fun comfort_noise/1
          % , fun(GenData) -> play(State, GenData) end
          , (f:curry(fun play/2))(State)
          ]
        ),

    {keep_state, NewData};
% }}-

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% `info` clauses (for FreeSWITCH events) %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Rationale for using `handle_event_function` callbacks {{- {{-
%% =====================================================
%%
%% Using `handle_event_function` callback mode as it is
%% the most straightforward way to massage all incoming
%% FreeSWITCH  events  (emitted by  `mod_erlang_event`)
%% into a  form that  is easier  to pattern  match (see
%% below and the specific steps for more).
%%
%% ### Mostly `info` type `gen_statem` events
%%
%% FreeSWITCH events are sent as regular messages, hence they are handled by matching on `info` type `gen_statem` events in the callbacks. With `state_functions`, the `info` events would have needed checking in every state, and a macro could have been used to type less, but it would have become cluttered nonetheless.
%%
%% Using _inserted events_ (or _stored events_) to add an event pre-processing step via the `{next_event, EventType, EventContent}` **transition action**. `gen_statem` also ensures that ["_the stored events are inserted in the queue as the next to process before any already queued events. The order of these stored events is preserved, so the first next_event in the containing list becomes the first to process._"](https://erlang.org/doc/man/gen_statem.html#type-action).
%%
%% TODO: Update https://erlang.org/doc/design_principles/statem.html#Inserted%20Events with the above quote from the `gen_statem` man page.
%%
%% ```erl
%% %% `state_functions` pseudo-code
%%
%% incoming_call(info, ModErlEventMsg, _Data) ->
%%     MassagedModErlEvent = do_the_deed,
%%     {keep_state_and_data, {next_event, internal, MassagedModErlEvent}};
%% incoming_call(internal, MassagedFSEvent, Data) ->
%%     do_something_here,
%%     {next_state, ...}.
%%
%% %% ... N more other states
%%
%% publication(info, ModErlEventMsg, _Data) ->
%%     MassagedModErlEvent = do_the_deed,
%%     {keep_state_and_data, {next_event, internal, MassagedModErlEvent}};
%% publication(internal, MassagedFSEvent, Data) ->
%%     do_something_here,
%%     {next_state, ...}.
%% ```
%%
%% With `handle_event_function` there is only one callback, `handle_event/4`, so there is only one pre-processing clause is needed (for `info` `gen_statem` events).
%%
%% ### Event-focused approach
%%
%% An event-focused approach is justified (as opposed to a state-focused one above) by how FreeSWITCH events drive the IVR forward, resulting in the majority of `gen_statem` events to be of `info` event type.
%%
%% ### Clearer structure
%%
%% If casts and calls will ever be implemented, if external call control use case(s) would arise, they could have their own sections.
%%
%% ```erlang
%% %% `handle_event_function` pseudocode
%%
%% %%%%%%%%%%%%%%%%%%%%%%%
%% %% FreeSWITCH events %%
%% %%%%%%%%%%%%%%%%%%%%%%%
%%
%% handle_event(info, ModErlEventMsg, _State, _Data) ->
%%     MassagedModErlEvent = do_the_deed,
%%     {keep_state_and_data, {next_event, internal, MassagedModErlEvent}};
%%
%% handle_event(internal, MassagedModErlEvent, State, Data) ->
%%     %% ...
%%     {next_state, ...}.
%%
%% handle_event(internal, MassagedModErlEvent, AnotherState, Data) ->
%%     %% ...
%%     {next_state, ...}.
%%
%% %%%%%%%%%%%%%%%%%%%%%%%
%% %% External commands %%
%% %%%%%%%%%%%%%%%%%%%%%%%
%%
%% handle_event(cast, ...) ->
%%
%% handle_event(call, ...) ->
%% ```
%%
%% ### Complex state
%%
%% An added bonus is to be able to use complex (or compound states). Apart from the IVR's state, also needed to track whether caller is authenticated or not, and `gen_statem` state is a nice place to put it. Otherwise it would live in `gen_statem`'s data or in the process registry.
%% }}- }}-

%% Step 1. Pre-process FreeSWITCH events {{- {{-
%% -------------------------------------
%% Convert all incoming FreeSWITCH events into a form that is easier to pattern match (in this case, a map).
%%
%% The `mod_erlang_event` format of `FSEventHeaders` is of
%% `[{HeaderString1, ValueString1}, ..., {HeaderStringN, ValueStringN}]`
%% , and the fastest way to parse that is using `proplists` function, and it can't be matched in function headers.
%% }}- }}-

%% TODO Once the  IVR is  implemented, filter out  FS events
%%      (and/or parts of the events) that are  irrelevant to
%%      lower overhead.

%% MOD_ERL_EVENT_MASSAGE (info) {{-
%% The   event  content   is   name  `ModErlEventMsg`   on {{- {{-
%% purpose, and not  `FSEvent` (i.e., FreeSWITCH event)
%% or   similar,  because   `mod_erlang_event`  creates
%% its  own  arbitrary  messages  and  wrappers  around
%% FreeSWITCH  events. Most  are  consistent in  having
%% the form  of `{ModErlEventCallStatus,  {event, [UUID
%% |  FSEventHeaders]}}`  but there  is  `{call_hangup,
%% UUID}`,  even  though   there  are  `CHANNEL_HANGUP`
%% events received besides it.
%%
%% The   actual    FreeSWITCH   event    is   basically
%% `FSEventHeaders`,  as they  consist of  header-value
%% pairs anyways, and an optional body.
%%
%% TODO It is rare for FreeSWITCH events to have body (or is
%%      it?) and not sure how  the Erlang message looks like
%%      in that case.
%% }}- }}-

handle_event(
  info,
  {ModErlEventCallStatus, {event, [UUID | FSEventHeaders]}} = _ModErlEventMsg,
  % {ModErlEventCallStatus, {event, [UUID | FSEventHeaders]}} = ModErlEventMsg,
  State,
  Data
) ->
    %% As this  clause preceeds **every**  state transition
    %% (not   just  state   changes!)   AND  always   keeps
    %% `gen_statem` state and data,  this log does not need
    %% to be  repeated in  `handle_event/4` below,  only to
    %% double-check matched values, calculations etc.
    % logger:debug(#{ self() => ["MOD_ERL_EVENT_MASSAGE", #{ data => Data, state => State, mod_erl_event_call_status => ModErlEventCallStatus }]}),

    MassagedModErlEvent =
        { UUID
        , ModErlEventCallStatus
        , maps:from_list(FSEventHeaders)
        },
    TransitionActions =
        [ {next_event, internal, MassagedModErlEvent}
        ],

    {_, _, E} = MassagedModErlEvent,
    #{ "Event-Name" := EventName } = E,
    % logger:emergency(#{ event => { EventName, MassagedModErlEvent}}),

    %% Keeping  state  and  data,  because  the  FreeSWITCH
    %% `info` messages  can come any time,  and this clause
    %% only transforms them.
    {keep_state_and_data, TransitionActions};
%% }}-

%% CALL_HANGUP (info) {{-

%% NOTE There  are two  related  events, CHANNEL_HANGUP  and
%%      CHANNEL_HANGUP_COMPLETE. Not sure  why, but probably
%%      to  denote certain  stages of  the call  termination
%%      process,  just  as  the  `Answer-State`  header  has
%%      multiple values (e.g., `early`, `answered`).
handle_event(
  info,                 % EventType
  {call_hangup, _UUID}, % EventContent = ModErlEventMsg
  State,                % State
  Data                  % Data
) ->
    % logger:debug(#{ self() => ["CALL_HANGUP", #{ data => Data, state => State }]}),
    %% Gave `normal` as reason, otherwise `gen_statem` will {{-
    %% crash  (which isn't  really a  problem, because  the
    %% started `gen_statem`  processes are not  linked, and
    %% could've  just trap  exits, but  why go  through the
    %% hassle, when  a demo timeout is  considered a normal
    %% behaviour as well.) }}-
    {stop, normal, Data};
%% }}-

%% SENDMSG_CONFIRMATION (info) {{-
handle_event(
  info,                          % EventType
  ok = Msg,
  State,
  _Data                               % Data
) ->
    % logger:debug(#{ self() => ["SENDMSG_CONFIRMATION", #{ message => Msg, state => State}]}),
    keep_state_and_data;
%% }}-

%% DEBUG (info) {{-
%% Catch-all clause to see unhandled `info` events.
handle_event(
  info,  % EventType
  Msg,   % EventContent
  State, % State
  _Data  % Data
) ->
    % logger:emergency(#{ self() => ["UNHANDLED_INFO_EVENTS", #{ unknown_msg => Msg, state => State}]}),
    keep_state_and_data;
%% }}-

%%%%%%%%%%%%%%%%%%%%%%%%
%% `internal` clauses %%
%%%%%%%%%%%%%%%%%%%%%%%%

% INTERNAL HANGUP (demo_hangup, inactivity_hangup) {{-
    % see `demo_hangup` on what happens next or how the call is terminated from the server side
% Hanging up, so stop processing other `internal` events [..]
% NOTE `info` (i.e., external) events are still processed, but they are irrelevant at this point. If need to save them for debugging, just modify MOD_ERL_EVENT_MASSAGE, or the DEBUG clause in the "`info` clauses (for FreeSwITCH events)" section above
handle_event(
  internal, % EventType
  EC, % EventContent
  {hangup, _},                % State
  Data                  % Data
) ->
    % logger:debug(#{ self() => ["in HANGUP state", #{ data => Data, state => hangup, event_content => EC }]}),

    % sending it synchronously to allow the playback to end
    sendmsg_locked(
        #{ command => hangup
         , args       => ["16"]
         }),
    %% Not stopping the  `gen_statem` process here, because
    %% there will be further events related to the `hangup`
    %% command above. See "CALL_HANGUP" `info` clause below.
    keep_state_and_data;
% }}-

%% mod_erlang_event: call (STATE: init -> incoming_call) {{-

%% Call init {{-
%% ====================================================
%% This is the only state where `call` event can arrive
%% -  or at  least this  is  how it  should be;  `call`
%% should happen only once in  a call, and becuase this
%% is  outbound mode,  and  the  process just  started,
%% there is not much else flying around.

%% Long story short, this is kind of the real `init` of
%% the call (`init/1` is to initialize the `gen_statem`
%% process), and thus some  variables are set here, and
%% will document call-global notes here as well.

%% }}-

handle_event(
  internal,                          % EventType
  { UUID                             % \
  , call                             % |
  , #{ "Channel-ANI" := CallerNumber % | EventContent = MassagedModErlEvent
     }                               % |
  },                                 % /
  init = State,
  #{ auth_status := unregistered }   = Data
) ->
    % logger:debug(#{ self() => ["INCOMING_CALL", #{ data => Data, state => State }]}),

    %% Implicit UUID {{-
    %% ====================================================
    %% Tried  to  convince   myself  to  always  explicitly
    %% include  the channel  ID  (i.e.,  `UUID`), but  just
    %% putting  it  in  the process  dictionary  is  easier
    %% because
    %%
    %%   + a `gen_statem` process will  only deal with one call
    %%     anyway,
    %%
    %%   + analogous  to  how FreeSwITCH  does  it  in the  XML
    %%     dialplan, and
    %%
    %%   + this  is  the  perfect  use  case  for  the  process
    %%     registry as  the UUID  won't change during  the call
    %%     (at least not in the Access News scenario).
    %% }}-
    put(uuid, UUID),

    sendmsg_locked(
        #{ command => execute
         , args       => ["answer", []]
         }),
    %% REMINDER (`playback_terminator`) {{-
    %% ====================================================
    %% https://freeswitch.org/confluence/display/FREESWITCH/playback_terminators
    %%
    %% The   default   `playback_terminator`  is   *,   and
    %% that   should  be   ok   because  it   is  tied   to
    %% going   up/back   a   menu,  but   playback   should
    %% be  killed  in most  cases  anyway, and disabling it
    %% would make it explicit (and also avoiding mysterious
    %% errors in the future).
    %%
    %% Kind of the FreeSWITCH-equivalent of the CSS reset.
    %%
    %% The same  could've been done in  the dialplan (e.g.,
    %% `freeswitch/dialplan/default.xml`) using
    %% ```xml
    %% <action application="set" data="playback_terminators=none"/>
    %% ```
    %% but this pertains  to each call, and  better to have
    %% everything related to the same call in one place.
    %% }}-
    fsend({api, uuid_setvar, UUID ++ " playback_terminators none"}),

    { NewAuthStatus
    , TransitionActions
    } =
        case is_user_registered(CallerNumber) of
            true ->
                { registered
                , []
                };
            false ->
                DemoTimeout =                       % Generic timeout
                    { {timeout, unregistered_timer} % { {timeout, Name}
                    , ?DEMO_TIMEOUT                 % , Time
                    , demo_hangup                       % , EventContent }
                    },
                { unregistered
                , [ DemoTimeout
                  ]
                }
        end,

    { next_state
    , incoming_call
    , Data#{ auth_status := NewAuthStatus }
    , TransitionActions
    };

    % { keep_state
    % , Data#{ auth_status := NewAuthStatus }
    % , TransitionActions
    % };
%% }}-

%% mod_erlang_event: call_event(CHANNEL_ANSWER) (STATE: incoming_call -> greeting) {{-
handle_event(
  internal,                               % EventType
  { _UUID                                 % \
  , call_event                            % |
  , #{ "Event-Name" := "CHANNEL_ANSWER" } % | EventContent = MassagedModErlEvent
  },                                      % /
  incoming_call = State,                       % State
  % #{ auth_status  := _AuthStatus           % \         Made  explicit  all  values  that  are  known,
  %  % , menu_history := []                   % | Data    because the only state change to this state is
  %  % , playback_ids := #{}                  % |         from  `incoming_call`, and  whatever has  been
  %  } =
  Data                               % /         set there must also be true.
) ->
    % logger:debug(#{ self() => ["CALL_ANSWERED", #{ state => State}]}),

    % NOTE No need for this, because greeting transitions to the main category never to be revisited again in the same session, and the timer will started there.
    % re_start_inactivity_timer(State),

    { next_state
    % , {greeting, content_root()}
    , greeting
    , Data
    };
%% }}-

%% HANDLE_DTMF_FOR_ALL_STATES (internal) {{-
handle_event(
  internal,                    % EventType
  { _UUID                      % \
  , call_event                 % |
  , #{ "DTMF-Digit" := Digit} = EventName % | EventContent = MassagedModErlEvent
  },                           % /
  State,
   % if `ReceivedDigits =/= []`, we are collecting digits
  #{
      received_digits := ReceivedDigits
   ,  auth_status := AuthStatus
   % ,  menu_history := MenuHistory
   } = Data
) ->
    % logger:debug(#{ self() => ["HANDLE_DTMF_FOR_ALL_STATES", #{ digit => Digit, state => State}]}),
    logger:debug( #{ a => "HANDLE_DTMF", state => State, digit => Digit, collected_digits => ReceivedDigits }),

    % Reset inactivity timers whenever a new DTMF signal comes in
    % TODO Amend when voice commands get implemented
    re_start_inactivity_timer(State),

    % {{-
    %% 1 2 3   \
    %% 4 5 6    > Interdigit time-out (IDT) = 2 sec (?)
    %% 7 8 9   /  EXCEPT when playing an article, then IDT = 0
    %% -----
    %% * 0 #   Interdigit time-out = 0

    % Then notion is that in every state when one of "123456789" is pressed, "collecting_digits" mode is initiated, and "*" and "#" will be ignored (except when received after the interdigit time-out (IDT) expired), but they will trigger an extra IDT seconds to get new digits.

    % DTMF tones are divided into two categories: category selectors, and single digit instant actions. Category selectors have interdigit timeout (that may be configurable), whereas single digits instant actions immediately respond by changing state, performing a command, etc.
    % Another difference is that category selectors only change state once the `interdigit_timer` expires, hence they will keep the state as is in this clause, but single digit instant actions do whatever necessary to move on (stop playback, change state, etc.).
    % }}-
    % {{- {{-
    % When `idt_running` is true, it means we are collecting digits for category selection. So when IDT is not running and 0 comes in, then go to main menu, but when IDT is running, then add it to `ReceivedDigits`, renew IDT, and keep collecting. Another example is when IDT is not running, and * (star) and # (pound) arrives, go back or go to next category respectively, but when IDT is running, then ignore these inputs and renew IDT, and keep collecting.

    % IDT race condition when collecting digits
    % ====================================================
    % It is possible that IDT times out while processing a DTMF digit. What then?

    % This probably isn't as bad as it sounds, or maybe not even bad at all. For example, a user is having trouble to dial digits quickly (device issues, disability, etc.), and one of the digits hits this race condition, but this is an important input, so IDT time-out can be ignore, and now this digit is saved and IDT extended.

    % Is there a scenario where this can turn sour? Someone start dialing for a category, but then change their minds, and wants to hit the main menu, go back, etc. Even in not a race condition status they would have to wait until the IDT runs out. If they do get into a category, they can always go back with * (star).

        %% Keep  playing  the   greeting,  continue  collecting
        %% digits,  and caller  will be  sent to  the specified
        %% category, if the `DTMFString` contains a valid entry
        %% after  the  interdigit  time-out.  Once  the  playback
        %% finishes,  the `PLAYBACK_STOP`  event is  emitted by
        %% FreeSWITCH,  and  the  state   will  be  changed  to
        %% `?CATEGORIES`, but  the behavior  there will  be the
        %% same, and  the digits will continue  to be collected
        %% (until they make sense).
        % NOTE ONLY: * (star) and # (pound) in `?CATEGORIES` {{-
        % a) Put category browsing with # (pound) on hold for now (see clause "# (pound) in any state"
        % b) Decided to put an option `main_menu` to be able to jump to the main categories, which means that * (star) should make it possible to jump back to where we came from
        % #{                state := ?CATEGORIES
        %  ,       received_digit := Digit
        %  ,    collecting_digits := false
        %  , digit_is_one_to_nine := false % not necessary, but explicit
        %  }
        % when Digit =:= "*";
        %      Digit =:= "#"
        % ->
        %     % Ignore
        %     keep_state_and_data;
        % }}-

    % }}- }}-

    % Only needed for COLLECT DIGITS case clause's guards
    % RootState =
    %     case is_tuple(State) of
    %         false ->
    %             State;
    %         true ->
    %             element(1, State)
    %     end,

    % case {State, Digit} of
    case State of
        % TODO # has no function (except greeting, main_menu, article)
        % Use it to browse categories (step into the next one, like in article?)
        % or forward in history instead of back?

        % === COLLECT_DIGITS % {{-
        % { collect_digits, Digit } ->
        collect_digits ->
            collect_digits(Digit, Data);

        % }}-
        % === GREETING (-> content_root) {{-
        greeting ->
            % Set `current_content` (it is `none` at this point)
            % QUESTION Should this be set in `init/1` instead?
            %          I think it is easier to understand it this way.
            NewData = set_current(content:root(), Data),

            case Digit of
                % [*#]       {{- => content root
                _ when Digit =:= "*"
                     ; Digit =:= "#"
                ->
                    {next_state, content_root, NewData};
                % }}-
                % 0          {{- => main_menu
                "0" ->
                    {next_state, main_menu, NewData};
                % }}-
                % [1-9]      {{- => collect digits
                _ ->
                    collect_digits(Digit, NewData)
                % }}-
            end;

        % }}-
        % === MAIN_MENU (loop) {{-
        main_menu ->
            case Digit of
                % * (star)  {{- <- Go back (current content)
                "*" ->
                    { next_state
                    , derive_state(Data)
                    , Data
                    };
                % }}-
                % TODO PROD sign_in
                % # (pound) {{- => Log in / Favourites (depending on AuthStatus)
                "#" ->
                    % TODO FEATURE add favorites
                    % NextMenu =
                    %     case AuthStatus of
                    %         registered   -> favourites;
                    %         unregistered -> sign_in
                    %     end,

                    {next_state, sign_in, Data};
                % }}-
                % 0         {{- => content_root
                "0" ->
                    next_content(content_root, Data);
                % }}-
                % TODO FEATURE tutorial
                % 1         {{- => tutorial
                "1" ->
                    % {next_state, tutorial, Data};
                    keep_state_and_data;
                % }}-
                % TODO FEATURE leave_message
                % 2         {{- => leave_message
                "2" ->
                    % {next_state, leave_message, Data};
                    keep_state_and_data;
                % }}-
                % TODO FEATURE settings
                % 3         {{- => settings
                "3" ->
                    % {next_state, settings, Data};
                    keep_state_and_data;
                % }}-
                % TODO PROD blindness_services
                % 4         {{- => blindness_services
                "4" ->
                    % {next_state, blindness_services, Data};
                    keep_state_and_data;
                % }}-
                % 5-9       {{- => UNASSIGNED (keep state and data)
                % TODO FEATURE random_play
                "5" -> keep_state_and_data;
                "6" -> keep_state_and_data;
                "7" -> keep_state_and_data;
                % TODO FEATURE where_am_i
                "8" -> keep_state_and_data;
                "9" -> keep_state_and_data
                % }}-
            end;

        % }}-
        % === CONTENT_ROOT (loop) {{-
        content_root ->
            case Digit of
                % TODO FEATURE implement content history so this will bring back to a previuos content
                % *          {{- => ignore
                "*" ->
                    keep_state_and_data;
                % }}-
                % 0          {{- => main_menu
                "0" ->
                    {next_state, main_menu, Data};
                % }}-
                % #          {{- => enter first child category
                "#" ->
                    next_content(first, Data);
                % }}-
                % [1-9]      {{- => collect digits
                _ when Digit =:= "1"
                     ; Digit =:= "2"
                     ; Digit =:= "3"
                     ; Digit =:= "4"
                     ; Digit =:= "5"
                     ; Digit =:= "6"
                     ; Digit =:= "7"
                     ; Digit =:= "8"
                     ; Digit =:= "9"
                ->
                    collect_digits(Digit, Data)
                % }}-
            end;

        % }}-
        % === CATEGORY (loop) {{-
        category ->
            case Digit of
                % *          {{- => back (i.e., up in content hierarchy)
                "*" ->
                    next_content(parent, Data);
                % }}-
                % 0          {{- => main_menu
                "0" ->
                    {next_state, main_menu, Data};
                % }}-
                % #          {{- => next category (i.e., next sibling)
                "#" ->
                    logger:debug(#{ self() => [ "content_root->first(debug)"
                                              , #{ event_name => EventName
                                                 , state => State
                                                 % , fs_event => FSEvent
                                                 }]}),
                    next_content(next, Data);
                % }}-
                % [1-9]      {{- => collect digits
                _ when Digit =:= "1"
                     ; Digit =:= "2"
                     ; Digit =:= "3"
                     ; Digit =:= "4"
                     ; Digit =:= "5"
                     ; Digit =:= "6"
                     ; Digit =:= "7"
                     ; Digit =:= "8"
                     ; Digit =:= "9"
                ->
                    collect_digits(Digit, Data)
                % }}-
            end;

        % }}-
        % === PUBLICATION (loop) {{-
        publication ->
            case Digit of
                % *          {{- => back (i.e., up in content hierarchy)
                "*" ->
                    next_content(parent, Data);
                % }}-
                % 0          {{- => main_menu
                "0" ->
                    {next_state, main_menu, Data};
                % }}-
                % #          {{- => next publication (i.e., next sibling)
                "#" ->
                    next_content(next, Data);
                % }}-
                % TODO PROD handle no articles
                % DONE? test
                % 1         {{- => Play FIRST article
                "1" ->
                    next_content(first, Data);
                % }}-
                % TODO FEATURE list articles
                % 2         {{- => UNASSIGNED (keep_state_and_data)
                "2" -> keep_state_and_data;
                % }}-
                % 3         {{- => Play LAST article
                "3" ->
                    next_content(last, Data);
                % }}-
                % 4-8       {{- => UNASSIGNED (keep_state_and_data)
                "4" -> keep_state_and_data;
                % TODO FEATURE play where_am_i
                "5" -> keep_state_and_data;
                % TODO FEATURE list articles (basically a shortcut for "*" to emphasize that one can just do that to listen to available publications in a category)
                "6" -> keep_state_and_data;
                "7" -> keep_state_and_data;
                % TODO FEATURE "continue from last time"
                "8" -> keep_state_and_data;
                % }}-
                % 9         {{- => PREVIOUS publication
                "9" ->
                    next_content(prev, Data)
                % }}-
            end;

        % }}-
        % === ARTICLE_INTRO (-> article) {{-
        article_intro ->
            case Digit of
                % *          {{- => back to publication
                "*" ->
                    next_content(parent, Data);
                % }}-
                % 0          {{- => main_menu
                "0" ->
                    {next_state, main_menu, Data};
                % }}-
                % #          {{- => next article (i.e., next sibling)
                "#" ->
                    next_content(next, Data);
                % }}-
                % TODO FEATURE reset volume/speed
                % NOTE Difference between this and the same clause in ARTICLE_HELP is that this will always start the article from the beginning (offset =:= 0), while ARTICLE_HELP should resume the playback from last saved position
                % 1-8        {{- => start article
                _ when Digit =:= "1"
                     ; Digit =:= "2"
                     ; Digit =:= "3"
                     ; Digit =:= "4"
                     ; Digit =:= "5"
                     ; Digit =:= "6"
                     ; Digit =:= "7"
                     ; Digit =:= "8"
                ->
                    { next_state
                    , derive_state(Data)
                    , Data#{ playback_offset := 0 }
                    };
                % }}-
                % 9          {{- => previous article
                "9" ->
                    next_content(prev, Data)
                % }}-
            end;

        % }}-
        % === ARTICLE (i.e., article playback) -> next article {{-
        %      see `play % article`
        article ->
            case Digit of
                % *          {{- => back (i.e., up in content hierarchy)
                "*" ->
                    next_content(parent, Data);
                % }}-
                % 0          {{- => main_menu
                "0" ->
                    {next_state, main_menu, Data};
                % }}-
                % #          {{- => next article (i.e., next sibling)
                "#" ->
                    next_content(next, Data);
                % }}-
                % TODO PROD in_progress implement controls
                % DONE? test
                % NOTE using `api` for now instead of `bgapi`
                % 1          {{- => rewind (10s)
                "1" ->
                    uuid_fileman("seek:-10000");
                % }}-
                % 2          {{- => pause (stop) article
                "2" ->
                    {next_state, article_help, Data};
                % }}-
                % 3          {{- => forward (10s)
                "3" ->
                    uuid_fileman("seek:+10000");
                % }}-
                % 4          {{- => slower
                "4" ->
                    % TODO PROD figure out the right amount
                    uuid_fileman("speed:-10");
                % }}-
                % 5          {{- => volume down
                "5" ->
                    % TODO PROD figure out the right amount
                    uuid_fileman("volume:-2");
                % }}-
                % 6          {{- => faster
                "6" ->
                    uuid_fileman("speed:+10");
                % }}-
                % TODO FEATURE add to favourites
                % 7          {{- => restart article
                "7" ->
                    uuid_fileman("restart");
                % }}-
                % 8          {{- => volume up
                "8" ->
                    uuid_fileman("volumeu:+2");
                % }}-
                % 9          {{- => previous article
                "9" ->
                    next_content(prev, Data)
                % }}-
            end;

        % }}-
        % === ARTICLE_HELP (loop) {{-
        _ when State =:= article_help
             ; State =:= article_intro
        ->
            case Digit of
                % *          {{- => back to publication
                "*" ->
                    next_content(parent, Data);
                % }}-
                % 0          {{- => main_menu
                "0" ->
                    {next_state, main_menu, Data};
                % }}-
                % #          {{- => next article (i.e., next sibling)
                "#" ->
                    next_content(next, Data);
                % }}-
                % TODO FEATURE reset volume/speed
                % TODO FEATURE listen to article meta

                % Almost the same here as in ARTICLE_INTRO; see note there
                % 1-8        {{- => resume article
                _ when Digit =:= "1"
                     ; Digit =:= "2"
                     ; Digit =:= "3"
                     ; Digit =:= "4"
                     ; Digit =:= "5"
                     ; Digit =:= "6"
                     ; Digit =:= "7"
                     ; Digit =:= "8"
                ->
                    {next_state, derive_state(Data), Data};
                % }}-
                % 9          {{- => previous article
                "9" ->
                    next_content(prev, Data)
                % }}-
            end;

        % }}-
        % === UNINTERRUPTABLE STATES
        % warnings -> current content
        % hangups  -> keep_state_and_data
        % [*#0-9]    {{- => ignore (keep_state_and_data)
        _ when State =:= invalid_selection
             ; State =:= inactivity_warning
             ; State =:= demo_hangup
             ; State =:= inactivity_hangup
             ; State =:= no_children
             ; State =:= no_prev_item
             % TODO FEATURE add option to jump to the first element
             ; State =:= no_next_item
        ->
            keep_state_and_data;

        % }}-

        % TODO Nothing should ever end up here, so remove once the basics are done
        % EXCEPT when adding a new state, and forgot to add a clause here to deal with DTMF input (e.g., to ignore them completely, such as with "collect_digits")
        Unhandled ->
            logger:emergency(#{ self() => ["UNHANDLED_DIGIT", Unhandled, {state, State}]}),
            keep_state_and_data
    end;

    %% (Why `keep_state`)FALSE and no inserted events? {{- {{-
    %% ====================================================
    % ### 1. Why no inserted events?
    %
    % Already using inserted events to make FreeSWITCH events more manageable (see `MOD_ERL_EVENT_MASSAGE`), and adding extra `internal` events would cause a race condition; "_the order of these stored events is preserved, so the first next_event in the containing list becomes the first to process_", thus massaged FreeSWITCH earlier (read further why this is a problem).
    %
    % It is also unnecessary because more natural transition opportunities will present themselves, such as `PLAYBACK_STOP` FreeSWITCH events whenever the playback is stopped. Adding an extra internal event is therefore superfluous as well.
    %
    % There is a hitch though: `PLAYBACK_STOP` events won't provide a context regarding what key press (i.e., `DTMF` event) caused the playback to stop, so these will need to get stored.
    %
    % ### 2. Why keep the state? (KEEP! See update below)
    %
    % Many key presses (in many states) should stop the playback and cause a state change (e.g., from `greeting` to `?CATEGORIES`), but if state is changed in this `handle_event/4` clause then it will be hard to understand how subsequent events need to handled.
    %
    % For example, `stop_playback/0` will cause FreeSWITCH to emit a `PLAYBACK_STOP` event, and if the state is changed, these events will have to be handled in those next states. But these next states will also receive DTMF events that stop the playback, and the handling of `PLAYBACK_STOP` events will be pushed to the subsequent states, and so on.
    %
    % It seems more logical to keep the state, and handle the `PLAYBACK_STOP` events where the playbacks have been started. Not to mention the natural state changes that result in playbacks stopping because all the text has been read (so loop, or next state). So if a state change is forced on `DTMF` events, and then playback relating to a specific state will have to be handled in the same state and in the next one. Will get messy real quick.
    %
    %% REMINDER
    %%
    %% When  the   `PLAYBACK_STOP`  event  comes   in  from
    %% FreeSWITCH it first
    %% 1. enters as an `info` message,
    %% 2. gets massaged  into more  pattern-matchable form
    %%    that
    %% 3. will be inserted as an `internal` event,
    %% 4. and only then will the `PLAYBACK_STOP` FreeSWITCH
    %%    event really handled.

% #### 2.1 Update - don't keep state

% The problem is race condition again. Take the 0# combo (for unregistered users to sign in) in `greeting`: user hits 0, playback stops, and if # (pound) comes in almost immediately then it will be evaluated in `greeting`, having completely different semantics.

% Another false assumption I made is that `PLAYBACK_STOP` is the only thing that needs checking, but `speak` (and probably other TTS engine players) only generate `CHANNEL_EXECUTE_COMPLETE`, although the problem remains the same.

    %% }}- }}-
%% }}-

%% HANDLE_CHANNEL_EXECUTE_COMPLETE (catching stopped playbacks mostly) {{-
handle_event(
  internal,                    % EventType
  { _UUID                      % \
  , call_event                 % |
  , #{ "Event-Name" := "CHANNEL_EXECUTE_COMPLETE"
       % TODO re-eval on any playback change! (external engine etc)
     , "Application" := Application
     % , "Application" := "speak"
     , "Application-UUID" := ApplicationUUID
     , "variable_playback_last_offset_pos" := LastOffset
     }                            % | EventContent = MassagedModErlEvent
  } = E,                           % /
  State,
  % #{ prev_state := PrevState } = Data
  #{    playbacks := Playbacks
   } = Data
)
when Application =:= "speak"
   ; Application =:= "playback"
->
    % PlaybackName =
    %     extract_playback_name(ApplicationUUID),

    % This dancing around is because functions in `maps` never fail, and this `handle_event/4` clause should crash if an `ApplicationUUID` is not in `Playbacks` as it would mean that a playback has been started without saving its ID in the `gen_statem` data, which should not happen. All menus use only `speak` (for now), so anything that gets in here is a playback that has been stopped (one way or another).
    % #{ ApplicationUUID :=
    %     #{ playback_name := PlaybackName
    %      ,    is_stopped := IsStopped
    %      }
    % } = Playbacks,
    { ApplicationUUID
    , StoppedPlayback
    , IsStopped
    } =
        proplists:lookup(ApplicationUUID, Playbacks),

    NewPlaybacks =
        proplists:delete(ApplicationUUID, Playbacks),
    NewData =
        Data#{ playbacks := NewPlaybacks },

    logger:debug(
        #{ a => "HANDLE_CHANNEL_EXECUTE_COMPLETE"
         , app_uuid => ApplicationUUID
         , state => State
         , prev_state => StoppedPlayback
         }),

    % NewData =
    %     ( composeFlipped(
    %         [ (curry(fun proplists:delete/2))(ApplicationUUID)
    %         , fun(Value) -> maps:update(playbacks, Value, Data) end
    %         ]
    %       )
    %     )(Data),

    % logger:debug(#{event => E}),

    % TODO every `play/2` clause should have a corresponding entry here
    %      (or vice versa, every state mentioned here should have a `play/2` clause`
    case State of

        % TODO list_articles (and lots others)

        % The `play/2` function of this state just reads back the pressed digits;
        % if this clause had not been here, triggering the collectoin would simply end up in clause 1 below but it has the same outcome so OK
        % if digit readback gets stopped by a new digit, it would be clause 2 below, but it has the same outcome so OK
        % if digit readback runs its course, it stops here, but without this clause it would end up
        % collect_digits ->
        %     {keep_state, NewData};

        _ when StoppedPlayback =:= article
             , StoppedPlayback =/= State
        ->
            % TODO BUG? Is there a possibility that this never gets saved?
            {keep_state, NewData#{playback_offset := LastOffset}};

        % 1
        % `is_stopped` should always be TRUE in this scenario
        _ when StoppedPlayback =/= State ->
            {keep_state, NewData};

        % 2
        _ when StoppedPlayback =:= State
             , IsStopped =:= true
        ->
            {keep_state, NewData};

        % 3
        % `is_stopped` should always be FALSE in this scenario
        % STATES TO BE LOOPED
        _ when StoppedPlayback =:= State
             , IsStopped =:= false
             , State =:= main_menu
             ; State =:= content_root
             ; State =:= category
             % ; State =:= publication
             ; State =:= article_help
        ->
            {repeat_state, NewData};

        % States where playback ended naturally,
        % but that should not loop
        % --------------------------------------
        greeting ->                     % |
            next_content(content_root, NewData);

        publication ->
            next_content(first, NewData);

        article ->
            next_content(next, NewData);

        collect_digits ->
            {keep_state, NewData};

        % UNINTERRUPTABLE states that only play a prompt
        % formerly {warning, _}         % | TODO already see a pattern
        _ when State =:= invalid_selection
             ; State =:= inactivity_warning
             ; State =:= no_children
             ; State =:= no_prev_item
             ; State =:= no_next_item
             ; State =:= article_intro
        ->
            {next_state, derive_state(NewData), NewData}

        % % TODO clean up {{-
        %                                 % |
        % % Not really necessary to handle this, but it's here for completeness sake
        % _ when State =:= demo_hangup;
        %        State =:= inactivity_hangup
        % ->
        %     keep_state_and_data;

        %     % Article =
        %     %     content(Current, first),

        %     % Article =
        %     %     #{ path := ArticlePath
        %     %      , offset := 0
        %     %      },

        %     { next_state
        %     , {content, content(Current, first)} % i.e., article
        %     % TODO checking the offset is done at play
        %     % -1 -> read article title and meta
        %     % other -> keep playing from given offset
        %     % NOTE: not touching `offset`; it is -1 when coming from publication (either because it played or because option 1 or 3 is selected)
        %     , NewData
        %     };

        % {content, #{type := article} = Current} ->
        %     case NewData of
        %         % article meta played (aka, `article_entry`)
        %         #{ offset := 0 } ->
        %             { next_state
        %             , State
        %             , NewData
        %             };
        %         % #{ offset := 1 } - article playback finished without interruptions
        %         % #{ offset := Offset } when Offset >= 1 - article playback was interrupted at one point
        %         #{ offset := Offset }
        %         % `when Offset > 0` would have sufficed but this documents it better
        %           when Offset =:= 1 % article playback finished without interruptions
        %              ; Offset > 1   % article playback was interrupted at one point (to go to a menu, etc.)
        %         ->
        %             { next_state
        %             , content(Current, next)
        %             , NewData#{ offset := -1 }
        %             }
        %     end,

        %     { next_state
        %     , NextMenu
        %     , NewData
        %     }

        % % article_entry ->
        % %     {next_state, article_playback, NewData};

        % % `collect_digits` has no menu playback (via `speak/3`), hence no clause here

        % % TODO do a playback CEC clause
        % % `article_playback` does have a `play/2` clause but it uses dptools:playback instead of speak
        %     % }}-
    end;
%% }}-

%% Debug clauses for `internal` events {{-
handle_event(
  internal,                          % EventType
  { _UUID                             % \
  , call_event                             % |
  % , #{ "Event-Name" := EventName } = FSEvent % | EventContent = MassagedModErlEvent
  , #{ "Event-Name" := EventName } = FSEvent % | EventContent = MassagedModErlEvent
  } = E,                                 % /
  State,
  _Data                               % Data
)
->
    % logger:debug(#{ self() => ["UNKNOWN_INTERNAL_CALL_EVENT", #{ event_name => EventName, state => State, fs_event => FSEvent}]}),
    % logger:debug(""),
    keep_state_and_data;

handle_event(
  internal,                          % EventType
  Msg,
  State,
  _Data                               % Data
) ->
    % logger:emergency(#{ self() => ["UNKNOWN_INTERNAL", #{ unknown_msg => Msg, state => State}]}),
    keep_state_and_data;

% handle_event(
%   internal,                          % EventType
%   _MassagedModErlEvent,
%   _State,
%   _Data                               % Data
% ) ->
    % filog:process_log(debug, #{ from => "another event that is not INCOMING_CALL and CALL_HANGUP" });

%% }}-

%%%%%%%%%%%%%%
%% Timeouts %%
%%%%%%%%%%%%%%

%% inactivity timers (info) {{-
% The timers are started with `re_start_inactivity_timer/0` when a call comes in, or restarted when a DTMF signal is received. The purpose of button presses is to change the state, so it is not important to restart the timers on the actual state change, and interdigit timers have a short value compared to the inactivity ones.
% This way is even better, because on invalid category selection the user will remain in the same state as long as they can't input a correct number, but the timers will get reset on each try.
handle_event
  ( info               % EventType
  , inactivity_warning % EventContent
  , _State
  , Data
  )
->
    % Inactive =
    %     "Please any key if you are still there.",

    % TODO Add notes on how this works.
    % TODO how is this applicable when in {article, _}?
    % NewDataP =
    %     warning(Inactive, Data),
    {next_state, inactivity_warning, Data};

handle_event(
  info,
  inactivity_hangup,
  _State,
  Data
) ->
    {next_state, inactivity_hangup, Data};
    % see `demo_hangup` on what happens next or how the call is terminated from the server side
%% }}-

%% unregistered_timer {{-
handle_event(
  {timeout, unregistered_timer}, % EventType
  demo_hangup,                       % EventContent
  State,
  #{ auth_status := AuthStatus } = Data
) ->
    % logger:debug(#{ self() => ["UNREGISTERED_TIMEOUT", #{ data => Data, state => State }]}),

    case AuthStatus of

        registered ->
            % ignore if user already signed in in the meantime
            keep_state_and_data;

        unregistered ->
            % TODO speak notice that hanging up as demo timer expired, and goodbye (don't forget to add new warning state to HANDLE_DTMF_FOR_ALL_STATES, and HANDLE_CHANNEL_EXECUTE_COMPLETE!)
            {next_state, demo_hangup, Data}
            % A state_enter callback occurs at this point (i.e., `handle_event(enter, ...` is called; an event does not have to come in to trigger it), that triggers playback stop, starts playing the `demo_hangup` playback. Meanwhile, when an event comes in, it will trigger INTERNAL HANGUP clause, formally issuing a hangup request towards freeswitch (synchronously to let playback finish), and it will ignore any subsequent FS events.
            % TODO verify the claim below
            % So any internal event will end up at INTERNAL HANGUP, and when the `call_hangup` event comes in, then will be the gen_statem stopped with `{stop, normal, Data}`
    end;
%% }}-

%% interdigit_timer {{-
handle_event
  ( {timeout, interdigit_timer} % EventType
  , eval_collected_digits       % EventContent
  , collect_digits   % State
  % , {collect_digits, Content}   % State
  , #{ received_digits := ReceivedDigits
     , current_content := CurrentContent
     } = Data
  )
->
    Selection =
        % ( composeFlipped(
        f:pipe
          ([ ReceivedDigits
           , fun lists:reverse/1
           , (f:cflip(fun string:join/2))("")
            % [ fun (List) -> string:join(List, "") end
            % Will throw on empty list but it should never happen the way collect_digits/2 is called
            % (to elaborate: `collect_digits/2` is only triggered by DTMF digits 1-9, hence it should never be empty. If it is, it means that the menu calling (i.e., DTMF-processing) logic is wrong, and should be corrected)
           , fun erlang:list_to_integer/1
           ]),
          % )
        % )(ReceivedDigits),

    % Always clear DTMF buffer when the `interdigit_timer` expires, because this is the point the buffer is evaluated. Outcome is irrelevant, because at this point user finished putting in digits, hence waiting for the result, and so a clean slate is needed.
    NewData =
        Data#{ received_digits := [] },

    % logger:debug(#{ a => "INTERDIGIT_TIMEOUT (category exists)", collected_digits => ReceivedDigits, state => State, category => Category}),

    SelectionResult =
        [  Child
        || Child
           <- content:pick(children, CurrentContent)
           ,  maps:find(selection, Child) =:= {ok, Selection}
        ],

                    logger:debug(#{ self() => [ "interdigit_timeout"
                                              , #{ selection => Selection
                                                 % , state => State
                                                 , selection_result => SelectionResult
                                                 % , fs_event => FSEvent
                                                 }]}),
    % case lists:keyfind(Selection, 2, SubCategories) of
    case SelectionResult of
        [] ->
            {next_state, invalid_selection, NewData};
        [SelectedContent] ->
            { next_state
            % `content_root` will never be possible here so pattern matching would have sufficed but this is more uniform
            , derive_state(SelectedContent)
            , set_current(SelectedContent, NewData)
            }
    end.
%% }}-

% TODO Eval on 2 digits immediately to speed up things
collect_digits(Digit, Data)
    when Digit =:= "*";
         Digit =:= "#"
->
    collect_digits("", Data);

collect_digits % {{-
  ( Digit
  , #{ received_digits := ReceivedDigits
     } = Data
  )
when Digit =:= "1";
     Digit =:= "2";
     Digit =:= "3";
     Digit =:= "4";
     Digit =:= "5";
     Digit =:= "6";
     Digit =:= "7";
     Digit =:= "8";
     Digit =:= "9";
     Digit =:= "0"
->
    NewData =
        Data#{received_digits := [Digit|ReceivedDigits]},
    % The notion is that the `InterDigitTimer` is restarted whenever {{-
    % a new DTMF signal arrives while we are collecting digits. According to [the `gen_statem` section in Design Principles](from https://erlang.org/doc/design_principles/statem.html#cancelling-a-time-out) (and whoever wrote it was not a fan of proper punctuation): "_When a time-out is started any running time-out of the same type; state_timeout, {timeout, Name} or timeout, is cancelled, that is, the time-out is restarted with the new time._"  The [man pages](https://erlang.org/doc/man/gen_statem.html#ghlink-type-generic_timeout) are clearer: "_Setting a [generic] timer with the same `Name` while it is running will restart it with the new time-out value. Therefore it is possible to cancel a specific time-out by setting it to infinity._"
    % }}-
    % TODO Does timer cancellation produce an event?
    InterDigitTimer =                 % Generic timeout
        { {timeout, interdigit_timer} % { {timeout, Name}
        , ?INTERDIGIT_TIMEOUT         % , Time
        , eval_collected_digits       % , EventContent }
        },

    % Keeping state because this is only a utility function collecting the DTMF signals. State only changes when the `interdigit_timer` times out.
    % (using `repeat_state` so that state enter clause would get triggered, and with that the entered DTMF digit would get read back (via `collect_digits` `play/2` function))
    { next_state
    , collect_digits
    , NewData
    , [ InterDigitTimer ]
    }.
% }}-

%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions %%
%%%%%%%%%%%%%%%%%%%%%%%

%% The async  version of  `is_user_registered/1`. Probably {{- {{-
%% better  suited  for  when the  phone  number  lookup
%% will  try to  reach  a remote  database. Handled  in
%% `handle_cast/2` above.
%%
%% CAVEAT: Probably  introduces a race  condition: User
%% calls  service, assumes  they are  registered, start
%% dialing single/double  digits for menus, and  if the
%% DB lookup is slow enough, it could be that the timer
%% (for DTMF  digits) expires, and  its `handle_info/2`
%% callback  fires   before  the  lookup   returns  (in
%% `handle_cast/2`)  that  a  user is  registered.  The
%% former  would  emit  a  warning  that  user  is  not
%% registered,  will be  kicked out  in 5  minutes, but
%% then  the  system  silently does  register  (if  the
%% lookup is successful).
%% Either  add another  notification that  registration
%% successful, or stay silent the first time and give a
%% warning  that 1  minute  remaining  for example,  if
%% lookup not successful.
%% }}- }}-
% caller_status(EventHeaders) -> % {{-
%     CallPid = self(),
%     F = fun() ->
%             CallerStatus = register_status(EventHeaders),
%             gen_statem:cast(CallPid, CallerStatus)
%         end,
%     spawn(F).
%% }}-

is_user_registered(CallerNumber) -> %% {{-
    PhoneNumber =
        case CallerNumber of
            %% Could've    matched   this    in    the   head    of
            %% `handle_event/4` but if the pattern match fails, the
            %% process would  just go down.  (Calling `terminate/3`
            %% first, in this case, as it is declared above.)
            "+1" ++ Number ->
                Number;
            _Invalid ->
                % filog:process_log(emergency, #{ from => ["IS_USER_REGISTERED", #{ caller_number => CallerNumber}]}),
                exit("invalid")
        end,
    look_up(PhoneNumber).
%% }}-

% TODO this should be in `user_db` module
look_up(PhoneNumber) ->
    gen_server:call(user_db, {look_up, PhoneNumber}).

%%%%%%%%%%%%%%%%%%%%%%%%
%% FreeSWITCH helpers %%
%%%%%%%%%%%%%%%%%%%%%%%%

%% sendmsg/2 {{-

%% Could've just saved the channel ID (i.e., `UUID`) to
%% the process registry, because it unique to this call
%% (hence to this process) but  this way it is explicit
%% on  every  call,  and  mirrors  the  FreeSWITCH  ESL
%% `sendmsg` syntax.

%% TODO Only implemented for `execute` and `hangup` for now;
%%      even these are lacking a thorough documentation.
%%
%%      See available `sendmsg` commands at
%%      https://freeswitch.org/confluence/display/FREESWITCH/mod_event_socket#mod_event_socket-3.9.1Commands
% event_uuid_header(UUID) ->
%     {"Event-UUID", UUID}.

spec_sendmsg_headers(execute, [App, Args]) when is_list(Args) ->
    %% TODO "loops"  header   and  alternate  format   for  long
    %%      messages (is it needed here?)  not added as they are
    %%      not needed yet.
    [ {"execute-app-name", App}
    , {"execute-app-arg", Args}
    ];

spec_sendmsg_headers(hangup, [HangupCode, _ApplicationUUID]) ->
    %% For hangup codes, see
    %% https://freeswitch.org/confluence/display/FREESWITCH/Hangup+Cause+Code+Table
    [{"hangup-cause", HangupCode}];

%% This will  blow up,  one way or  the other,  but not
%% planning to get there anyway.
spec_sendmsg_headers(_SendmsgCommand, _Args) ->
    [].

% `Prefix` is mostly to identify a playback/speak. Menus, such as greeting, main_menu, etc., are also states, so most of the time this will be the state of the `gen_statem` process. But sometimes it just needs a label that it is a warning etc.
do_sendmsg(
  #{ command := SendmsgCommand
   , args       := SendmsgArgs
   , app_uuid_prefix   := Prefix
   , event_lock      := IsLocked
   }
)
when is_list(SendmsgArgs)
->
    LockHeaderList =
        case IsLocked of
            false -> [];
            true  -> [{"event-lock", "true"}]
        end,

    EventUUIDHeaderList =
        case Prefix =:= none of
            true ->
                [];
            false ->
                PoorMansUUID =
                    erlang:make_ref(),
                ApplicationUUID =
                    stringify(Prefix)
                    ++ "|"
                    ++ stringify(PoorMansUUID),
                % NOTE Event-UUId =?= ApplicationUUID {{-
                % This seems like a discrepancy here, but this is how `mod_event_socket` operates:
                % https://freeswitch.org/confluence/display/FREESWITCH/mod_event_socket
                % > When an application is executed via sendmsg, CHANNEL_EXECUTE and CHANNEL_EXECUTE_COMPLETE events are going to be generated. If you would like to correlate these two events then add an Event-UUID header with your custom UUID. In the corresponding events, the UUID will be in the Application-UUID header. If you do not specify an Event-UUID, Freeswitch will automatically generate a UUID for the Application-UUID.
                % }}-
                [{"Event-UUID", ApplicationUUID}]
        end,

    FinalHeaders =
        [{"call-command", atom_to_list(SendmsgCommand)}]
        ++ spec_sendmsg_headers(SendmsgCommand, SendmsgArgs)
        ++ EventUUIDHeaderList
        ++ LockHeaderList,
    fsend({sendmsg, get(uuid), FinalHeaders}),
    % logger:debug(#{ a => ["DO_SENDMSG", #{ app_id => ApplicationUUID, final_headers => FinalHeaders }]}),

    case EventUUIDHeaderList of
        [] ->
            not_used;
        [{"Event-UUID", AppUUID}] ->
            AppUUID % same as ApplicationUUID above
    end;

do_sendmsg(
  #{ command := SendmsgCommand
   , args       := SendmsgArgs
   , event_lock      := IsLocked
   } = Args
) ->
    % logger:debug(#{ a => ["DO_SENDMSG", #{args => Args}]}),
    do_sendmsg(
      Args#{ app_uuid_prefix => none }
    ).

sendmsg(
  #{ command := _
   , args       := SendmsgArgs
   } = Args
) ->
    logger:debug(#{ a => ["SENDMSG", #{args => Args}]}),
    do_sendmsg(Args#{ event_lock => false }).

sendmsg_locked(
  #{ command := _
   , args       := SendmsgArgs
   } = Args
) ->
    do_sendmsg(Args#{ event_lock => true }).
%% }}-

fsend(Msg) ->
    %% Why the `lofa` atom:
    %% https://stackoverflow.com/questions/58981920/
    {lofa, ?FS_NODE} ! Msg.

uuid_fileman(Command) ->
    fsend
      ({ api
       , uuid_fileman
       , stitch([get(uuid), Command])
       }).

stop_playback(#{ playbacks := [] } = Data) ->
    Data;

% Nothing is playing and the most recent playback has been stopped before.
% For example, ?CATEGORIES is playing, "*" is sent when nothing is in history, `warning/?` stops playback, warning starts playing, ends naturally, so HANDLE_CHANNEL_EXECUTE_COMPLETE get clears {warning, false}, and calls that scenario in its `case`, which is `repeat/?`, that also has a `stop_playback/1`, and the most recent playback is the stopped ?CATEGORIES. Subsequent HANDLE_CHANNEL_EXECUTE_COMPLETE clauses will clear out these entries. (or should...)
% TODO make sure that the Data#playbacks stack gets cleared properly
% TODO QUESTION Where are the stopped recordings filtered out? Or does this act as a history? But not all menus play prompts.
% TODO QUESTION If there is only one playback allowed at one time, why is `Data#playbacks a list?
stop_playback(#{ playbacks := [{_, _, {stopped, true}}|_] } = Data) ->
    Data;

% Stop the currently playing prompt
% (but just in case, stop all, even though only one should be playing at any time)
stop_playback( % {{-
  #{ playbacks :=
     [ { ApplicationUUID  % |
       , PlaybackName     % | currently playing
       , {stopped, false} % |
       }
       | Rest
     ]
   } = Data
) ->
    % logger:debug("stop playback"),
    % TODO Should this  be `bgapi`? Will the  synchronous `api`
    %      call wreak havoc when many users are calling?

    % TODO: is stopped status necessary? If not, is there a scenario where it may be in the future?
    % NOTE it is superfluous: {{-
    % When CHANNEL_EXECUTE_COMPLETE playback of the same name as the state comes in, it means that nothing has stopped the playback, because the `stopped_playback/1` function is only called in the state enter function on state change. As a corollary, every other time where PlaybackName =/= State means that the playback has been stopped
    % That is, the stopped status can be derived from the current state and ended playback.
    % }}-
    fsend({api, uuid_break, get(uuid) ++ " all"}),
    StoppedPlayback = {ApplicationUUID, PlaybackName, {stopped, true}},
    Data#{ playbacks := [StoppedPlayback|Rest] }.
% }}-

% Access News controls {{-
% Press 8 for the help menu.
% Press 0 to pause playback.
% Press 1 to skip back one article.
% Press 2 to restart the current article.
% Press 3 to go forward to the next article.
% Press 4 to jump back 10 seconds in article.
% Press 5 to change volume up or down.
% Press 6 to skip forward 10 seconds.
% Press 7 to slow the playback down.
% Press 9 to speed the playback up.
% Press * key once to return to the main system menu.
% Press # to skip to the last article in this category.
% }}-

%% Playback-related {{-
% TODO Sift throught these notes. {{-
%% Using  `sendmsg_locked`  most   of  the  time  (with
%% `speak`   or   `playback`)    because   it   enables
%% synchronous execution on FreeSWITCH, so `event_lock`
%% events  get  queued   up  and  called  sequentially.
%% Otherwise playbacks would overlap.

% NOTE: "Locked" (i.e., synced) and async events can be mix-and-matched. Just did a `comfort_noise/0`, followed by a `speak` (both "locked"), and then an async hangup (with simple `sendmsg/2`). Everything went fine, and after the all media played the channel got hung up. Another test: sync `comfort_noise/0` followed by an async `speak` and async `hangup`. Could hear the `speak` kick in, but once `hangup` was received the call got terminated.
%%
%% Although  this  does   not  mean  that  `gen_statem`
%% stands  still! Once  the  command is  sent off  with
%% `sendmsg_locked`,  the process  continues execution,
%% and waits for coming events (such as DTMF events).

% Welcome to Access News, a service of Society For The Blind in Sacramento, California for blind, low-vision, and print-impaired individuals.

% You are currently in demo mode, and have approximately 5 minutes to try out the system before getting disconnected. To log in, dial 0# followed by your code, or if you would like to sign up up for Access News, please call us at 916 889 7519, or dial 02 to leave a message with your contact details.

% For the main menu, press 0.
% To listen to the tutorial, dial 00.
% To learn about other blindness services, dial 01.
% If you have any questions, comments, or suggestions, please call 916 889 7519, or dial 02 to leave a message.
% To start exploring menu items one by one, press the pound sign.
% Press # again to skip to the next, and dial #0 to jump back to the previous one.

% If you would like to learn about the national Federation of the blind nfb newsline service with access to more than 300 newspapers and magazines including the Sacramento Bee. Please call 410-659-9314. If you would like to learn about the California Braille and talking book Library, please call 80095 to 5666.

% `Prompt` is either a state name or custom designation (such a prompt on timeout).
% `Data` always refers to `gen_fsm` process data (a.k.a., state in `gen_server`)
% play(
%   Prompt,
%   #{ playback_ids := PlaybackIDs } = Data
% ) ->
%     ApplicationUUID =
%         play(Prompt, Data),
%     NewPlaybackIDs =
%         % `=>` update if present, or create new
%         % `:=` only update when present, otherwise crash
%         PlaybackIDs#{ Prompt => ApplicationUUID },
%     NewData =
%         Data#{playback_ids := NewPlaybackIDs }.
% }}-

comfort_noise(Milliseconds, Data) -> % {{-
    % logger:debug("play comfort noise"),
    ComfortNoise =
           "silence_stream://"
        ++ integer_to_list(Milliseconds)
        ++ ",1400",

    logger:debug(
        #{ a => "COMFORT_NOISE"
         , data => Data
         }),

    playback(comfort_noise, Data, ComfortNoise).
    % sendmsg_locked(
    %     #{ command => execute
    %      , args    => ["playback", ComfortNoise]
    %      , app_uuid_prefix   => comfort_noise
    %      }).

comfort_noise(Data) ->
    comfort_noise(750, Data).
% }}-

% TODO make a module for utterances? {{-
sign_up() ->
    "If you would like to sign up for Access News, please call us at 916, 889, 7519.".

% invalid_selection() ->
%     "Invalid selection. Please try again.".
% }}-

% TODO Try out `mod_vlc` to play aac and m4a files. {{-
% sendmsg(UUID, execute, ["playback", "/home/toraritte/clones/main.mp3"]),
% sendmsg_locked(UUID, execute, ["playback", "/home/toraritte/clones/phone-service/ro.mp3"]),
% }}-

play(init, Data) -> Data;
play(incoming_call, Data) -> Data;
play(collect_digits, Data) -> Data;

% play % COLLECT_DIGITS {{-
% ( collect_digits = State
% , #{ received_digits := [Digit|_] } = Data
% )
% ->
%     % QUESTION Why does this work (theoretically) even though this special looping is only allowed to play this prompt once? {{-
%     % ANSWER
%     % a_state -> collect_digits transition (that is when collection is triggered) will end up in CHANNEL_EXECUTE_COMPLETE clause 1 while reading back the most recent DTMF digit already started
%     % readback finishes uninterrupted: there is still time ahead probably for the ?INTERDIGIT_TIMEOUT to run out, and playback shouldn't be repeated. The CHANNEL_EXECUTE_COMPLETE event falls through all the clauses (the transition is collect_digits -> collect_digits, and IsStopped false) to its own collect_digits clause
%     % readback is interrupted by a new digit (this case `collect_digits/2` is invoked that ends with `repeat_state`, so state enter will invoke a playback on the new digit) or the ?INTERDIGIT_TIMEOUT (that does not stay in collect_digits state): in this case it ends up in clause 2, which is no action
%     % }}-
%     speak(State, Data, Digit);

% % }}-
play % GREETING {{-
( greeting = State
, #{ auth_status := AuthStatus } = Data
)
->
    % logger:debug("play greeting"),
    Anchor = "Welcome to Access News, a service of Society For The Blind in Sacramento, California, for blind, low-vision, and print-impaired individuals.",
    PoundOrStar = "If you know your selection, you may enter it at any time, or press star or pound to skip to listen to the main categories.",
    Unregistered =
        case AuthStatus of
            registered ->
                "";
            unregistered ->
                "You are currently in demo mode, and have approximately 5 minutes to try out the system before getting disconnected. To log in, dial zero pound, followed by your code."
                ++ sign_up()
                ++ "To leave a message with your contact details, dial zero two."
        end,
    MainMenu =
        "For the main menu, press 0.",
    % TODO FEATUREs
    % GoToTutorial = "To listen to the tutorial, dial zero one.",
    % GoToBlindnessServices = "To learn about other blindness services, dial zero four.",
    % LeaveMessage = "If you have any questions, comments, or suggestions, please call 916 889 7519, or dial zero two to leave a message.",

    PromptList =
        [ Anchor
        , Unregistered
        , PoundOrStar
        , MainMenu
        % , GoToTutorial
        % , GoToBlindnessServices
        % , LeaveMessage
        ],

    speak(State, Data, PromptList);

% }}-
play % MAIN_MENU {{-
  ( main_menu        = State
  , #{auth_status := AuthStatus} = Data
  )
->
    Anchor = "Main menu.",
    Star = "To go back, press star.",
    DevComment = "Comment: All the options following this are still not implemented. The only ones slated to make it into production for now are the login and blindness resources.",
    Pound =
        case AuthStatus of
              registered -> "For your Favourites, ";
            unregistered -> "To log in, "
        end
        ++ "press pound.",
    % Zero = history listing
    One = "To start the tutorial, press one.",
    Two = "To leave a message, press two.",
    Three = "For settings, press three.",
    Four = "To learn about other blindness resources, press four.",
    Five = "randomize playback",

    PromptList =
        [ Anchor
        , Star
        , DevComment
        , Pound
        , One
        , Two
        , Three
        , Four
        , Five
        ],

    speak(State, Data, PromptList);

% }}-
play % CONTENT_ROOT {{-
  ( content_root = State
  , #{ current_content :=
       #{title := Title} = CurrentContent
     } = Data
  )
->
    PromptList =
        [ Title
        , "For the main menu, press 0."
        , "To enter the first category, press pound."
        ]
        ++ choice_list(CurrentContent),

    speak(State, Data, PromptList);

% }}-
play % CATEGORY {{-
  ( category = State
  , #{ current_content :=
       #{title := Title} = CurrentContent
     } = Data
  )
->
    PromptList =
        [ Title ]
        ++ common_options(State)
        ++ choice_list(CurrentContent),

    logger:debug(
        #{ a => "PLAY_CATEGORY"
         , current_content => CurrentContent
         , data => Data
         , prompt => PromptList
         }),

    speak(State, Data, PromptList);
    % f:pipe(
    %   [ PromptList
    %   , fun stitch/1
    %   , ((f:curry(fun speak/3))(State))(Data)
    %   ]
    % );

% }}-
play % PUBLICATION {{-
  ( publication = State
  , #{ current_content :=
       #{title := Title} = CurrentContent
     } = Data
  )
->
    NumberOfArticles =
        % (length . content:pick(children)) CurrentContent
        length(content:pick(children, CurrentContent)),

    PromptList =
        [ Title
        , "There are " ++ integer_to_list(NumberOfArticles) ++ " articles in this publication. The first article will start automatically when playback of this menu is finished."
        , common_options(State)
        , "To start the first article, press 1."
        , "To jump to the last article, press 3."
        , "To go to the previous publication in this category, press 9."
        , "Starting first article."
        ],

    speak(State, Data, PromptList);

% }}-
% TODO FEATURE article metadata
% PROGRESS it is called `article_intro`; test it
play % ARTICLE_INTRO {{-
( article_intro = State
, Data
)
->
    speak(article_intro, Data, ["Press 2 at any time to pause the article and listen to the help menu."]);
    % playback(State, Data, Path);
    % TODO FEATURE read article meta and figure out how to store the latter
    %      (also, should article_meta be a separate state?)
    % Saving this snippet to demonstrate a solution {{-
    %
    %   1. that multiple heterogeneous playbacks (e.g., Application="speak" &  Application="playback") can be queued
    %      TODO Figure out how to handle CHANNEL_EXECUTE_COMPLETEs
    %
    %   2. how to play audio from cloud blob storage
    %
    % sendmsg_locked(
    %     #{ sendmsg_command => execute
    %     , arguments       => ["speak", "flite|kal|" ++ "Testing! Testing! 27."]
    %     , app_uuid_prefix   => testrun
    %     }),

    % sendmsg_locked(
    %     #{ sendmsg_command => execute
    %     , arguments       => ["playback",  "shout://accessnews.blob.core.windows.net/safeway/ro.mp3"]
    %     , app_uuid_prefix   => lofa
    %     });
    % }}-

% }}-
play % ARTICLE {{-
  ( article = State
  , #{ current_content :=
       #{ path := Path
        , playback_offset := Offset
        } = CurrentContent
     } = Data
  )
->
    % NOTE Why ARTICLE_INTRO had to split out into its own state {{-
    % speak(article_intro, Data, ...),
    % The CHANNEL_EXECUTE_COMPLETE clause could have been twisted into compliance, but it would have been more complex. As it stands this is still not a piece of cake.
    % }}-

    % TODO FEATURE resume playback from last position (see offset in `init/1`)
    playback(State, Data, Path ++ "@@" ++ Offset );

% }}-
play % ARTICLE_HELP {{-
  ( article_help = State
  , Data
  )
->
    % TODO FEATURE resume playback from last position (see offset in `init/1`)
    PromptList =
        [ "Article paused."
        , "To resume, press any button between 1 and 8."
        , "During playback, you have the following controls available:"
        , "For the publication menu, press star."
        , "For the main menu, press 0."
        , "To play the next article, press pound."
        , "For the previous article, press 9."
        , "To pause playback, press 2."
        , "To restart the article, press 7."
        , "To go back 10 seconds, press 1."
        , "To go forward 10 seconds, press 3."
        , "To slow down the recording, press 4."
        , "To speed up, press 6."
        , "To turn down the volume, press 5."
        , "To turn it up, press 8."
        ],

    speak(State, Data, PromptList);

% }}-
play % INVALID_SELECTION % {{-
( invalid_selection = State
, Data
)
->
    speak(State, Data, ["Invalid selection."]);

% }}-
play % INACTIVITY_WARNING {{-
( inactivity_warning = State
, Data
)
->
    speak
      ( State
      , Data
      , ["Please press any key if you are still there."]
      );

% }}-
play % DEMO_HANGUP {{-
( demo_hangup = State
, Data
)
->
    TextList =
        [ "End of demo session."
        , sign_up()
        , "Thank you for trying out the service!"
        ],

    speak(State, Data, TextList);

% }}-
play % INACTIVITY_HANGUP {{-
( inactivity_hangup = State
, Data
)
->
    speak(State, Data, ["Goodbye."]);

% }}-
play % NO_CHILDREN {{-
( no_children = State
, Data
)
->
    CurrentContentType =
        derive_state(Data),
    Prompt =
        "This "
        ++ atom_to_list(CurrentContentType)
        ++ " is empty.",

    speak(State, Data, Prompt);

% }}-
play % NO_PREV_ITEM {{-
( no_prev_item = State
, Data
)
->
    CurrentContentType =
        derive_state(Data),
    Prompt =
        "This is the first "
        ++ atom_to_list(CurrentContentType)
        ++ ".",

    speak(State, Data, Prompt);

% }}-
play % NO_NEXT_ITEM {{-
( no_next_item = State
, Data
)
->
    CurrentContentType =
        derive_state(Data),
    Prompt =
        "This is the last "
        ++ atom_to_list(CurrentContentType)
        ++ ".",

    speak(State, Data, Prompt).

% }}-

% TODO ok to delete old `play(article, ...)`s? {{-
% `article_entry`
% play
%   ( { content
%     , #{ type := article
%        , title := Title
%        } = Current}
%   , #{ offset := -1 } = Data
%   )
% -> % {{-
%     % Shows that metadata has been spoken (aka, `article_entry`)
%     NewData =
%         Data#{ offset := 0 },
%     % TODO `speak` article metadata

% % }}-
% % This clause plays the actual recording
% play
%   ( { content
%     , #{ type := article
%        , title := Title
%        } = Current}
%   , #{ offset := 0 } = Data
%   )
% -> % {{-
%     % Changing the offset from zero, because if the file plays through without interruption (pause, enter other menus etc) then when it is finished playing, the CHANNEL_EXECUTE_COMPLETE `handle_event` clauses would assume that `article_entry` just played and would start playing the same recording all over again. This way, it will know to skip to the next article.

%     % TODO WHEN TO RESET THE OFFSET TO -1
%     %   1. Playback finished uninterrupted (i.e., implicit next in CHANNEL_EXECUTE_COMPLETE.)
%     %   2. Switching to another article (next, last, first, etc.)
%     %   3. Leaving the publication (e.g., next publication, jump to main category from main_menu).
%     % TODO Save article offsets at one point for "Would you like to continue where you left off?" when visiting the same article next time
%     NewData =
%         Data#{ offset := 1 }, % otherwise the same article would get played because of the clause in HANDLE_CHANNEL_EXECUTE_COMPLETE
%     % TODO `speak` article metadata

% % }}-
% play(publication_menu, Data) ->
%     done;

% play(article_menu, Data) ->
%     done;
% }}-

% http://erlang.org/pipermail/erlang-questions/2005-April/015279.html
% extract_playback_name(ApplicationUUID) ->
%     PlaybackIDString =
%         string:sub_word(ApplicationUUID, 1, $|),
%     {ok, Tokens, _Line} =
%         erl_scan:string( PlaybackIDString ++ "." ),
%     {ok, PlaybackName} =
%         erl_parse:parse_term(Tokens),
%     PlaybackName.

% speak(State, Data, PromptList)
%     when erlang:is_list(PromptList)
% ->
%     speak(
%       #{ playback_name => State
%        , data => Data
%        , text => stitch(PromptList)
%       }
%     ).

regurgitate
( ApplicationUUID
, PlaybackName
, #{ playbacks := Playbacks } = Data
)
->
    Playback =
        { ApplicationUUID
        , PlaybackName % Only for convenience; already part of `ApplicationUUID`.
        % TODO: is stopped status necessary? If not, is there a scenario where it may be in the future?
        % NOTE it is superfluous: {{-
        % When CHANNEL_EXECUTE_COMPLETE playback of the same name as the state comes in, it means that nothing has stopped the playback, because the `stopped_playback/1` function is only called in the state enter function on state change. As a corollary, every other time where PlaybackName =/= State means that the playback has been stopped
        % That is, the stopped status can be derived from the current state and ended playback.
        % }}-
        , {stopped, false} % Has the playback been stopped?
        },
        % #{ playback_name => PlaybackName
        %  ,    is_stopped => false
        %  },
    NewPlaybacks =
        [ Playback | Playbacks ],
        % Playbacks#{ ApplicationUUID => Playback },

    Data#{ playbacks := NewPlaybacks }.

speak % {{-
( PlaybackName
, Data
, TextList
)
->
    % return the application UUID string.
    ApplicationUUID =
        sendmsg_locked(
          #{ command => execute
           , args       => ["speak", "flite|kal|" ++ stitch(TextList)]
           , app_uuid_prefix   => PlaybackName
           }),

    logger:debug(
        #{ a => "SPEAK"
         , "state|playbackname" => PlaybackName
         , data => Data
         , text_list => TextList
         }),

    regurgitate(ApplicationUUID, PlaybackName, Data).
% }}-

playback % {{-
( PlaybackName
, Data
, Path
)
->
    % return the application UUID string.
    ApplicationUUID =
        sendmsg_locked(
          #{ command => execute
           , args       => ["playback", Path]
           , app_uuid_prefix   => PlaybackName
           }),

    logger:debug(
        #{ a => "PLAYBACK"
         , "state|playbackname" => PlaybackName
         , data => Data
         , path => Path
         }),

    regurgitate(ApplicationUUID, PlaybackName, Data).
% }}-

stitch([Utterance]) ->
    Utterance;
stitch([Utterance|Rest]) ->
    Utterance ++ " " ++ stitch(Rest).

% TODO ok to delete this section? {{-
%% Push/pop history {{-
% push_history(Data, State)
% when State =:= incoming_call;
%      State =:= greeting;
%      % State =:= main_menu;
%      % State =:= quick_help;
%      State =:= collect_digits;
%      % TODO These are compound states!
%      element(1, State) =:= warning
%      % State =:= publication;
%      % State =:= article
% ->
%     push_history(Data, {skip, State});

% push_history(#{ menu_history := History } = Data, State) ->
%     logger:debug(#{ self() => ["PUSH_HISTORY", {history, History}, {state, State}]}),
%     Data#{ menu_history := [State | History] }.

% % Why not check if history is empty? {{-
% % ====================================================
% % Because it will (or at least should) not happen: Call comes in, FreeSWITCH start `ivr.erl`, `init/0` sets history to `[?CATEGORIES]`, meaning that states before that (i.e., `incoming_call` and `greeting`) can never be revisited. When traversing the history backwards with * (star), `HANDLE_DTMF_FOR_ALL_STATES` will make sure that when the `?HISTORY_ROOT` is reached, pressing the * (star) will be ignored (see `case` clause starting with comment "Go up/back").
% % UPDATE
% % What is said above still holds, but the root is now [].
% % }}-
% pop_history(#{ menu_history := [] } = Data) ->
%     { ?CATEGORIES
%     , Data
%     };

% % TODO adjust when final form of `article` state is known
% pop_history(
%   #{ menu_history :=
%      [ {article, _}
%      , _Publication
%      , Category
%      | RestHistory
%      ]
%    } = Data
% ) ->
%     pop_history(Data#{ menu_history := [Category|RestHistory] });

% pop_history(#{ menu_history := [{skip, _} | RestHistory] } = Data) ->
%     pop_history(Data#{ menu_history := RestHistory });

% pop_history(#{ menu_history := [PrevState | RestHistory] } = Data) ->
%     logger:debug(#{ self() => ["POP_HISTORY", {history, [PrevState|RestHistory]}]}),
%     { PrevState
%     , Data#{ menu_history := RestHistory }
%     }.
% % }}-

% Playback keeps going while accepting DTMFs, and each subsequent entry has a pre-set IDT
% 1-9 trigger collection (all states except some, see HANDLE_DTMF_FOR_ALL_STATES)
% *, 0, # are single digit instant actions
% (TODO make users able to set it)

% TODO warning will become a new state, so eliminate this clause
%      and create a `play` clause for it
% warning(WarningPrompt, Data) -> % {{-
%     logger:debug("SPEAK_WARNING: " ++ WarningPrompt),
%     NewData = stop_playback(Data),
%     comfort_noise(),
%     speak(
%       #{ playback_name => warning
%        , data => NewData
%        , text => WarningPrompt
%        }).
% }}-

% never push history (no point because just replaying menu prompt)
% repeat_menu( % {{-
%   #{ menu := Menu
%    , data := Data
%    }
%  ) ->
%     logger:debug("REPEAT_MENU"),
%     % Not necessary when playback ends naturally,
%     % but calling it multiple times doesn't hurt.
%     % NewDataP = stop_playback(Data),
%     % comfort_noise(),
%     % NewDataPP = play(Menu, NewDataP),
%     {keep_state, NewDataPP}.
% % }}-

%       = NextState
% next_menu
% ( NextState
% , CurrentContent
% , #{ menu_history := MenuHistory } = Data
% )
% ->  % {{-

%     logger:debug("NEXT_MENU"),

%     NewData = Data#{ menu_history := [{NextState, CurrentContent}|MenuHistory] },
%     % Update menu history (if permitted) {{-
%     % NewDataH =
%     %     push_history(Data, CurrentState),
%         % case {CurrentState, NextMenu} of {{-

%         %     % TODO re-write so that always pushing history; more straightforward, plus extra debug info. mark states that shouldn't be revisited by user via *
%         %     % State changes when history is not updated  with prev
%         %     % state (corner cases)
%         %     % ====================================================
%         %     % NOTE: This is adding behaviour to `next_menu/1`, which {{-
%         %     % I do not like, but these are the only cases when history is not pushed (so far). Another advantage is that all corner cases are listed in one place.
%         %     % }}-
%         %     % NOTE: `main_menu` -> `?CATEGORIES` {{-
%         %     % Go to `?CATEGORIES` from `main_menu` (only forward option from `main_menu`), but do not add `main_menu` to history
%         %     % NOTE Currently it is possible to accumulate `?CATEGORIES` in history by going to `main_menu` and forward to `?CATEGORIES`, as it will add `?CATEGORIES` each time but leaving it as is
%         %     % }}-
%         %     {main_menu, ?CATEGORIES}  -> Data;
%         %     % {{-
%         %     %   + `greeting` -> `?CATEGORIES` (i.e., when pressing * or #  in `greeting` or playback stops)
%         %         % No `next_menu/1` here to keep the history empty so that `greeting` cannot be revisited, making `?CATEGORIES` the root. Whatever navigations happen there
%         %         % Why not keep state? Reminder: 0# (not very relevant in this particular case, but keeping state changes consistent makes it easier to think about each `handle_event/4` clause. Read note at the very end of HANDLE_DTMF_FOR_ALL_STATES `handle_event/4` clause)
%         %     % }}-
%         %     {greeting, ?CATEGORIES}   -> Data;
%         %     {greeting, main_menu}     -> Data;
%         %     {incoming_call, greeting} -> Data;

%         %     % On any other state change
%         %     _ -> push_history(Data, CurrentState)
%         % end,
%     % }}-

%     % NewDataP = stop_playback(Data),
%     % comfort_noise(), }}-

%     % TODO put play/2 in CEC
%     % NewDataPP =
%     %     play(NextMenu, NewDataP),

%     {next_state, NextState, NewData}.
% % }}-

% prev_menu(#{ menu_history := History } = Data) -> % {{-
%   % #{ current_state := CurrentState
%   %  , data := #{ menu_history := History } = Data
%   %  }
% % ) ->
%     logger:debug("PREV_MENU"),
%     % case {CurrentState, History} of {{-
%     %     {?CATEGORIES, []} -> % {{-
%     %     % Nowhere to go back to; give warning and repeat
%     %         EmptyHistory = "Nothing in history.",
%     %         NewDataP =
%     %             warning(EmptyHistory, Data),

%     %         {keep_state, NewDataP};

%     %     % }}-
%     %     {main_menu, []} -> % {{-
%     %     % The only  way this is  (or should be)  possible when
%     %     % coming to `main_menu` from `greeting` (`greeting` is
%     %     % not pushed  to history  during that  transition; see
%     %     % `next_menu/1`).
%     %         next_menu(
%     %           #{ menu => ?CATEGORIES
%     %            , data => Data
%     %            , current_state => main_menu
%     %            });
%     %     % }}-
%     %     _ ->
%     % }}-

%     % NewDataP = stop_playback(Data),
%     % comfort_noise(),
%     {PrevState, NewDataPH} =
%         pop_history(NewDataP),
%     % TODO put play/2 in CEC (solves CD warning prev)
%     % NewDataPHP =
%     %     play(PrevState, NewDataPH),

%     {next_state, PrevState, NewDataPHP}.
%     % end.
% % }}-
% }}-

% start,   when call is answered
% restart, when a DTMF signal comes in
% NOTE: impossible to time out while collecting digits, because a DTMF signal restarts the timers, and the interdigit timeout is a couple seconds
re_start_inactivity_timer(State) -> % {{-

    timer:cancel( get(inactivity_warning) ),
    timer:cancel( get(inactivity_hangup ) ),

    TimeoutsInMinutes =
        case State of
            % TODO this should be variable; there can be long articles, but after e.g., 3 hours of no activity they may be asleep (nudge: "if you are still there, please press 0 (that will pause and they can always start from the same place)
            {article, _} ->
                {90, 100};
            _ ->
                {10, 12}
        end,
    % TODO Handle `article` state!
    % For example, once a recording finished playing, there is a state change jumping to the next. (TODO: {article, ArticleID} as state?).
    % BUT,
    %   if no DTMF events received in an hour (?) the do the prompt, 10 minutes later kick out
    WarningTimeout    = element(1, TimeoutsInMinutes) * 60 * 1000,
    InactivityTimeout = element(2, TimeoutsInMinutes) * 60 * 1000,

    {ok, IWref} = timer:send_after(WarningTimeout,    inactivity_warning),
    {ok, ITref} = timer:send_after(InactivityTimeout, inactivity_hangup),

    put(inactivity_warning, IWref),
    put(inactivity_hangup,  ITref).
% }}-

common_options(ContentType) % {{-
  when ContentType =:= category
     ; ContentType =:= publication
     ; ContentType =:= article
->
    Star =
        "To go up, press star.",
    Pound =
        "To jump to the next "
        ++ atom_to_list(ContentType)
        ++ ", press pound.",
    Zero =
        "For the main menu, press 0.",

    [Zero, Star, Pound].

% TODO What if there are no choices?
choice_list(Content) ->
    [  "Press "
       ++ integer_to_list(Selection)
       ++ " for "
       ++ Title
       ++ "."
    || #{ title := Title
        , selection := Selection
        }
       <- content:pick(children, Content)
    ].

derive_state(#{ current_content := Content } = _Data) ->
    derive_state(Content);

derive_state(#{ selection := _, type := _ } = Content) ->
    % case maps:get(current_content, Data) of
    case Content of
        #{ selection := 0 } ->
            content_root;
        #{ type := article } ->
            % Implies that ARTICLE_INTRO is always played (even if article is resumed)
            article_intro;
        #{ type := ContentType } ->
            ContentType
    end.
% }}-

% TODO add content history
set_current(Content, Data) ->
    Data#{ current_content := Content }.

get_current(#{ current_content := Content } = _Data) ->
    Content.

next_content(Direction, #{ current_content := CurrentContent } = Data) % {{-
  % `children` and a bad direction return a list which blow up when saved as current_content (hence the explicit whitelisting, instead of just blacklisting `children`)
  when Direction =:= parent;
       Direction =:= first;
       Direction =:= last;
       Direction =:= next;
       Direction =:= prev;
       Direction =:= content_root
->
    NextContent =
        content:pick(Direction, CurrentContent),

    case {NextContent, Direction} of
        % TODO PROD implement states otherwise it will crash
        % DONE? test
        {none, next}  -> {next_state, no_next_item, Data};
        {none, prev}  -> {next_state, no_prev_item, Data};
        {none, first} -> {next_state, no_children, Data};
        {none, last}  -> {next_state, no_children, Data};
        % {none, parent} - this would only be possible if content_root would be the current content, but * is dissabled there (see DTMF processing)
        _ ->
            % NewData = set_current(NextContent, Data),
            % Not simply pattern-matching `NewData` because if "up" results in the root category then the next state is `content_root` and not `category`
            % NextState = derive_state(NewData),
            { next_state
            , derive_state(NextContent)
            , set_current(NextContent, Data)
            }
    end.
    % NewData =
    %     f:pipe
    %       ([ Data
    %        , (f:curry(fun get_content/2))(Direction)
    %        , (f:cflip(fun set_current/2))(Data)
    %        ]),
    % NextState =
    %     derive_state(NewData),

    % { next_state, NextState, NewData }.
% }}-

% TODO INVENTORY! (stringify, curry, composeFlipped, serialize/deserialize, etc.) % {{-
% NOTE `go_to/1` and `retrieve/1` will crash if used improperly. It is not handled, because these are strictly internal functions not intended to be used from anywhere else, and if they crash then the culprit is an error in the surrounding code
% next(Data, Content) ->
%     % { ContentType
%     % , _Selection
%     % , _Metadata
%     % } =
%         content(get, current),

%     {next_state, Content, Data}.

% retrieve(Direction) ->
%     call_content(get, Direction).

% NOTE The corresponding serialization function is in content.erl (also commented out)
% deserialize(
%   { serialized_digraph
%   , VerticeList
%   , EdgeList
%   , NeighbourList
%   , _Cyclicity
%   }
% ) ->
%     Graph =
%     { digraph
%     , Vertices
%     , Edges
%     , Neighbours
%     , true
%     } =
%         digraph:new([cyclic, protected]), % default values made explicit

%     ets:insert(Vertices, VerticeList),
%     ets:insert(Edges, EdgeList),
%     ets:insert(Neighbours, NeighbourList),
%     Graph.

% e.g., `stringify` is also in `content.erl`. How many other similar functions are there? Put them in a utility module.
% curry is not used (is composeFlipped?) but they are interesting and a by-product of experimenting (and procrastinating); figure out what to do with them
stringify(Term) ->
    R = io_lib:format("~p",[Term]),
    lists:flatten(R).

% }}-
% }}-

% vim: set fdm=marker:
% vim: set foldmarker={{-,}}-:
% vim: set nowrap:
