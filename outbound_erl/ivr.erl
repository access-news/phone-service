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
    , realize/0
    , realize/1
    , add_recordings/2
    , publication_guide/0
    ]).

-define(FS_NODE, 'freeswitch@tr2').

% Replaced this with a `main_category()` function at one point but had to revert because needed to use this in guards when handling DTMFs, and that will not work.
-define(CONTENT_ROOT, "/home/toraritte/clones/phone-service/content-root/").
-define(CATEGORIES, {category, ?CONTENT_ROOT}).

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

    logger:debug("==========================="),
    logger:debug("==========================="),
    logger:debug("==========================="),

    % TODO Why was this necessary?
    process_flag(trap_exit, true),

    State = incoming_call,
    Data =
        #{ recvd_digits => ""
         ,  auth_status => unregistered % | registered
         , menu_history => [] % used as a stack
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
    {ok, State, Data}.
% }}-

callback_mode() ->
    [handle_event_function, state_enter].
    % handle_event_function.

terminate(Reason, State, Data) ->
    logger:debug(#{ self() => ["TERMINATE (normal-ish)", #{ data => Data, reason => Reason, state => State }]}).
     % filog:process_log(debug, #{ from => ["TERMINATE", #{ reason => Reason, state => State, data => Data }]}),
     % filog:remove_process_handler(?MODULE).

handle_event(enter, OldState, State, Data) ->
    HistoryFun =
        fun(D) ->
        % If we repeat the state (because playback stopped naturally, and we want to loop) then it is superfluous to add the state again to history.
            case OldState =:= State of
                true ->
                    Data;
                false ->
                    push_history(Data, OldState)
            end
        end,
    PlayFun =
        fun(D) ->
            play(State, D)
        end,
    % ComfortFun =
    %     fun(D) ->
    %         comfort_noise(),
    %         D
    %     end,
    GetNewData =
        composeFlipped(
          [ HistoryFun
          , fun stop_playback/1
          % , ComfortFun
          , PlayFun
          ]
        ),

    {keep_state, GetNewData(Data)};

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
    logger:emergency(#{ event => { EventName, MassagedModErlEvent}}),

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

%% inactivity timers (info) {{-
% The timers are started with `re_start_inactivity_timer/0` when a call comes in, or a DTMF signal is received. The purpose of button presses to change the state, so it is not important to restart the timers on the actual state change, and interdigit timers have a short value compared to the inactivity ones.
% This way is even better, because on invalid category selection the user will remain in the same state as long as they can't input a correct number, but the timers will get reset on each try.
handle_event(
  info,
  inactivity_warning,
  State,
  Data
) ->
    Inactive =
        "Please press 0 if you are still there.",

    % TODO Add notes on how this works.
    % TODO how is this applicable when in {article, _}?
    NewDataP =
        warning(Inactive, Data),
    {keep_state, NewDataP};

handle_event(
  info,
  inactivity_hangup,
  State,
  Data
) ->

    next_menu(
      #{ menu => {hangup, inactivity}
       , data => Data
       , current_state => State
       });
%% }}-

%% DEBUG (info) {{-
%% Catch-all clause to see unhandled `info` events.
handle_event(
  info,  % EventType
  Msg,   % EventContent
  State, % State
  _Data  % Data
) ->
    logger:emergency(#{ self() => ["UNHANDLED_INFO_EVENTS", #{ unknown_msg => Msg, state => State}]}),
    keep_state_and_data;
%% }}-

%%%%%%%%%%%%%%%%%%%%%%%%
%% `internal` clauses %%
%%%%%%%%%%%%%%%%%%%%%%%%

% Hanging up, so stop processing other `internal` events [..] {{-
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
        #{ sendmsg_command => hangup
         , arguments       => ["16"]
         }),
    %% Not stopping the  `gen_statem` process here, because
    %% there will be further events related to the `hangup`
    %% command above. See "CALL_HANGUP" `info` clause below.
    keep_state_and_data;
% }}-

%% INCOMING_CALL (STATE: incoming_call -> greeting) {{-

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
  incoming_call                      = State,
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

    sendmsg(
        #{ sendmsg_command => execute
         , arguments       => ["answer", []]
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
                    , hang_up                       % , EventContent }
                    },
                { unregistered
                , [ DemoTimeout
                  ]
                }
        end,

    { keep_state
    , Data#{ auth_status := NewAuthStatus }
    , TransitionActions
    };
%% }}-

%% CALL_ANSWERED (STATE: greeting -> greeting) {{-

handle_event(
  internal,                               % EventType
  { _UUID                                 % \
  , call_event                            % |
  , #{ "Event-Name" := "CHANNEL_ANSWER" } % | EventContent = MassagedModErlEvent
  },                                      % /
  incoming_call = State,                       % State
  #{ auth_status  := _AuthStatus           % \         Made  explicit  all  values  that  are  known,
   , menu_history := []                   % | Data    because the only state change to this state is
   % , playback_ids := #{}                  % |         from  `incoming_call`, and  whatever has  been
   } = Data                               % /         set there must also be true.
) ->
    % logger:debug(#{ self() => ["CALL_ANSWERED", #{ state => State}]}),

    re_start_inactivity_timer(State),
    next_menu(
      #{ menu => greeting
       , data => Data
       , current_state => State
       });
%% }}-

%% HANDLE_DTMF_FOR_ALL_STATES (internal) {{-
handle_event(
  internal,                    % EventType
  { _UUID                      % \
  , call_event                 % |
  , #{ "DTMF-Digit" := Digit}  % | EventContent = MassagedModErlEvent
  },                           % /
  State,
   % if `ReceivedDigits =/= []`, we are collecting digits
  #{
      recvd_digits := ReceivedDigits
   ,  auth_status := AuthStatus
   ,  menu_history := History
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
    RootState =
        case is_tuple(State) of
            false ->
                State;
            true ->
                element(1, State)
        end,

    case
        #{                state => State
         ,       received_digit => Digit
         % `ReceivedDigits` emptied after eval on `interdigit_timer` timeout
         % ,    collecting_digits => ReceivedDigits =/= []
         }
    of
        % TODO # has no function (except greeting, main_menu, article)
        % Use it to browse categories (step into the next one, like in article?)
        % or forward in history instead of back?

        % === GREETING
        % * (star)   {{- => ?CATEGORIES
        #{                state := greeting = State
         ,       received_digit := "*"
         % ,    collecting_digits := false
         } ->
            next_menu(
              #{ menu => ?CATEGORIES
               , data => Data
               , current_state => State
               });

        % }}-
        % # (pound)  {{- => ?CATEGORIES
        #{                state := greeting = State
         ,       received_digit := "#"
         % ,    collecting_digits := false
         } ->
            next_menu(
              #{ menu => ?CATEGORIES
               , data => Data
               , current_state => State
               });

        % }}-
        % 0          {{- => main_menu
        #{                state := greeting = State
         ,       received_digit := "0"
         % ,    collecting_digits := false
         } ->
            next_menu(
              #{ menu => main_menu
               , data => Data
               , current_state => State
               });

        % }}-
        % [1-9]      {{- => sub-categories (via COLLECT DIGITS)
        %   See COLLECT DIGITS case clause at the end.
        % }}-

        % === MAIN_MENU
        % * (star)  {{- <- Go back (previous menu)
        #{                state := main_menu = State
         ,       received_digit := "*"
         % ,    collecting_digits := false
         } ->
            prev_menu(
              #{ current_state => State
               , data => Data
               });

        % }}-
        % # (pound) {{- => Log in / Favorites (depending on AuthStatus)
        #{                state := main_menu = State
         ,       received_digit := "#"
         % ,    collecting_digits := false
         } ->
            NextMenu =
                case AuthStatus of
                    registered   -> favourites;
                    unregistered -> sign_in
                end,

            next_menu(
              #{ menu => NextMenu
               , data => Data
               , current_state => State
               });

        % }}-
        % 0         {{- => quick_help
        #{                state := main_menu = State
         ,       received_digit := "0"
         % ,    collecting_digits := false
         } ->
            next_menu(
              #{ menu => quick_help
               , data => Data
               , current_state => State
               });

        % }}-
        % 1         {{- => tutorial
        #{                state := main_menu = State
         ,       received_digit := "1"
         % ,    collecting_digits := false
         } ->
            next_menu(
              #{ menu => tutorial
               , data => Data
               , current_state => State
               });

        % }}-
        % 2         {{- => leave_message
        #{                state := main_menu = State
         ,       received_digit := "2"
         % ,    collecting_digits := false
         } ->
            next_menu(
              #{ menu => leave_menu
               , data => Data
               , current_state => State
               });

        % }}-
        % 3         {{- => blindness_services
        #{                state := main_menu = State
         ,       received_digit := "3"
         % ,    collecting_digits := false
         } ->
            next_menu(
              #{ menu => blindness_services
               , data => Data
               , current_state => State
               });

        % }}-
        % 4         {{- => UNASSIGNED (keep state and data)
        % NOTE: Adding explicit clause so that the numbers do not trigger digit collection. Not necessary for this in main_menu as the COLLECT DIGITS clause has a guard, but never hurts to be explicit, and this will be crucial for `{publication, _, _}` and `{article,_,_}` states because there are no guards for that in COLLECT DIGITS (see NOTE there)
        #{                state := main_menu = State
         ,       received_digit := "4"
         % ,    collecting_digits := false
         } ->
            keep_state_and_data;

        % }}-
        % 5         {{- => settings
        #{                state := main_menu = State
         ,       received_digit := "5"
         % ,    collecting_digits := false
         } ->
            next_menu(
              #{ menu => settings
               , data => Data
               , current_state => State
               });

        % }}-
        % 6         {{- => UNASSIGNED (keep state and data)
        % NOTE: Adding explicit clause so that the numbers do not trigger digit collection. Not necessary for this in main_menu as the COLLECT DIGITS clause has a guard, but never hurts to be explicit, and this will be crucial for `{publication, _, _}` and `{article,_,_}` states because there are no guards for that in COLLECT DIGITS (see NOTE there)
        #{                state := main_menu = State
         ,       received_digit := "6"
         % ,    collecting_digits := false
         } ->
            keep_state_and_data;

        % }}-
        % TODO "previous category"
        % 7         {{- => UNASSIGNED (keep state and data)
        % NOTE: Adding explicit clause so that the numbers do not trigger digit collection. Not necessary for this in main_menu as the COLLECT DIGITS clause has a guard, but never hurts to be explicit, and this will be crucial for `{publication, _, _}` and `{article,_,_}` states because there are no guards for that in COLLECT DIGITS (see NOTE there)
        #{                state := main_menu = State
         ,       received_digit := "7"
         % ,    collecting_digits := false
         } ->
            keep_state_and_data;

        % }}-
        % 8         {{- => ?CATEGORIES
        #{                state := main_menu = State
         ,       received_digit := "8"
         % ,    collecting_digits := false
         } ->
            next_menu(
              #{ menu => ?CATEGORIES
               , data => Data
               , current_state => State
               });

        % }}-
        % TODO "next category"
        % 9         {{- => UNASSIGNED (keep state and data)
        % NOTE: Adding explicit clause so that the numbers do not trigger digit collection. Not necessary for this in main_menu as the COLLECT DIGITS clause has a guard, but never hurts to be explicit, and this will be crucial for `{publication, _, _}` and `{article,_,_}` states because there are no guards for that in COLLECT DIGITS (see NOTE there)
        #{                state := main_menu = State
         ,       received_digit := "9"
         % ,    collecting_digits := false
         } ->
            keep_state_and_data;

        % }}-

        % === CATEGORIES (i.e., `{category, [..]}`)
        % * (star)  {{- => Go back (previous menu) OR ignore
        #{                state := {category, _} = State
         ,       received_digit := "*"
         % ,    collecting_digits := false
         } ->
            prev_menu(
              #{ current_state => State
               , data => Data
               });

        % }}-
        % #             => UNASSIGNED (see last case clause)
        % 0         {{- => main_menu
        #{                state := {category, _} = State
         ,       received_digit := "0"
         % ,    collecting_digits := false
         } ->
            next_menu(
              #{ menu => main_menu
               , data => Data
               , current_state => State
               });

        % }}-
        % [1-9]     {{- => sub-categories (via COLLECT DIGITS)
        %   See COLLECT DIGITS case clause at the end.
        % }}-

        % === PUBLICATIONS
        % [1-9] when playing an article
        #{                state := {publication, _, _} = State
         ,       received_digit := "0"
         % ,    collecting_digits := false
         } ->
            next_menu(
              #{ menu => main_menu
               , data => Data
               , current_state => State
               });

        % === PLAY ARTICLE
        % [1-9] when playing an article
        #{                state := {article, _} = State
         ,       received_digit := "0"
         % ,    collecting_digits := false
         } ->
            next_menu(
              #{ menu => main_menu
               , data => Data
               , current_state => State
               });

        % === COLLECT DIGITS
        % [1-9] in any state AKA start collecting digits
        % (except for `main_menu`, `{publication, _, _}`, and `article`) {{-
        % NOTE Most of the guards are unnecessary (as previous clauses have already handled them), and are only here to make this clause explicit. TODO HOWEVER, they may be removed, because all possibilities are explicitly spelled out for each state (so hitting the unassigned 4 in main_menu shouldn't end up here to start collecting digits), and couldn't put the compound states (article & publication) in the guards as they are tuples, guards cannot be nested.
        % Not checking whether we are collecting digits, because {{-
        % this clause should be executed either way when digits 1 to 9 are pressed any time; if false, it means we are starting a new collection, and if true, we should keep collecting until the `interdigit_timer` times out (when the collected digits get evaluated).
        % }}-
        #{
                    state := _State
         , received_digit := Digit
         }
        % These states never collect digits and so [1-9] are instant actions without any IDTs. Listing them here is unecessary, as these digits within these states are called in previous clauses, but doing so makes it nice and explicit.
        when
            RootState =/= main_menu,
            RootState =/= publication,
            RootState =/= article,

        %      % Only take digits in the [1-9] range
             Digit =/= "0",
             Digit =/= "*",
             Digit =/= "#"
        ->
            collect_digits(Data, Digit);

        % }}-
        % 0 in any state when collecting digits {{-
        #{                state := collect_digits
         ,       received_digit := "0"
         }
        ->
            % Same as the previous clause, and zero is just an element now in the DTMF buffer that will get evaluated when the `interdigit_timer` expires
            % Could've put in previous clause, but more explicit this way
            collect_digits(Data, "0");

        % }}-
        % * (star) and # (pound) in any state when collecting digits (except `greeting`) {{-
        % Ignore (i.e., add empty string, will be flattened during eval anyway), but renew `interdigit_timer`
        % When a user presses them accidentally when signing in,
        % or when selecting the category, it will get filtered
        % out.
        #{                state := collect_digits
         ,       received_digit := Digit
         }
        when Digit =:= "*";
             Digit =:= "#"
             % Digit =:= "#",

             % Innecessary; if collecting digits then `greeting` cannot be skipped anyway
             % State =/= greeting
        % Guards and logic reminder {{-
        % (fun ({A, B})
        %      when B =:= x,            B =:= x;                   B =:= z;
        %           A =:= 0;            A =:= 0,                   A =:= 0,
        %           B =:= y             B =:= y                    B =:= y;
        %      ->                                                  A =/=7,
        %          yay;                                            B =:= x
        %      (_) ->
        %          nono
        %  end)    ({0, x}).  yay       ({0, x}).  yay            ({1, x}).  yay
        %          ({1, x}).  nono      ({1, x}).  yay            ({0, z}).  yay
        %          ({0, y}).  yay       ({0, y}).  yay            ({1, z}).  yay
        %          ({1, y}).  yay       ({1, y}).  nono           ({7, z}).  yay
        %                                                         ({0, y}).  yay
        %                                                         ({7, y}).  nono
        %                                                         ({7, x}).  nono
        %                                                         ({6, x}).  yay
        % }}-
        ->
            collect_digits(Data, "");
        % }}-

        % TODO Nothing should ever end up here, so remove once the basics are done
        % EXCEPT when adding a new state, and forgot to add a clause here to deal with DTMF input (e.g., to ignore them completely, such as with "collect_digits")
        UnhandledDigit ->
            logger:emergency(#{ self() => ["UNHANDLED_DIGIT", UnhandledDigit, {state, State}]}),
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
     , "Application" := "speak"
     , "Application-UUID" := ApplicationUUID
     }                            % | EventContent = MassagedModErlEvent
  } = E,                           % /
  State,
  % #{ prev_state := PrevState } = Data
  #{ playbacks := Playbacks
   } = Data
) ->
    % PlaybackName =
    %     extract_playback_name(ApplicationUUID),

    logger:debug(#{ a => "HANDLE_CHANNEL_EXECUTE_COMPLETE", bapp_id => ApplicationUUID, state => State}),

    % This dancing around is because functions in `maps` never fail, and this `handle_event/4` clause should crash if an `ApplicationUUID` is not in `Playbacks` as it would mean that a playback has been started without saving its ID in the `gen_statem` data, which should not happen. All menus use only `speak` (for now), so anything that gets in here is a playback that has been stopped (one way or another).
    % #{ ApplicationUUID :=
    %     #{ playback_name := PlaybackName
    %      ,    is_stopped := IsStopped
    %      }
    % } = Playbacks,
    { ApplicationUUID
    , PlaybackName
    , IsStopped
    } =
        proplists:lookup(ApplicationUUID, Playbacks),
    NewPlaybacks =
        proplists:delete(ApplicationUUID, Playbacks),
    NewData =
        Data#{ playbacks := NewPlaybacks },

    % logger:debug(#{event => E}),

    % Document the six possible cases with truth table and their notes
    case
        #{    current_state => State
         , stopped_playback => PlaybackName
         ,       is_stopped => IsStopped
         }
    of
        % The only playback that can be stopped in `greeting` is when the `greeting` playback finished playing, and instead of looping like in other menus, `greeting` should only play once, and should transition to `?CATEGORIES`
        % `is_stopped` is set to expect false, because it should not be possible to receive a `CHANNEL_EXECUTE_COMPLETE` of greeting in greeting that is true. Any input results in moving to another state while stopping greeting, therefore the resulting CHANNEL_EXECUTE_COMPLETE will arrive in some other state. And if greeting finished playing, it means we do not loop, but go to ?CATEGORIES.
        #{    current_state := greeting
         , stopped_playback := greeting
         ,       is_stopped := false
         }
        ->
    logger:debug(#{ a => "HANDLE_CHANNEL_EXECUTE_COMPLETE", the_case => {State, PlaybackName} }),
            next_menu(
              #{ menu => ?CATEGORIES
               , data => NewData
               , current_state => greeting
               });

        % Playback stopped naturally, repeat menu.
        #{    current_state := _State
         , stopped_playback := warning
         ,       is_stopped := false
         }
        ->
    logger:debug(#{ a => "HANDLE_CHANNEL_EXECUTE_COMPLETE", the_case => {State, warning, false} }),
            % comfort_noise(2000),
            repeat_menu(
                #{ menu => State
                 , data => NewData
                 });

        % Warning playback interrupted by user input; either to go to another menu (so a playback is already playing), or another warning (so ignore a stopped warning, because the other warning is running, and when it stops naturally, it will repeat the current menu (see above))
        #{    current_state := _State
         , stopped_playback := warning
         ,       is_stopped := true
         }
        ->
    logger:debug(#{ a => "HANDLE_CHANNEL_EXECUTE_COMPLETE", the_case => {State, warning, true} }),
            {keep_state, NewData};

        % playback has been stopped but state remains the same (e.g., to play `invalid_selection/0`), so ignore. Once the warning finishes, it will repeat the same state menu
        #{ is_stopped := true }
        when State =:= PlaybackName
        ->
    logger:debug(#{ a => "HANDLE_CHANNEL_EXECUTE_COMPLETE", the_case => {State, PlaybackName, true} }),
            {keep_state, NewData};

        % playback stopped naturally in the same state where it was started, so repeat it.
        #{ is_stopped := false }
        when State =:= PlaybackName
        ->
    logger:debug(#{ a => "HANDLE_CHANNEL_EXECUTE_COMPLETE", the_case => {State, PlaybackName, false} }),
            % comfort_noise(2000),
            repeat_menu(
                #{ menu => State
                 , data => NewData
                 });

        % CHANNEL_EXECUTE_COMPLETE "residue" after a state change
        #{ is_stopped := true }
        when State =/= PlaybackName ->
    logger:debug(#{ a => "HANDLE_CHANNEL_EXECUTE_COMPLETE", the_case => {State, PlaybackName} }),
            {keep_state, NewData};

        % the only possibility not checked yet (unless I'm an idiot), and this shouldn't be possible. It would mean that a playback has stopped naturally, and `gen_statem` wandered off into another state.
        % As the above should never happen, this is also good to test whether I did something wrong, and it actually does happen by making the process crash.
        #{ is_stopped := false }
        when State =/= PlaybackName ->
    logger:debug(#{ a => "HANDLE_CHANNEL_EXECUTE_COMPLETE", the_case => {State, PlaybackName} }),
            % BUT this clause may be a possibility when adding `collect_digits` state - user hits [1-9] to start collection in eligible states, the playback is running, user has two seconds the enter the next digit, AND the edge case happens: the playback stops naturally, and we are in another state
            % WHAT TO DO? probably just ignore (i.e., {keep_state, NewData}) because the interdigit_timer will time out in a second, triggering evaluation of collected digits, so depending on the outcome there will be a warning, go back to the previous menu, or go forth to the selected one etc.
            crash_because_you_are_an_idiot
    end;

        % % `greeting` stopped (for whatever reason; no loop anyway) {{-
        % RESERVED Playback stopped by `unregistered_timer`
        % RESERVED Exit prompt finished playing -> hang up


        % %     IN main_menu {{-
        % % An example on how to get here: {{-
        % % 1. playback started in CALL_ANSWERED via `enter_menu/1`
        % %    (state change: incoming_call -> greeting)
        % % 2. 0 pressed while greeting was playing, async playback stop command issued, and state changed from `greeting` to `main_menu` via `leave_menu/1`
        % % 3. A `CHANNEL_EXECUTE_COMPLETE` FreeSWITCH event arrived, signifying that something stopped a playback. The current state is main_menu (because of step 2. above), the map in Data#playback_ids only has one entry containing the application UUID when starting a playback in greeting. If the guard matches, it means that this event is a confirmation from FreeSWITCH is from the stopped greeting.
        % % }}-
        % #{        state := main_menu
        %  , playback_ids := #{ greeting := PlaybackID }
        %  }
        % when PlaybackID =:= ApplicationUUID
        % ->
        %     logger:debug("HANDLE_CHANNEL_EXECUTE_COMPLETE - greeting stop IN main_menu"),
        %     play_menu(main_menu, Data);
        % % }}-

        % %     IN ?CATEGORIES {{-
        % #{        state := ?CATEGORIES
        %  , playback_ids := #{ greeting := PlaybackID }
        %  }
        % when PlaybackID =:= ApplicationUUID
        % ->
        %     logger:debug("HANDLE_CHANNEL_EXECUTE_COMPLETE - greeting stop IN ?CATEGORIES"),
        %     play_menu(?CATEGORIES, Data);
        % % }}-

        % %     IN a sub-category (i.e., `{category, [..]}`) {{-
        % #{        state := State
        %  , playback_ids := #{ greeting := PlaybackID }
        %  }
        % when element(1, State) =:= category,
        %      PlaybackID =:= ApplicationUUID
        % ->
        %     logger:debug("HANDLE_CHANNEL_EXECUTE_COMPLETE - greeting stop IN " ++ s(State)),
        %     play_menu(State, Data);
        % % }}-

        % %     IN greeting (i.e., playback ran its course) {{-
        % #{        state := greeting
        %  % NOTE This and the guard shouldn't even be necessary, {{-
        %  % because the only other playback that can end in `greeting` is the comfort noise, but why not check it if it is already there (plus I may have messed up)
        %  % }}-
        %  , playback_ids := #{ greeting := PlaybackID }
        %  }
        % when PlaybackID =:= ApplicationUUID
        % ->
        %     logger:debug("HANDLE_CHANNEL_EXECUTE_COMPLETE - greeting stop IN greeting (natural stop)"),
        %     enter_menu(
        %       #{ from => greeting
        %        , to   => ?CATEGORIES
        %        , data => Data
        %        }
        %     );
        % % }}-

        % %     IN any other state, start the menu where it arrived {{-
        % % TODO is this going to be a general case?

        % % TODO so how to deal with events like the 0# transition?
        % % 1. incoming_call -> greeting starts
        % % 2. (STATE: greeting) 0 pressed
        % % 3. (STATE: greeting)
        % %    FreeSWITCH DTMF event processed in HANDLE_DTMF_FOR_ALL_STATES the case clause,
        % %    issues `leave_menu/1` (requests async playback stop, and sets state to main_menu)

        % % SCENARIO I
        % % 4. (STATE: main_menu)
        % %    CHANNEL_EXECUTE_COMPLETE (confirming that playback stopped) processed in
        % %    HANDLE_CHANNEL_EXECUTE_COMPLETE (`enter_menu/1` resulting in `main_menu` playback

        % % SCENARIO II
        % % 4. (STATE: main_menu) # pressed
        % % 5. DTMF event processed in HANDLE_DTMF_FOR_ALL_STATES
        % %    calling `leave_menu/1` (async playback stop, and go to sign_in/favorites)
        % %    ===> will not result any event, because there is no other playback
        % % 6. (STATE: sign_in/favorites) CHANNEL_EXECUTE_COMPLETE comes in for the stopped greeting
        % % 7. HANDLE_CHANNEL_EXECUTE_COMPLETE clause
        % #{        state := State
        %  , playback_ids := #{ greeting := PlaybackID }
        %  }
        % when PlaybackID =:= ApplicationUUID
        % ->
        %     logger:debug("HANDLE_CHANNEL_EXECUTE_COMPLETE - greeting stop IN " ++ s(State)),
        %     % Ignore this event entirely
        %     keep_state_and_data;
        % % }}-

    % end;
        % % }}-
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
    % logger:debug(#{event => E}),
    % logger:debug(#{ self() => ["OTHER_INTERNAL_CALL_EVENT", #{ event_name => EventName, fs_event_data => FSEvent,  state => State}]}),
    logger:debug(#{ self() => ["UNKNOWN_INTERNAL_CALL_EVENT", #{ event_name => EventName, state => State}]}),
    % logger:debug(""),
    keep_state_and_data;

handle_event(
  internal,                          % EventType
  Msg,
  State,
  _Data                               % Data
) ->
    logger:emergency(#{ self() => ["UNKNOWN_INTERNAL", #{ unknown_msg => Msg, state => State}]}),
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

%% unregistered_timer {{-
handle_event(
  {timeout, unregistered_timer}, % EventType
  hang_up,                       % EventContent
  State,
  #{ auth_status := AuthStatus } = Data
) ->
    % logger:debug(#{ self() => ["UNREGISTERED_TIMEOUT", #{ data => Data, state => State }]}),

    case AuthStatus of

        registered ->
            % ignore if user already signed in in the meantime
            keep_state_and_data;

        unregistered ->
            next_menu(
              #{ menu => {hangup, demo}
               , data => Data
               , current_state => State
               })
    end;
%% }}-

%% interdigit_timer {{-
handle_event(
  {timeout, interdigit_timer}, % EventType
  eval_collected_digits,        % EventContent
  State,
  #{ recvd_digits := ReceivedDigits } = Data
) ->
    % TODO Prod system along, or .
    % Always clear DTMF buffer when the `interdigit_timer` expires, because this is the point the buffer is evaluated. Outcome is irrelevant, because at this point user finished putting in digits, hence waiting for the result, and so a clean slate is needed.
    NewData =
        Data#{ recvd_digits := [] },

    case eval_collected_digits(ReceivedDigits, State) of

        % `greeting`  -> `Category`  state change  is a  valid
        % one, so when there is an invalid entry in `greeting`
        % when collecting  digits, play  `?CATEGORIES`, ignore
        % the return value  (because `repeat_menu/1` ends with
        % `keep_state`), and drop to `?CATEGORIES
        invalid when State =:= greeting ->
            logger:debug(#{ a => "INTERDIGIT_TIMEOUT (invalid in greeting)", collected_digits => ReceivedDigits, state => State}),
            NewDataWithUpdatedPlaybacks =
                warning( invalid_selection(), NewData ),
            {next_state, ?CATEGORIES, NewDataWithUpdatedPlaybacks};

        invalid ->
            logger:debug(#{ a => "INTERDIGIT_TIMEOUT (simply invalid)", collected_digits => ReceivedDigits, state => State}),
            NewDataWithUpdatedPlaybacks =
                warning( invalid_selection(), NewData ),
            {keep_state, NewDataWithUpdatedPlaybacks};

        Category ->
            logger:debug(#{ a => "INTERDIGIT_TIMEOUT (category exists)", collected_digits => ReceivedDigits, state => State, category => Category}),
            next_menu(
              #{ menu => Category
               , data => NewData
               , current_state => State
               })
    end.
%% }}-

% TODO Eval on 2 digits immediately to speed up things
collect_digits( % {{-
  #{recvd_digits := ReceivedDigits} = Data,
  Digit % string
) ->
    NewData =
        Data#{recvd_digits := ReceivedDigits ++ Digit},
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
    {next_state, collect_digits, NewData, [ InterDigitTimer ]}.
% }}-

eval_collected_digits([_|_] = ReceivedDigits, State) -> % {{-
    CurrentCategoryDir =
        case State of
            greeting ->
                ?CONTENT_ROOT;

            {category, CategoryDir} ->
                CategoryDir

            % Not checking for {publication, _, _} because digit collection is disallowed there
        end,

    SelectionDir =
        filename:join(
          CurrentCategoryDir,
          ReceivedDigits
        ),

    case filelib:is_dir(SelectionDir) of
        false ->
            invalid;
        true ->
            {category, SelectionDir}
    end.
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
event_uuid_header(UUID) ->
    {"Event-UUID", UUID}.

sendmsg_headers(execute, [App, Args], UUID) when is_list(Args) ->
    %% TODO "loops"  header   and  alternate  format   for  long
    %%      messages (is it needed here?)  not added as they are
    %%      not needed yet.
    [ {"execute-app-name", App}
    , {"execute-app-arg", Args}
    , event_uuid_header(UUID)
    ];

sendmsg_headers(hangup, [HangupCode], _UUID) ->
    %% For hangup codes, see
    %% https://freeswitch.org/confluence/display/FREESWITCH/Hangup+Cause+Code+Table
    [{"hangup-cause", HangupCode}];

%% This will  blow up,  one way or  the other,  but not
%% planning to get there anyway.
sendmsg_headers(_SendmsgCommand, _Args, _UUID) ->
    % filog:process_log(
    %   emergency,
    %   ["`sendmsg` command not implemented yet"
    %   , SendmsgCommand
    %   , Args
    %   ]
    % ),
    [].

% `PlaybackName` is to identify the given playback. Menus, such as greeting, main_menu, etc., are also states, so most of the time this will be the state of the `gen_statem` process. But sometimes it just needs a label that it is a warning etc.
do_sendmsg(
  #{ sendmsg_command := SendmsgCommand
   , arguments       := SendmsgArgs
   , playback_name   := PlaybackName
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
    PoorMansUUID =
        erlang:make_ref(),
    ApplicationUUID =
           stringify(PlaybackName)
        ++ "|"
        ++ stringify(PoorMansUUID),
    FinalHeaders =
        [{"call-command", atom_to_list(SendmsgCommand)}]
        ++ sendmsg_headers(SendmsgCommand, SendmsgArgs, ApplicationUUID)
        ++ LockHeaderList,
    fsend({sendmsg, get(uuid), FinalHeaders}),
    % logger:debug(#{ a => ["DO_SENDMSG", #{ app_id => ApplicationUUID, final_headers => FinalHeaders }]}),
    ApplicationUUID;

do_sendmsg(
  #{ sendmsg_command := SendmsgCommand
   , arguments       := SendmsgArgs
   , event_lock      := IsLocked
   } = Args
) ->
    % logger:debug(#{ a => ["DO_SENDMSG", #{args => Args}]}),
    do_sendmsg(
      Args#{ playback_name => not_applicable }
    ).

sendmsg(
  #{ sendmsg_command := _
   , arguments       := SendmsgArgs
   } = Args
) ->
    logger:debug(#{ a => ["SENDMSG", #{args => Args}]}),
    do_sendmsg(Args#{ event_lock => false }).

sendmsg_locked(
  #{ sendmsg_command := _
   , arguments       := SendmsgArgs
   } = Args
) ->
    do_sendmsg(Args#{ event_lock => true }).
%% }}-

fsend(Msg) ->
    %% Why the `lofa` atom:
    %% https://stackoverflow.com/questions/58981920/
    {lofa, ?FS_NODE} ! Msg.

stop_playback(#{ playbacks := [] } = Data) ->
    Data;

% Nothing is playing and the most recent playback has been stopped before.
% For example, ?CATEGORIES is playing, "*" is sent when nothing is in history, `warning/?` stops playback, warning starts playing, ends naturally, so HANDLE_CHANNEL_EXECUTE_COMPLETE get clears {warning, false}, and calls that scenario in its `case`, which is `repeat/?`, that also has a `stop_playback/1`, and the most recent playback is the stopped ?CATEGORIES. Subsequent HANDLE_CHANNEL_EXECUTE_COMPLETE clauses will clear out these entries. (or should...)
% TODO make sure that the Data#playbacks stack gets cleared properly
stop_playback(#{ playbacks := [{_, _, true}|_] } = Data) ->
    Data;

% Stop the currently playing prompt
% (but just in case, end everything, even though only one should be playing only)
stop_playback( % {{-
  #{ playbacks :=
     [ { ApplicationUUID % |
       , PlaybackName    % | currently playing
       , false           % |
       }
       | Rest
     ]
   } = Data
) ->
    % logger:debug("stop playback"),
    % TODO Should this  be `bgapi`? Will the  synchronous `api`
    %      call wreak havoc when many users are calling?
    fsend({api, uuid_break, get(uuid) ++ " all"}),
    Data#{ playbacks := [{ApplicationUUID, PlaybackName, true}|Rest] }.
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

comfort_noise(Milliseconds) -> % {{-
    % logger:debug("play comfort noise"),
    ComfortNoise =
           "silence_stream://"
        ++ integer_to_list(Milliseconds)
        ++ ",1400",
    sendmsg_locked(
        #{ sendmsg_command => execute
         , arguments       => ["playback", ComfortNoise]
         , playback_name   => comfort_noise
         }).
% }}-

comfort_noise() ->
    comfort_noise(750).

% TODO make a module for utterances? {{-
sign_up() ->
    "If you would like to sign up for Access News, please call us at 916, 889, 7519.".

invalid_selection() ->
    "Invalid selection. Please try again from the following categories.".
% }}-

% TODO Try out `mod_vlc` to play aac and m4a files. {{-
% sendmsg(UUID, execute, ["playback", "/home/toraritte/clones/main.mp3"]),
% sendmsg_locked(UUID, execute, ["playback", "/home/toraritte/clones/phone-service/ro.mp3"]),
% }}-
play(
  greeting = State,
  #{ auth_status := AuthStatus
   } = Data
) -> % {{-
    % logger:debug("play greeting"),
    Anchor = "Welcome to Access News, a service of Society For The Blind in Sacramento, California, for blind, low-vision, and print-impaired individuals.",
    PoundOrStar = "If you know your selection, you may enter it at any time, or press star or pound to skip to listen to the main categories.",
    Unregistered =
        case AuthStatus of
            registered ->
                "";
            unregistered ->
                "You are currently in demo mode, and have approximately 5 minutes to try out the system before getting disconnected. To log in, dial 0, pound, followed by your code."
                ++ sign_up()
                ++ "To leave a message with your contact details, dial 0 2."
        end,
    GoToTutorial = "To listen to the tutorial, dial 01.",
    GoToBlindnessServices = "To learn about other blindness services, dial 03.",
    LeaveMessage = "If you have any questions, comments, or suggestions, please call 916 889 7519, or dial 02 to leave a message.",

    speak(
      #{ playback_name => State
       , data => Data
       , text =>
           stitch(
             [ Anchor
             , Unregistered
             , PoundOrStar
             , GoToTutorial
             , GoToBlindnessServices
             , LeaveMessage
             ])
       }
    );
% }}-

play(
  main_menu = State,
  #{ auth_status := AuthStatus
   } = Data
) -> % {{-
    Anchor = "Main menu.",
    Zero = "For quick help, press 0.",
    Star = "To go back to he previous menu, press star.",
    Pound =
        case AuthStatus of
            registered ->
                "For your favorites, ";
            unregistered ->
                "To log in, "
        end
        ++ "press pound.",
    Eight = "To go back to the main categories, press 8.",
    Five = "For settings, press 5.",
    One = "To start the tutorial, press 1.",
    Two = "To leave a message, press 2.",
    Three = "To learn about other blindness resources, press 3.",

    speak(
      #{ playback_name => State
       , data => Data
       , text =>
           stitch(
             [ Anchor
             , Zero
             , One
             , Star
             , Pound
             , Eight
             , Five
             , Two
             , Three
             ])
       }
    );
% }}-

% play({category, Vertex}, _Data) ->
play({category, CategoryDir} = State, Data) -> % {{-
    {category, _, Anchor} =
        get_meta(CategoryDir),
    Zero =
        "For the main menu, press 0.",
    Star =
        "To go back to he previous menu, press star.",
    SubCategories =
        stitch(
          list_category_entries(CategoryDir)
        ),
    % EnterFirstCategory = "To start exploring the categories one by one, press pound, 9 to enter the first category.",
    % CategoryBrowsePound = "To enter the next item, press the pound sign. Dial pound, 0 to get back into the previous item.",

    speak(
      #{ playback_name => State
       , data => Data
       , text =>
           stitch(
             [ Anchor
             , Zero
             , Star
             , SubCategories
             ])
       }
     );
% }}-

play({hangup, demo} = State, Data) -> % {{-
    speak(
      #{ playback_name => State
       , data => Data
       , text =>
           stitch(
             [ "End of demo session."
             , sign_up()
             , "Thank you for trying out the service!"
             ])
       }
    );
% }}-

play({hangup, inactivity} = State, Data) -> % {{-
    speak(
      #{ playback_name => State
       , data => Data
       , text => "Goodbye."
       }).
% }}-

metafile_name() ->
    "meta.erl".

get_meta(CategoryDir) -> % {{-
    MetaPath =
        filename:join(
          CategoryDir,
          metafile_name()
        ),
    {ok, Meta} =
        file:script(MetaPath),
    Meta.
% }}-

list_category_entries(CategoryDir) -> % {{-
    { ok
    , SubCategoryDirectories
    } =
        file:list_dir(CategoryDir),
    MetaList =
        lists:map(
          fun(SubDir) ->
              MetaPath =
                  filename:join([CategoryDir, SubDir, metafile_name()]),
              {ok, {_, N, SubCategory} } =
                  file:script(MetaPath),

              "Press "
              ++ integer_to_list(N)
              ++ " for "
              ++ SubCategory
              ++ "."
          end,
          SubCategoryDirectories -- [metafile_name()]
        ),
    ordsets:from_list(MetaList).
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

speak( % {{-
  #{ playback_name := PlaybackName
   , data := #{ playbacks := Playbacks } = Data
   , text := Text
   }
) ->
    % return the application UUID string.
    ApplicationUUID =
        sendmsg_locked(
          #{ sendmsg_command => execute
           , arguments       => ["speak", "flite|kal|" ++ Text]
           , playback_name   => PlaybackName
           }),
    Playback =
        { ApplicationUUID
        , PlaybackName % Only for convenience; already part of `ApplicationUUID`.
        , false % Has the playback been stopped?
        },
        % #{ playback_name => PlaybackName
        %  ,    is_stopped => false
        %  },
    NewPlaybacks =
        [ Playback | Playbacks ],
        % Playbacks#{ ApplicationUUID => Playback },

    Data#{ playbacks := NewPlaybacks }.
% }}-

stitch([Utterance]) ->
    Utterance;
stitch([Utterance|Rest]) ->
    Utterance ++ " " ++ stitch(Rest).
%% }}-

%% Push/pop history {{-
push_history(Data, State)
when State =:= incoming_call;
     State =:= greeting;
     % State =:= main_menu;
     % State =:= quick_help;
     State =:= collect_digits;
     % TODO These are compound states!
     element(1, State) =:= warning
     % State =:= publication;
     % State =:= article
->
    push_history(Data, {skip, State});

push_history(#{ menu_history := History } = Data, State) ->
    logger:debug(#{ self() => ["PUSH_HISTORY", {history, History}, {state, State}]}),
    Data#{ menu_history := [State | History] }.

% Why not check if history is empty? {{-
% ====================================================
% Because it will (or at least should) not happen: Call comes in, FreeSWITCH start `ivr.erl`, `init/0` sets history to `[?CATEGORIES]`, meaning that states before that (i.e., `incoming_call` and `greeting`) can never be revisited. When traversing the history backwards with * (star), `HANDLE_DTMF_FOR_ALL_STATES` will make sure that when the `?HISTORY_ROOT` is reached, pressing the * (star) will be ignored (see `case` clause starting with comment "Go up/back").
% UPDATE
% What is said above still holds, but the root is now [].
% }}-
pop_history(#{ menu_history := [] } = Data) ->
    { ?CATEGORIES
    , Data
    };

% TODO adjust when final form of `article` state is known
pop_history(
  #{ menu_history :=
     [ {article, _}
     , _Publication
     , Category
     | RestHistory
     ]
   } = Data
) ->
    pop_history(Data#{ menu_history := [Category|RestHistory] });

pop_history(#{ menu_history := [{skip, _} | RestHistory] } = Data) ->
    pop_history(Data#{ menu_history := RestHistory });

pop_history(#{ menu_history := [PrevState | RestHistory] } = Data) ->
    logger:debug(#{ self() => ["POP_HISTORY", {history, [PrevState|RestHistory]}]}),
    { PrevState
    , Data#{ menu_history := RestHistory }
    }.
% }}-

% Playback keeps going while accepting DTMFs, and each subsequent entry has a pre-set IDT
% 1-9 trigger collection (all states except some, see HANDLE_DTMF_FOR_ALL_STATES)
% *, 0, # are single digit instant actions
% (TODO make users able to set it)

% TODO warning will become a new state, so eliminate this clause
%      and create a `play` clause for it
warning(WarningPrompt, Data) -> % {{-
    logger:debug("SPEAK_WARNING: " ++ WarningPrompt),
    NewData = stop_playback(Data),
    comfort_noise(),
    speak(
      #{ playback_name => warning
       , data => NewData
       , text => WarningPrompt
       }).
% }}-

% never push history (no point because just replaying menu prompt)
repeat_menu( % {{-
  #{ menu := Menu
   , data := Data
   }
 ) ->
    logger:debug("REPEAT_MENU"),
    % Not necessary when playback ends naturally,
    % but calling it multiple times doesn't hurt.
    % NewDataP = stop_playback(Data),
    % comfort_noise(),
    % NewDataPP = play(Menu, NewDataP),
    {keep_state, NewDataPP}.
% }}-

%       = NextState
next_menu(NextMenu, Data) ->  % {{-
  % #{ menu := NextMenu
  %  , data := Data
  %  , current_state := CurrentState
  %  }
% )
% when CurrentState =/= NextMenu % use `repeat_menu/3` otherwise
% ->
    logger:debug("NEXT_MENU"),

    % Update menu history (if permitted)
    % NewDataH =
    %     push_history(Data, CurrentState),
        % case {CurrentState, NextMenu} of {{-

        %     % TODO re-write so that always pushing history; more straightforward, plus extra debug info. mark states that shouldn't be revisited by user via *
        %     % State changes when history is not updated  with prev
        %     % state (corner cases)
        %     % ====================================================
        %     % NOTE: This is adding behaviour to `next_menu/1`, which {{-
        %     % I do not like, but these are the only cases when history is not pushed (so far). Another advantage is that all corner cases are listed in one place.
        %     % }}-
        %     % NOTE: `main_menu` -> `?CATEGORIES` {{-
        %     % Go to `?CATEGORIES` from `main_menu` (only forward option from `main_menu`), but do not add `main_menu` to history
        %     % NOTE Currently it is possible to accumulate `?CATEGORIES` in history by going to `main_menu` and forward to `?CATEGORIES`, as it will add `?CATEGORIES` each time but leaving it as is
        %     % }}-
        %     {main_menu, ?CATEGORIES}  -> Data;
        %     % {{-
        %     %   + `greeting` -> `?CATEGORIES` (i.e., when pressing * or #  in `greeting` or playback stops)
        %         % No `next_menu/1` here to keep the history empty so that `greeting` cannot be revisited, making `?CATEGORIES` the root. Whatever navigations happen there
        %         % Why not keep state? Reminder: 0# (not very relevant in this particular case, but keeping state changes consistent makes it easier to think about each `handle_event/4` clause. Read note at the very end of HANDLE_DTMF_FOR_ALL_STATES `handle_event/4` clause)
        %     % }}-
        %     {greeting, ?CATEGORIES}   -> Data;
        %     {greeting, main_menu}     -> Data;
        %     {incoming_call, greeting} -> Data;

        %     % On any other state change
        %     _ -> push_history(Data, CurrentState)
        % end,
    % }}-

    % NewDataP = stop_playback(Data),
    % comfort_noise(),

    % TODO put play/2 in CEC
    % NewDataPP =
    %     play(NextMenu, NewDataP),

    {next_state, NextMenu, NewDataPP}.
% }}-

prev_menu(#{ menu_history := History } = Data) -> % {{-
  % #{ current_state := CurrentState
  %  , data := #{ menu_history := History } = Data
  %  }
% ) ->
    logger:debug("PREV_MENU"),
    % case {CurrentState, History} of {{-
    %     {?CATEGORIES, []} -> % {{-
    %     % Nowhere to go back to; give warning and repeat
    %         EmptyHistory = "Nothing in history.",
    %         NewDataP =
    %             warning(EmptyHistory, Data),

    %         {keep_state, NewDataP};

    %     % }}-
    %     {main_menu, []} -> % {{-
    %     % The only  way this is  (or should be)  possible when
    %     % coming to `main_menu` from `greeting` (`greeting` is
    %     % not pushed  to history  during that  transition; see
    %     % `next_menu/1`).
    %         next_menu(
    %           #{ menu => ?CATEGORIES
    %            , data => Data
    %            , current_state => main_menu
    %            });
    %     % }}-
    %     _ ->
    % }}-

    % NewDataP = stop_playback(Data),
    % comfort_noise(),
    {PrevState, NewDataPH} =
        pop_history(NewDataP),
    % TODO put play/2 in CEC (solves CD warning prev)
    % NewDataPHP =
    %     play(PrevState, NewDataPH),

    {next_state, PrevState, NewDataPHP}.
    % end.
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

% TODO put in another module {{-
% Section I is to abstract away the details to access Access News media content and navigation (e.g., show next/previous category/article, list category/publication).
% Section II is there because the Access News web service is not yet ready, and so the content will be served locally at first. Section I hides these and any future implementation details.

% I. Content-access abstraction {{-

% }}-
% II. Access News web service model {{-

% NOTE for the resulting graph when determining neighbours:
% to avoid excessive amount of connections (and facing the problems updating them would cause), it will be done by finding the parent category, and adding/subtracting one from their "ID".
% The publication guide below is just a representation of future data of the yet-to-be-implemented core web service, and its data may not contain such IDs, but that could be done on this end by ordering and adding that via a script.
publication_guide() -> % {{-
    [ { {category, 1, "Store sales advertising"}
      , [ { {category, 1, "Grocery stores"}
          , [ {publication, 1, "Safeway"}
            , {publication, 2, "Raley's"}
            , {publication, 3, "La Superior"}
            , {publication, 4, "Food source"}
            , {publication, 5, "Savemart"}
            , {publication, 6, "Foods Co"}
            , {publication, 7, "Trader Joe's"}
            , {publication, 8, "Sprouts"}
            , {publication, 9, "Lucky Supermarkets"}
            ]
          }
        , { {category, 2, "Drug stores"}
          , [ {publication, 1, "CVS"}
            , {publication, 2, "Rite Aid"}
            , {publication, 3, "Walgreen's"}
            ]
          }
        , { {category, 3, "Discount stores"}
          , [ {publication, 1, "Target"}
            , {publication, 2, "Walmart"}
            ]
          }
        ]
      }

    , { {category, 2, "Sacramento newspapers and magazines"}
      , [ { {category, 1, "Sacramento newspapers"}
          , [ {publication, 1, "Sacramento Bee"}
            , {publication, 2, "Sacramento News & Review"}
            , {publication, 3, "Sacramento Press"}
            , {publication, 4, "Sacramento Business Journal"}
            , {publication, 5, "East Sacramento News by Valley Community Newspapers"}
            , {publication, 6, "The Land Park News by Valley Community Newspapers"}
            , {publication, 7, "The Pocket News by Valley Community Newspapers"}
            ]
          }
        , { {category, 2, "Sacramento magazines"}
          , [ {publication, 1, "Comstocks"}
            , {publication, 2, "SacTown"}
            , {publication, 3, "Sacramento Magazine"}
            ]
          }
        ]
      }

    , { {category, 3, "Greater Sacramento area newspapers"}
      , [ {publication, 1, "Carmichael Times"}
        , {publication, 2, "Arden Carmichael News"}
        , {publication, 3, "California Kids"}
        , {publication, 4, "Davis Enterprise"}
        , {publication, 5, "Roseville Press Tribune"}
        , {publication, 6, "Woodland Daily Democrat"}
        , {publication, 7, "Carmichael Times"}
        , {publication, 8, "Auburn Journal"}
        , {publication, 9, "Grass Valley-Nevada City Union"}
        , {publication, 10, "Arden Carmichael News by Valley Community Newspapers"}
        , {publication, 11, "El Dorado County Mountain Democrat"}
        ]
      }

    , { {category, 4, "Central California newspapers"}
      , [ {publication, 1, "Modesto Bee"}
        , {publication, 2, "Stockton Record"}
        ]
      }

    , { {category, 5, "San Francisco and Bay Area newspapers"}
      , [ {publication, 1, "Vallejo Times Herald"}
        , {publication, 2, "Santa Rosa Press Democrat"}
        , {publication, 3, "SF Gate"}
        , {publication, 4, "San Francisco Bay Guardian"}
        , {publication, 5, "East Bay Times"}
        , {publication, 6, "SF Weekly"}
        , {publication, 7, "KQED Bay Area Bites"}
        ]
      }

    , { {category, 6, "Northern California newspapers"}
      , [ {publication, 1, "Fort Bragg Advocate News"}
        , {publication, 2, "The Mendocino Beacon"}
        , {publication, 3, "Humboldt Senior Resource Center's Senior News"}
        , {publication, 4, "North Coast Journal"}
        , {publication, 5, "Mad River Union"}
        , {publication, 6, "Eureka Times Standard"}
        , {publication, 7, "Ferndale Enterprise"}
        ]
      }
    ].
% }}-

write_meta_file({_, _, _} = Category, Dir) -> % {{-
    MetaFilePath =
        filename:join(Dir, metafile_name()),
    file:write_file(
      MetaFilePath,
      stringify(Category) ++ "."
    ),
    Dir.
% }}-

make_dir_and_meta_file({_, N, _} = Category, Path) -> % {{-
    Dir =
        filename:join(
          Path,
          integer_to_list(N)
        ),
    file:make_dir(Dir),
    write_meta_file(Category, Dir).
% }}-

realize() ->
    realize(?CONTENT_ROOT).

realize(ContentRoot) -> % {{-
    case file:make_dir(ContentRoot) of
        ok ->
            write_meta_file(
              {category, 0, "Main category"},
              ContentRoot
            ),
            realize(publication_guide(), ContentRoot);
        {error, _} = Error ->
            Error
    end.
% }}-

realize( % {{-
  [ { {category, _, _} = Category
    , [_|_] = SubCategories
    }
    | Rest
  ],
  Path
)
->
    NewPath =
        make_dir_and_meta_file(Category, Path),

    realize(SubCategories, NewPath),
    realize(Rest, Path);
% }}-

realize([], _Path) ->
    done;

realize([{publication, _, _} = Publication | Rest], Path) ->
    make_dir_and_meta_file(Publication, Path),
    realize(Rest, Path).

add_recordings(FromDir, ToDir) -> % {{-
    {ok, FileList} =
        file:list_dir(FromDir),
    MoveAndRenameFile =
        fun (File) ->
            FromPath =
                filename:join(FromDir, File),
            NewBaseFileName =
                integer_to_list(os:system_time()),
            OldFileExt =
                filename:extension(File),
            ToPath =
                filename:join(
                  ToDir,
                  NewBaseFileName ++ OldFileExt
                ),
            file:copy(FromPath, ToPath)
        end,
    lists:foreach(
      MoveAndRenameFile,
      FileList
    ).
% }}-

stringify(Term) ->
    R = io_lib:format("~p",[Term]),
    lists:flatten(R).

% Recursive left-to-right composition instead of a traditional one (i.e., more like a pipe); instead of (b -> c) -> (a -> b) -> (a -> c), it is (a -> b) -> (b -> c) -> ... -> (x -> y) -> (y -> z)
% See PureScript's Control.Semigroupoid.composeFlipped (>>>) or Haskell's Control.Arrow.>>>
composeFlipped([G|[]]) ->
    G;
composeFlipped([F,G|Rest]) ->
    Composition =
        fun(X) ->
            G(F(X))
        end,
    composeFlipped([Composition|Rest]).

%    }}-
% }}-

% vim: set fdm=marker:
% vim: set foldmarker={{-,}}-:
% vim: set nowrap:

-module(mtest).

-compile(export_all).

-define(CONTENT_ROOT, "/home/toraritte/clones/phone-service/content-root/").
% -define(CATEGORIES, {category, ?CONTENT_ROOT}).
-define(CATEGORIES, {category, root}).

composeFlipped([G|[]]) ->
    G;
composeFlipped([F,G|Rest]) ->
    Composition =
        fun(X) ->
            G(F(X))
        end,
    composeFlipped([Composition|Rest]).

curry(AnonymousFun) ->
    {arity, Arity} =
        erlang:fun_info(AnonymousFun, arity),

    do_curry(AnonymousFun, Arity, [[], [], []]).

do_curry(Fun, 0, [Fronts, Middle, Ends] = X) ->
    % Fronts ++ Middle ++ ")" ++ Ends;
    [F, M, E] =
        lists:map(fun(L) -> string:join(L, "") end, X),
    Fstring =
        F ++ "Run(" ++ string:trim(M, trailing, ",") ++ ")" ++ E,

    {ok, Tokens, _} =
        erl_scan:string(Fstring ++ "."),
    {ok, Parsed} =
        erl_parse:parse_exprs(Tokens),

    FunBinding =
        erl_eval:add_binding(
          'Run',
          Fun,
          erl_eval:new_bindings()
        ),
    {value ,CurriedFun, _} =
        erl_eval:exprs(Parsed, FunBinding),

    CurriedFun;

do_curry(Fun, Arity, [Fronts, Middle, Ends]) ->
    VarName = [64 + Arity],
    NewFronts = ["fun(" ++ VarName ++ ") -> " | Fronts] ,
    NewMiddle = [VarName ++ ","|Middle],
    NewEnds = [" end"|Ends],
    do_curry(Fun, Arity-1, [NewFronts, NewMiddle, NewEnds]).


% lti(L) -> (fun erlang:list_to_integer/1)(L).

% miez() ->
%     compose(
%       [ fun lti/1
%       , fun(B) -> B + 5 end
%       , fun(C) -> erlang:integer_to_list(C) end
%       ]
%     ).

% call_state_graph() ->

metafile_name() ->
    "meta.erl".

get_meta(CategoryDir) -> % {{-
    MetaPath =
        filename:join(
          CategoryDir,
          metafile_name()
        ),
    {ok, Meta} =
        file:script(MetaPath),
    Meta.

% list_dir_sorted_with_full_path(Graph, Dir) ->
%     composeFlipped(
%       [ fun file:list_dir/1
%       , fun(T) -> element(2,T) end
%     % , (curry(fun erlang:element/2))(2)
%       , fun ordsets:from_list/1
%       , (curry(fun lists:map/2))
%           ( fun(SubDir) ->
%                 FullPath = filename:join(Dir, SubDir),
%                 Meta = get_meta(FullPath),
%                 digraph:add_vertex(Graph, Meta),
%                 {
%             end
%           )
      % , fun(L) ->
      %       lists:map(
      %         fun(E) ->
      %           Meta
      %           filename:join(Dir, E)
      %         end,
      %         L
      %       )
      %   end
      % ]
    % ).

% make_content_graph(ContentRoot) ->
%     Graph =
%         digraph:new([cyclic, protected]),
%     RootVertex =
%         digraph:add_vertex(Graph, ?CATEGORIES, ["Main category."]),

%     do_make(Graph, RootVertex, ContentRoot),
%     Graph.

% do_make(
%   digraph:new([cyclic, protected]),
%   { ?CONTENT_ROOT
%   , digraph:add_vertex(Graph, ?CATEGORIES, ["Main category."])
%   }
% )

% add_next_edges([_]) ->
%     done;
% add_next_edges([A,B|Rest]) ->
%     digraph:add_edge(

do_make(Graph, {ParentDir, ParentVertex}) ->
    ParentDirList =
        case file:list_dir(ParentDir) of
            {ok, List} ->
                lists:map(
                  fun(SubDir) ->
                      FullPath = filename:join(ParentDir, SubDir),
                      Meta = get_meta(FullPath),
                      % Add content vertex
                      digraph:add_vertex(Graph, Meta),
                      % ParentVertex ---ContentTuple--> ContentVertex
                      % E.g.,
                      % {category, root} ---{category, 1, "Ads"}--> {category, 1, "Ads"}
                      digraph:add_edge(Graph, Meta, ParentVertex, Meta, []),
                      {FullPath, Meta}
                  end,
                  ParentDir
                ),
                [do_make(Graph, PathVertexTuple) || PathVertexTuple <- PathVertexPairs];
            {error, enotdir} ->
                done
        end.

    PathVertexPairs =
        ( composeFlipped(
            [
             fun file:list_dir/1
            , fun(T) -> element(2,T) end
            % , (curry(fun erlang:element/2))(2)
            % , fun ordsets:from_list/1
            , (curry(fun lists:map/2))
                ( fun(SubDir) ->
                      FullPath = filename:join(ParentDir, SubDir),
                      Meta = get_meta(FullPath),
                      % Add content vertex
                      digraph:add_vertex(Graph, Meta),
                      % ParentVertex ---ContentTuple--> ContentVertex
                      % E.g.,
                      % {category, root} ---{category, 1, "Ads"}--> {category, 1, "Ads"}
                      digraph:add_edge(Graph, Meta, ParentVertex, Meta, []),
                      {FullPath, Meta}
                  end
                )
            ])
        )(ParentDir),
    [do_make(Graph, PathVertexTuple) || PathVertexTuple <- PathVertexPairs].




% do_make(Graph, ParentVertex, [_]) ->
%     done;

% do_make(Graph, ParentVertex, [ChildDir1, ChildDir2 | Rest]) ->
%     V1 = digraph:add_vertex(G,

% do_make(Graph, ParentVertex, ParentDir) ->
%     DirList =
%         list_dir_sorted(ParentDir),



% make_content_graph(PubList) ->
%     Graph = digraph:new([cyclic, protected]),
%     RootVertex = digraph:add_vertex(Graph, ?CATEGORIES),
%     do_make(PubList, Graph, RootVertex).

% do_make(
%   [ { {category, _ContentChoiceID, _Name} = Category
%     , SubContentList
%     }
%     | Rest
%   ],
%   Graph,
%   ParentVertex
% ) ->
%     V = digraph:add_vertex(G, Category),
%     digraph:add_edge(G, Category, ParentVertex, V, [])

% do_make([Publication | Rest], G, ParentVertex)
