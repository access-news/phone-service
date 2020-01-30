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
-define(CATEGORIES, {category, []}).


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
        #{ category_selectors => [] % ["7", "2", "", "3", ...]
         ,        auth_status => unregistered % | registered
         ,        history     => []
         % There should be only one playback running at any time, and, a corollary, each state should only have one active playback associated in this map. T
         ,       playback_ids => #{}
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
    handle_event_function.

terminate(Reason, State, Data) ->
    logger:debug(#{ self() => ["TERMINATE (normal-ish)", #{ data => Data, reason => Reason, state => State }]}).
     % filog:process_log(debug, #{ from => ["TERMINATE", #{ reason => Reason, state => State, data => Data }]}),
     % filog:remove_process_handler(?MODULE).

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
    logger:debug(#{ self() => ["MOD_ERL_EVENT_MASSAGE", #{ data => Data, state => State, mod_erl_event_call_status => ModErlEventCallStatus }]}),

    MassagedModErlEvent =
        { UUID
        , ModErlEventCallStatus
        , maps:from_list(FSEventHeaders)
        },
    TransitionActions =
        [ {next_event, internal, MassagedModErlEvent}
        ],

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
    logger:debug(#{ self() => ["CALL_HANGUP", #{ data => Data, state => State }]}),
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
    logger:debug(#{ self() => ["SENDMSG_CONFIRMATION", #{ message => Msg, state => State}]}),
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
    logger:emergency(#{ self() => ["UNHANDLED_INFO_EVENTS", #{ unknown_msg => Msg, state => State}]}),
    keep_state_and_data;
%% }}-

%%%%%%%%%%%%%%%%%%%%%%%%
%% `internal` clauses %%
%%%%%%%%%%%%%%%%%%%%%%%%

% Hanging up, so stop processing `internal` events
% NOTE `info` (i.e., external) events are still processed, but they are irrelevant at this point. If need to save them for debugging, just modify MOD_ERL_EVENT_MASSAGE, or the DEBUG clause in the "`info` clauses (for FreeSwITCH events)" section above
handle_event(
  internal, % EventType
  EC, % EventContent
  {hangup, _},                % State
  Data                  % Data
) ->
    logger:debug(#{ self() => ["in HANGUP state", #{ data => Data, state => hangup, event_content => EC }]}),

    % sending it synchronously to allow the playback to end
    sendmsg_locked(hangup, ["16"]),
    %% Not stopping the  `gen_statem` process here, because
    %% there will be further events related to the `hangup`
    %% command above. See "CALL_HANGUP" `info` clause below.
    keep_state_and_data;

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
    logger:debug(#{ self() => ["INCOMING_CALL", #{ data => Data, state => State }]}),

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

    sendmsg(execute, ["answer", []]),
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
   , history      := []                   % | Data    because the only state change to this state is
   , playback_ids := #{}                  % |         from  `incoming_call`, and  whatever has  been
   } = Data                               % /         set there must also be true.
) ->
    logger:debug(#{ self() => ["CALL_ANSWERED", #{ state => State}]}),

    re_start_inactivity_timer(State),
    next_menu(greeting, State, Data);
%% }}-

%% HANDLE_DTMF_FOR_ALL_STATES (internal) {{-
handle_event(
  internal,                    % EventType
  { _UUID                      % \
  , call_event                 % |
  , #{ "DTMF-Digit" := Digit}  % | EventContent = MassagedModErlEvent
  },                           % /
  State,
   % if `CategorySelectors =/= []`, we are collecting digits
  #{ category_selectors := CategorySelectors
   ,        auth_status := AuthStatus
   ,            history := History
   } = Data
) ->
    logger:debug(#{ self() => ["HANDLE_DTMF_FOR_ALL_STATES", #{ digit => Digit, state => State}]}),

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

    % DigitIsOneToNine =
    %     case string:find("123456789", Digit) of
    %         nomatch ->
    %             false;
    %         _ ->
    %             true
    %     end,

    % {{- {{-
    % When `idt_running` is true, it means we are collecting digits for category selection. So when IDT is not running and 0 comes in, then go to main menu, but when IDT is running, then add it to `CategorySelectors`, renew IDT, and keep collecting. Another example is when IDT is not running, and * (star) and # (pound) arrives, go back or go to next category respectively, but when IDT is running, then ignore these inputs and renew IDT, and keep collecting.

    % IDT race condition when collecting digits
    % ====================================================
    % It is possible that IDT times out while processing a DTMF digit. What then?

    % This probably isn't as bad as it sounds, or maybe not even bad at all. For example, a user is having trouble to dial digits quickly (device issues, disability, etc.), and one of the digits hits this race condition, but this is an important input, so IDT time-out can be ignore, and now this digit is saved and IDT extended.

    % Is there a scenario where this can turn sour? Someone start dialing for a category, but then change their minds, and wants to hit the main menu, go back, etc. Even in not a race condition status they would have to wait until the IDT runs out. If they do get into a category, they can always go back with * (star).
    % }}- }}-
    case
        #{                state => State
         ,       received_digit => Digit
         % `CategorySelectors` emptied after eval on `interdigit_timer` timeout
         ,    collecting_digits => CategorySelectors =/= []
         % , digit_is_one_to_nine => DigitIsOneToNine
         }
    of
        % Category selectors (i.e., collecting digits) KEEP_STATE {{-
        % TODO don't forget that GREETING -> CATEGORY N is a valid transition!
        %      When the `interdigit_timer` expires in `greeting` to (signaling that digit collection is finished), do not use `next_menu/1` but the two lines from "* (star) and # (pound) in `greeting`", otherwise `greeting` will be added to the navigation history, and navigation should not be possible back there

        % [1-9] in any state (except `article` and `main_menu`) {{-
        % will trigger DTMF collection, [1-9] have different semantics though when in `main_menu` or playing an article.

        % also triggered when collecting digits for passcode when logging in
        #{ state := State
         , received_digit := Digit
        % , digit_is_one_to_nine := true
        % G = fun ({Digit, S}) when Digit =/= "0", Digit =/= "*", Digit =/= "#", S =/= lofa, S =/= miez -> {yay, Digit, S}; ({Digit, State}) -> {nono, Digit, State} end.
        % lists:map(G, [{"*", lofa}, {"#", miez}, {"0", mas}, {"1", lofa}, {"2", miez}, {"3", finally}]).
        % Not checking whether we are collecting digits, because {{-
        % this clause is explicitly for that scenario (i.e., digits 1 to 9 are pressed not in `main_menu` or in `article`), and collecting_digits may be `false` if this is the first round. With each new digit, the interdigit timer (IDT) gets restarted. The "timeout" `handle_event/4` clauses will determine whether the contents of the DTMF buffer (`CategorySelectors`) is useful, and it will also clear it on timeout.
        % Another reason is that * (star), # (pound), and 0 are instant actions (no IDT when pressed), so if those are pressed, this case clause will simply be skipped, and subsequent clauses will determine what to do with them, depending on whether digits are collected.
        % }}-
        %, collecting_digits := true
         }
        % The reason why `article` and `main_menu` are excluded is because they only have single digit instant actions (no IDT when a digit is dialed), and numbers have different semantics.
        when State =/= article,
             State =/= main_menu,

             Digit =/= "0",
             Digit =/= "*",
             Digit =/= "#"
        ->
            % `next_state_on_playback_stop` and `history` fields update in `Data` not necessary, as any playback is allowed to run its course here, and state change only happens when the `interdigit_timer` expires (see timeout clauses).
            collect_digits(Data, Digit);
            % }}-

        % 0 in any state when collecting digits {{-
        #{                state := _State
         ,       received_digit := "0"
         ,    collecting_digits := true
         % , digit_is_one_to_nine := false % not necessary, but explicit
         }
        ->
            % Same as the previous clause, and zero is just an element now in the DTMF buffer that will get evaluated when the `interdigit_timer` expires
            % Could've put in previous clause, but more explicit this way
            collect_digits(Data, "0");
            % }}-

        % * (star) and # (pound) in any state when collecting digits, and not in `greeting` {{-
        % Ignore (i.e., add empty string, will be flattened during eval anyway), but renew `interdigit_timer`
        % When a user presses them accidentally when signing in,
        % or when selecting the category, it will get filtered
        % out.
        #{                state := _State
         ,       received_digit := Digit
         ,    collecting_digits := true
         % , digit_is_one_to_nine := false % not necessary, but explicit
         }
        when Digit =:= "*";
             Digit =:= "#",

             State =/= greeting
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
        % }}-

        % * (star) and # (pound) in `greeting`  {{-
        %   * (star)  \  skip to `?CATEGORIES`
        %   # (pound) /
        %   0         - go to `main_menu`
        %   [1-9]     - collect digits to select next category
        %               from `?CATEGORIES` (handled in "Category selectors" clauses above)
        #{                state := greeting
         ,       received_digit := Digit
         ,    collecting_digits := false
         % , digit_is_one_to_nine := false % not necessary, but explicit
         }
        when Digit =:= "*";
             Digit =:= "#"
        ->
            next_menu(?CATEGORIES, greeting, Data);
            % stop_playback(),
            % comfort_noise(),
            % play(?CATEGORIES, Data),
            % {next_state, ?CATEGORIES, Data};
            % leave_menu(
            %   #{ from => greeting
            %    , to   => ?CATEGORIES
            %    , data => Data
            %    }
            % );
        % }}-

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

        % Go to main menu
        % 0 in any state when NOT collecting digits, except `main_menu` {{-
        #{                state := State
         ,       received_digit := "0"
         ,    collecting_digits := false
         % , digit_is_one_to_nine := false % not necessary, but explicit
         }
        when State =/= main_menu
        ->
            next_menu(main_menu, State, Data);
            % leave_menu(
            %   #{ from => State
            %    , to   => main_menu
            %    , data => Data
            %    }
            % );
        % }}-

        % Go up/back (the forward direction is only 0 or collecting digits!) {{-

        %     * (star) in main_menu, when previously in `greeting` {{-
        #{                state := main_menu
         ,       received_digit := "*"
         ,    collecting_digits := false
         % , digit_is_one_to_nine := false % not necessary, but explicit
         }
        % The only time when history is not updated when entering `main_menu` is when coming from `greeting` (or at least, this should be the case). See "* (star) and # (pound) in `greeting`" case clause above (it does not update the history).
        when History =/= []
        ->
            next_menu(?CATEGORIES, main_menu, Data);
            % leave_menu(
            %   #{ from => main_menu
            %    , to   => ?CATEGORIES
            %    , data => Data
            %    }
            % );
        % }}-

        %     * (star) in any state, except `greeting` {{-
        % NOTE - `?CATEGORIES` may use * (star) because it can be part of the history, whereas `greeting` should not be reached (only from a new incoming call)
        #{                state := State
         ,       received_digit := "*"
         ,    collecting_digits := false
         % , digit_is_one_to_nine := false % not necessary, but explicit
         }
        when State =/= greeting
        ->
            case {State, History} of
                % In `?CATEGORIES, and history is empty, i.e., nowhere to go back to, so ignore
                {?CATEGORIES, []} ->
                    keep_state_and_data;
                    % TODO maybe? {{-
                    % step 1. prompt: nowhere to go back (or smth like it)
                    %    1a - stop_playback
                    %    1b - start new playback in HANDLE_CHANNEL_EXECUTE_COMPLETE
                    % step 2.
                    % }}-
                _ ->
                    % When (or if) moving forward in history with # (pound) will be implemented, this could be the template for `prev_menu/1` (after `next_menu/1`).
                    comfort_noise(),
                    stop_playback(),
                    {PrevState, NewData} = pop_history(Data),
                    play(PrevState, Data),
                    {next_state, PrevState, NewData}
            end;
        % }}-
        % }}-

        % No function (TODO maybe to move forward in history? would need a double linked list though)
        % # (pound) in any state except `greeting` and `main_menu`{{-

        % ON HOLD - category browsing with # (pound) {{-
        % # (pound) in any state, except `greeting`, `main_menu`, and `?CATEGORIES`
        % #{                state := State
        %  ,       received_digit := "#"
        %  ,    collecting_digits := false
        %  , digit_is_one_to_nine := false % not necessary, but explicit
        %  }
        % when State =/= greeting,
        %      State =/= ?CATEGORIES,
        %      State =/= main_menu
        % ->
        %     stop_playback(),
        %     NextState = next_category
        %     {next_state, NextState, NewData};
        % }}-
        % ON HOLD - category browsing with # (pound) {{-
        % # (pound) in `?CATEGORIES`
        % #{                state := ?CATEGORIES
        %  ,       received_digit := "#"
        %  ,    collecting_digits := false
        %  , digit_is_one_to_nine := false % not necessary, but explicit
        %  }
        % ->
        %     stop_playback()
        %     NewData = push_history(Data, category(1)),
        %     {next_state, category(1), NewData};
        % }}-

        #{                state := State
         ,       received_digit := "#"
         ,    collecting_digits := false
         % , digit_is_one_to_nine := false % not necessary, but explicit
         }
        when State =/= greeting, % not necessary, but explicit
             State =/= main_menu
             % TODO prepare to include `?CATEGORIES` when # will allow moving forward in history
        ->
            keep_state_and_data;
        % }}-

        % # (pound) in `main_menu` (sign-in or favorites) {{-
        #{                state := main_menu
         ,       received_digit := "#"
         ,    collecting_digits := false
         % , digit_is_one_to_nine := false
         }
        ->
            NextMenu =
                case AuthStatus of
                    registered ->
                        % TODO replace this placeholder with `favorites` when its implementation is done
                        % favorites
                        sign_in;
                    unregistered ->
                        sign_in
                end,
            next_menu(NextMenu, main_menu, Data);
            % leave_menu(
            %   #{ from => main_menu
            %    , to   => NextState
            %    , data => Data
            %    }
            % );
        % }}-

        % [1-9] in `main_menu` {{-
        #{                state := main_menu
         ,       received_digit := Digit
         ,    collecting_digits := false
         % , digit_is_one_to_nine := true
         }
        ->
            NextMenu =
                case Digit of
                    0 -> quick_help;
                    1 -> tutorial;
                    2 -> leave_message;
                    3 -> blindness_services;
                    5 -> settings;
                    8 -> ?CATEGORIES;
                  % * -> see "* (star) in any state, except `greeting`"
                  % # -> see "# (pound) in `main_menu` (sign-in or favorites)"
                    _ ->
                        % TODO prompt: invalid entry
                        main_menu
                end,

            case NextMenu of
                main_menu ->
                    repeat_menu(
                      main_menu,
                      Data,
                      #{with_warning => invalid_selection()}
                    );
                _ ->
                    next_menu(NextMenu, main_menu, Data)
            end;
        % }}-

        % [1-9] when playing an article
        % TODO NEXT

        UnhandledDigit ->
            logger:emergency(#{ self() => ["UNHANDLED_DIGIT", UnhandledDigit]})

        %% TODO When greeting stops (`PLAYBACK_STOP` is received) jump to `?CATEGORIES`.

        %% Keep  playing  the   greeting,  continue  collecting
        %% digits,  and caller  will be  sent to  the specified
        %% category, if the `DTMFString` contains a valid entry
        %% after  the  interdigit  time-out.  Once  the  playback
        %% finishes,  the `PLAYBACK_STOP`  event is  emitted by
        %% FreeSWITCH,  and  the  state   will  be  changed  to
        %% `?CATEGORIES`, but  the behavior  there will  be the
        %% same, and  the digits will continue  to be collected
        %% (until they make sense).
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
  },                           % /
  State,
   % if `CategorySelectors =/= []`, we are collecting digits
  #{        auth_status := AuthStatus % TODO needed?
   ,            history := History
   ,       playback_ids := PlaybackIDs
   } = Data
) ->
    logger:debug(#{ from => "HANDLE_CHANNEL_EXECUTE_COMPLETE", app_id => ApplicationUUID, play_ids => PlaybackIDs}),
    case
        #{        state => State
         % , playback_ids => PlaybackIDs
         }
    of
        % The only playback that can be stopped in `greeting` is when the `greeting` playback stops by itself, and so we would like to naturally transition to `?CATEGORIES`
        #{ state := greeting } ->
            logger:debug("HANDLE_CHANNEL_EXECUTE_COMPLETE - greeting stop IN greeting (natural stop)"),
            next_menu(?CATEGORIES, greeting, Data);
            % enter_menu(
            %   #{ from => greeting
            %    , to   => ?CATEGORIES
            %    , data => Data
            %    }
            % );

        % TODO inactivity timeout! (state timeouts?)
        % #{        state := ?CATEGORIES
        %  , playback_ids := #{ ?CATEGORIES := PlaybackID }
        %  }
        % when PlaybackID =:= ApplicationUUID
        % % i.e., the playback of `?CATEGORIES` menu has finished, no input from user
        % ->

        #{        state := State
         % , playback_ids => PlaybackIDs
         }
        ->
            repeat_menu(State, Data)
            % play_menu(State, Data)
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
  , #{ "Event-Name" := EventName } = FSEvent % | EventContent = MassagedModErlEvent
  },                                 % /
  State,
  _Data                               % Data
) ->
    logger:debug(""),
    % logger:debug(#{ self() => ["OTHER_INTERNAL_CALL_EVENT", #{ event_name => EventName, fs_event_data => FSEvent,  state => State}]}),
    logger:debug(#{ self() => ["UNKNOWN_INTERNAL_CALL_EVENT", #{ event_name => EventName, state => State}]}),
    logger:debug(""),
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
    logger:debug(#{ self() => ["UNREGISTERED_TIMEOUT", #{ data => Data, state => State }]}),

    case AuthStatus of
        registered ->
            % ignore if user already signed in in the meantime
            keep_state_and_data;
        unregistered ->
            next_menu({hangup, demo}, State, Data)
    end;
%% }}-

% TODO don't forget that GREETING -> CATEGORY N is a valid transition!
%      When the `interdigit_timer` expires in `greeting` to (signaling that digit collection is finished), do not use `next_menu/1` but the two lines from "* (star) and # (pound) in `greeting`", otherwise `greeting` will be added to the navigation history, and navigation should not be possible back there
%% interdigit_timer {{-
%% TODO Not sure if an event is generated when a timer is canceled. Be prepared for cryptic error messages if that is the case.
handle_event(
  {timeout, interdigit_timer}, % EventType
  eval_collected_digits,        % EventContent
  State,
  #{ category_selectors := CategorySelectors } = Data
) ->
    % TODO Prod system along, or .
    % Always clear DTMF buffer when the `interdigit_timer` expires, because this is the point the buffer is evaluated. Outcome is irrelevant, because at this point user finished putting in digits, hence waiting for the result, and so a clean slate is needed.
    NewData =
        Data#{ category_selectors := [] },

    case eval_collected_digits(CategorySelectors) of
        invalid ->
            repeat_menu(
              State,
              NewData,
              #{with_warning => invalid_selection()}
            );

        Category ->
            next_menu(Category, State, NewData)
    end;
%% }}-

%% inactivity timers (info) {{-
handle_event(
  info,
  inactivity_warning,
  State,
  Data
) ->
    InactiveWarning =
        "Please press 0 if you are still there.",

    % TODO how is this applicable when in {article, _}?
    repeat_menu(State, Data, #{with_warning => InactiveWarning});

handle_event(
  info,
  inactivity_hangup,
  State,
  Data
) ->

    next_menu({hangup, inactivity}, State, Data).
%% }}-

eval_collected_digits(_) ->
    noop.

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

sendmsg_headers(execute, [App, Args], UUID) ->
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
    [{"hangup-cause", HangupCode}
    ];

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

do_sendmsg(SendmsgCommand, Args, IsLocked) ->
    LockHeaderList =
        case IsLocked of
            false -> [];
            true  -> [{"event-lock", "true"}]
        end,
    ApplicationUUID =
        % lists:flatten(io_lib:format("~p", [now()])),
        integer_to_list( erlang:system_time() ),
    FinalHeaders =
        [{"call-command", atom_to_list(SendmsgCommand)}]
        ++ sendmsg_headers(SendmsgCommand, Args, ApplicationUUID)
        ++ LockHeaderList,
    fsend({sendmsg, get(uuid), FinalHeaders}),
    ApplicationUUID.

sendmsg(SendmsgCommand, Args) when is_list(Args) ->
    do_sendmsg(SendmsgCommand, Args, false).

sendmsg_locked(SendmsgCommand, Args) when is_list(Args) ->
    do_sendmsg(SendmsgCommand, Args, true).
%% }}-

fsend(Msg) ->
    %% Why the `lofa` atom:
    %% https://stackoverflow.com/questions/58981920/
    {lofa, ?FS_NODE} ! Msg.

stop_playback() ->
    logger:debug("stop playback"),
    % TODO Should this  be `bgapi`? Will the  synchronous `api`
    %      call wreak havoc when many users are calling?
    fsend({api, uuid_break, get(uuid) ++ " all"}).

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


comfort_noise(Milliseconds) ->
    logger:debug("play comfort noise"),
    ComfortNoise =
        "silence_stream://"
        ++ integer_to_list(Milliseconds)
        ++ ",1400",
    sendmsg_locked(execute, ["playback", ComfortNoise]).

comfort_noise() ->
    comfort_noise(750).

sign_up_prompt() ->
    "If you would like to sign up for Access News, please call us at 916, 889, 7519.".

invalid_selection() ->
    "Invalid selection."
    ++ " "
    ++ "Please try again from the following categories.".

% TODO Try out `mod_vlc` to play aac and m4a files. {{-
% sendmsg(UUID, execute, ["playback", "/home/toraritte/clones/main.mp3"]),
% sendmsg_locked(UUID, execute, ["playback", "/home/toraritte/clones/phone-service/ro.mp3"]),
% }}-
play(greeting, #{ auth_status := AuthStatus}) -> % {{-
    logger:debug("play greeting"),
    Welcome = "Welcome to Access News, a service of Society For The Blind in Sacramento, California, for blind, low-vision, and print-impaired individuals.",
    SelectionSkip = "If you know your selection, you may enter it at any time, or press star or pound to skip to listen to the main categories.",
    Unregistered =
        case AuthStatus of
            registered ->
                "";
            unregistered ->
                "You are currently in demo mode, and have approximately 5 minutes to try out the system before getting disconnected. To log in, dial 0, pound, followed by your code."
                ++ sign_up_prompt()
                ++ "To leave a message with your contact details, dial 0 2."
        end,
    GoToTutorial = "To listen to the tutorial, dial 01.",
    GoToBlindnessServices = "To learn about other blindness services, dial 03.",
    LeaveMessage = "If you have any questions, comments, or suggestions, please call 916 889 7519, or dial 02 to leave a message.",
    speak(
         Welcome ++ " "
      ++ Unregistered          ++ " "
      ++ SelectionSkip         ++ " "
      ++ GoToTutorial          ++ " "
      ++ GoToBlindnessServices ++ " "
      ++ LeaveMessage          ++ " "
    );
% }}-

% play(main_menu) ->

%     speak().

play(?CATEGORIES, _Data) -> % {{-
    MainCategory = "Main category.",
    GoToMainMenu = "For the main menu, press 0.",
    % EnterFirstCategory = "To start exploring the categories one by one, press pound, 9 to enter the first category.",
    % CategoryBrowsePound = "To enter the next item, press the pound sign. Dial pound, 0 to get back into the previous item.",
    speak(
         MainCategory
      ++ GoToMainMenu
      % ++ EnterFirstCategory % nem kene
     );
% }}-

play({hangup, demo}, _Data) ->
    DemoTimedOut =
            "End of demo session."
        ++ sign_up_prompt()
        ++ "Thank you for trying out the service!",
    speak(DemoTimedOut);

play({hangup, inactivity}, _Data) ->
    speak("Goodbye.").

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

speak(Text) ->
    % return the application UUID string.
    sendmsg_locked(execute, ["speak", "flite|kal|" ++ Text]).

speak_warning(none) ->
    "";

speak_warning(WarningPrompt) ->
    comfort_noise(),
    speak(WarningPrompt).

%% }}-

push_history(#{ history := History } = Data, State) ->
    Data#{ history := [State | History] }.

% Why not check if history is empty? {{-
% ====================================================
% Because it will (or at least should) not happen: Call comes in, FreeSWITCH start `ivr.erl`, `init/0` sets history to `[?CATEGORIES]`, meaning that states before that (i.e., `incoming_call` and `greeting`) can never be revisited. When traversing the history backwards with * (star), `HANDLE_DTMF_FOR_ALL_STATES` will make sure that when the `?HISTORY_ROOT` is reached, pressing the * (star) will be ignored (see `case` clause starting with comment "Go up/back").
% UPDATE
% What is said above still holds, but the root is now [].
% }}-
pop_history(#{ history := [PrevState | RestHistory] } = Data) ->
    { PrevState
    , Data#{ history := RestHistory }
    }.

% Playback keeps going while accepting DTMFs, and each subsequent entry has a pre-set IDT
% 1-9 trigger collection (all states except some, see HANDLE_DTMF_FOR_ALL_STATES)
% *, 0, # are single digit instant actions
% (TODO make users able to set it)

collect_digits(
  #{category_selectors := CategorySelectors} = Data,
  Digit
) ->
    NewData =
        Data#{category_selectors := CategorySelectors ++ [Digit]},
    % The notion is that the `InterDigitTimer` is restarted whenever {{-
    % a new DTMF signal arrives while we are collecting digits. According to [the `gen_statem` section in Design Principles](from https://erlang.org/doc/design_principles/statem.html#cancelling-a-time-out) (and whoever wrote it was not a fan of proper punctuation): "_When a time-out is started any running time-out of the same type; state_timeout, {timeout, Name} or timeout, is cancelled, that is, the time-out is restarted with the new time._"  The [man pages](https://erlang.org/doc/man/gen_statem.html#ghlink-type-generic_timeout) are clearer: "_Setting a [generic] timer with the same `Name` while it is running will restart it with the new time-out value. Therefore it is possible to cancel a specific time-out by setting it to infinity._"
    % }}-
    % TODO Does timer cancellation produce an event?
    InterDigitTimer =                 % Generic timeout
        { {timeout, interdigit_timer} % { {timeout, Name}
        , ?INTERDIGIT_TIMEOUT         % , Time
        , eval_collected_digits       % , EventContent }
        },
    {keep_state, NewData, [ InterDigitTimer ]}.

category(N) ->
    {category, [N]}.

category(N, M) ->
    {category, [N, M]}.

category(N, M, O) ->
    {category, [N, M, O]}.

category(N, M, O, P) ->
    {category, [N, M, O, P]}.

% never push history (no point because just replaying menu prompt)
repeat_menu(Menu, Data, #{ with_warning := WarningPrompt}) ->
    % Not necessary when playback ends naturally,
    % but calling it multiple times doesn't hurt.
    stop_playback(),
    speak(WarningPrompt),
    comfort_noise(),
    play(Menu, Data),
    {keep_state, Data}.

repeat_menu(Menu, Data) ->
    repeat_menu(Menu, Data, #{ with_warning => none }).

next_menu(NextMenu, CurrentState, Data)
when CurrentState =/= NextMenu % use `repeat_menu/3` otherwise
->
    % NewData =
    %     case Prompt of
    %         stop ->
    %             stop_playback(),
    %             Data;
    %         _ ->
    %             ApplicationUUID =
    %                 play(Prompt, Data),
    %             NewPlaybackIDs =
    %                 % `=>` update if present, or create new
    %                 % `:=` only update when present, otherwise crash
    %                 PlaybackIDs#{ Prompt => ApplicationUUID },

    %             Data#{playback_ids := NewPlaybackIDs }
    %     end,

    stop_playback(),
    comfort_noise(),
    play(NextMenu, Data),

    NewData =
        case {CurrentState, NextMenu} of

            % State changes when history is not updated  with prev
            % state (corner cases)
            % ====================================================
            % NOTE: This is adding behaviour to `next_menu/1`, which {{-
            % I do not like, but these are the only cases when history is not pushed (so far). Another advantage is that all corner cases are listed in one place.
            % }}-
            % NOTE: `main_menu` -> `?CATEGORIES` {{-
            % Go to `?CATEGORIES` from `main_menu` (only forward option from `main_menu`), but do not add `main_menu` to history
            % NOTE Currently it is possible to accumulate `?CATEGORIES` in history by going to `main_menu` and forward to `?CATEGORIES`, as it will add `?CATEGORIES` each time but leaving it as is
            % }}-
            {main_menu, ?CATEGORIES}  -> Data;
            % {{-
            %   + `greeting` -> `?CATEGORIES` (i.e., when pressing * or #  in `greeting` or playback stops)
                % No `next_menu/1` here to keep the history empty so that `greeting` cannot be revisited, making `?CATEGORIES` the root. Whatever navigations happen there
                % Why not keep state? Reminder: 0# (not very relevant in this particular case, but keeping state changes consistent makes it easier to think about each `handle_event/4` clause. Read note at the very end of HANDLE_DTMF_FOR_ALL_STATES `handle_event/4` clause)
            % }}-
            {greeting, ?CATEGORIES}   -> Data;
            {greeting, main_menu}     -> Data;
            {incoming_call, greeting} -> Data;

            % On any other state change
            _ ->
                push_history(Data, CurrentState)
        end,

    {next_state, NextMenu, NewData}.

% start,   when call is answered
% restart, when a DTMF signal comes in
% NOTE: impossible to time out while collecting digits, because a DTMF signal restarts the timers, and the interdigit timeout is a couple seconds
re_start_inactivity_timer(State) ->

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

s(Term) ->
    R = io_lib:format("~p",[Term]),
    lists:flatten(R).

% vim: set fdm=marker:
% vim: set foldmarker={{-,}}-:
% vim: set nowrap:
