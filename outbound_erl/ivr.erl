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

-define(CATEGORIES, {category, []}).

-define(DEMO_TIMEOUT, 600000).
-define(INTERDIGIT_TIMEOUT, 2000).
-define(DTMF_DIGIT_TIMEOUT, 5000).

start_link(Ref) ->
    {ok, Pid} = gen_statem:start_link(?MODULE, [], []),
    {Ref, Pid}.

start(Ref) ->
    {ok, Pid} = gen_statem:start(?MODULE, [], []),
    {Ref, Pid}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_statem callbacks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

% Welcome to Access News, a service of Society For The Blind in Sacramento, California for blind, low-vision, and print-impaired individuals.

% You are currently in demo mode, and have approximately 5 minutes to try out the system before getting disconnected. To log in, dial 0# followed by your code, or if you would like to sign up up for Access News, please call us at 916 889 7519, or dial 02 to leave a message with your contact details.

% For the main menu, press 0.
% To listen to the tutorial, dial 00.
% To learn about other blindness services, dial 01.
% If you have any questions, comments, or suggestions, please call 916 889 7519, or dial 02 to leave a message.
% To start exploring menu items one by one, press the pound sign.
% Press # again to skip to the next, and dial #0 to jump back to the previous one.

% If you would like to learn about the national Federation of the blind nfb newsline service with access to more than 300 newspapers and magazines including the Sacramento Bee. Please call 410-659-9314. If you would like to learn about the California Braille and talking book Library, please call 80095 to 5666.

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
init(_Args) ->
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
        #{ category_selectors => []
         , auth_status => unregistered
         , history     => []
         },
    {ok, State, Data}.

callback_mode() ->
    handle_event_function.

terminate(Reason, State, Data) ->
    logger:debug(#{ self() => ["TERMINATE (normal-ish)", #{ data => Data, reason => Reason, state => State }]}).
     % filog:process_log(debug, #{ from => ["TERMINATE", #{ reason => Reason, state => State, data => Data }]}),
     % filog:remove_process_handler(?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% `info` clauses (for FreeSWITCH events) %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% {{-

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
%% }}-

%%%%%%%%%%%%%%%%%%%%%%%%
%% `internal` clauses %%
%%%%%%%%%%%%%%%%%%%%%%%%

%% INCOMING_CALL (internal) (aka call init, see below)  {{-

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

    { next_state
    , greeting
    , Data#{auth_status := NewAuthStatus}
    , TransitionActions
    };
%% }}-

%% GREETING (internal) {{-
%% That is, events that are handled in the `greeting` state.

handle_event(
  internal,                               % EventType
  { _UUID                                 % \
  , call_event                            % |
  , #{ "Event-Name" := "CHANNEL_ANSWER" } % | EventContent = MassagedModErlEvent
  },                                      % /
  greeting                                = State,
  #{ auth_status := AuthStatus            % \
   , history     := []                    % | Data
   }                                      % /
) ->
    logger:debug(#{ self() => ["GREETING", #{ state => State}]}),

    comfort_noise(750),
    ApplicationUUID =
        play({State, AuthStatus}),
    NewHistory =
        [{greeting, ApplicationUUID}],
    NewData =
        Data#{ history := NewHistory },
    {keep_state, NewData};
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
   , auth_status := AuthStatus
   % When `idt_running` is true, it means we are collecting digits for category selection.
   % , collecting_digits := CollectorStatus
   % , idt_running := IDTRunning
   , history := History
   } = Data
) ->
    logger:debug(#{ self() => ["HANDLE_DTMF_FOR_ALL_STATES", #{ digit => Digit, state => State}]}),

    %% 1 2 3   \
    %% 4 5 6    > Interdigit time-out (IDT) = 2 sec (?)
    %% 7 8 9   /  EXCEPT when playing an article, then IDT = 0
    %% -----
    %% * 0 #   Interdigit time-out = 0

    % Then notion is that in every state when one of "123456789" is pressed, "collecting_digits" mode is initiated, and "*" and "#" will be ignored (except when received after the interdigit time-out (IDT) expired), but they will trigger an extra IDT seconds to get new digits.

    WhenCollectingDigits =
        fun (#{category_selectors := CategorySelectors} = _Data, Digits) ->
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
            {keep_state, NewData, [ InterDigitTimer ]},

    DigitIsOneToNine =
        case string:find("123456789", Digit) of
            nomatch ->
                false;
            _ ->
                true
        end,

    % DTMF tones are divided into two categories: category selectors, and single digit instant actions. Category selectors have interdigit timeout (that may be configurable), whereas single digits instant actions immediately respond by changing state, performing a command, etc.
    % Another difference is that category selectors only change state once the `interdigit_timer` expires, hence they will keep the state as is in this clause, but single digit instant actions do whatever necessary to move on (stop playback, change state, etc.).

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
         , digit_is_one_to_nine => DigitIsOneToNine
         }
    of
        % Category selectors (i.e., collecting digits) {{-
        % [1-9] in any state (except `article` and `main_menu`) {{-
        % will trigger DTMF collection, [1-9] have different semantics though when in `main_menu` or playing an article.
        #{ state := State
         , received_digit := Digit
         , digit_is_one_to_nine := true
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
            WhenCollectingDigits(Data, Digit);
            % }}-

        % 0 in any state when collecting digits {{-
        #{                state := _State
         ,       received_digit := "0"
         ,    collecting_digits := true
         , digit_is_one_to_nine := false % not necessary, but explicit
         }
        ->
            % Same as the previous clause, and zero is just an element now in the DTMF buffer that will get evaluated when the `interdigit_timer` expires
            WhenCollectingDigits(Data, "0");
            % }}-

        % * (star) and # (pound) in any state when collecting digits, and not in `greeting` {{-
        % Simply ignore, but renew `interdigit_timer`
        #{                state := _State
         ,       received_digit := Digit
         ,    collecting_digits := true
         , digit_is_one_to_nine := false % not necessary, but explicit
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
            % Same as the previous clause, and zero is just an element now in the DTMF buffer that will get evaluated when the `interdigit_timer` expires
            WhenCollectingDigits(Data, "");
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
         , digit_is_one_to_nine := false % not necessary, but explicit
         }
        when Digit =:= "*";
             Digit =:= "#"
        ->
            stop_playback(),
            {next_state, ?CATEGORIES};
        % }}-

        % * (star) in `?CATEGORIES`
        #{                state := `?CATEGORIES`
         ,       received_digit := Digit
         ,    collecting_digits := false
         , digit_is_one_to_nine := false % not necessary, but explicit
         }
        when Digit =:= "*"
        ->
            keep_state_and_data;

        % 0 in any state when NOT collecting digits, and not in `main_menu`
        #{                state := State
         ,       received_digit := "0"
         ,    collecting_digits := false
         , digit_is_one_to_nine := false % not necessary, but explicit
         }
        ->
            stop_playback(),
            % No need to cancel any timers as there shouldn't be any; if we were collecting digits, zero would be handled in the previous clause, otherwise we would never get here.
            NewData =
                Data#{

% # (pound) in `?CATEGORIES`
% * (star) in any state, except `greeting`, and `?CATEGORIES`
% # (pound) in any state, except `greeting`, `main_menu`, and `?CATEGORIES`
% 1-9 in `main_menu`
% 1-9 when playing an article
% # (pound) in `main_menu` (just handle in a `main_menu` clause)
        #{ state := State, digit := Digit, idt_running := true }
            when
                State =/= article,
                Digit =/= "*",
                Digit =/= "#"
        ->
            Data;

        %% Brings to main menu (`main_menu`) in every state
        {State, Digit} when Digit =:= "0" ->                % |
            stop_playback(),                                   % |
            Data#{ category_selectors   := CategorySelectors ++ [Digit]
                    , prev_category := State
                    };
            % {next_state, main_menu, Data};                     % |
        %% ------------------------------------------------------*

        %% Stop   greeting,   and  (once   `PLAYBACK_STOP`   is
        %% received) jump to `?CATEGORIES`.
        %% TODO * - unassigned
        %% TODO # - unassigned
        % Leaving them unassigned (for now) because it may ease the cognitive load if functionality is consistent across menus. With that said, they may be utilized as a shortcat for "favorites", "language selection", etc., or just have the user re-define them.
        {greeting, Digit} when Digit =:= "*"; Digit =:= "#" -> % |
            stop_playback(),                                   % |
            % No need to save * (star) or # (pound) because their only function in `greeting` state is to fast-forward to `?CATEGORIES`
            Data;
            % {next_state, ?CATEGORIES, Data};                   % |

        %% ?CATEGORIES - * (star) and # (pound) has no functionality;
        %%                ignored when pressed.
        % When a user presses it accidentally when signing in,
        % or when selecting the category, it will get filtered
        % out.
        %% TODO * - unassigned
        %% TODO # - unassigned
        % Leaving them unassigned (for now) because it may ease the cognitive load if functionality is consistent across menus. With that said, they may be utilized as a shortcat for "favorites", "language selection", etc., or just have the user re-define them.
        {?CATEGORIES, Digit} when Digit =:= "*"; Digit =:= "#" -> % |
            noop;
            % keep_state_and_data;                   % |

        %% # (pound) means "next category" in any other state
        %% (e.g., when  listening to an article,  it means jump
        %% to the next publication)
        {_State, Digit} when Digit =:= "#" ->                % |
            stop_playback();                                   % |
            % {next_state, main_menu, Data};                     % |

        %% * (star) means "go back / previous menu" in any other state
        {_State, Digit} when Digit =:= "*" ->                % |
            stop_playback();                                   % |
            % {next_state, main_menu, Data};                     % |
        %% ------------------------------------------------------*

        %% Keep  playing  the   greeting,  continue  collecting
        %% digits,  and caller  will be  sent to  the specified
        %% category, if the `DTMFString` contains a valid entry
        %% after  the  interdigit  time-out.  Once  the  playback
        %% finishes,  the `PLAYBACK_STOP`  event is  emitted by
        %% FreeSWITCH,  and  the  state   will  be  changed  to
        %% `?CATEGORIES`, but  the behavior  there will  be the
        %% same, and  the digits will continue  to be collected
        %% (until they make sense).

        %% It is implicit  that `Digits` can only  be [1-9] (or
        %% at least that is how it should be...)
        {State, Digit} when State =/= article ->
            % NewData = Data#{ category_selectors := CategorySelectors ++ [Digit] },
            % {next_state, ?CATEGORIES, NewData};
            lofa
            % Data#{ category_selectors := CategorySelectors ++ [Digit] };
    end,

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
    {keep_state, NewData};
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
    logger:debug(#{ self() => ["OTHER_INTERNAL_CALL_EVENT", #{ event_name => EventName, fs_event_data => FSEvent,  state => State}]}),
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

        unregistered ->
            keep_state_and_data;

        registered ->
            %% The generated `CALL_HANGUP` event  will be caught by
            %% "CALL_HANGUP" `handle_event/4`.
            sendmsg(hangup, ["16"]),
            %% Not stopping the  `gen_statem` process here, because
            %% there will be further events related to the `hangup`
            %% command above.
            {keep_state, Data#{termination_reason => "No logins during the demo period."}}
    end.
%% }}-
%% interdigit_timer {{-
%% TODO Not sure if an event is generated when a timer is canceled. Be prepared for cryptic error messages if that is the case.
handle_event(
  {timeout, interdigit_timer}, % EventType
  eval_collected_digits,        % EventContent
  _State,
  #{ category_selectors := _DTMFDigits } = Data
) ->
    % TODO Prod system along, or play prompt (e.g., "Selection is invalid") if category does not exist.
    handle_digits,
    % Always clear DTMF buffer when the `interdigit_timer` expires, because at this point the buffer has been evaluated (outcome is irrelevant, because at this point user finished putting in digits, hence waiting for the result), and so a clean slate is needed.
    {keep_state, Data#{ category_selectors := [] }}.
%% }}-

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
    [ {"Event-UUID", UUID} ].

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
        lists:flatten(io_lib:format("~p", [now()])),
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

comfort_noise(Milliseconds) ->
    logger:debug("play comfort noise"),
    ComfortNoise = "silence_stream://" ++ Milliseconds ++ ",1400",
    sendmsg_locked(execute, ["playback", ComfortNoise]).

% TODO Try out `mod_vlc` to play aac and m4a files. {{-
% sendmsg(UUID, execute, ["playback", "/home/toraritte/clones/main.mp3"]),
% sendmsg_locked(UUID, execute, ["playback", "/home/toraritte/clones/phone-service/ro.mp3"]),
% }}-
play({greeting, AuthStatus}) ->
    logger:debug("play greeting"),
    Welcome = "Welcome to Access News, a service of Society For The Blind in Sacramento, California, for blind, low-vision, and print-impaired individuals.",
    SelectionSkip = "If you know your selection, you may enter it at any time, or press star or pound to skip to listen to the main categories",
    Unregistered =
        case AuthStatus of
            registered ->
                "";
            unregistered ->
                "You are currently in demo mode, and have approximately 5 minutes to try out the system before getting disconnected. To log in, dial 0, pound, followed by your code, or if you would like to sign up up for Access News, please call us at 916, 889, 7519, or dial 0 2 to leave a message with your contact details."
        end,
    GoToTutorial = "To listen to the tutorial, dial 01.",
    GoToBlindnessServices = "To learn about other blindness services, dial 03.",
    LeaveMessage = "If you have any questions, comments, or suggestions, please call 916 889 7519, or dial 02 to leave a message.",
    % GoToMainMenu = "For the main menu, press 0.",
    % EnterFirstCategory = "To start exploring the categories one by one, press pound, 9 to enter the first category.",
    % CategoryBrowsePound = "To enter the next item, press the pound sign. Dial pound, 0 to get back into the previous item.",
    speak(
         Welcome
      ++ Unregistered
      % ++ GoToMainMenu
      ++ SelectionSkip
      ++ GoToTutorial
      ++ GoToBlindnessServices
      ++ LeaveMessage
    ).
%% }}-

speak(Text) ->
    sendmsg_locked(execute, ["speak", "flite|kal|" ++ Text]).

% vim: set fdm=marker:
% vim: set foldmarker={{-,}}-:
% vim: set nowrap:
