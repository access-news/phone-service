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
-define(DEMO_TIMEOUT, 600000).
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
% If you have any questions, comments, or suggestions, please call 916 889 7519, or dial 02 to leave a message.
% To learn about other blindness services, dial 03.
% To start exploring menu items one by one, press #. Press # again to skip to the next, and dial #0 to jump back to the previous one.

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
       %%   {IVRState,      CallerStatus}
    logger:debug("==========================="),
    logger:debug("==========================="),
    logger:debug("==========================="),

    process_flag(trap_exit, true),
    State = {incoming_call, unregistered},
    Data = #{dtmf_digits => ""},
    {ok, State, Data}.

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
  {ModErlEventCallStatus, {event, [UUID | FSEventHeaders]}} = ModErlEventMsg,
  % {ModErlEventCallStatus, {event, [UUID | FSEventHeaders]}} = ModErlEventMsg,
  State,
  Data
) ->
    MassagedModErlEvent =
        { UUID
        , ModErlEventCallStatus
        , maps:from_list(FSEventHeaders)
        },
    TransitionActions =
        [ {next_event, internal, MassagedModErlEvent}
        ],
    %% As this  clause preceeds **every**  state transition
    %% (not   just  state   changes!)   AND  always   keeps
    %% `gen_statem` state and data,  this log does not need
    %% to be  repeated in  `handle_event/4` below,  only to
    %% double-check matched values, calculations etc.

    % filog:process_log(debug, #{ from => ["MOD_ERL_EVENT_MASSAGE", #{ data => Data, mod_erlang_event_message => ModErlEventMsg, friendly_mod_erlang_event_message => MassagedModErlEvent,  state => State }]}),
    % logger:debug(#{ self() => ["MOD_ERL_EVENT_MASSAGE", #{ data => Data, mod_erlang_event_message => ModErlEventMsg, friendly_mod_erlang_event_message => MassagedModErlEvent,  state => State }]}),
    logger:debug(#{ self() => ["MOD_ERL_EVENT_MASSAGE", #{ data => Data, state => State, mod_erl_event_call_status => ModErlEventCallStatus }]}),
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
    % filog:process_log(debug, #{ from => ["CALL_HANGUP", #{ data => Data, state => State }]}),
    logger:debug(#{ self() => ["CALL_HANGUP", #{ data => Data, state => State }]}),
    {stop, normal};
%% }}-

handle_event(
  info,                          % EventType
  ok = Msg,
  State,
  _Data                               % Data
) ->
    logger:debug(#{ self() => ["SENDMSG_CONFIRMATION", #{ message => Msg, state => State}]}),
    keep_state_and_data;

handle_event(
  info,                          % EventType
  Msg,
  State,
  _Data                               % Data
) ->
    logger:emergency(#{ self() => ["UNKNOWN", #{ unknown_msg => Msg, state => State}]}),
    keep_state_and_data;

%%%%%%%%%%%%%%%%%%%%%%%%
%% `internal` clauses %%
%%%%%%%%%%%%%%%%%%%%%%%%

%% INCOMING_CALL (internal) {{-

%% This is the only state where `call` event can arrive
%% -  or at  least this  is  how it  should be;  `call`
%% should happen only once in  a call, and becuase this
%% is  outbound mode,  and  the  process just  started,
%% there is not much else flying around.

handle_event(
  internal,                          % EventType
  { UUID                             % \
  , call                             % |
  , #{ "Channel-ANI" := CallerNumber % | EventContent = MassagedModErlEvent
     }                               % |
  },                                 % /
  {incoming_call, unregistered} = State,     % State
  Data                               % Data
) ->
    % filog:process_log(debug, #{ from => {mod_erlang_event_call_status, "INCOMING_CALL"} }),
    logger:debug(#{ self() => ["INCOMING_CALL", #{ data => Data, state => State }]}),

    sendmsg(UUID, execute, ["answer", []]),

    { CallerStatus
    , TransitionActions
    } =
        case is_user_registered(CallerNumber) of
            true ->
                { registered
                , []
                };
            false ->
                { unregistered
                , [ { {timeout, unregistered_timer}
                    , ?DEMO_TIMEOUT
                    , {hang_up, UUID}
                    }
                  ]
                }
        end,
    {next_state, {greeting, CallerStatus}, Data, TransitionActions};
%% }}-

%% GREETING (internal) {{-
handle_event(
  internal,                          % EventType
  { UUID                             % \
  , call_event                             % |
  , #{ "Event-Name" := "CHANNEL_ANSWER" } % | EventContent = MassagedModErlEvent
  },                                 % /
  {greeting, _CallerStatus} = State,     % State
  _Data                               % Data
) ->
    logger:debug(#{ self() => ["GREETING", #{ state => State}]}),

    % TODO start playback of main menu here (don't forget of repeats)

    % TODO Try out `mod_vlc` to play aac and m4a files.

    % REMINDER The default `playback_terminator` is  *, but that is
    %          ok because  it is tied  to going up/back a  menu, so
    %          playback should be stopped anyway.
    %          https://freeswitch.org/confluence/display/FREESWITCH/playback_terminators

    % sendmsg(UUID, execute, ["playback", "/home/toraritte/clones/main.mp3"]),
    sendmsg_locked(UUID, execute, ["playback", "silence_stream://750,1400"]),
    % sendmsg_locked(UUID, execute, ["speak", "flite|kal|Welcome to Access News, a service of Society For The Blind in Sacramento, California for blind, low-vision, and print-impaired individuals."]),
    sendmsg_locked(UUID, execute, ["playback", "/home/toraritte/clones/phone-service/ro.mp3@@1300000"]),

    keep_state_and_data;
%% }}-

%% HANDLE_DTMF (internal) {{-
handle_event(
  internal,                          % EventType
  { UUID                             % \
  , call_event                             % |
  , #{ "DTMF-Digit" := Digit} % | EventContent = MassagedModErlEvent
  },                                 % /
  State,
  Data                               % Data
) ->
    logger:debug(#{ self() => ["DTMF", #{ digit => Digit, state => State}]}),
    GenStatemReturn = handle_dtmf(State, Data, Digit, UUID),
    sendmsg_locked(UUID, execute, ["speak", "flite|kal|Welcome to Access News, a service of Society For The Blind in Sacramento, California for blind, low-vision, and print-impaired individuals."]),
    GenStatemReturn;
%% }}-

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

%%%%%%%%%%%%%%
%% Timeouts %%
%%%%%%%%%%%%%%

%% unregistered_timer {{-
handle_event(
  {timeout, unregistered_timer}, % EventType
  {hang_up, _UUID},              % EventContent
  {_IVRState, registered} = State,       % State
  Data                          % Data
 ) ->
    logger:debug(#{ self() => ["UNREGISTERED_TIMEOUT", #{ data => Data, state => State }]}),
    keep_state_and_data;

handle_event(
  {timeout, unregistered_timer}, % EventType
  {hang_up, UUID},               % EventContent
  {_IVRState, unregistered}      = State,
  Data                           % Data
 ) ->
    %% See "CALL_HANGUP" `handle_event/4`
    logger:debug(#{ self() => ["UNREGISTERED_TIMEOUT", #{ data => Data, state => State }]}),
    sendmsg(UUID, hangup, ["16"]),
    %% Gave `normal` as reason, otherwise `gen_statem` will
    %% crash  (which isn't  really a  problem, because  the
    %% started `gen_statem`  processes are not  linked, and
    %% could've  just trap  exits, but  why go  through the
    %% hassle, when  a demo timeout is  considered a normal
    %% behaviour.)
    {stop, normal, Data#{termination_reason => "user did not log in"}}.
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

sendmsg_headers(execute, [App, Args]) ->
    %% TODO "loops"  header   and  alternate  format   for  long
    %%      messages (is it needed here?)  not added as they are
    %%      not needed yet.
    [ {"execute-app-name", App}
    , {"execute-app-arg", Args}
    ];

sendmsg_headers(hangup, [HangupCode]) ->
    %% For hangup codes, see
    %% https://freeswitch.org/confluence/display/FREESWITCH/Hangup+Cause+Code+Table
    [{"hangup-cause", HangupCode}];

%% This will  blow up,  one way or  the other,  but not
%% planning to get there anyway.
sendmsg_headers(SendmsgCommand, Args) ->
    % filog:process_log(
    %   emergency,
    %   ["`sendmsg` command not implemented yet"
    %   , SendmsgCommand
    %   , Args
    %   ]
    % ),
    [].

do_sendmsg(UUID, SendmsgCommand, Args, IsLocked) ->
    LockHeaderList =
        case IsLocked of
            false -> [];
            true  -> [{"event-lock", "true"}]
        end,
    FinalHeaders =
        [{"call-command", atom_to_list(SendmsgCommand)}]
        ++ sendmsg_headers(SendmsgCommand, Args)
        ++ LockHeaderList,
    fsend({sendmsg, UUID, FinalHeaders}).

sendmsg(UUID, SendmsgCommand, Args) when is_list(Args) ->
    do_sendmsg(UUID, SendmsgCommand, Args, false).

sendmsg_locked(UUID, SendmsgCommand, Args) when is_list(Args) ->
    do_sendmsg(UUID, SendmsgCommand, Args, true).
%% }}-

fsend(Msg) ->
    %% Why the `lofa` atom:
    %% https://stackoverflow.com/questions/58981920/
    {lofa, ?FS_NODE} ! Msg.

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

% TR2 controls {{-
% }}-

% handle_dtmf({publication, _CallerStatus, DTMFString} = State, "5") ->
%     timer:send_after(?DTMF_DIGIT_TIMEOUT, dtmf_digit_timeout),
%     fsend({bgapi, uuid_fileman, get(uuid) ++ " restart"}),
%     State;

handle_dtmf({greeting, CallerStatus}, #{ dtmf_digits := StateDigits } = Data, Digit, UUID) ->
    % stop playback
    fsend({api, uuid_break, UUID ++ " all"}),
    keep_state_and_data;
    %case Digit of
    %    0 ->
    %        %next_state with next_event actually
    %        {next_state, {main_menu, CallerStatus}, Data};

handle_dtmf(State, Data, Digit, _UUID) ->
    logger:debug(""),
    logger:emergency(#{ self() => ["UNHANDLED_DTMF", #{ data => Data, digit => Digit, state => State}]}),
    logger:debug(""),
    keep_state_and_data.

% vim: set fdm=marker:
% vim: set foldmarker={{-,}}-:
% vim: set nowrap:
