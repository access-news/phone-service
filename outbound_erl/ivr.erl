-module(ivr).
-behaviour(gen_statem).

-export(
    [ start/1
    , start_link/1

    % gen_server callbacks
    , init/1
    , callback_mode/0
    , handle_event/4
    , terminate/3
    ]).

-define(FS_NODE, 'freeswitch@tr2').
-define(DEMO_TIMEOUT, 600000).
-define(DTMF_DIGIT_TIMEOUT, 5000).

start_link(Ref) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    {Ref, Pid}.

start(Ref) ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
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
%               |<--- "MOD_ERL_CALL"
%               |
%               V
% *------------------------------------*
% | {main_menu, CallerStatus}          |
% *------------------------------------*

% }}- }}-
init(_Args) ->
    %% Set up logging to file. {{- {{-
    filog:add_process_handler(?MODULE),
    filog:process_handler_filter(?MODULE),
    filog:process_log(debug, #{ from => ["INIT"] }),
    %% }}- }}-
    %%   {IVRState,      CallerStatus}
    {ok, {incoming_call, unregistered}, #{dtmf_digits => ""}}.

callback_mode() ->
    handle_event_function.

%% Remove log handler
terminate(Reason, State, Data) -> %% {{-
    filog:process_log(debug, #{ from => ["TERMINATE", #{ reason => Reason, state => State, data => Data }]}),
    filog:remove_process_handler(?MODULE).
%% }}-

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
%% incoming_call(info, ModErlEvent, _Data) ->
%%     NewFSEvent = do_the_deed,
%%     {keep_state_and_data, {next_event, internal, NewFSEvent}};
%% incoming_call(internal, MassagedFSEvent, Data) ->
%%     do_something_here,
%%     {next_state, ...}.
%%
%% %% ... N more other states
%%
%% publication(info, ModErlEvent, _Data) ->
%%     NewFSEvent = do_the_deed,
%%     {keep_state_and_data, {next_event, internal, NewFSEvent}};
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
%% handle_event(info, ModErlEvent, _State, _Data) ->
%%     NewFSEvent = do_the_deed,
%%     {keep_state_and_data, {next_event, internal, NewFSEvent}};
%%
%% handle_event(internal, NewFSEvent, State, Data) ->
%%     %% ...
%%     {next_state, ...}.
%%
%% handle_event(internal, NewFSEvent, AnotherState, Data) ->
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
%% The   event  content   is   name  `ModErlEvent`   on {{- {{-
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
  {ModErlEventCallStatus, {event, [UUID | FSEventHeaders]}} = ModErlEvent,
  State,
  Data
) ->
    NewFSEvent =
        { UUID
        , ModErlEventCallStatus
        , maps:from_list(FSEventHeaders)
        },
    TransitionActions =
        [ {next_event, internal, NewFSEvent}
        ],
    %% As this  clause preceeds **every**  state transition
    %% (not   just  state   changes!)   AND  always   keeps
    %% `gen_statem` state and data,  this log does not need
    %% to be  repeated in  `handle_event/4` below,  only to
    %% double-check matched values, calculations etc.
    filog:process_log(debug, #{ from => ["MOD_ERL_EVENT_MASSAGE", #{ data => Data, fs_event => ModErlEvent, new_fs_event => NewFSEvent,  state => State }]}),
    {keep_state_and_data, TransitionActions};
%% }}-

%% CALL_HANGUP (info) {{-
handle_event(
  info,               % EventType                
  {call_hangup, UUID} % EventContent = ModErlEvent
  State,              %                           
  Data                %                         
) ->
    filog:process_log(debug, #{ from => ["CALL_HANGUP", #{ data => Data, state => State }]}),
    {stop, normal}.

  % State
  {main_menu, _CallerStatus},

  % Data
  Data
) ->
%% }}-

%% MOD_ERL_CALL (internal) {{-
handle_event(
  internal = _EventType,             % EventType
  { UUID                             % \
  , call = _ModErlEventCallStatus    % |
  , #{ "Channel-ANI" := CallerNumber % | EventContent = MassagedModErlEvent
     }                               % |
  },                                 % /
  {incoming_call, unregistered},     % State
  Data                               % Data
) ->
    filog:process_log(debug, #{ from => {mod_erlang_event_call_status, "MOD_ERL_CALL"} }),

    sendmsg(UUID, execute, ["answer", []]),

    { CallerStatus
    , TransitionActions
    } =
        case is_user_registered(CallerNumber) of
            true ->
                {registered, []};
            false ->
                { unregistered
                , [{{timeout, unregistered_timer}, ?DEMO_TIMEOUT, {hang_up, UUID}}]
                }
        end,
    {next_state, {main_menu, CallerStatus}, Data, TransitionActions}.
%% }}-

%%%%%%%%%%%%%%
%% Timeouts %%
%%%%%%%%%%%%%%

%% unregistered_timer {{-
handle_event(
  {timeout, unregistered_timer}, % EventType
  {hang_up, UUID},               % EventContent
  {_IVRState, registered},       % State
  _Data                          % Data
 ) ->
    keep_state_and_data;

handle_event(
  {timeout, unregistered_timer}, % EventType
  {hang_up, UUID},               % EventContent
  {_IVRState, registered}        = State,
  Data                           % Data
 ) ->
    %% See "CALL_HANGUP" `handle_event/4`
    sendmsg(UUID, hangup, ["16"]),
    {next_state, hangup, State, Data}.
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
%             gen_server:cast(CallPid, CallerStatus)
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
                filog:process_log(emergency, #{ from => ["IS_USER_REGISTERED", #{ caller_number => CallerNumber}]}),
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
sendmsg_headers(SendmsgCommand, Args) -> %% {{-
    filog:process_log(
      emergency,
      ["`sendmsg` command not implemented yet"
      , SendmsgCommand
      , Args
      ]
    ),
    [].
%% }}-

do_sendmsg(UUID, SendmsgCommand, Args, IsLocked) -> % {{-
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
%% }}-

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

handle_dtmf({publication, _CallerStatus, DTMFString} = State, "5") ->
    timer:send_after(?DTMF_DIGIT_TIMEOUT, dtmf_digit_timeout),
    fsend({bgapi, uuid_fileman, get(uuid) ++ " restart"}),
    State.

% vim: set fdm=marker:
% vim: set foldmarker={{-,}}-:
% vim: set nowrap:
