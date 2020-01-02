-module(call_control).
-behaviour(gen_server).

-export(
    [ start/1
    , start_link/1

    % gen_server callbacks
    , init/1
    , handle_info/2
    % , handle_call/3
    % , handle_cast/2
    , terminate/2

    % FreeSWITCH helpers
    , sendmsg/2
    , sendmsg_locked/2
    , get_header_value/2
    ]).

-define(FS_NODE, 'freeswitch@tr2').

%% How to start this as outbound node on the terminal? {{{2
%% To yank the command lines without the comments, do
%% qaq
%% :'<,'>g/^[^#]/y A
%% :let @*=@a
%% See https://vi.stackexchange.com/questions/11217/ as well
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% erl -eval 'cover:compile_directory("./outbound_erl").' -eval '{lofa, freeswitch@tr2} ! register_event_handler.' -run tr2_logger -run user_db -sname access_news -setcookie <cookie>

% erl                                                      \
% # Make sure all necessary Erlang files are compiled.
% -eval 'cover:compile_directory("./outbound_erl").'       \
% # Register  node   as  a  FreeSWITCH   event  handler.
% # Could've  put this  into one  of the  `-run` modules
% # below, but this  way it is more  explicit, and won't
% # forget about it.
% -eval '{lofa, freeswitch@tr2} ! register_event_handler.' \
% # Both  have  a  `start/0` function  that  is  invoked
% # automatically.
% -run tr2_logger                                          \
% -run user_db                                             \
% -sname access_news                                       \
% -setcookie <cookie>

% Vim command to parse logs {{{2
% execute 'g/\({"Event-Name\)/s//\r\1/g ' | execute 'g/^{/s/}/}\r/g' | execute 'g/\\r\\n/s//\r/g' | g/^20\(19\|20\)/s/^/\r/g

%% The  `start*` functions  follow the  requirements of {{{1
%% FreeSWITCH's  `mod_erlang_event`. See  section 7.1.3
%% in the `mod_erlang_event` documentation.
start_link(Ref) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    tr2_logger:add_handler(Pid),
    {Ref, Pid}.

start(Ref) ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    tr2_logger:add_handler(Pid),
    {Ref, Pid}.

init(_Args) ->
    %%   {FSMState, DTMFString}
    {ok, {init, ""}}.

%% Match on `init` because  this module is for outbound
%% mode,  so   any  incoming  calls  should   meet  the
%% gen_server in `init` state.
handle_info(
  {call, {event, [UUID | EventHeaders]}} = Info,
                      {init, DTMFString} = State
 ) ->
    logger:debug(#{ {incoming_call, self(), handle_info, State} => Info}),
    %% Would  name the  outbound  call handler  gen_servers
    %% after the call UUIDs if that would be handed down by
    %% FreeSWITCH instead of a Ref,  because it is a static
    %% value unique to each call, so the process dictionary
    %% is the next best thing.
    put(uuid, UUID),
    FSMState = register_status(EventHeaders),
    %% Schedule hangup if user is not registered.
    case FSMState of
        registered ->
            noop;
        unregistered ->
            %% Demo mode set to 10 minutes
            timer:send_after(600000, unregistered_timer)
            %% Timeout handled in the next handle_info clauses.
    end,
    sendmsg("execute", ["answer", []]),
    sendmsg("execute", ["playback", "/home/toraritte/clones/main.mp3"]),
    {noreply, {FSMState, DTMFString}};

handle_info(unregistered_timer, {registered, _DTMFString} = State) ->
    {noreply, State};

handle_info(unregistered_timer, {unregistered, _DTMFString} = State) ->
    sendmsg("hangup", ["16"]),
    {noreply, State};

handle_info({call_hangup, _ChannelID} = Info, {_FSMState, DTMFString} = State) ->
    tr2_logger:log(debug, [handle_info, State, Info]),
    {stop, normal, {hangup, DTMFString}};

% Ez mire kellett?
% handle_info(disconnected = Info, State) ->
%     tr2_logger:log(handle_info, [State, Info]),
%     {stop, Info, State};

handle_info(
  {call_event, {event, [UUID | EventHeaders]}},
  {FSMState, DTMFString} = State
 ) ->
    process_call_event(EventHeaders),
    Digit = get_header_value("DTMF-Digit", EventHeaders),
    % tr2_logger:log("DTMF digit", Digit),
    {noreply, State};

handle_info(Info, State) ->
    tr2_logger:log(warning, [handle_info, "not_handled_message", State, Info]),
    {noreply, State}.

%% -- used with `caller_status/1` {{{2
%% Match  on  `unregistered` state,  because,  ideally,
%% this type  of cast should  only be received  in this
%% state.
% handle_cast(CallerStatus, {unregistered, DTMFString} = _State) ->
%     tr2_logger:log(handle_cast, CallerStatus),
%     %%        NewState
%     {noreply, {CallerStatus, DTMFString}}.

% implement it for e.g., when the caller gets disconnected
% see "Hangup Cause Code Table" for the reason of disconnect
% if the caller hangs up, it will be NORMAL_CLEARING
% TODO should the system care if the hangup is other than normal, or
% just save the state indiscriminately?

terminate(Reason, State) ->
    tr2_logger:log(debug, [terminate, Reason, State]),
    tr2_logger:remove_handler().

%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions %%  {{{2
%%%%%%%%%%%%%%%%%%%%%%%

%% The async version of `register_status/1`. Probably
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
caller_status(EventHeaders) -> % {{{1
    CallPid = self(),
    F = fun() ->
            CallerStatus = register_status(EventHeaders),
            gen_server:cast(CallPid, CallerStatus)
        end,
    spawn(F).

register_status(EventHeaders) ->
    CallerID = get_header_value("Channel-ANI", EventHeaders),
    PhoneNumber =
        case CallerID of
            %% Could've matched this with `get_header_value` but if
            %% the pattern  match fails, the gen_server  would just
            %% quietly go down. (If this would be a gen_server then
            %% `terminate/2`  would be  called  first; if  defined,
            %% that is.)
            "+1" ++ Number -> Number;
            _Invalid ->
                tr2_logger:log(debug, [register_status, "Can't pull phone number from " ++ CallerID]),
                exit("invalid")
        end,
    case lookup(PhoneNumber) of
        true  -> registered;
        false -> unregistered
    end.

lookup(PhoneNumber) ->
    gen_server:call(user_db, PhoneNumber).

%% FreeSWITCH helpers

%% Only implemented for `execute` and `hangup` for now.
%% Even these aren't that well documented.
%% See available `sendmsg` commands at
%% https://freeswitch.org/confluence/display/FREESWITCH/mod_event_socket#mod_event_socket-3.9.1Commands
sendmsg_headers("execute", [App, Args]) -> 
    %% "loops"  header   and  alternate  format   for  long
    %% messages (is it needed here?)  not added as they are
    %% not needed yet.
    [ {"execute-app-name", App}
    , {"execute-app-arg", Args}
    ];

sendmsg_headers("hangup", [HangupCode]) ->
    %% For hangup codes, see
    %% https://freeswitch.org/confluence/display/FREESWITCH/Hangup+Cause+Code+Table
    [{"hangup-cause", HangupCode}];

%% This will  blow up,  one way or  the other,  but not
%% planning to get there anyway.
sendmsg_headers(SendmsgCommand, Args) ->
    tr2_logger:log(
      emergency,
      ["`sendmsg` command not implemented yet"
      , SendmsgCommand
      , Args
      ]
    ),
    [].

do_sendmsg(SendmsgCommand, Args, IsLocked) ->
    UUID = get(uuid),
    LockHeaderList =
        case IsLocked of
            false -> [];
            true  -> [{"event-lock", "true"}]
        end,
    FinalHeaders =
        [{"call-command", SendmsgCommand}]
        ++ sendmsg_headers(SendmsgCommand, Args)
        ++ LockHeaderList,
    {sendmsg, ?FS_NODE} ! {sendmsg, UUID, FinalHeaders}.

sendmsg(SendmsgCommand, Args) when is_list(Args) ->
    do_sendmsg(SendmsgCommand, Args, false).

sendmsg_locked(SendmsgCommand, Args) when is_list(Args) ->
    do_sendmsg(SendmsgCommand, Args, true).

get_header_value(Header, EventHeaders) ->
    proplists:get_value(Header, EventHeaders).

process_call_event(EventHeaders) ->
    EventName = get_header_value("Event-Name", EventHeaders).

% vim: set fdm=marker:
