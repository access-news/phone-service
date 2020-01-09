-module(ivr).
-behaviour(gen_statem).

-export(
    [ start/1
    , start_link/1

    % gen_server callbacks
    , init/1
    , callback_mode/0
    , terminate/3

    % state callbacks
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

init(_Args) ->
    %% Set up logging to file. {{-
    filog:add_process_handler(?MODULE),
    filog:process_handler_filter(?MODULE),
    %% }}-
    %%   {IVRState, CallerStatus, DTMFString}
    {ok, unregistered, #{dtmf_digits => ""}}.

callback_mode() ->
    state_functions.

%% Remove log handler
terminate(Reason, State, Data) -> %% {{-
    filog:process_log(debug, #{from_function=> terminate, args => #{ reason => Reason, state => State, data => Data}}),
    filog:remove_process_handler(?MODULE).
%% }}-

%%%%%%%%%%%%%%%%%%%%%
%% State callbacks %%
%%%%%%%%%%%%%%%%%%%%%

unregistered(
  info,
  {call, {event, [UUID | FSEventHeaders]}} = EventContent,
  #{ dtmf_digits := Digits } = Data
) ->
    {next_event, internal, Data#{ fs_event_headers


%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions %%
%%%%%%%%%%%%%%%%%%%%%%%

%% The async  version of  `register_status/1`. Probably {{-
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
%% }}-
caller_status(EventHeaders) -> % {{-
    CallPid = self(),
    F = fun() ->
            CallerStatus = register_status(EventHeaders),
            gen_server:cast(CallPid, CallerStatus)
        end,
    spawn(F).
%% }}-

register_status(EventHeaders) -> %% {{-
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
                filog:process_log(emergency, [register_status, "Can't pull phone number from " ++ CallerID]),
                exit("invalid")
        end,
    case lookup(PhoneNumber) of
        true  -> registered;
        false -> unregistered
    end.
%% }}-

%% Only for emphasizing the side effect (i.e., the timer). {{-
%% ----------------------------------------------------
%% Could've just shoved  it in `register_status/1`, but
%% (1) it would've  been easy to miss,  and (2) neither
%% function  names  show  that  there will  be  a  side
%% effect, so this way it is a bit more explicit.
%% ----------------------------------------------------
%% TODO:  How   would  this  be  done   in  Haskell  or
%%        PureScript? Monads?
%% }}-
authenticate_caller(EventHeaders) ->
    CallerStatus = register_status(EventHeaders),
    case CallerStatus of
        registered ->
            noop;
        unregistered ->
            timer:send_after(?DEMO_TIMEOUT, unregistered_timer)
            %% Timeout handled in handle_info clauses.
    end,
    CallerStatus.

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

do_sendmsg(SendmsgCommand, Args, IsLocked) -> % {{-
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
    fsend({sendmsg, UUID, FinalHeaders}).
%% }}-

sendmsg(SendmsgCommand, Args) when is_list(Args) ->
    do_sendmsg(SendmsgCommand, Args, false).

sendmsg_locked(SendmsgCommand, Args) when is_list(Args) ->
    do_sendmsg(SendmsgCommand, Args, true).

fsend(Msg) ->
    {lofa, ?FS_NODE} ! Msg.

get_header_value(Header, EventHeaders) ->
    proplists:get_value(Header, EventHeaders).

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
