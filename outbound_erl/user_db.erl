-module(user_db).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Requires `phone_numbers` file to be present, holding %%
%% the registered users' phone numbers!                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export(
   [ start/0
   , start_link/0

   % gen_server callbacks
   , init/1
   , handle_call/3
   , handle_cast/2
   , terminate/2

   % private functions
   , load_phone_numbers/0
   ]).

-define(USER_FILE, "phone_numbers").

start_link() ->
    {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    Pid.

start() ->
    {ok, Pid} = gen_server:start({local, ?MODULE}, ?MODULE, [], []),
    Pid.

init(_Args) ->
    %% Set up logging.
    filog:add_singleton_handler(?MODULE),
    filog:singleton_handler_filter(?MODULE),
    %% Init DB
    PhoneNumberSet = load_phone_numbers(),
    {ok, PhoneNumberSet}.

handle_call(PhoneNumber, {Pid, _Ref}, PhoneNumberSet) ->
    RegisteredStatus =
        sets:is_element(
            list_to_binary(PhoneNumber),
            PhoneNumberSet
         ),
    log(debug, [look_up_number, Pid, PhoneNumber, RegisteredStatus]),
    {reply, RegisteredStatus, PhoneNumberSet}.

handle_cast(reload_db = Request, _PhoneNumberSet) ->
    log(debug, [reload_db, Request]),
    NewPhoneNumberSet = load_phone_numbers(),
    {noreply, NewPhoneNumberSet}.

terminate(Reason, _PhoneNumberSet) ->
    log(debug, [terminate_making_sure, Reason]),
    filog:remove_singleton_handler(?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions %%
%%%%%%%%%%%%%%%%%%%%%%%

load_phone_numbers() ->
    {ok, FileBin} = file:read_file(?USER_FILE),
    BinStrings = string:split(FileBin, "\n", all),
    sets:from_list(BinStrings).

log(Level, ValueList) ->
    filog:log(Level, ?MODULE, ValueList).

% vim: set fdm=marker:
% vim: set foldmarker={{-,}}-:
