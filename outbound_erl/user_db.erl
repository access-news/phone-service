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

   , load_phone_numbers/0
   ]).

-define(USER_FILE, "phone_numbers").

start_link() ->
    {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    tr2_logger:add_handler(Pid),
    Pid.

start() ->
    {ok, Pid} = gen_server:start({local, ?MODULE}, ?MODULE, [], []),
    tr2_logger:add_handler(Pid),
    Pid.

init(_Args) ->
    PhoneNumberSet = load_phone_numbers(),
    {ok, PhoneNumberSet}.

handle_call(PhoneNumber, From, PhoneNumberSet) ->
    tr2_logger:log(debug, [look_up_number, From, PhoneNumber]),
    RegisteredStatus =
        sets:is_element(
            list_to_binary(PhoneNumber),
            PhoneNumberSet
         ),
    {reply, RegisteredStatus, PhoneNumberSet}.

handle_cast(reload_db = Request, _PhoneNumberSet) ->
    tr2_logger:log(debug, [reload_db, Request]),
    NewPhoneNumberSet = load_phone_numbers(),
    {noreply, NewPhoneNumberSet}.

terminate(Reason, PhoneNumberSet) ->
    tr2_logger:log(debug, [terminate_making_sure, Reason]),
    tr2_logger:remove_handler().

%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions %%
%%%%%%%%%%%%%%%%%%%%%%%

load_phone_numbers() ->
    {ok, FileBin} = file:read_file(?USER_FILE),
    BinStrings = string:split(FileBin, "\n", all),
    sets:from_list(BinStrings).
