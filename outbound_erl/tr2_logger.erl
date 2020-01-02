-module(tr2_logger).
-export(
    [ start/0
    , add_handler/1
    , log/2
    , remove_handler/0
    ]).

%% Because I keep forgetting:
%% emergency | alert | critical | error | warning | notice | info | debug
start() ->
    logger:set_primary_config(level, debug),
    %% To make less noise on the console:
    %% logger:set_handler_config(default, level, notice),
    add_handler(self()).

add_handler(Pid) when is_pid(Pid) ->
    Nodename = atom_to_list(node()),
    % In Elixir,
    % ---------------------------------------------------
    % iex(6)> \
    % :os.timestamp()                                    /
    % |> Tuple.to_list()                                 /
    % |> Enum.reduce("", &(&2 <> Integer.to_string(&1)))
    % #> "1576864416865548"
    % ---------------------------------------------------
    % or simply
    % ---------------------------------------------------
    % iex(7)> DateTime.utc_now() |> DateTime.to_iso8601()
    % #> "2019-12-20T17:53:52.995118Z"
    % ---------------------------------------------------
    Timestamp =
        lists:foldl(
            fun(Int, Acc) -> Acc ++ integer_to_list(Int) end,
            "",
            tuple_to_list(os:timestamp())
         ),
    PidString =
        lists:filter(
            fun(Elem) -> not(lists:member(Elem, [$<, $>])) end,
            pid_to_list(Pid)
         ),
    Filename =
        "./" ++ Nodename  ++
        "-"  ++ Timestamp ++
        "-"  ++ PidString ++ ".log",
    Config =
        #{ config =>
            #{ file  => Filename }
             , level => debug
         },
    logger:add_handler(
      list_to_atom(Filename),
      logger_std_h,
      Config
     ).

remove_handler() ->
    logger:remove_handler(
      hd(
        logger:get_handler_ids()
       )
     ).

log(Level, Args) ->
    logger:Level(
        #{ self() => Args }
     ).
