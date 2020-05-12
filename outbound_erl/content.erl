-module(content).
-behaviour(gen_server).

-define(CONTENT_ROOT_DIR, "/home/toraritte/clones/phone-service/content-root/").  % -define(CONTENT_ROOT, {category, 0, "Main category"}).

-export(
    [ start/0
    , start_link/0

    % gen_server callbacks
    , init/1
    , handle_call/3
    % , handle_cast/2
    % , terminate/2

    % public API
    , pick/2
    , root/0

    % private functions
    , make_content_graph/0
    , refresh_content_graph/1
    , realize/0

    , get_vertex/3
    , process_call/3
    , get_meta/1
    % , current/1
    % , update_history/2
    ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks                                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    Pid.

start() ->
    {ok, Pid} = gen_server:start({local, ?MODULE}, ?MODULE, [], []),
    Pid.

init(_Args) ->
    Graph = make_content_graph(),
    % digraph:add_vertex(Graph, history, []),
    % Necessary because ?CONTENT_ROOT gets written to the metafile, but that tuple is going to be changed when building the graph (and reading back from the file system).
    % ContentRoot =
    %     lists:keyfind(0, 2, digraph:vertices(Graph)),
    % digraph:add_edge
    %     ( Graph
    %     , current         % edge name
    %     , history         % from vertex
    %     , ContentRoot   % to
    %     , [ContentRoot] % label, moonlighting as history stack
    %     ),

    {ok, Graph}.

%           request,      from,        state
handle_call({Direction, Vertex}, _From, Graph)
% handle_call({Action, Direction}, _From, Graph)
  when Direction =:= parent       % \
     ; Direction =:= first        % |
     ; Direction =:= last         % |
     ; Direction =:= next         % | Vertex
     ; Direction =:= prev         % |
     ; Direction =:= content_root % |
     % ; Direction =:= current      % /
     ; Direction =:= children     %   [ Vertex ]
->
    { reply
    % , process_action(Graph, Action, Direction)
    , process_call(Graph, Vertex, Direction)
    , Graph
    };

handle_call(_Command, _From, Graph) ->
    {reply, invalid_command, Graph}.

% HOW TO SERIALIZE A DIGRAPH {{-
% Serialization is implemented in ivr.erl (probably also commented out)
% process_action
%   ( {digraph, Vertices, Edges, Neighbours, Cyclicity}
%   , get
%   , graph
%   )
% ->
%     { serialized_digraph
%     , ets:tab2list(Vertices)
%     , ets:tab2list(Edges)
%     , ets:tab2list(Neighbours)
%     , Cyclicity
%     };

% Direction -> Vertex
% process_action(Graph, get, current) ->
%     current(Graph);
% }}-

% Direction -> Vertex
process_call(Graph, _Vertex, content_root) ->
    [  Vertex 
    || Vertex
       <- digraph:vertices(Graph)
       , maps:find(selection, Vertex) =:= {ok, 0}
    ];

%! Direction -> [ Vertex ]
process_call(Graph, Vertex, children) ->
    get_vertex(Graph, Vertex, child);

process_call(Graph, Vertex, Direction) ->
    % TODO These are the only 2 options here, and if this does crash it means there's a logical error somewhere in this module.
    get_vertex(Graph, Vertex, Direction).
    % case get_vertex(Graph, Vertex, Direction) of
        % This means that the current vertex does not have that specific direction. E.g., articles won't have child, first, last edges.
        % [] ->
        %     nothing;
        % [_Vertex] = Result ->
        %     Result
        % % [Vx] -> Vx
    % end.

% process_call(Graph, go_to, Direction)
%   when Direction =:= children;
%        Direction =:= invalid_action
% ->
%     invalid_action;

% process_call(Graph, go_to, Vertex)
%   when erlang:is_tuple(Vertex)
% ->
%     update_history(Graph, Vertex),
%     Vertex;

% process_call(Graph, go_to, Direction) ->
%     % TODO
%     % http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html
%     Vertex = process_action(Graph, get, Direction),
%     process_action(Graph, go_to, Vertex).

% handle_cast(reload_db = Request, _PhoneNumberSet) ->
%     log(debug, [reload_db, Request]),
%     NewPhoneNumberSet = load_phone_numbers(),
%     {noreply, NewPhoneNumberSet}.

% terminate(Reason, _Graph) ->
%     log(debug, [terminate_making_sure, Reason]),
%     filog:remove_singleton_handler(?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API                                                         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Direction -> Vertex
% Direction =
%     parent | first | last | next | prev | content_root
% CurrentVertex =
%     #{ type := ContentType, ...} (see content.erl)
% ContentType =
%     category | publication | article
pick(Direction, CurrentVertex) -> % List Content | []
    % { ContentType
    % , _Selection
    % , #{ anchor := AnchorText }
    % } =
    Result =
        gen_server:call
          ( content
          , {Direction, CurrentVertex}
          ),
    case Result of
        [Vertex] -> Vertex; % all except `children`
        [_|_] -> Result;    % `children`
        [] -> none       % Vertex has no specified direction (i.e., it is the last item without a `next` edge, content root was called with parent/next/etc and so on)
    end.
    % content:process_action
      % ( get(content_graph)
      % ( content
      % , CurrentVertex
      % , Direction
      % ).
        % call_content(get, current),
    % Data#{ anchor := AnchorText }.

root() ->
    pick(content_root, ignore).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% CONTENT GRAPH {{-
% import(PublicationGuide) ->
%     Graph =
%         digraph:new([cyclic, protected]), % default values made explicit
%     % "no_parent" means that when adding edges (child, parent, etc) will fail with an `error` tuple, but it will not crash
%     add_content_vertex(Graph, "no_parent", PublicationGuide),

%     % do_items(Graph, ItemList),
%     Graph.

% add_content_vertex
% ( Graph
% , ParentVertex
% , #{  "type" := Type
%    , "title" := Title
%    , "id"    := ID
%    % This presumes that the canonical form of each content map has an `"items"` key (with empty list if no children)
%    , "items" := ItemList
%    } = Map
% )
% ->
%     Vertex   = #{ "title" => Title, "id" => ID }
% ,   digraph:add_vertex(Graph, Vertex)
% ,   add_hierarcy_edges(Graph, ParentVertex, Vertex)

% ,   add_meta_vertices_and_edges(Graph, Vertex, Map)
% ,   do_items(Graph, Vertex, [first|ItemList])
% ,   Vertex
% .

% do_items(Graph, ParentVertex, [LastItem]) ->

% do_items(_Graph, _ParentVertex, [first]) ->
%     done;

% do_items
% ( Graph
% , ParentVertex
% , [ first | [FirstItem|_] = Itemlist]
% )
% ->
%     add_content_vertex(Graph, ParentVertex, FirstItem),
%     do_add_meta(Graph, First

% do_items
% ( Graph
% , ParentVertex
% , [ItemA, ItemB | Rest] = Items
% ) ->

% TODO Depends on the internal representation.
%      Refactor when the web service is ready (or usable).
make_content_graph() ->
    make_content_graph(
      filename:join(?CONTENT_ROOT_DIR, "0")
    ).

make_content_graph(ContentRootDir) -> % {{-
    Graph =
        digraph:new([cyclic, protected]), % default values made explicit
    RootMeta =
        get_meta(ContentRootDir),

    digraph:add_vertex(Graph, RootMeta),
    % NOTE Theoretically it is not necessary because `get_vertex/2` returns [] for non-existing edges
    % [ digraph:add_edge
    %     ( Graph
    %     , {Edge, RootMeta}
    %     , RootMeta
    %     , RootMeta
    %     , []
    %     )
    % || Edge <- [parent, prev, next]
    % ],
    do_make(Graph, ContentRootDir),
    Graph.
% }}-

refresh_content_graph(Graph) ->
    digraph:delete(Graph),
    make_content_graph().

% `EdgeNote` and not `EdgeLabel` because that is already taken for `digraph:add_edge/5`
add_edge(Graph, EdgeNote, FromVertex, ToVertex) ->
    % `digraph` is just 3 ETS tables, and so edges need to be unique, hence the `erlang:system_time/0`. The `Counter` in the tuple is to make temporal ordering absolutely possible (e.g., `first` and `last` edges) because `erlang:system_time/0` values are not **strictly** monotonically increasing values (that is, calling it fast enough may result in the same value, but not lower).
    digraph:add_edge(
      Graph,                 % digraph
      { EdgeNote             % |
      % the UU(ish)ID is needed (e.g., 2 siblings will have the same {parent, ...} edge)
      , erlang:system_time() % | edge
      , ToVertex             % |
      },                     % |
      FromVertex,            % from vertex
      ToVertex,              % to vertex
      []                     % label
    ).

add_hierarcy_edges(Graph, ParentVertex, ChildVertex) -> % {{-
    add_edge(Graph, child,  ParentVertex, ChildVertex),
    add_edge(Graph, parent, ChildVertex, ParentVertex).
% }}-

% add_next_edge(Graph, From, To) ->
%     digraph:add_edge(Graph, {next, To}, From, To, []).

% add_prev_edge(Graph, From, To) ->
%     digraph:add_edge(Graph, {prev, To}, From, To, []).

add_meta_to_path(_ContentType, _Dir, "meta.erl") -> % {{-
    % logger:notice("meta"),
    false;

add_meta_to_path(ContentType, Dir, Path) ->
    % logger:notice(#{path => Path, ct => ContentType}),
    FullPath = filename:join(Dir, Path),
    Meta =
        case ContentType of
               category -> get_meta(FullPath);
            % TODO the anchor text should hold the name of the article's title
            % TODO previous todo also implies that each article should alsw have a metafile, requiring extensive rewrite
            publication ->
                #{ type => article
                 , path => FullPath
                 , title => "article" % formerly known as "anchortext"
                 % FORGET ABOUT MUTABLE STATE IN VERTEX!
                 % , offset => -1 
                 }
        end,
    {true, {Meta, FullPath}}.
% }}-

do_make(Graph, Dir) ->
    case file:list_dir(Dir) of
        {error, _} ->
            done;
        {ok, List} ->
            OrderedDirList =
                ordsets:from_list(List),
            % If ContentType =:= publication then DirList will consist entirely of files (meta.erl + audio files)
            % {ContentType, _, _} = Meta =
            #{ type := ContentType } = Meta =
                get_meta(Dir),
            MetaPathTuples =
                lists:filtermap(
                  % ((curry(fun add_meta_to_path/3))(ContentType))(Dir),
                  fun(Path) -> add_meta_to_path(ContentType, Dir, Path) end,
                  OrderedDirList
                ),
            do_dirlist(Graph, Meta, [first|MetaPathTuples])
    end.

% add_meta_vertices_and_edges(Graph, Vertex, Map) ->
%     MetaMap =
%         maps:filter
%           ( fun(Key, _) ->
%                 not(lists:member(Key, ["title", "id", "items"]))
%             end
%           , Map
%           ),
%     [  do_add_meta(Graph, Vertex, MapElem)
%     || MapElem <- maps:to_list(MetaMap)
%     ].

% do_add_meta(Graph, Vertex, {_Key, _Value} = Tuple) ->
%     do_add_meta(Graph, Vertex, [Tuple]);

% do_add_meta(_Graph, _Vertex, []) ->
%     done;

% do_add_meta(Graph, Vertex, [{Key, Value}|Rest]) ->
%     Meta = #{ Key => Value },
%     digraph:add_vertex(Graph, Meta),
%     add_edge(Graph, meta, Meta, Vertex),
%     do_add_meta(Graph, Vertex, Rest).



do_dirlist(_Graph, _ParentMeta, []) ->
    done;

do_dirlist(Graph, ParentMeta, [{_, "meta.erl"}|Rest]) ->
    do_dirlist(Graph, ParentMeta, Rest);

do_dirlist(_Graph, _ParentMeta, [first]) ->
    empty_dir;

do_dirlist( % {{-
  Graph,
  ParentMeta,
  [ first
  , {Meta, FullPath} = MetaPath
  | Rest
  ]
) ->
    logger:notice(#{ first => MetaPath }),
    % add_vertex_and_parent_edge(Graph, ParentMeta, Meta, first),
    % digraph:add_vertex(Graph, Meta, first),
    digraph:add_vertex(Graph, Meta),
    % digraph:add_edge(Graph, {first, Meta}, ParentMeta, Meta, []),
    add_edge(Graph, first, ParentMeta, Meta),
    add_hierarcy_edges(Graph, ParentMeta, Meta),
    % add_vertex_and_parent_edge(Graph, ParentMeta, Meta),
    case Rest of
        [] ->
            add_edge(Graph, last, ParentMeta, Meta),
            done;
        [_|_] ->
            do_make(Graph, FullPath),
            do_dirlist(Graph, ParentMeta, [MetaPath|Rest])
    end;
% }}-

do_dirlist( % {{-
  Graph,
  ParentMeta,
  [ {MetaA, _} = M
  , {MetaB, FullPathB} = MetaPath
  | Rest
  ]
) ->
    logger:notice(#{ metapath_a => M, metapath_b => MetaPath}),
    % { NewDirList
    % , VertexBLabel
    % } =
    % add_vertex_and_parent_edge(Graph, ParentMeta, MetaB),
    % digraph:add_vertex(Graph, MetaB, VertexBLabel),
    digraph:add_vertex(Graph, MetaB),
    add_hierarcy_edges(Graph, ParentMeta, MetaB),
    % add_prev_edge(Graph, MetaB, MetaA),
    % add_next_edge(Graph, MetaA, MetaB),
    add_edge(Graph, prev, MetaB, MetaA),
    add_edge(Graph, next, MetaA, MetaB),
    do_make(Graph, FullPathB),

    NewDirList =
        case Rest =:= [] of
            true ->
                % digraph:add_edge(Graph, {last, MetaB}, ParentMeta, MetaB, []),
                add_edge(Graph, last, ParentMeta, MetaB),
                [];
            false ->
                [MetaPath|Rest]
        end,
    do_dirlist(Graph, ParentMeta, NewDirList).
% }}-
% }}-

% Graph -> Vertex
% current(Graph) ->
%     {current, history, Current, _History} =
%         digraph:edge(Graph, current),
%     Current.

% Graph -> Direction -> List Vertex
% TODO Direction is misleading because it means smth else in different contexts
% Direction = parent | next | prev | first | last | child
get_vertex(Graph, Vertex, Direction) ->
    % Current = current(Graph),

    % The below usage would be helpful but pattern matching won't work in the generator pattern.
    % https://erlang.org/doc/programming_examples/list_comprehensions.html#variable-bindings-in-list-comprehensions
    % > [ X || {lofa, X} <- [{lofa, 7}, {lofa, 27}]].
    % [7,27]
    % > [ X || {lofa, X} <- [{lofa, 7}, {miez, 27}]].
    % [7]

    % EdgeResults =
        % [  digraph:edge(Graph, Edge)
    [  V
    || {_Dir, _UUishID, V} = Edge
    <- digraph:out_edges(Graph, Vertex),
       erlang:element(1, Edge) =:= Direction
    ].

    % [  Vx % Vertex % this would not shadow the function argument, but confusing
    % || { {Direction, _} % edge
    %    , _              % from
    %    , Vx             % to
    %    , []             % edge label
    %    }
    %    <- EdgeResults
    % ].

% Graph -> Vertex -> current | noop
% update_history(Graph, With) ->
%     { current % edge name
%     , history % from
%     , Current % to
%     , History % edge label
%     } =
%         digraph:edge(Graph, current),

    % case With =:= Current of
    %     true ->
    %         noop;
    %     false ->
    %         digraph:del_edge(Graph, current),
    %         digraph:add_edge
    %             ( Graph
    %             , current        % edge name
    %             , history        % from vertex
    %             , With           % to
    %             , [With|History] % label, moonlighting as history stack
    %             )
    % end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL MODEL OF THE YET TO BE BUILT ACCESS NEWS WEB SERVICE      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% {{-
% NOTE for the resulting graph when determining neighbours:
% to avoid excessive amount of connections (and facing the problems updating them would cause), it will be done by finding the parent category, and adding/subtracting one from their "ID".
% The publication guide below is just a representation of future data of the yet-to-be-implemented core web service, and its data may not contain such IDs, but that could be done on this end by ordering and adding that via a script.

publication_guide() -> % {{-
    [ { {category, 0, "Main category"}
      , [ { {category, 1, "Store sales advertising"}
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
        ]
      }
    ].
% }}-

% TODO converting this to JSON should be trivial
% TODO `meta` edges
%      "directional" edges follow the pattern {dir, #{...}} so tag, type, periodicity (what else?) nodes will be tagged as meta and will be incident edges (is that the right term?)

% Yes, this could have been just the one string below, but it is not.
% string:join([ erlang:integer_to_list(erlang:system_time()) | tl(string:lexemes(erlang:ref_to_list(erlang:make_ref()), ".>"))], "-")
make_id() ->
    NowString =
        f:pipe(
          [ erlang:system_time()
          , fun erlang:integer_to_list/1
          ]
        ),
    RefNumbers =
        f:pipe(
          [ erlang:make_ref()
          , fun erlang:ref_to_list/1
          , (f:cflip(fun string:lexemes/2))(".>")
          ]
        ),
    string:join([NowString|tl(RefNumbers)], "-").

% digraph ets query notes {{-
% 201> {_,V,_,_,_} = G = digraph:new().
% {digraph,#Ref<0.3770885502.3804626945.151678>,
%          #Ref<0.3770885502.3804626945.151679>,
%          #Ref<0.3770885502.3804626945.151680>,true}
% 202> ets:tab2list(V).                
% []
% 203> digraph:add_vertex(G, A, D).                                       
% {#Ref<0.3770885502.3804495875.151576>,1587669065273556493}
% 204> digraph:add_vertex(G, B, E).
% {#Ref<0.3770885502.3804495875.151577>,1587669065273569069}
% 205> digraph:add_vertex(G, C, F).
% {#Ref<0.3770885502.3804495875.151578>,1587669065273579919}
% 206> ets:tab2list(V).            
% [{{#Ref<0.3770885502.3804495875.151577>,1587669065273569069},
%   #{"id" => 6,"items" => [],"periodicity" => "weekly",
%     "tags" => ["flyer","ads"],
%     "title" => "La Superior","type" => "publication"}},
%  {{#Ref<0.3770885502.3804495875.151578>,1587669065273579919},
%   #{"id" => 7,"items" => [],"periodicity" => "weekly",
%     "tags" => ["flyer","ads"],
%     "title" => "Food Source","type" => "publication"}},
%  {{#Ref<0.3770885502.3804495875.151576>,1587669065273556493},
%   #{"id" => 5,"items" => [],"periodicity" => "weekly",
%     "tags" => ["flyer","ads"],
%     "title" => "Raley's","type" => "publication"}}]

% `ets:lookup/2` or `match_object/2` can be used to find row by ID, and `ets:select/[1,2,3]` if lookup is needed by other columns.

% 208> ets:select(V, [{{'_', #{ "title" => "Food Source"}}, [], ['$_']}]).
% [{{#Ref<0.3770885502.3804495875.151578>,1587669065273579919},
%   #{"id" => 7,"items" => [],"periodicity" => "weekly",
%     "tags" => ["flyer","ads"],
%     "title" => "Food Source","type" => "publication"}}]
%
% 210> ets:match_object(V, {'_', #{ "title" => "Raley's"}}).
% [{{#Ref<0.3770885502.3804495875.151576>,1587669065273556493},
%   #{"id" => 5,"items" => [],"periodicity" => "weekly",
%     "tags" => ["flyer","ads"],
%     "title" => "Raley's","type" => "publication"}}]

% `ets:match/2` can be used to return only certain fields. For example, return all the available publications. (Adding a category vertex to as all the current ones are publications.)
% 213> digraph:add_vertex(G, 1, #{"type" => "category", "title" => "Grocery stores"}).                                                   
% 1                                                                                                                                      
% 214> ets:match(V, {'_', #{ "title" => '$1'}}).                                                                                         
% [["La Superior"],                                 
%  ["Grocery stores"],
%  ["Food Source"],
%  ["Raley's"]]
% 215> ets:match(V, {'_', #{ "type" => "publication", "title" => '$1'}}).
% [["La Superior"],["Food Source"],["Raley's"]]

% http://erlang.org/doc/efficiency_guide/tablesDatabases.html
% The functions ets:select/2 and mnesia:select/3 are to be preferred over ets:match/2, ets:match_object/2, and mnesia:match_object/3.

% Overkill, but want to remember the thought:
% The rows for the content digraph (i.e., the vertices and edges stored in the underlying ETS tables) could be normalized for easier lookup by creating another table that will simply store the map as an N-tuple (N equals the number of keys in the map).
% (or, better yet, replicate and normalize to mnesia)

% !!!! ETS performance and limits
% http://erlang.org/pipermail/erlang-questions/2018-September/096299.html
% }}-

% STAGES and NOTES {{-
% 1. this
% 2. move everything over to the Core Phoenix web server,
%    and implement API to query data from here (i.e., the phone service)
% 3. see what better models there are for more robust persistence
%    (currently digraph (i.e., ETS, meaning in-memory) and cloud storage + metadata)
%    there is an mnesia-backed digraph project on github, but mnesia is no panacea
%    (if things crash, some stuff in memory still won't get written out)
% 4. maybe a "proper" database?

% See hypothes.is notes to https://docs.microsoft.com/en-us/azure/architecture/best-practices/api-design


% }}-

% publication_guide2() -> % {{-
%     #{ "type"  => "category"
%      , "title" => "Main category"
%      , "id"    => 1
%      , "items" =>
%          [ #{ "type"  => "category"
%             , "title" => "Store sales advertising"
%             , "id"    => 2
%             , "items" =>
%                 [ #{ "type"  => "category"
%                    , "title" => "Grocery stores"
%                    , "id"    => 3
%                    , "items" =>
%                           % TODO "type" are going to be nodes of #{ type => type, name => value}
%                           % TODO find specific type vertices
%                           % [V || V <- digraph:vertices(G), maps:find(type, V) =:= {ok, publication}]
%                        [ #{ "type"  => "publication"
%                           , "title" => "Safeway"
%                           , "id"    => 4
%                           % TODO "tags" are going to be nodes of #{ type => tag, name => value}
%                           , "tags"  => ["flyer", "ads"]
%                           % TODO "periodicity" are going to be nodes of #{ type => periodicity, name => value}
%                           , "periodicity" => "weekly"
%                           , "items" => []
%                           }
%                        , #{ "type"  => "publication"
%                           , "title" => "Raley's"
%                           , "id"    => 5
%                           , "tags"  => ["flyer", "ads"]
%                           , "periodicity" => "weekly"
%                           , "items" => []
%                           }
%                        , #{ "type"  => "publication"
%                           , "title" => "La Superior"
%                           , "id"    => 6
%                           , "tags"  => ["flyer", "ads"]
%                           , "periodicity" => "weekly"
%                           , "items" => []
%                           }
%                        , #{ "type"  => "publication"
%                           , "title" => "Food Source"
%                           , "id"    => 7
%                           , "tags"  => ["flyer", "ads"]
%                           , "periodicity" => "weekly"
%                           , "items" => []
%                           }
%                        , #{ "type"  => "publication"
%                           , "title" => "Savemart"
%                           , "id"    => 8
%                           , "tags"  => ["flyer", "ads"]
%                           , "periodicity" => "weekly"
%                           , "items" => []
%                           }
%                        , #{ "type"  => "publication"
%                           , "title" => "Foods Co"
%                           , "id"    => 9
%                           , "tags"  => ["flyer", "ads"]
%                           , "periodicity" => "weekly"
%                           , "items" => []
%                           }
%                        , #{ "type"  => "publication"
%                           , "title" => "Trader Joe's"
%                           , "id"    => 10
%                           , "tags"  => ["flyer", "ads"]
%                           , "periodicity" => "weekly"
%                           , "items" => []
%                           }
%                        , #{ "type"  => "publication"
%                           , "title" => "Sprouts"
%                           , "id"    => 11
%                           , "tags"  => ["flyer", "ads"]
%                           , "periodicity" => "weekly"
%                           , "items" => []
%                           }
%                        , #{ "type"  => "publication"
%                           , "title" => "Lucky Supermarkets"
%                           , "id"    => 12
%                           , "tags"  => ["flyer", "ads"]
%                           , "periodicity" => "weekly"
%                           , "items" => []
%                           }
%                        ]
%                    }
%                 , #{ "type"  => "category"
%                    , "title" => "Drug stores"
%                    , "id"    => 13
%                    , "items" =>
%                        [ #{ "type"  => "publication"
%                           , "title" => "CVS"
%                           , "id"    => 14
%                           , "tags"  => ["flyer", "ads"]
%                           , "periodicity" => "weekly"
%                           , "items" => []
%                           }
%                        , #{ "type"  => "publication"
%                           , "title" => "Rite Aid"
%                           , "id"    => 15
%                           , "tags"  => ["flyer", "ads"]
%                           , "periodicity" => "weekly"
%                           , "items" => []
%                           }
%                        , #{ "type"  => "publication"
%                           , "title" => "Walgreen's"
%                           , "id"    => 16
%                           , "tags"  => ["flyer", "ads"]
%                           , "periodicity" => "weekly"
%                           , "items" => []
%                           }
%                        ]
%                    }
%                 , #{ "type"  => "category"
%                    , "title" => "Discount stores"
%                    , "id"    => 17
%                    , "items" =>
%                        [ #{ "type"  => "publication"
%                           , "title" => "Target"
%                           , "id"    => 18
%                           , "tags"  => ["flyer", "ads"]
%                           , "periodicity" => "weekly"
%                           , "items" => []
%                           }
%                        , #{ "type"  => "publication"
%                           , "title" => "Walmart"
%                           , "id"    => 19
%                           , "tags"  => ["flyer", "ads"]
%                           , "periodicity" => "weekly"
%                           , "items" => []
%                           }
%                        ]
%                    }
%                 ]
%             }
%          , #{ "type"  => "category"
%             , "title" => "Sacramento newspapers and magazines"
%             , "id"    => 20
%             , "items" =>
%                 [ #{ "type"  => "category"
%                    , "title" => "Sacramento newspapers"
%                    , "id"    => 21
%                    , "items" =>
%                           % TODO "type" are going to be nodes of #{ type => type, name => value}
%                           % TODO find specific type vertices
%                           % [V || V <- digraph:vertices(G), maps:find(type, V) =:= {ok, publication}]
%                        [ #{ "type"  => "publication"
%                           , "title" => "Sacramento Bee"
%                           % TODO "tags" are going to be nodes of #{ type => tag, name => value}
%                           , "id"    => 22
%                           , "tags"  => ["general"]
%                           , "publication_type" => "newspaper"
%                           % TODO add "location" and "publisher" to others as well
%                           , "location" => ["Sacramento", "Sacramento Valley", "Sacramento County"] % TODO What else?
%                           , "publisher" => "McClatchy Company"
%                           % TODO "periodicity" are going to be nodes of #{ type => periodicity, name => value}
%                           , "periodicity" => "daily"
%                           , "items" => []
%                           }
%                        , #{ "type"  => "publication"
%                           , "title" => "Sacramento News and Review"
%                           , "id"    => 23
%                           , "tags"  => ["alternative", "free", "general"]
%                           , "periodicity" => "weekly"
%                           , "items" => []
%                           }
%                        , #{ "type"  => "publication"
%                           , "title" => "Sacramento Press"
%                           , "id"    => 24
%                           , "tags"  => ["general"]
%                           , "periodicity" => "daily"
%                           , "items" => []
%                           }
%                        , #{ "type"  => "publication"
%                           , "title" => "Sacramento Business Journal"
%                           , "id"    => 25
%                           , "tags"  => ["business"]
%                           , "periodicity" => "weekly"
%                           , "items" => []
%                           }
%                        , #{ "type"  => "publication"
%                           , "title" => "East Sacramento News"
%                           , "id"    => 26
%                           , "tags"  => ["small community news", "general"]
%                           , "publisher" => "Valley Community Newspapers"
%                           , "periodicity" => "weekly"
%                           , "items" => []
%                           }
%                        , #{ "type"  => "publication"
%                           , "title" => "Land Park News"
%                           , "id"    => 27
%                           , "tags"  => ["small community news", "general"]
%                           , "publisher" => "Valley Community Newspapers"
%                           , "periodicity" => "weekly"
%                           , "items" => []
%                           }
%                        , #{ "type"  => "publication"
%                           , "title" => "Pocket News"
%                           , "id"    => 28
%                           , "tags"  => ["small community news", "general"]
%                           , "publisher" => "Valley Community Newspapers"
%                           , "periodicity" => "weekly"
%                           , "items" => []
%                           }
%                        ]
%                    }
%                 , #{ "type"  => "category"
%                    , "title" => "Sacramento magazines"
%                    , "id"    => 29
%                    , "items" =>
%                        [ #{ "type"  => "publication"
%                           , "title" => "Comstock's"
%                           , "id"    => 30
%                           , "tags"  => ["magazine"]
%                           , "periodicity" => "weekly"
%                           , "items" => []
%                           }
%                        , #{ "type"  => "publication"
%                           , "title" => "SacTown"
%                           , "id"    => 31
%                           , "tags"  => ["magazine"]
%                           , "periodicity" => "weekly"
%                           , "items" => []
%                           }
%                        , #{ "type"  => "publication"
%                           , "title" => "Sacramento Magazine"
%                           , "id"    => 32
%                           , "tags"  => ["magazine"]
%                           , "periodicity" => "weekly"
%                           , "items" => []
%                           }
%                        ]
%                    }
%                 ]
%             }
%          , #{ "type"  => "category"
%             , "title" => "Greater Sacramento area newspapers"
%             , "id"    => 33
%             , "items" =>
%                 [ #{ "type"  => "publication"
%                    , "title" => "Carmichael Times"
%                    , "id"    => 34
%                    , "tags"  => ["newspaper"]
%                    , "periodicity" => "weekly"
%                    , "items" => []
%                    }
%                 , #{ "type"  => "publication"
%                    , "title" => "Arden Carmichael News"
%                    , "id"    => 35
%                    , "tags"  => ["small community news", "general"]
%                    , "publisher" => "Valley Community Newspapers"
%                    , "periodicity" => "weekly"
%                    , "items" => []
%                    }
%                 , #{ "type"  => "publication"
%                    , "title" => "Davis Enterprise"
%                    , "id"    => 36
%                    , "tags"  => ["newspaper"]
%                    , "periodicity" => "weekly"
%                    , "items" => []
%                    }
%                 , #{ "type"  => "publication"
%                    , "title" => "Rosevill Press Tribune"
%                    , "id"    => 37
%                    , "tags"  => ["newspaper"]
%                    , "periodicity" => "weekly"
%                    , "items" => []
%                    }
%                 , #{ "type"  => "publication"
%                    , "title" => "Woodland Daily Democrat"
%                    , "id"    => 38
%                    , "tags"  => ["newspaper"]
%                    , "periodicity" => "weekly"
%                    , "items" => []
%                    }
%                 , #{ "type"  => "publication"
%                    , "title" => "Auburn Journal"
%                    , "id"    => 39
%                    , "tags"  => ["newspaper"]
%                    , "periodicity" => "weekly"
%                    , "items" => []
%                    }
%                 , #{ "type"  => "publication"
%                    , "title" => "The Union"
%                    , "id"    => 40
%                    , "location"  => ["Grass Valley", "Nevada City"]
%                    , "periodicity" => "weekly"
%                    , "items" => []
%                    }
%                 , #{ "type"  => "publication"
%                    , "title" => "Mountain Democrat"
%                    , "id"    => 41
%                    , "location"  => ["Placerville", "El Dorado County"]
%                    , "periodicity" => "weekly"
%                    , "items" => []
%                    }
%                 ]
%             }
%         ]
%      }.
% % }}-

write_meta_file
  ( {ContentType, Selection, Title} = _Category
  , Dir
  )
-> % {{-
    MetaFilePath =
        filename:join(Dir, metafile_name()),
    Content =
        stringify(
          #{ type => ContentType
           , selection => Selection
           , title => Title
           }
        ),
    file:write_file(
      MetaFilePath,
      Content ++ "."
    ),
    Dir.
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
    realize(?CONTENT_ROOT_DIR).

realize(ContentRoot) -> % {{-
    case file:make_dir(ContentRoot) of
        ok ->
            % write_meta_file(
            %   ?CONTENT_ROOT,
            %   ContentRoot
            % ),
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
% }}-

% log(Level, ValueList) ->
%     filog:log(Level, ?MODULE, ValueList).

% vim: set fdm=marker:
% vim: set foldmarker={{-,}}-:
% vim: set nowrap:
