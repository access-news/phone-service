-module(content).
-behaviour(gen_server).

-define(CONTENT_ROOT_DIR, "/home/toraritte/clones/phone-service/content-root/").
-define(PUBLICATION_ROOT, "/home/toraritte/clones/phone-service/publications/").
% -define(CONTENT_ROOT, {category, 0, "Main category"}).

-export(
    [ start/0
    , start_link/0

    % gen_server callbacks
    , init/1
    , handle_call/3
    , handle_cast/2
    % , terminate/2

    % public API
    , pick/2
    , root/0
    , redraw/0
    , add_label/2
    , get_label/1

    % private functions
    % , draw_content_graph/1
    % , refresh_content_graph/1
    % TODO Make this part of the public API
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

init(_Args) -> % {{-
    % TODO PROD How to set up the graph? ("do not overthink" notes below) {{-
    % It `realize/0`s the dir structure at the moment if it does not exit, but in subsequent phases the graph will be based on a remote cloud storage - it is cheap to redraw the entire graph by reading local files, but that will not cut it later. The graph will need to be de-serialized and kept up to date via messages, then saved to disk on startup.
    % }}-

    % Doesn't throw so if exists then things go on.
    file:make_dir(?PUBLICATION_ROOT),

    % There is no subtle diffing algorithm: when the content menu structure changes, recreate the entire directory hierarchy.
    os:cmd("rm -r " ++ ?CONTENT_ROOT_DIR),
    realize(?CONTENT_ROOT_DIR),


    ContentRootDir =
        filename:join(?CONTENT_ROOT_DIR, "00"),
    Graph =
        draw_content_graph({from_dir, ContentRootDir}),

    {ok, Graph}.
% }}-

handle_call({pick, Direction, Vertex}, _From, Graph)
  when Direction =:= parent       % \
     ; Direction =:= first        % |
     ; Direction =:= last         % |
     ; Direction =:= next         % | Vertex
     ; Direction =:= prev         % |
     ; Direction =:= content_root % |
     ; Direction =:= children     %   [ Vertex ]
->
    { reply
    , process_call(Graph, Vertex, Direction)
    , Graph
    };

handle_call({get_label, Vertex}, _From, Graph) when is_map(Vertex) ->
    {Vertex, Label} = digraph:vertex(Graph, Vertex),
    {reply, Label, Graph}.

handle_cast({add_label, Vertex, Label}, Graph) when is_map(Vertex) ->
    digraph:add_vertex(Graph, Vertex, Label),
    {noreply, Graph};

handle_cast(redraw, Graph) ->
    {ok, NewGraph} = refresh_content_graph(Graph),
    {noreply, NewGraph}.

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
    gen_server:call
        ( ?MODULE
        , {pick, Direction, CurrentVertex}
        ).

root() ->
    pick(content_root, ignore).

redraw() ->
    gen_server:cast(?MODULE, redraw).

add_label(Vertex, Label) ->
    gen_server:cast
        ( ?MODULE
        , {add_label, Vertex, Label}
        ).

get_label(Vertex) ->
    gen_server:call
        ( ?MODULE
        , {get_label, Vertex}
        ).

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

% #{selection => 0,title => "Main category",type => category}
% #{selection => 1,title => "Store sales advertising",type => category}
% #{selection => 1,title => "Grocery stores",type => category}
% #{selection => 1,title => "Safeway",type => publication}

% TODO Depends on the internal representation.
%      Refactor when the web service is ready (or usable).
draw_content_graph() ->
    Graph =
        digraph:new([cyclic, protected]), % default values made explicit

    do_category
      ( Graph
      , 0
      , publication_guide()
      ).

draw_content_graph
( [ { {category, _} = ContentItem
    , [_|_] = SubItems
    }
  ]
, 0
) ->
    [ ContentRoot ] =
        make_meta(ContentItem, 0),

    do_category(Graph, ContentRoot, SubItems, 1)

% TODO Ez az vegkirterium nem stimmel
draw_content_graph(_Graph, _ParentVertex, [], _ItemNumber) ->
    done;

draw_content_graph([{publication, _, _} = Publication | Rest], Path) ->
    make_dir_and_meta_file(Publication, Path),
    realize(Rest, Path).

do_category
( Graph
, ItemNumber
, [ { {category, ContentTitle} = ContentItem
    , [_|_] = SubItems
    }
    | Rest
  ]
)
(
) -> % {{-
    do_subitems(Graph, ParentVertex
    [ ItemVertex ] =
        make_meta(ContentItem, ItemNumber),

    digraph:add_vertex(Graph, ItemVertex),

    do_make(Graph, ContentRootDir),
    Graph.
% }}-

refresh_content_graph(Graph) ->
    digraph:delete(Graph),
    init(ignore).

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
                 , title => "" % formerly known as "anchortext"
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
            % TODO order articles by date, newest first
            % 7> string:lexemes(os:cmd("ls -t"), [$\n]).
            % Current workaround: add datetime to filename and sort
            % https://erlang-questions.erlang.narkive.com/j5wggu6l/external-sorting-for-large-files-in-erlang
            % NOTE rationale {{-
            % When new recordings are added to a publication, the graph is either re-created from scratch (`refresh_content_graph/2`), or new recordings are copied in the publication directory and added by hand. This is going to change fundamentally once Access News Core is up and running so too much effort is a waste of time.
            % }}-
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


do_dirlist(_Graph, _ParentMeta, []) ->
    done;

% do_dirlist(Graph, ParentMeta, [{_, "meta.erl"}|Rest]) ->
%     do_dirlist(Graph, ParentMeta, Rest);

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
    % logger:notice(#{ first => MetaPath }),
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
    % logger:notice(#{ metapath_a => M, metapath_b => MetaPath}),
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
    [ { {category, "Main category"}
      , [ { {category, "Store sales advertising"} % {{-
          , [ { {category, "Grocery stores"}
              , [ {publication, "Safeway"}
                , {publication, "Raley's"}
                , {publication, "La Superior"}
                , {publication, "Food source"}
                , {publication, "Savemart"}
                , {publication, "Foods Co"}
                , {publication, "Trader Joe's"}
                , {publication, "Sprouts"}
                , {publication, "Lucky Supermarkets"}
                ]
              }
            , { {category, "Drug stores"}
              , [ {publication, "CVS"}
                , {publication, "Rite Aid"}
                , {publication, "Walgreen's"}
                ]
              }
            , { {category, "Discount stores"}
              , [ {publication, "Target"}
                , {publication, "Walmart"}
                ]
              }
            ]
          } % }}-
        , { {category, "Northern California newspapers"} % {{-
          , [ { {category, "Sacramento newspapers and magazines"}
              , [ { {category, "Sacramento newspapers"} % {{-
                  , [ { {category, "Sacramento Bee sections"} % {{-
                      , [ {publication, "Sports"}
                        , {publication, "News"}
                        , {publication, "Obituaries"}
                        ]
                      } % }}-
                    , {publication, "Sacramento News & Review"}
                    , {publication, "Sacramento Press"}
                    , {publication, "Sacramento Business Journal"}
                    , {publication, "Sacramento Observer"}
                    , {publication, "Sacramento City Express"}
                    , {publication, "East Sacramento News"}
                    , {publication, "The Land Park News"}
                    , {publication, "The Pocket News"}
                    ]
                  } % }}-
                , { {category, "Sacramento magazines"} % {{-
                  , [ {publication, "Comstocks"}
                    , {publication, "SacTown"}
                    , {publication, "Sacramento Magazine"}
                    ]
                  } % }}-
                ]
              }

            , { {category, "Greater Sacramento area newspapers"}
              , [ {publication, "Carmichael Times"}
                , {publication, "Arden Carmichael News"}
                , {publication, "Davis Enterprise"}
                , {publication, "Roseville Press Tribune"}
                , {publication, "Woodland Daily Democrat"}
                , {publication, "Elk Grove Citizen"}
                , {publication, "Auburn Journal"}
                , {publication, "Grass Valley-Nevada City Union"}
                , {publication, "El Dorado County Mountain Democrat"}
                ]
              }

            , { {category, "San Francisco and Bay Area newspapers"}
              , [ {publication, "Vallejo Times Herald"}
                , {publication, "Santa Rosa Press Democrat"}
                , {publication, "SF Gate"}
                , {publication, "San Francisco Bay Guardian"}
                , {publication, "East Bay Times"}
                , {publication, "SF Weekly"}
                , {publication, "KQED Bay Area Bites"}
                ]
              }

            , { {category, "Central California newspapers"}
              , [ {publication, "Modesto Bee"}
                , {publication, "Stockton Record"}
                ]
              }

            , { {category, "Mendocino county newspapers"}
              , [ {publication, "Fort Bragg Advocate News"}
                , {publication, "The Mendocino Beacon"}
                ]
              }

            , { {category, "Humboldt & Trinity county newspapers"}
              , [ {publication, "Humboldt Senior Resource Center's Senior News"}
                , {publication, "North Coast Journal"}
                , {publication, "Eureka Times Standard"}
                , {publication, "Ferndale Enterprise"}
                , {publication, "Mad River Union"}
                ]
              }
            ]
          } % }}-
        , { {category, "Blindness resources"} % {{-
          % , [ { {category, "Newsletters"}
          %     , [ {publication, "Society for the Blind"}
          %       , {publication, "SFB Connection"}
          %       , {publication, "The Earle Baum Center"}
          %       , {publication, "Sierra Services for the Blind"}
          %       , {publication, "California Council of the Blind"}
          %       ]
          %     }
          , [ { {category, "Blindness organizations"}
              , [ { {category, "Society for the Blind"}
                  , [ {publication, "SFB Connection"}
                    , {publication, "Monthly newsletter"}
                    ]
                  }
                , {publication, "The Earle Baum Center"}
                , {publication, "Sierra Services for the Blind"}
                , {publication, "California Council of the Blind"}
                ]
              }
            , { { category, "Publications" }
              , [ {publication, "Braille Monitor"}
                , {publication, "Client Assistence Program"}
                ]
              }
            ]
          } % }}-
        , { {category, "Educational materials"} % {{-
          , [ {publication, "Society for the Blind's student handbook"}
            , {publication, "Balance exercises"}
            , {publication, "Achieve a healthy weight by UC Davis"}
            ]
          } % }}-
        , { {category, "General information"} % {{-
          , [ {publication, "Yuba-Sutter Meals On Wheels"}
            ]
          } % }}-
        , { {category, "Popular magazines"} % {{-
          , [ {publication, "Newsweek"}
            , {publication, "Fortune"}
            , {publication, "Capital Public Radio"}
            , {publication, "Travel & Leisure"}
            , {publication, "Entertainment Weekly"}
            , {publication, "Mental Floss"}
            , {publication, "Atlas Obscura"}
            , {publication, "New Scientist"}
            ]
          } % }}-
        , { {category, "Games"} % {{-
          , [ {publication, "Crosswords"}
            , {publication, "Trivia"}
            ]
          } % }}-
        , { {category, "Community content"} % {{-
          , [ { {category, "Podcasts"}
              , [ {publication, "Society for the Blind"}
                , {publication, "SFB Connection"}
                , {publication, "The Earle Baum Center"}
                , {publication, "Sierra Services for the Blind"}
                , {publication, "California Council of the Blind"}
                ]
              }
            , { { category, "Poetry" }
              , [ {publication, "Brad Buchanan"}
                , {publication, "Writer's on the air"}
                ]
              }
            ]
          } % }}-
        , { {category, "Old Time Radio Theater"} % {{-
          , [ { {category, "Mystery and drama"}
              , [ {publication, "Broadway's my Beet"}
                , {publication, "Black Stone the Magic Detective"}
                , {publication, "Boston Blacky"}
                , {publication, "Crime Does Not Pay"}
                , {publication, "Drag Net"}
                , {publication, "Gang Busters"}
                , {publication, "Inner Sanctum"}
                , {publication, "Mercury Radio Theater"}
                , {publication, "Mystery Traveler"}
                , {publication, "Richard Diamond Private Detective"}
                , {publication, "Adventures of Sam Spaid"}
                , {publication, "The Shadow"}
                , {publication, "Suspense"}
                , {publication, "The Whistler"}
                , {publication, "Light's Out"}
                ]
              }
            , { {category, "Comedy"}
              , [ {publication, "Abbot and Costello"}
                , {publication, "The Adventures of Ozzie and Harriet"}
                , {publication, "The Bickerson's"}
                , {publication, "Father Knows Best"}
                , {publication, "Fibber McGee and Molly"}
                , {publication, "The Fred Allen Show"}
                , {publication, "George Burns and Gracie Allen"}
                , {publication, "Life of Riley"}
                , {publication, "The Red Skelton Show"}
                ]
              }
            , { {category, "Westerns"}
              , [ {publication, "The Cisco Kid"}
                , {publication, "Gun Smoke"}
                , {publication, "The Lone Ranger"}
                , {publication, "Tales of the Texas Rangers"}
                ]
              }
            , { {category, "Science fiction and fantasy"}
              , [ {publication, "The Blue Beetle"}
                , {publication, "Escape"}
                , {publication, "The Green Hornet"}
                , {publication, "X Minus 1"}
                ]
              }
            , { {category, "Commercials"}
              , [ {publication, "Commercials"}
                ]
              }
            ]
          } % }}-
        ]
      } % main category
    ].
% }}-

% TODO converting this to JSON should be trivial
% TODO `meta` edges
%      "directional" edges follow the pattern {dir, #{...}} so tag, type, periodicity (what else?) nodes will be tagged as meta and will be incident edges (is that the right term?)

% Yes, this could have been just the one string below, but it is not.
% string:join([ erlang:integer_to_list(erlang:system_time()) | tl(string:lexemes(erlang:ref_to_list(erlang:make_ref()), ".>"))], "-")
% TODO So what was the point of this? {{-
% make_id() ->
%     NowString =
%         f:pipe(
%           [ erlang:system_time()
%           , fun erlang:integer_to_list/1
%           ]
%         ),
%     RefNumbers =
%         f:pipe(
%           [ erlang:make_ref()
%           , fun erlang:ref_to_list/1
%           , (f:cflip(fun string:lexemes/2))(".>")
%           ]
%         ),
%     string:join([NowString|tl(RefNumbers)], "-").
% }}-

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
  ( {ContentType, Selection, Title} = _ContentItem
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

get_meta(ContentItemDir) -> % {{-
    MetaPath =
        filename:join(
          ContentItemDir,
          metafile_name()
        ),
    {ok, Meta} =
        file:script(MetaPath),
    Meta.
% }}-

% TODO Again, what was the point of this?
% list_category_entries(CategoryDir) -> % {{-
%     { ok
%     , SubCategoryDirectories
%     } =
%         file:list_dir(CategoryDir),
%     MetaList =
%         lists:map(
%           fun(SubDir) ->
%               MetaPath =
%                   filename:join([CategoryDir, SubDir, metafile_name()]),
%               {ok, {_, N, SubCategory} } =
%                   file:script(MetaPath),

%               "Press "
%               ++ integer_to_list(N)
%               ++ " for "
%               ++ SubCategory
%               ++ "."
%           end,
%           SubCategoryDirectories -- [metafile_name()]
%         ),
%     ordsets:from_list(MetaList).
% % }}-

% make_dir_and_meta_file
make_meta
( { publication, ContentTitle } = ContentItem
, ItemNumber
) -> % {{-
    publicationDir =
        filename:join(?PUBLICATION_ROOT, ContentTitle),

    % no fuss if exists, won't throw
    file:make_dir(publicationDir),

    % TODO read articles (ls -t)


make_meta
( { category, ContentTitle } = ContentItem
% , Path
, ItemNumber
) -> % {{-
    Meta =
        #{ type      => ContentType
         , selection => ItemNumber
         , title     => ContentTitle
         },
    [ Meta ].
    % case ContentType of

    %     category ->
    %         SubDir =
    %             integer_to_list(ItemNumber),
    %         Dir =
    %             filename:join(Path, SubDir),

    %         file:make_dir(Dir);

    %     publication ->
    %         file:make_dir(

    % end,
    % write_meta_file(ContentItem, Dir).
% }}-

realize(ContentRoot) -> % {{-
    case file:make_dir(ContentRoot) of
        ok ->
            % write_meta_file(
            %   ?CONTENT_ROOT,
            %   ContentRoot
            % ),
            realize(publication_guide(), ContentRoot, 0);
        {error, _} = Error ->
            Error
    end.
% }}-

realize % {{-
( [ { {category, _, _} = ContentItem
    , [_|_] = SubItems
    }
    | Rest
  ]
, Path
, ItemNumber
)
->
    NewPath =
        make_dir_and_meta_file(ContentItem, Path, ItemNumber),

    realize(SubItems, NewPath),
    realize(Rest, Path);
% }}-

realize([], _Path) ->
    done;

realize([{publication, _, _} = Publication | Rest], Path) ->
    make_dir_and_meta_file(Publication, Path),
    realize(Rest, Path).

% TODO Another mystery function
% add_recordings(FromDir, ToDir) -> % {{-
%     {ok, FileList} =
%         file:list_dir(FromDir),
%     MoveAndRenameFile =
%         fun (File) ->
%             FromPath =
%                 filename:join(FromDir, File),
%             NewBaseFileName =
%                 integer_to_list(os:system_time()),
%             OldFileExt =
%                 filename:extension(File),
%             ToPath =
%                 filename:join(
%                   ToDir,
%                   NewBaseFileName ++ OldFileExt
%                 ),
%             file:copy(FromPath, ToPath)
%         end,
%     lists:foreach(
%       MoveAndRenameFile,
%       FileList
%     ).
% % }}-

stringify(Term) ->
    R = io_lib:format("~p",[Term]),
    lists:flatten(R).
% }}-

% log(Level, ValueList) ->
%     filog:log(Level, ?MODULE, ValueList).

% vim: set fdm=marker:
% vim: set foldmarker={{-,}}-:
% vim: set nowrap:
