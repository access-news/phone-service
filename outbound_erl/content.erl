-module(content).
-behaviour(gen_server).
% -define(CONTENT_ROOT_DIR, "/home/toraritte/clones/phone-service/content-root/").
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
    , publication_guide/0

    % private functions
    , draw_content_graph/0
    , publication_guide/0
    % , refresh_content_graph/1
    % TODO Make this part of the public API
    % , realize/0
    , get_vertex/3
    , process_call/3
    % , get_meta/1
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

    % There is no subtle diffing algorithm: when the content menu structure changes, recreate the entire directory hierarchy.
    % os:cmd("rm -r " ++ ?CONTENT_ROOT_DIR),
    % realize(?CONTENT_ROOT_DIR),


    % ContentRootDir =
    %     filename:join(?CONTENT_ROOT_DIR, "00"),
    Graph =
        draw_content_graph(),
        % draw_content_graph({from_dir, ContentRootDir}),

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

% TODO Depends on the internal representation.
%      Refactor when the web service is ready (or usable).
draw_content_graph() ->

    % Doesn't throw so if exists then things go on.
    file:make_dir(?PUBLICATION_ROOT),

    [ { [category, _] = RootContentItem
      , [_|_] = MainMenuItems
      }
    ] =
        publication_guide(),

    ContentRoot =
        make_meta(RootContentItem, 0),

    Graph =
        digraph:new([cyclic, protected]), % default values made explicit

    digraph:add_vertex(Graph, ContentRoot),

    do_draw
      ( [first|MainMenuItems]
      , Graph
      , ContentRoot
      ),

    Graph.

% TODO Don't think this is possible
do_draw([], _Graph, _ParentVertex) ->
    done;

% Empty publications end condition
do_draw([first], _Graph, ParentVertex) ->
%     erlang:display([done, [first], ParentVertex]),
    done;

% End condition for all other loops
do_draw  % ContentType, [ PrevItemVertex ] {{-
( [ #{} = _PrevItemVertex
  | [] = _Rest
  ]
, _Graph
, _ParentVertex
)
->
    done;
% }}-

do_draw  % category, [          first, ContentItem | []    = Rest ] {{-
( [ first
  , { [category|_] = ContentItem
    , [_|_] = SubItems % Explicitly disallow empty categories
    }
  | [] = _Rest
  ]
, Graph
, ParentVertex
)
->
    draw_category
      ( first_and_last
      , Graph
      , ParentVertex
      , ContentItem
      , 1
      , SubItems
      , []
      );

% }}-
do_draw  % category, [          first, ContentItem | [_|_] = Rest ] {{-
( [ first
  , { [category|_] = ContentItem
    , [_|_] = SubItems % Explicitly disallow empty categories
    }
  | [_|_] = Rest
  ]
, Graph
, ParentVertex
)
->
    draw_category
      ( first
      , Graph
      , ParentVertex
      , ContentItem
      , 1
      , SubItems
      , Rest
      );

% }}-
do_draw  % category, [ PrevItemVertex, ContentItem | []    = Rest ] {{-
( [ #{ selection := ItemNumber } = PrevItemVertex
  , { [category|_] = ContentItem
    , [_|_] = SubItems % Explicitly disallow empty categories
    }
  | [] = _Rest
  ]
, Graph
, ParentVertex
)
->
    draw_category
      ( {PrevItemVertex, last}
      , Graph
      , ParentVertex
      , ContentItem
      , ItemNumber + 1
      , SubItems
      , []
      );

% }}-
do_draw  % category, [ PrevItemVertex, ContentItem | [_|_] = Rest ] {{-
( [ #{ selection := ItemNumber } = PrevItemVertex
  , { [category|_] = ContentItem
    , [_|_] = SubItems % Explicitly disallow empty categories
    }
  | [_|_] = Rest
  ]
, Graph
, ParentVertex
)
->
    draw_category
      ( PrevItemVertex
      , Graph
      , ParentVertex
      , ContentItem
      , ItemNumber + 1
      , SubItems
      , Rest
      );
% }}-

% The   `do_draw/3`  for   publications  is   slightly
% different in that  they accept a 3-tuple  as well in
% the `publication_guide/0`.  The rationale is  that a
% publication  may be  divided into  section and  thus
% the sub-sections become  the publication-tuples that
% in  turn will  become  the publication  directories.
% Other  publications divided  into  section may  also
% hold  a  sub-section  with   the  same  name  (e.g.,
% News, Obituaries)  so that  is a possible  source of
% conflict. The  last element of the  3-tuple is meant
% to be a naming prefix to fix this.
do_draw  % publication, [          first, ContentItem | []    = Rest ] {{-
( [ first
  , [publication|_] = ContentItem
  | [] = _Rest
  ]
, Graph
, ParentVertex
)
->
    draw_publication
      ( first_and_last
      , Graph
      , ParentVertex
      , ContentItem
      , 1
      , [] % Rest
      );

% }}-
do_draw  % publication, [          first, ContentItem | [_|_] = Rest ] {{-
( [ first
  , [publication|_] = ContentItem
  | [_|_] = Rest
  ]
, Graph
, ParentVertex
)
->
    draw_publication
      ( first
      , Graph
      , ParentVertex
      , ContentItem
      , 1
      , Rest
      );

% }}-
do_draw  % publication, [ PrevItemVertex, ContentItem | []    = Rest ] {{-
( [ #{ selection := ItemNumber } = PrevItemVertex
  , [publication|_] = ContentItem
  | [] = _Rest
  ]
, Graph
, ParentVertex
)
->
    draw_publication
      ( {PrevItemVertex, last}
      , Graph
      , ParentVertex
      , ContentItem
      , ItemNumber + 1
      , [] % Rest
      );

% }}-
do_draw  % publication, [ PrevItemVertex, ContentItem | [_|_] = Rest ] {{-
( [ #{ selection := ItemNumber } = PrevItemVertex
  , [publication|_] = ContentItem
  | [_|_] = Rest
  ]
, Graph
, ParentVertex
)
->
    draw_publication
      ( PrevItemVertex
      , Graph
      , ParentVertex
      , ContentItem
      , ItemNumber + 1
      , Rest
      );
% }}-

do_draw  % article, [          first, ItemVertex | []    = Rest ]      {{-
( [ first
  , #{ type := article} = ItemVertex
  | [] = _Rest
  ]
, Graph
, ParentVertex
)
->
    draw_article
      ( first_and_last
      , Graph
      , ParentVertex
      , ItemVertex
      , [] % Rest
      );

% }}-
do_draw  % article, [          first, ItemVertex | [_|_] = Rest ]      {{-
( [ first
  , #{ type := article} = ItemVertex
  | [_|_] = Rest
  ]
, Graph
, ParentVertex
)
->
    draw_article
      ( first
      , Graph
      , ParentVertex
      , ItemVertex
      , Rest
      );

% }}-
do_draw  % article, [ PrevItemVertex, ItemVertex | []    = Rest ]      {{-
( [ #{ type := article} = PrevItemVertex
  , #{ type := article} = ItemVertex
  | [] = _Rest
  ]
, Graph
, ParentVertex
)
->
    draw_article
      ( {PrevItemVertex, last}
      , Graph
      , ParentVertex
      , ItemVertex
      , [] % Rest
      );

% }}-
do_draw  % article, [ PrevItemVertex, ItemVertex | [_|_] = Rest ]      {{-
( [ #{ type := article} = PrevItemVertex
  , #{ type := article} = ItemVertex
  | [_|_] = Rest
  ]
, Graph
, ParentVertex
)
->
    draw_article
      ( PrevItemVertex
      , Graph
      , ParentVertex
      , ItemVertex
      , Rest
      ).
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

add_hierarchy_edges(Graph, ParentVertex, ChildVertex) -> % {{-
    add_edge(Graph, child,  ParentVertex, ChildVertex),
    add_edge(Graph, parent, ChildVertex, ParentVertex).
% }}-

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL MODEL OF THE YET TO BE BUILT ACCESS NEWS WEB SERVICE      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% {{-
% NOTE for the resulting graph when determining neighbours:
% to avoid excessive amount of connections (and facing the problems updating them would cause), it will be done by finding the parent category, and adding/subtracting one from their "ID".
% The publication guide below is just a representation of future data of the yet-to-be-implemented core web service, and its data may not contain such IDs, but that could be done on this end by ordering and adding that via a script.

publication_guide() -> % {{-
    [ { [category, "Main category"]
      , [ { [category, "Store sales advertising"] % {{-
          , [ { [category, "Grocery stores"]
              , [ [publication, "Safeway"]
                , [publication, "Raley's"]
                , [publication, "La Superior"]
                , [publication, "Food source"]
                , [publication, "Savemart"]
                , [publication, "Foods Co"]
                , [publication, "Trader Joe's"]
                , [publication, "Sprouts"]
                , [publication, "Lucky Supermarkets"]
                ]
              }
            , { [category, "Drug stores"]
              , [ [publication, "CVS"]
                , [publication, "Rite Aid"]
                , [publication, "Walgreen's"]
                ]
              }
            , { [category, "Discount stores"]
              , [ [publication, "Target"]
                , [publication, "Walmart"]
                ]
              }
            ]
          } % }}-
        , { [category, "Northern California newspapers"] % {{-
          , [ { [category, "Sacramento newspapers and magazines"]
              , [ { [category, "Sacramento newspapers"] % {{-
                  , [ { [category, "Sacramento Bee sections"] % {{-
                      , [ [publication, "Sports", ]
                        , [publication, "News"]
                        , [publication, "Obituaries"]
                        ]
                      } % }}-
                    , [publication, "Sacramento News & Review"]
                    , [publication, "Sacramento Press"]
                    , [publication, "Sacramento Business Journal"]
                    , [publication, "Sacramento Observer"]
                    , [publication, "Sacramento City Express"]
                    , [publication, "East Sacramento News"]
                    , [publication, "The Land Park News"]
                    , [publication, "The Pocket News"]
                    ]
                  } % }}-
                , { [category, "Sacramento magazines"] % {{-
                  , [ [publication, "Comstocks"]
                    , [publication, "SacTown"]
                    , [publication, "Sacramento Magazine"]
                    ]
                  } % }}-
                ]
              }

            , { [category, "Greater Sacramento area newspapers"]
              , [ [publication, "Carmichael Times"]
                , [publication, "Arden Carmichael News"]
                , [publication, "Davis Enterprise"]
                , [publication, "Roseville Press Tribune"]
                , [publication, "Woodland Daily Democrat"]
                , [publication, "Elk Grove Citizen"]
                , [publication, "Auburn Journal"]
                , [publication, "Grass Valley-Nevada City Union"]
                , [publication, "El Dorado County Mountain Democrat"]
                ]
              }

            , { [category, "San Francisco and Bay Area newspapers"]
              , [ [publication, "Vallejo Times Herald"]
                , [publication, "Santa Rosa Press Democrat"]
                , [publication, "SF Gate"]
                , [publication, "San Francisco Bay Guardian"]
                , [publication, "East Bay Times"]
                , [publication, "SF Weekly"]
                , [publication, "KQED Bay Area Bites"]
                ]
              }

            , { [category, "Central California newspapers"]
              , [ [publication, "Modesto Bee"]
                , [publication, "Stockton Record"]
                ]
              }

            , { [category, "Mendocino county newspapers"]
              , [ [publication, "Fort Bragg Advocate News"]
                , [publication, "The Mendocino Beacon"]
                ]
              }

            , { [category, "Humboldt & Trinity county newspapers"]
              , [ [publication, "Humboldt Senior Resource Center's Senior News"]
                , [publication, "North Coast Journal"]
                , [publication, "Eureka Times Standard"]
                , [publication, "Ferndale Enterprise"]
                , [publication, "Mad River Union"]
                ]
              }
            ]
          } % }}-
        , { [category, "Blindness resources"] % {{-
          % , [ { [category, "Newsletters"]
          %     , [ [publication, "Society for the Blind"]
          %       , [publication, "SFB Connection"]
          %       , [publication, "The Earle Baum Center"]
          %       , [publication, "Sierra Services for the Blind"]
          %       , [publication, "California Council of the Blind"]
          %       ]
          %     }
          , [ { [category, "Blindness organizations"]
              , [ { [category, "Society for the Blind"]
                  , [ [publication, "SFB Connection"]
                    , [publication, "Monthly newsletter"]
                    ]
                  }
                , [publication, "The Earle Baum Center"]
                , [publication, "Sierra Services for the Blind"]
                , [publication, "California Council of the Blind"]
                ]
              }
            , { { category, "Publications" }
              , [ [publication, "Braille Monitor"]
                , [publication, "Client Assistence Program"]
                ]
              }
            ]
          } % }}-
        , { [category, "Educational materials"] % {{-
          , [ [publication, "Society for the Blind's student handbook"]
            , [publication, "Balance exercises"]
            , [publication, "Achieve a healthy weight by UC Davis"]
            ]
          } % }}-
        , { [category, "General information"] % {{-
          , [ [publication, "Yuba-Sutter Meals On Wheels"]
            ]
          } % }}-
        , { [category, "Popular magazines"] % {{-
          , [ [publication, "Newsweek"]
            , [publication, "Fortune"]
            , [publication, "Capital Public Radio"]
            , [publication, "Travel & Leisure"]
            , [publication, "Entertainment Weekly"]
            , [publication, "Mental Floss"]
            , [publication, "Atlas Obscura"]
            , [publication, "New Scientist"]
            ]
          } % }}-
        , { [category, "Games"] % {{-
          , [ [publication, "Crosswords"]
            , [publication, "Trivia"]
            ]
          } % }}-
        , { [category, "Community content"] % {{-
          , [ { [category, "Podcasts"]
              , [ [publication, "Society for the Blind"]
                , [publication, "SFB Connection"]
                , [publication, "The Earle Baum Center"]
                , [publication, "Sierra Services for the Blind"]
                , [publication, "California Council of the Blind"]
                ]
              }
            , { { category, "Poetry" }
              , [ [publication, "Brad Buchanan"]
                , [publication, "Writer's on the air"]
                ]
              }
            ]
          } % }}-
        , { [category, "Old Time Radio Theater"] % {{-
          , [ { [category, "Mystery and drama"]
              , [ [publication, "Broadway's my Beet"]
                , [publication, "Black Stone the Magic Detective"]
                , [publication, "Boston Blacky"]
                , [publication, "Crime Does Not Pay"]
                , [publication, "Drag Net"]
                , [publication, "Gang Busters"]
                , [publication, "Inner Sanctum"]
                , [publication, "Mercury Radio Theater"]
                , [publication, "Mystery Traveler"]
                , [publication, "Richard Diamond Private Detective"]
                , [publication, "Adventures of Sam Spaid"]
                , [publication, "The Shadow"]
                , [publication, "Suspense"]
                , [publication, "The Whistler"]
                , [publication, "Light's Out"]
                ]
              }
            , { [category, "Comedy"]
              , [ [publication, "Abbot and Costello"]
                , [publication, "The Adventures of Ozzie and Harriet"]
                , [publication, "The Bickerson's"]
                , [publication, "Father Knows Best"]
                , [publication, "Fibber McGee and Molly"]
                , [publication, "The Fred Allen Show"]
                , [publication, "George Burns and Gracie Allen"]
                , [publication, "Life of Riley"]
                , [publication, "The Red Skelton Show"]
                ]
              }
            , { [category, "Westerns"]
              , [ [publication, "The Cisco Kid"]
                , [publication, "Gun Smoke"]
                , [publication, "The Lone Ranger"]
                , [publication, "Tales of the Texas Rangers"]
                ]
              }
            , { [category, "Science fiction and fantasy"]
              , [ [publication, "The Blue Beetle"]
                , [publication, "Escape"]
                , [publication, "The Green Hornet"]
                , [publication, "X Minus 1"]
                ]
              }
            , { [category, "Commercials"]
              , [ [publication, "Commercials"]
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

make_publication_dir
( [ publication, ContentTitle | OptionalPrefix ] = ContentItem
)
->
    Publication =
        case OptionalPrefix of
            [] ->
                ContentTitle;
            [Prefix] ->
                Prefix ++ "-" ++ ContentTitle
        end,

    PublicationDir =
        filename:join(?PUBLICATION_ROOT, Publication),

    % no fuss if exists, won't throw
    file:make_dir(PublicationDir),
%     erlang:display([make_publication_dir, PublicationDir]),
    PublicationDir.

list_recording_vertices(PublicationDir) ->
    % Only  MP3s  and  WAVs are  supported  by  FreeSWITCH
    % out-of-the-box. Also, could've  just simply `ls` the
    % publication folder, and crash on unsupported formats
    % but  I'm an  idiot,  and this  is  may help  prevent
    % disasters. (Or cause some more.)
    Extensions =
        fun (Filename) ->
            case filename:extension(Filename) of
                ".wav" -> true;
                ".mp3" -> true;
                _ -> false
            end
        end,

    MakeMeta =
        fun (Filename) ->
            futil:pipe
              ([ Filename
               , (futil:cflip(fun filename:absname/2))(PublicationDir)
               , fun make_recording_meta/1
               ])
        end,

%         erlang:display([list_recording_vertices, enter, PublicationDir]),
    futil:pipe
         % NOTE The extra  quotes are  needed because  otherwise the
         %      special  characters  in   `PublicationDir`  will  be
         %      treated literally by the shell.
         % ``` text
         % $ ls Raley's
         % # VS
         % $ ls "Raley's"
         % ```

         % NOTE Calling `ls`  without any  modifiers means  that the
         %      order  of the  returned list  depends on  consistent
         %      file naming!
         % TODO Make  an upload  mechanism  that takes  care of  the
         %      naming  based on  given  parameters  (e.g., sort  by
         %      time, numbering, etc.).
      ([ os:cmd("ls \"" ++ PublicationDir ++ "\"")
       % , fun (X) -> case X =/= [] of true -> erlang:display([list_recording_vertices, PublicationDir,  X]); false -> noop end, X end
       , (futil:cflip(fun string:lexemes/2))([$\n])
%      , fun (X) -> erlang:display([list_recording_vertices, X]), X end
       , (futil:curry(fun lists:filter/2))(Extensions)
%      , fun (X) -> erlang:display([list_recording_vertices, X]), X end
       % NOTE It would have made more logical sense to put this in
       % `draw_item/7`  but it  would have  been a  hassle to
       % figure  out `PublicationDir`  - plus  it would  have
       % been an extra loop
       , (futil:curry(fun lists:map/2))(MakeMeta)
%      , fun (X) -> erlang:display([list_recording_vertices, X]), X end
       ]).

draw_publication
( Direction
, Graph
, ParentVertex
, [publication|_] = ContentItem
, ItemNumber
, Rest
)
->
    RecordingVertices =
        futil:pipe
          ([ ContentItem
           , fun make_publication_dir/1
           , fun list_recording_vertices/1
           ]),

%     erlang:display(RecordingVertices),

    ItemVertex =
        make_meta(ContentItem, ItemNumber),

    draw_item
      ( Direction
      , Graph
      , ParentVertex
      , ItemVertex
      , RecordingVertices
      , Rest
      ).

draw_category % {{-
( Direction
, Graph
, ParentVertex
, [ category | OptionalPrefix ] = ContentItem
, ItemNumber
, SubItems
, Rest
)
->
    ItemVertex =
        make_meta(ContentItem, ItemNumber),

    draw_item
      ( Direction
      , Graph
      , ParentVertex
      , ItemVertex
      , SubItems
      , Rest
      ).

% }}-
draw_article % {{-
( Direction
, Graph
, ParentVertex
, #{} = ItemVertex
, Rest
)
->
    draw_item
      ( Direction
      , Graph
      , ParentVertex
      , ItemVertex
      , []
      , Rest
      ).

% }}-
draw_item
( Direction
, Graph
, ParentVertex
, #{} = ItemVertex
, SubItems
, Rest
)
->
%     % erlang:display([draw_item, #{direction => Direction}]),
%     % erlang:display([draw_item, #{ parent => ParentVertex}]),
%     % erlang:display([draw_item, #{ item => ItemVertex}]),
%     % erlang:display([draw_item, #{ subitems => SubItems}]),
%     % erlang:display([draw_item, #{ rest => Rest}]),

    digraph:add_vertex(Graph, ItemVertex),
    add_hierarchy_edges(Graph, ParentVertex, ItemVertex),

    case Direction of
        first ->
            add_edge(Graph, first, ParentVertex, ItemVertex);

        % NOTE Not possible, because the being the last will always
        %      involve  other  edges  as well;  either  `first`  or
        %      `prev` and `next` respectively.
        % last ->
        %     add_edge(Graph, last, ParentMeta, MetaB);

        first_and_last ->
            add_edge(Graph, first, ParentVertex, ItemVertex),
            add_edge(Graph, last, ParentVertex, ItemVertex);

        PrevItemVertex when is_map(PrevItemVertex) ->
            add_edge(Graph, prev, ItemVertex, PrevItemVertex),
            add_edge(Graph, next, PrevItemVertex, ItemVertex);

        {PrevItemVertex, last} ->
            add_edge(Graph, prev, ItemVertex, PrevItemVertex),
            add_edge(Graph, next, PrevItemVertex, ItemVertex),
            add_edge(Graph, last, ParentVertex, ItemVertex)
    end,

    do_draw([first|SubItems], Graph, ItemVertex),
    do_draw([ItemVertex|Rest], Graph, ParentVertex).

make_recording_meta(AbsFilename) ->
    #{ type  => article
     , path  => AbsFilename
     , title => ""
     }.

make_meta
( { ContentType, ContentTitle } = _ContentItem
% , Path
, ItemNumber
) ->
    #{ type      => ContentType
     , selection => ItemNumber
     , title     => ContentTitle
     }.

% vim: set fdm=marker:
% vim: set foldmarker={{-,}}-:
% vim: set nowrap:
