-module(content).
-behaviour(gen_server).

% NOTE 2022-10-19_1917 Vaguely remember experiment, but not this one. It can probably go.
% -define(CONTENT_ROOT_DIR, "/home/toraritte/clones/phone-service/content-root/").

-define(PUBLICATION_ROOT, "/home/tr2-admin/publications/").
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
    , all_vertices/0
    , redraw/0
    , add_label/2
    , get_label/1
    , publication_guide/0

    % private functions
    , draw_content_graph/0
    % , refresh_content_graph/1
    % TODO Make this part of the public API
    % , realize/0
    , get_vertex/3
    , process_call/3
    % , get_meta/1
    % , current/1
    % , update_history/2
    ]).

start_link() ->
    {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    Pid.

start() ->
    {ok, Pid} = gen_server:start({local, ?MODULE}, ?MODULE, [], []),
    Pid.

%% ====================================================================
%% gen_server callbacks
%% ====================================================================

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

handle_call(all_vertices, _From, Graph) ->
    {reply, digraph:vertices(Graph), Graph};

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

all_vertices() ->
    gen_server:call
        ( ?MODULE
        , all_vertices
        ).

% TODO No diff algo involved so `publications` directory has
%      to be  manually renamed  if one  would like  to have
%      changes in `publication_guide/0` to take effect!
%
%      BEST CURRENT USE CASE
%      Audio files  have been  added to  the `publications`
%      directory, and this will  re-read the directory, and
%      add `article` vertices to the content graph.
%
%      At  the  moment,  the  only way  to  add  new  audio
%      files, on  initial setup  or after  a change  in the
%      publication guide, is to to
%
%      1. Move old `publications` directory
%      2. `content:redraw()`
%      3. Place files in the desired publications
%      4. `content:redraw()`
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

    % Doesn't throw so if exists  then things will just go
    % on.
    % Read TODO at `redraw/0`

    % NOTE 20221019_2001 Will fail if parent dirs are not present!
    file:make_dir(?PUBLICATION_ROOT),

    [ { { category, _ } = RootContentItem
      , [_|_] = MainMenuItems
      }
    ] =
        publication_guide(),

    ContentRoot =
        make_meta(RootContentItem, 0),

    FirstRunGraph =
        digraph:new([cyclic, protected]), % default values made explicit

    digraph:add_vertex(FirstRunGraph, ContentRoot),

    do_draw
      ( [first|MainMenuItems]
      , FirstRunGraph
      , ContentRoot
      ),

    % NOTE Linking hack % {{-
    % This silly solution is to make sure linking works in
    % the  publication  guide.  On  first  go-round,  when
    % `DirOptions` have  a `link`  tuple, but there  is no
    % directory, it  is probably  because it has  not been
    % created yet.  On the second run,  which is basically
    % `refresh_content_graph/1`, the dir  should be there,
    % and linking will take effect.
    %
    % The  elegant  solution would  have  been  to send  a
    % `redraw` message  at the  end of `init/0`  (to allow
    % the process  to finish  starting) but this  setup is
    % more than likely to be temporary anyway.
    %
    % LINKING FAIL:
    % If there  is none, or  more than one,  a "link_fail"
    % directory is created; this will be deleted after the
    % first  run, but  if  there is  still  one after  the
    % second one, it  means that the `publication_guide/0`
    % will need to be amended.
    % }}-
    digraph:delete(FirstRunGraph),
    file:del_dir("link_fail"),
    SecondRunGraph =
        digraph:new([cyclic, protected]),
    digraph:add_vertex(SecondRunGraph, ContentRoot),
    do_draw
      ( [first|MainMenuItems]
      , SecondRunGraph
      , ContentRoot
      ),

    SecondRunGraph.

% TODO Don't think this is possible
do_draw([], _Graph, _ParentVertex) ->
    done;

% Empty publications end condition
do_draw([first], _Graph, ParentVertex) ->
%     erlang:display([done, [first], ParentVertex]),
    done;

% End condition for all other
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
      );
% }}-

do_draw  % content_with_subitems, [          first, ContentItem | []    = Rest ] {{-
( [ first
  , { ContentItem, SubItems }
  | [] = _RestContentItemWithSubItems
  ]
, Graph
, ParentVertex
)
% Would be nice for Erlang to have Haskell-like type system and then this guard would be unnecessary (as { A, [_|_] } then wouldn't mean both {A, "lofa"} and {A, [{a,b}, {c, d}]}...)
when is_tuple(ContentItem)
->
    draw_content_with_subitems
      ( first_and_last
      , Graph
      , ParentVertex
      , ContentItem
      , 1
      , SubItems
      , []
      );

% }}-
do_draw  % content_with_subitems, [          first, ContentItem | [_|_] = Rest ] {{-
( [ first
  , { ContentItem, SubItems }
  | [_|_] = RestContenItemWithSubItems
  ]
, Graph
, ParentVertex
)
when is_tuple(ContentItem)
->
    draw_content_with_subitems
      ( first
      , Graph
      , ParentVertex
      , ContentItem
      , 1
      , SubItems
      , RestContenItemWithSubItems
      );

% }}-
do_draw  % content_with_subitems, [ PrevItemVertex, ContentItem | []    = Rest ] {{-
( [ #{ selection := ItemNumber } = PrevItemVertex
  , { ContentItem, SubItems }
  | [] = _RestContentItemWithSubItems
  ]
, Graph
, ParentVertex
)
when is_tuple(ContentItem)
->
    draw_content_with_subitems
      ( {PrevItemVertex, last}
      , Graph
      , ParentVertex
      , ContentItem
      , ItemNumber + 1
      , SubItems
      , []
      );

% }}-
do_draw  % content_with_subitems, [ PrevItemVertex, ContentItem | [_|_] = Rest ] {{-
( [ #{ selection := ItemNumber } = PrevItemVertex
  , { ContentItem, SubItems }
  | [_|_] = RestContenItemWithSubItems
  ]
, Graph
, ParentVertex
)
when is_tuple(ContentItem)
->
    draw_content_with_subitems
      ( PrevItemVertex
      , Graph
      , ParentVertex
      , ContentItem
      , ItemNumber + 1
      , SubItems
      , RestContenItemWithSubItems
      );
% }}-

% NOTE `leaf_item`s
%      More like "pseudo-leaf" because these will appear as
%      leaves  in  the  `publication_guide/0`  but  article
%      recordings read by the  directories will be the real
%      end vertices. (Unless they  have no recordings, then
%      they are truly leaves.)
do_draw  % leaf_item, [          first, ContentItem | []    = Rest ] {{-
( [ first
  , LeafContentItem
  | [] = _Rest
  ]
, Graph
, ParentVertex
)
->
    draw_leaf_item
      ( first_and_last
      , Graph
      , ParentVertex
      , LeafContentItem
      , 1
      , [] % Rest
      );

% }}-
do_draw  % leaf_item, [          first, ContentItem | [_|_] = Rest ] {{-
( [ first
  , LeafContentItem
  | [_|_] = Rest
  ]
, Graph
, ParentVertex
)
->
    draw_leaf_item
      ( first
      , Graph
      , ParentVertex
      , LeafContentItem
      , 1
      , Rest
      );

% }}-
do_draw  % leaf_item, [ PrevItemVertex, ContentItem | []    = Rest ] {{-
( [ #{ selection := ItemNumber } = PrevItemVertex
  , LeafContentItem
  | [] = _Rest
  ]
, Graph
, ParentVertex
)
->
    draw_leaf_item
      ( {PrevItemVertex, last}
      , Graph
      , ParentVertex
      , LeafContentItem
      , ItemNumber + 1
      , [] % Rest
      );

% }}-
do_draw  % leaf_item, [ PrevItemVertex, ContentItem | [_|_] = Rest ] {{-
( [ #{ selection := ItemNumber } = PrevItemVertex
  , LeafContentItem
  | [_|_] = Rest
  ]
, Graph
, ParentVertex
)
->
    draw_leaf_item
      ( PrevItemVertex
      , Graph
      , ParentVertex
      , LeafContentItem
      , ItemNumber + 1
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

% TODO This is configuration data, thus it shouldn't be here.
% TODO could have written a simple title_to_proper_filename function to convert e.g., "The Bickerson's" to "the-bickersons" but it is not applicable everywhere, and did not figure out how to make one opt out (entirely or in certain cases only). Currently it is fully explicit
% TODO NOTE LINKING % {{-
% Linking currently only works if the directory has been created before (
% }}-
% TODO make this explicit (i.e., dir_prefix, dir, title)
% Only 2 places need to be amended, by simply a matching for tuples
% + `make_title_dir/1`
% + `make_meta/2`
% Maybe add both forms
% TODO FAVORITES
% Linking can now be used to add publications to your favorites!
% , {publication, ["",  "Yuba-Sutter Meals On Wheels", "Meals on wheels"]}
publication_guide() -> % {{-
    [ { {category, "Main menu"}
    % [ { {category, "Main category"}
      , [ { { category % ads {{-
            , { "Store sales advertising"
              , [ {dir_prefix, "ads"} ]
              }
            }
          , [ { { category % grocery stores % {{-
                , { "Grocery stores"
                  , [ {dir_prefix, "grocery"} ]
                  }
                }
              , [ { { category, "Grocery stores with names beginning with 'A' through 'K'" }
                  , [ {publication, {"Aldi",        [{ alt, "aldi"        }]}}
                    , {publication, {"Foods Co",    [{ alt, "foods-co"    }]}}
                    % , {publication, {"Food Source", [{ alt, "food-source" }]}}
                    , {publication, {"Kroger",      [{ alt, "kroger"      }]}}
                    ]
                  }
                , { { category, "Grocery stores with names beginning with 'L' through 'R'" }
                  , [ {publication, {"La Superior",               [{ alt, "la-superior"                }]}}
                    , {publication, {"Lidl",                      [{ alt, "lidl"                       }]}}
                    , {publication, {"Lucky Supermarkets",        [{ alt, "lucky"                      }]}}
                    , {publication, {"Raley's",                   [{ alt, "raleys-sue"                 }]}}
                    , {publication, {"Rancho San Miguel Markets", [{ alt, "rancho-san-miguel-markets"  }]}}
                    ]
                  }
                , { { category, "Grocery stores with names beginning with 'S' through 'Z'" }
                  , [ {publication, {"Sacramento Natural Foods Co-op", [{ alt, "sac-coop" }]}} 
                    , { { sectioned_publication, "Safeway" } % {{-
	                    , [ {section, {"Weekly ads", [{alt, "safeway-this-week"}]}}
                        , {section, {"Big Book of Savings",  [{alt, "safeway-big-book"}]}}
                        % , {section, {"Weekly ads from February 3rd to the 9th",  [{alt, "safeway-last-week"}]}}
                        ]
                      } % }}-
                    , {publication, "Savemart"}
                    , {publication, "Sprouts"}
                    , {publication, {"Trader Joe's", [{ alt, "trader-joes" }]}}
                    ]
                  }
                ]
              } % }}-
          % , [ { { category % grocery stores % {{-
          %       , { "Grocery stores"
          %         , [ {dir_prefix, "grocery"} ]
          %         }
          %       }
          %     % , [ { publication, { "Safeway",  [{alt, "safeway-sue"  }]}} % 1
          %     , [ { { sectioned_publication, "Safeway" }
          %           % , { "Safeway"
          %           %   , [ {dir_prefix, "safeway"} ]
          %           %   }
          %           % }
          %         , [ {section, {"Big Book of Savings, valid from February 1st to March 2nd",  [{alt, "safeway-big-book"}]}}
          %           % , {section, {"Weekly ads from February 3rd to the 9th",  [{alt, "safeway-last-week"}]}}
		    % , {section, {"Weekly ads", [{alt, "safeway-this-week"}]}}
          %           ]
          %         }
          %       , { publication, { "Raley's",  [{alt, "raleys-sue"  }]}} % 2
          %       % , { { sectioned_publication
          %       %     , { "Raley's"
          %       %       , [ {dir_prefix, "raleys"} ]
          %       %       }
          %       %     }
          %       %   , [ {section, {"This week's ads",  [{alt, "latest"}]}}
          %       %     , {section, {"Week 8/26/2020 to 9/1/2020",  [{alt, "08262020"}]}}
		    % % , {section, {"Week 8/19/2020 to 8/25/2020", [{alt, "08192020"}]}}
		    % % , {section, {"Week 8/12/2020 to 8/18/2020", [{alt, "08122020"}]}}
		    % % , {section, {"Week 8/4/2020 to 8/11/2020",  [{alt, "08042020"}]}}
		    % % , {section, {"Week 7/28/2020 to 8/3/2020",  [{alt, "07282020"}]}}
		    % % , {section, {"Week 7/22/2020 to 7/27/2020", [{alt, "07222020"}]}}
          %       %     ]
          %       %   }
          %       % , {publication, {"Raley's",            [{ alt, "raleys"      }]}}
          %       , {publication, {"La Superior",        [{ alt, "la-superior"          }]}} % 3
          %       , {publication, {"Food Source",        [{ alt, "food-source"          }]}} % 4
          %       , {publication, "Savemart"}                                                % 5
          %       , {publication, {"Foods Co",           [{ alt, "foods-co"             }]}} % 6
          %       , {publication, {"Trader Joe's",       [{ alt, "trader-joes"          }]}} % 7
          %       , {publication, "Sprouts"}                                                 % 8
          %       , {publication, {"Lucky Supermarkets", [{ alt, "lucky"                }]}} % 9
          %       , {publication, {"Sacramento Natural Foods Co-op", [{ alt, "sac-coop" }]}} % 10
          %       , {publication, {"Aldi",                           [{ alt, "aldi"     }]}} % 11
          %       , {publication, {"Lidl",                           [{ alt, "lidl"     }]}} % 12
          %       , {publication, {"Kroger",                         [{ alt, "kroger"   }]}} % 13
          %       ]
          %     } % }}-
            , { { category % drug stores {{-
                , { "Drug stores"
                  , [ {dir_prefix, "drug"} ]
                  }
                }
              , [ {publication, "CVS"}
                , {publication, {"Rite Aid",   [{ alt, "rite-aid"  }]}}
                , {publication, {"Walgreen's", [{ alt, "walgreens" }]}}
                , {publication, {"Pharmaca",   [{ alt, "pharmaca"  }]}}
                ]
              } % }}-
            , { { category % discount stores {{-
                , { "Discount stores"
                  , [ {dir_prefix, "discount"} ]
                  }
                }
              , [ {publication, "Target"}
                , {publication, "Walmart"}
                , {publication, {"Big Lots",   [{ alt, "big-lots"  }]}}
                , {publication, "Costco"}
                ]
              } % }}-
            ]
          } % }}-
        , { { category % northern california newspapers and magazines {{-
            , { "Northern California newspapers and magazines"
              , [ {dir_prefix, "norcal"} ]
              }
            }
          , [ { { category % sacramento newspapers and mags {{-
                , { "Sacramento newspapers and magazines"
                  , [ {dir_prefix, "sac"} ]
                  }
                }
              , [ { { category % newspapers{{-
                    , { "Sacramento newspapers"
                      , [ {dir_prefix, "newspapers"} ]
                      }
                    }
                  , [ { { sectioned_publication % sacramento bee {{-
                        , { "Sacramento Bee sections"
                          , [ {dir_prefix, "sacbee"} ]
                          }
                        }
                      , [ {section, "Sports"}
                        , {section, "News"}
                        , {section, "Obituaries"}
                        ]
                      } % }}-
                    , {publication, {"Sacramento News & Review",    [{ alt, "SNR"              }]}}
                    % , {publication, {"Sacramento Press",            [{ alt, "sacramento-press" }]}}
                    , {publication, {"Sacramento Business Journal", [{ alt, "business-journal" }]}}
                    , {publication, {"Sacramento Observer",         [{ alt, "observer"         }]}}
                    , {publication, {"Sacramento City Express",     [{ alt, "city-express"     }]}}
                    , {publication, {"East Sacramento News",        [{ alt, "east-sac-news"    }]}}
                    % , {publication, {"The Land Park News",          [{ alt, "land-park-news"   }]}}
                    % , {publication, {"The Pocket News",             [{ alt, "pocket-news"      }]}}
                    , {publication, {"Cal Matters",                 [{ alt, "cal-matters"      }]}}
                    ]
                  } % }}-
                , { { category  % magazines {{-
                    , { "Sacramento magazines"
                      , [ {dir_prefix, "magazines"} ]
                      }
                    }
                  , [ {publication, "Comstocks"}
                    , {publication, "SacTown"}
                    , {publication, {"Sacramento Magazine", [{alt, "sacramento-magazine"}]}}
                    ]
                  } % }}-
                ]
              } % }}-
            , { { category  % greater sac {{-
                , { "Greater Sacramento area newspapers"
                  , [ {dir_prefix, "greater-sac"} ]
                  }
                }
              , [ {publication, {"Carmichael Times",                   [{ alt, "carmichael-times"                   }]}}
                % , {publication, {"Arden Carmichael News",              [{ alt, "arden-carmichael-news"              }]}}
                , {publication, {"Davis Enterprise",                   [{ alt, "davis-enterprise"                   }]}}
                , {publication, {"Roseville Press Tribune",            [{ alt, "roseville-press-tribune"            }]}}
                , {publication, {"Woodland Daily Democrat",            [{ alt, "woodland-daily-democrat"            }]}}
                , {publication, {"Elk Grove Citizen",                  [{ alt, "elk-grove-citizen"                  }]}}
                , {publication, {"Auburn Journal",                     [{ alt, "auburn-journal"                     }]}}
                , {publication, {"Grass Valley-Nevada City Union",     [{ alt, "grass-valley-nevada-city-union"     }]}}
                , {publication, {"El Dorado County Mountain Democrat", [{ alt, "el-dorado-county-mountain-democrat" }]}}
                , {publication, {"Loomis News",                        [{ alt, "loomis-news"                        }]}}
                ]
              } % }}-
            , { { category  % sf and bay area {{-
                , { "San Francisco and Bay Area newspapers"
                  , [ {dir_prefix, "bay-area"} ]
                  }
                }
              % , [ {publication, {"Vallejo Times Herald",       [{ alt, "vallejo-times-herald"      }]}}
              , [ {publication, {"Santa Rosa Press Democrat",  [{ alt, "santa-rosa-press-democrat" }]}}
                , {publication, {"SF Chronicle",               [{ alt, "sf-chronicle"              }]}}
                , {publication, {"SF Gate",                    [{ alt, "sf-gate"                   }]}}
                % , {publication, {"San Francisco Bay Guardian", [{ alt, "sf-bay-guardian"           }]}}
                , {publication, {"East Bay Times",             [{ alt, "east-bay-times"            }]}}
                , {publication, {"SF Weekly",                  [{ alt, "sf-weekly"                 }]}}
                , {publication, {"KQED",                       [{ alt, "KQED-bay-area-bites"       }]}}
                ]
              } % }}-
            , { { category % central california {{-
                , { "Central California newspapers"
                  , [ {dir_prefix, "central-cal"} ]
                  }
              }
              , [ {publication, {"Modesto Bee",     [{ alt, "modesto-bee"     }]}}
                , {publication, {"Stockton Record", [{ alt, "stockton-record" }]}}
                ]
              } % }}-
            , { { category % mendocino {{-
                , { "Mendocino county newspapers"
                  , [ {dir_prefix, "mendocino"} ]
                  }
              }
              , [ {publication, {"Fort Bragg Advocate News", [{ alt, "fort-bragg-advocate-news" }]}}
                , {publication, {"The Mendocino Beacon",     [{ alt, "mendocino-beacon"         }]}}
                ]
              } % }}-
            , { { category % humboldt and trinity counties {{-
                , { "Humboldt & Trinity county newspapers"
                  , [ {dir_prefix, "humboldt-trinity"} ]
                  }
              }
              , [ {publication, {"Humboldt Senior Resource Center's Senior News", [{ alt, "senior-news"           }]}}
                , {publication, {"North Coast Journal",                           [{ alt, "north-coast-journal"   }]}}
                , {publication, {"Eureka Times Standard",                         [{ alt, "eureka-times-standard" }]}}
                , {publication, {"Ferndale Enterprise",                           [{ alt, "ferndale-enterprise"   }]}}
                , {publication, {"Mad River Union",                               [{ alt, "mad-river-union"       }]}}
                ]
              } % }}-
            ]
          } % }}-
        , { { category % popular magazines {{-
            , { "Popular magazines"
              , [ {dir_prefix, "pop"} ]
              }
            }
          , [ { { category, "News, Entertainment, and Finance" } % {{-
              , [ {publication, "Newsweek"}
                , {publication, {"Entertainment Weekly", [{ alt, "EW"                 }]}}
                , {publication, "Fortune"}
                , {publication, {"Capital Public Radio", [{ alt, "CPR"                }]}}
                , {publication, {"Braille Monitor",      [{ alt, "braille-monitor"    }]}}
                ]
              } % }}-
            , { { category, "History, Science, Travel, and Culture" } % {{-
              , [ {publication, {"Mental Floss",         [{ alt, "mental-floss"       }]}}
                , { { sectioned_publication, "Atlas Obscura" } % {{-
                  , [ {publication, {"Latest articles", [{ alt, "atlas-obscura"      }]}}
                    , {publication, {"2016 articles",   [{ alt, "atlas-mental-archive-2016" }]}}
                    , {publication, {"2017 articles",   [{ alt, "atlas-mental-archive-2017" }]}}
                    , {publication, {"2018 articles",   [{ alt, "atlas-mental-archive-2018" }]}}
                    , {publication, {"2019 articles",   [{ alt, "atlas-mental-archive-2019" }]}}
                    , {publication, {"2020 articles",   [{ alt, "atlas-mental-archive-2020" }]}}
                    ]
                  } % }}-
                , {publication, {"New Scientist",        [{ alt, "new-scientist"      }]}}
                % , {publication, {"Travel & Leisure",     [{ alt, "travel-and-leisure" }]}}
                , {publication, {"Wild West",            [{ alt, "wild-west"          }]}}
                , {publication, {"Civil War Times",      [{ alt, "civil-war-times"    }]}}
                ]
              } % }}-
            ]
          } % }}-
        , { { category % old time radio {{-
            , { "Old Time Radio Theater"
              , [ {dir_prefix, "OTR"} ]
              }
            }
          , [ { {category, "Mystery"} % {{-
              , [
                  {publication, {"Inner Sanctum",                     [{ alt, "inner-sanctum"         }]}}
                , {publication, {"Mercury Radio Theater",             [{ alt, "mercury-radio-theater" }]}}
                , {publication, {"Mystery Traveler",                  [{ alt, "mystery-traveler"      }]}}
                , {publication, {"The Shadow",                        [{ alt, "the-shadow"            }]}}
                , {publication, "Suspense"}
                , {publication, {"The Whistler",                      [{ alt, "the-whistler"          }]}}
                , {publication, {"Light's Out",                       [{ alt, "lights-out"            }]}}
                , { { sectioned_publication % {{- The Lone Ranger
                    , { "CBS Radio Mystery Theater"
                      , [ {dir_prefix, "cbs-radio-mystery-theater"} ]
                      }
                    }
                  , [
                      { section, { "Episodes    1 to   50", [{ alt, "cbs_radio_mystery_theater-0001-0050" }] } } % 1
                    , { section, { "Episodes   51 to  100", [{ alt, "cbs_radio_mystery_theater-0051-0100" }] } } % 2
                    , { section, { "Episodes  101 to  150", [{ alt, "cbs_radio_mystery_theater-0101-0150" }] } } % 3
                    , { section, { "Episodes  151 to  200", [{ alt, "cbs_radio_mystery_theater-0151-0200" }] } } % 4
                    , { section, { "Episodes  201 to  250", [{ alt, "cbs_radio_mystery_theater-0201-0250" }] } } % 5
                    , { section, { "Episodes  251 to  300", [{ alt, "cbs_radio_mystery_theater-0251-0300" }] } } % 6
                    , { section, { "Episodes  301 to  350", [{ alt, "cbs_radio_mystery_theater-0301-0350" }] } } % 7
                    , { section, { "Episodes  351 to  400", [{ alt, "cbs_radio_mystery_theater-0351-0400" }] } } % 8
                    , { section, { "Episodes  401 to  450", [{ alt, "cbs_radio_mystery_theater-0401-0450" }] } } % 9
                    , { section, { "Episodes  451 to  500", [{ alt, "cbs_radio_mystery_theater-0451-0500" }] } } % 10
                    , { section, { "Episodes  501 to  550", [{ alt, "cbs_radio_mystery_theater-0501-0550" }] } } % 11
                    , { section, { "Episodes  551 to  600", [{ alt, "cbs_radio_mystery_theater-0551-0600" }] } } % 12
                    , { section, { "Episodes  601 to  650", [{ alt, "cbs_radio_mystery_theater-0601-0650" }] } } % 13
                    , { section, { "Episodes  651 to  700", [{ alt, "cbs_radio_mystery_theater-0651-0700" }] } } % 14
                    , { section, { "Episodes  701 to  750", [{ alt, "cbs_radio_mystery_theater-0701-0750" }] } } % 15
                    , { section, { "Episodes  751 to  800", [{ alt, "cbs_radio_mystery_theater-0751-0800" }] } } % 16
                    , { section, { "Episodes  801 to  850", [{ alt, "cbs_radio_mystery_theater-0801-0850" }] } } % 17
                    , { section, { "Episodes  851 to  900", [{ alt, "cbs_radio_mystery_theater-0851-0900" }] } } % 18
                    , { section, { "Episodes  901 to  950", [{ alt, "cbs_radio_mystery_theater-0901-0950" }] } } % 19
                    , { section, { "Episodes  951 to 1000", [{ alt, "cbs_radio_mystery_theater-0951-1000" }] } } % 20
                    , { section, { "Episodes 1001 to 1050", [{ alt, "cbs_radio_mystery_theater-1001-1050" }] } } % 21
                    , { section, { "Episodes 1051 to 1100", [{ alt, "cbs_radio_mystery_theater-1051-1100" }] } } % 22
                    , { section, { "Episodes 1101 to 1150", [{ alt, "cbs_radio_mystery_theater-1101-1150" }] } } % 23
                    , { section, { "Episodes 1151 to 1200", [{ alt, "cbs_radio_mystery_theater-1151-1200" }] } } % 24
                    , { section, { "Episodes 1201 to 1250", [{ alt, "cbs_radio_mystery_theater-1201-1250" }] } } % 25
                    , { section, { "Episodes 1251 to 1300", [{ alt, "cbs_radio_mystery_theater-1251-1300" }] } } % 26
                    , { section, { "Episodes 1301 to 1350", [{ alt, "cbs_radio_mystery_theater-1301-1350" }] } } % 27
                    , { section, { "Episodes 1351 to 1399", [{ alt, "cbs_radio_mystery_theater-1351-1399" }] } } % 28
                    ]
                  } % }}-
                , {publication, {"The Sealed Book",                   [{ alt, "the-sealed-book"       }]}}
                ]
              } % }}-
            , { {category, "Crime"} % {{-
              , [
                  {publication, {"Broadway's my Beat",                [{ alt, "broadways-my-beat"     }]}}
                , {publication, {"Black Stone the Magic Detective",   [{ alt, "black-stone"           }]}}
                , {publication, {"Boston Blacky",                     [{ alt, "boston-blacky"         }]}}
                , {publication, {"Crime Does Not Pay",                [{ alt, "crime-does-not-play"   }]}}
                , {publication, "Dragnet"}
                , {publication, {"Gang Busters",                      [{ alt, "gang-busters"          }]}}
                , {publication, {"Richard Diamond Private Detective", [{ alt, "richard-diamond"       }]}}
                , {publication, {"Adventures of Sam Spade",           [{ alt, "sam-spade"             }]}}
                ]
              } % }}-
            , { {category, "Comedy"} % {{-
              , [ {publication, {"Abbot and Costello",                  [{ alt, "abbot-and-costello"            }]}}
                , {publication, {"The Adventures of Ozzie and Harriet", [{ alt, "ozzie-and-harriet"             }]}}
                , {publication, {"The Bickerson's",                     [{ alt, "the-bickersons"                }]}}
                , {publication, {"Father Knows Best",                   [{ alt, "father-knows-best"             }]}}
                , {publication, {"Fibber McGee and Molly",              [{ alt, "fibber-mcgee-and-molly"        }]}}
                , {publication, {"The Fred Allen Show",                 [{ alt, "the-fred-allen-show"           }]}}
                , {publication, {"George Burns and Gracie Allen",       [{ alt, "george-burns-and-gracie-allen" }]}}
                , {publication, {"Life of Riley",                       [{ alt, "life-of-riley"                 }]}}
                , {publication, {"The Red Skelton Show",                [{ alt, "the-red-skelton-show"          }]}}
                ]
              } % }}-
            , { {category, "Westerns"} % {{-
              , [ {publication, {"The Cisco Kid",              [{ alt, "the-cisco-kid" }]}}
                , {publication, {"Gun Smoke",                  [{ alt, "gun-smoke"     }]}}
                , { { sectioned_publication % {{- The Lone Ranger
                    , { "The Lone Ranger"
                      , [ {dir_prefix, "lone-ranger"} ]
                      }
                    }
                  , [ { section, { "Introduction",          [{ alt,  "intro"  }] } }
                    , { section, { "Episodes    1 to  100", [{ alt,  "to-100" }] } }
                    , { section, { "Episodes  101 to  200", [{ alt,  "to-200" }] } }
                    , { section, { "Episodes  201 to  300", [{ alt,  "to-300" }] } }
                    , { section, { "Episodes  301 to  400", [{ alt,  "to-400" }] } }
                    , { section, { "Episodes  401 to  500", [{ alt,  "to-500" }] } }
                    , { section, { "Episodes  501 to  600", [{ alt,  "to-600" }] } }
                    , { section, { "Episodes  601 to  700", [{ alt,  "to-700" }] } }
                    , { section, { "Episodes  701 to  800", [{ alt,  "to-800" }] } }
                    , { section, { "Episodes  801 to  900", [{ alt,  "to-900" }] } }
                    , { section, { "Episodes  901 to 1000", [{ alt, "to-1000" }] } }
                    , { section, { "Episodes 1001 to 1100", [{ alt, "to-1100" }] } }
                    , { section, { "Episodes 1101 to 1200", [{ alt, "to-1200" }] } }
                    , { section, { "Episodes 1201 to 1300", [{ alt, "to-1300" }] } }
                    , { section, { "Episodes 1301 to 1400", [{ alt, "to-1400" }] } }
                    , { section, { "Episodes 1401 to 1500", [{ alt, "to-1500" }] } }
                    , { section, { "Episodes 1501 to 1600", [{ alt, "to-1600" }] } }
                    , { section, { "Episodes 1601 to 1700", [{ alt, "to-1700" }] } }
                    , { section, { "Episodes 1701 to 1800", [{ alt, "to-1800" }] } }
                    , { section, { "Episodes 1801 to 1900", [{ alt, "to-1900" }] } }
                    , { section, { "Episodes 1901 to 2000", [{ alt, "to-2000" }] } }
                    , { section, { "Episodes 2000 to 2100", [{ alt, "to-2100" }] } }
                    , { section, { "Episodes 2101 to 2200", [{ alt, "to-2200" }] } }
                    , { section, { "Episodes 2201 to 2300", [{ alt, "to-2300" }] } }
                    , { section, { "Episodes 2301 to 2400", [{ alt, "to-2400" }] } }
                    ]
                  } % }}-
                , {publication, {"Tales of the Texas Rangers", [{ alt, "texas-rangers" }]}}
                ]
              } % }}-
            , { {category, "Science fiction and fantasy"} % {{-
              , [ {publication, {"The Blue Beetle",  [{ alt, "blue-beetle"  }]}}
                , {publication, "Escape"}
                , {publication, {"The Green Hornet", [{ alt, "green-hornet" }]}}
                , {publication, {"X Minus 1",        [{ alt, "x-minus-1"    }]}}
                ]
              } % }}-
            , {publication, "Commercials"}
            ]
          } % }}-
        , { { category % games{{-
            , { "California General Election November 8th"
              , [ {dir_prefix, "ca-election"} ]
              }
            }
          , [ { publication, {"Voter Information Guide", [{ alt, "vig"}]}}
            , { { category % candidate statements
                , { "Candidate statements"
                  , [ {dir_prefix, "candidates"} ]
                  }
                }
              , [ { publication, {"U.S. Senate, full term", [{ alt, "senate-full"}]}}
                , { publication, {"U.S. Senate, partial term", [{ alt, "senate-partial"}]}}
                , { publication, {"governor", [{ alt, "gov"}]}}
                , { publication, {"lieutenant governor", [{ alt, "lt-gov"}]}}
                , { publication, {"attorney general", [{ alt, "ag"}]}}
                , { publication, {"secretary of state", [{ alt, "sos"}]}}
                , { publication, {"controller", [{ alt, "cont"}]}}
                , { publication, {"insurance commissioner", [{ alt, "comm"}]}}
                , { publication, {"treasurer", [{ alt, "treasurer"}]}}
                , { { category % board of equalization
                    , { "board of equalization"
                      , [{ dir_prefix, "boe"}]
                      }
                    }
                  , [ { publication, {"District 1", [{ alt, "district-1"}]}}
                    , { publication, {"District 2", [{ alt, "district-2"}]}}
                    , { publication, {"District 3", [{ alt, "district-3"}]}}
                    , { publication, {"District 4", [{ alt, "district-4"}]}}
                    ]
                  }
                , { publication, {"superintendent of public instruction", [{ alt, "sopi"}]}}
                ]
              }
            , { { category % propositions
                , { "Propositions"
                  , [ {dir_prefix, "props"} ]
                  }
                }
              , [ { publication, {"Proposition 1", [{ alt, "prop-1"}]}}
                , { publication, {"Propositions 26", [{ alt, "prop-26"}]}}
                , { publication, {"Propositions 27", [{ alt, "prop-27"}]}}
                , { publication, {"Propositions 28", [{ alt, "prop-28"}]}}
                , { publication, {"Propositions 29", [{ alt, "prop-29"}]}}
                , { publication, {"Propositions 30", [{ alt, "prop-30"}]}}
                , { publication, {"Propositions 31", [{ alt, "prop-31"}]}}
                ]
              }
            ]
          } % }}-
        , { { category % community {{-
            , { "Community information and resources"
              , [ {dir_prefix, "community-resources"} ]
              }
            }
          % , [ { { category, "Voter information guides" } % {{-
          %     , [ {publication, {"California voter guide",        [{ alt, "cal-voter-guide" }]}}
          %       , {publication, {"Sacramento County voter guide", [{ alt, "sac-county-voter-guide" }]}}
          %       , { { sectioned_publication
          %           , { "Propositions"
          %             , [ {dir_prefix, "propositions"} ]
          %             }
          %           }
          %         , [ {section, { "Proposition 14", [{alt, "props-14"}]}}
		    % , {section, { "Proposition 15", [{alt, "props-15"}]}}
		    % , {section, { "Proposition 16", [{alt, "props-16"}]}}
		    % , {section, { "Proposition 17", [{alt, "props-17"}]}}
		    % , {section, { "Proposition 18", [{alt, "props-18"}]}}
		    % , {section, { "Proposition 19", [{alt, "props-19"}]}}
		    % , {section, { "Proposition 20", [{alt, "props-20"}]}}
		    % , {section, { "Proposition 21", [{alt, "props-21"}]}}
		    % , {section, { "Proposition 22", [{alt, "props-22"}]}}
		    % , {section, { "Proposition 23", [{alt, "props-23"}]}}
		    % , {section, { "Proposition 24", [{alt, "props-24"}]}}
		    % , {section, { "Proposition 25", [{alt, "props-25"}]}}
          %           ]
          %         }
          %       ]
          %     } % }}-
          , [ { { category, "Podcasts"} % {{-
              , [ { publication
                  , { "Beyond Barriers Project"
                    , [ {dir_prefix, "SFTB"}
                      , {link, "beyond-barriers"}
                      ]
                    }
                  }
                , { publication
                  , { "Beyond Barriers Project's music theory with Jim"
                    , [ {dir_prefix, "SFTB"}
                      , {link, "bbu-music-theory-with-jim" }
                      ]
                    }
                  }
                , {publication, {"Live Audiozine", [{alt, "live-audiozine"}]}}
                % , {publication, {"The Redacted Files Podcast", [{alt, "TRP"}]}}
                ]
              } % }}-
            % , { { category, "Poetry" } % {{-
            %   , [ {publication, {"Brad Buchanan",       [{ alt, "brad-buchanan"      }]}}
            %     , {publication, {"Writer's on the air", [{ alt, "writers-on-the-air" }]}}
            %     ]
            %   } % }}-
            ]
          } % }}-
        , { { category % blindness resources {{-
            , { "Blindness information and resources"
              , [ {dir_prefix, "blindness-resources"} ]
              }
            }
          , [ { { category % organizations {{-
                , { "Organizations"
                  , [ {dir_prefix, "orgs"} ]
                  }
                }
              , [ { { category % SFTB {{-
                    , { "Society for the Blind"
                      , [ {dir_prefix, "SFTB"} ]
                      }
                    }
                  , [ {publication, {"SFB Connection",                                  [{ alt, "sfb-connection"   }]}}
                    , {publication, {"Monthly newsletter",                              [{ alt, "newsletter"       }]}}
                    , {publication, {"Society for the Blind's student handbook",        [{ alt, "student-handbook" }]}}
                    , {publication, {"Beyond Barriers Project",                         [{ alt, "beyond-barriers"  }]}}
                    , {publication, {"Beyond Barriers Project's music theory with Jim", [{ alt, "bbu-music-theory-with-jim"  }]}}
                    ]
                  } % }}-
                , {publication, {"The Earle Baum Center",           [{ alt, "EBC"   }]}}
                , {publication, {"Sierra Services for the Blind",   [{ alt, "SSFTB" }]}}
                , {publication, {"California Council of the Blind", [{ alt, "CCB"   }]}}
                , {publication, {"The Council of Citizens with Low Vision international", [{ alt, "CCLVI"   }]}}
                ]
              } % }}-
            , { { category % publications {{-
                , { "Publications"
                  , [ {dir_prefix, "publications" } ]
                  }
                }
              , [ {publication, {"Braille Monitor",           [{ link, "braille-monitor" }]}}
                , {publication, {"Client Assistence Program", [{ link, "CAP"              }]}}
                ]
              } % }}-
            ]
          } % }}-
        , { { category  % education and resources {{-
            , { "Education and resources"
              , [ {dir_prefix, "edu"} ]
              }
            }
          , [ {publication, {"Society for the Blind's student handbook", [{ link, "student-handbook"       }]}}
            % , {publication, {"Balance exercises",                        [{ alt, "balance-exercises"       }]}}
            % , {publication, {"Achieve a healthy weight by UC Davis",     [{ alt, "uc-davis-healthy-weight" }]}}
            % , {publication, {"Yuba-Sutter Meals On Wheels",              [{ alt, "YSMOW"                   }]}}
            % , {publication, {"Client Assistence Program",                [{ alt, "CAP"                     }]}}
            ]
          } % }}-
        % , { { category  % access news {{-
        %     , { "Access News"
        %       , [ {dir_prefix, "access-news"} ]
        %       }
        %     }
        %   , [ {publication, {"Updates and announcements", [{ alt, "updates" }]}}
        %     , {publication, {"Our volunteer readers",     [{ alt, "readers" }]}}
        %     , {publication, {"History of Access News",    [{ alt, "history" }]}}
        %     ]
        %   } % }}-
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

% TODO So this could safely be set to [], but need to make sure that it doesn't affect anything. At least, tried renaming it Rest, and use the
% logger:debug(#{make_title_dir => Rest})
% and the only thing that came up was []
%                                      |
%                                      V
% make_title_dir([ Prefix, Title | _]) when is_list(Prefix) ->
%     make_title_dir(Prefix ++ Title);
%  NOTE This was unnecessary here; taken care of in `prefix_leaf_items/2` where it should be

do_dir_options(Title, []) ->
    make_title_dir(Title);

do_dir_options % alt {{-
  ( Title
  , [ {alt, AlternativeName} | Opts ]
  )
->
    do_dir_options(AlternativeName, Opts);

% }}-
do_dir_options % dir_prefix {{-
  ( Title
  , [ {dir_prefix, Prefix} | Opts ]
  )
->
    do_dir_options
      ( Prefix ++ "-" ++ Title
      , Opts
      );

% }}-
do_dir_options % link {{-
  ( _Title
  , [ {link, EndMatch} | Opts ]
  )
->
    {ok, PublicationDirs} =
        file:list_dir(?PUBLICATION_ROOT),

    % Could have matched anywhere else, but matching at the end seemed the most logical
    MatchAtTheEnd =
        fun(Dirname) ->
            case re:run(Dirname, EndMatch ++ "$") of
                nomatch -> false;
                      _ -> true
            end
        end,

    % There should only be one match when linking.
    LinkDir =
        case lists:filter(MatchAtTheEnd, PublicationDirs) of
            [ Dir ] ->
                Dir;
            _ ->
                FailDir = "link_fail",
                file:make_dir(FailDir),
                FailDir
        end,

    do_dir_options
      ( LinkDir
      , Opts
      ).

% }}-
make_title_dir({Title, UnsortedOpts}) ->
    do_dir_options
      ( Title
      % `alt` has to come before dir_prefix
      % see NOTE at `draw_content_with_subitems/7`
      %
      % Preserves the order of the tuples of a given key
      % lists:keysort(1, [{b, "miez"}, {a, "lofa"}, {b, "balabab"}]).
      % [{a,"lofa"},{b,"miez"},{b,"balabab"}]
      , lists:keysort(1, UnsortedOpts)
      );

% NOTE Fixed the  above clause from  list to tuple,  so the
%      guard could simply read `is_list(Title)` but this is
%      way more explicit that a string is needed
make_title_dir([Char|_] = Title) when is_integer(Char) -> % {{-
    Dir =
        filename:join(?PUBLICATION_ROOT, Title),
    % no fuss if exists, won't throw
    file:make_dir(Dir),
%     erlang:display([make_title_dir, Dir]),
    Dir.

% }}-
list_recording_vertices(Dir) -> % {{-
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
                % TODO ffmpeg
                _ -> false
            end
        end,

    MakeRecordingMeta =
        fun (Filename) ->
            futil:pipe
              ([ Filename
               , (futil:cflip(fun filename:absname/2))(Dir)
               , fun make_recording_meta/1
               ])
        end,

%         erlang:display([list_recording_vertices, enter, Dir]),
    futil:pipe
      % NOTE The extra  quotes are  needed because  otherwise the {{-
      %      special  characters  in   `Dir`  will  be
      %      treated literally by the shell.
      % ``` text
      % $ ls Raley's
      % # VS
      % $ ls "Raley's"
      % ```
      % Also,  `-r` because  right  now  the recordings  are
      % numbered, and  the higher  the number the  newer the
      % recording.
      % }}-

      % NOTE Calling `ls`  without any  modifiers means  that the
      %      order  of the  returned list  depends on  consistent
      %      file naming!
      % TODO Make  an upload  mechanism  that takes  care of  the
      %      naming  based on  given  parameters  (e.g., sort  by
      %      time, numbering, etc.).
      ([ os:cmd("ls \"" ++ Dir ++ "\"")
       , (futil:cflip(fun string:lexemes/2))([$\n])
%      , fun (X) -> erlang:display([list_recording_vertices, X]), X end
       , (futil:curry(fun lists:filter/2))(Extensions)
%      , fun (X) -> erlang:display([list_recording_vertices, X]), X end
       % NOTE It would have made more logical sense to put this in
       % `draw_item/7`  but it  would have  been a  hassle to
       % figure  out `Dir`  - plus  it would  have
       % been an extra loop
       , (futil:curry(fun lists:map/2))(MakeRecordingMeta)
%      , fun (X) -> erlang:display([list_recording_vertices, X]), X end
       ]).
% }}-

% LeafContentItem = { LeafItemType, TitleMaybeWithOptions }
% LeafItemType = publication | section
% TitleMaybeWithOptions = Title | { Title, DirOptions }
% Title = String
% DirOptions = [ {Option, String} ]
% Option = dir_prefix | alt
draw_leaf_item % {{-
( Direction
, Graph
, ParentVertex
, { LeafItemType, TitleMaybeWithOptions } = LeafContentItem
, ItemNumber
, Rest
)
when LeafItemType =:= publication
   ; LeafItemType =:= section
->
    RecordingVertices =
        futil:pipe
          ([ TitleMaybeWithOptions
           , fun make_title_dir/1
           , fun list_recording_vertices/1
           ]),
    % => [ Map ]

    LeafItemVertex =
        make_meta(LeafContentItem, ItemNumber),

    draw_item
      ( Direction
      , Graph
      , ParentVertex
      , LeafItemVertex
      , RecordingVertices
      , Rest
      ).

% }}-
% ContentItemWithSubItemsType = category | sectioned_publication
% TitleMaybeWithOptions = Title | { Title, DirOptions }
% Title = String
% DirOptions = [ {Option, String} ]
% Option = dir_prefix   % no `alt`!!!
%
% NOTE "NO `alt`"
%      It  does   not  make  sense   to  use  `alt`   on  a
%      ContentItemWithSubItems (and may  even cause a crash
%      because then  every LeafSubItems would get  the same
%      alternative directory  name (effectively  making all
%      LeafSubItems  linking to  the  same  dir under  that
%      ContentItemWithSubItems, right?)
draw_content_with_subitems % {{-
( Direction
, Graph
, #{ type := _ContentType} = ParentVertex
% , [ category, _ContentTitle | MaybePrefix ] = ContentItemWithSubItems
, { ContentItemWithSubItemsType
  , TitleMaybeWithOptions
  } = ContentItemWithSubItems
, ItemNumber
% Explicitly disallow ContentItemWithSubItems without any sub items
, [_|_] = SubItems
, Rest
)
when ContentItemWithSubItemsType =:= category
   ; ContentItemWithSubItemsType =:= sectioned_publication
->
    PrefixedSubItems =
        prefix_subitems(SubItems, TitleMaybeWithOptions),

    ItemVertex =
        make_meta(ContentItemWithSubItems, ItemNumber),

    draw_item
      ( Direction
      , Graph
      , ParentVertex
      , ItemVertex
      , PrefixedSubItems
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
    BaseMeta =
        #{ type  => article
         , path  => AbsFilename
         , title => ""
         },
    add_id(BaseMeta).

% TODO, NOTE, whatever
% `ContentType` of each vertex in the content graph is
% used as  the state of  the IVR state machine  at one
% point - that  is, almost; `ivr:derive_state/1` keeps
% `content.erl` and `ivr.erl` decoupled.
make_meta
( { ContentType, {Title, _Options} } = _ContentItem
, ItemNumber
) ->
    make_meta
      ( { ContentType, Title }
      , ItemNumber
      );

make_meta
( { ContentType, [Char|_] = Title } = _ContentItem
, ItemNumber
)
when is_integer(Char)
->
    BaseMeta =
        #{ type      => ContentType
         , selection => ItemNumber
         , title     => Title
         },
    add_id(BaseMeta).

% This is what makes linking the same  publication  in
% different categories possible.  Without IDs the same
% `publication`  item   in  the  `publication_guide/0`
% would link  to multiple categories, that  is, create
% loop, and content graph would not be a tree anymore.
add_id(BaseMeta) ->
    BaseMeta#{id => erlang:make_ref()}.

% prefix_subitems
% ( [ {publication, [_, _]}
%   | _
%   ] = PrefixedPublicationsOrSections
% , _Prefix
% ) ->
%     PrefixedPublicationsOrSections;

% prefix_subitems
% ( [ {section, Title}
%   | Rest
%   ]
% , Prefix
% ) ->
%     prefix_subitems([{publication, Title}|Rest], Prefix);

% prefix_subitems
% ( [ {SubItemType, Title}
%   | Rest
%   ]
% , Prefix
% ) ->
%     NewRest =
%         Rest ++ [{SubItemType, [Prefix, Title]}],

%     prefix_subitems(NewRest, Prefix).

% NOTE If  any of  the parent  had a  `DirOptions` list,  we
%      would never  get here, because it  will trickle down
%      once present
prefix_subitems
  ( SubItems
  , [Char|_] = _TitleMaybeWithOptions
  )
when is_integer(Char)
->
    SubItems;

prefix_subitems
  ( SubItems
  , { Title, DirOptions } = TitleMaybeWithOptions
  )
->
    do_prefix(SubItems, DirOptions, []).

do_prefix([], _DirOptions, Acc) ->
    % The order matters because of automatic item numbering
    lists:reverse(Acc);

do_prefix
  ( [ SubItem | RestSubItems ]
  , DirOptions
  , Acc
  )
->
    SubItemWithMergedOptions =
        case SubItem of
            { { ContentItemWithSubItemsType
              , TitleMaybeWithOptions
              } = ContentItemWithSubItems
            % ContentItemWithSubItems should never be empty
            , [_|_] = SubItemSubItems
            }
            ->
                { { ContentItemWithSubItemsType
                  , merge_dir_options(TitleMaybeWithOptions, DirOptions)
                  }
                , SubItemSubItems
                };

            { LeafItemType, TitleMaybeWithOptions } ->
                { LeafItemType
                , merge_dir_options(TitleMaybeWithOptions, DirOptions)
                }
        end,

    do_prefix
      ( RestSubItems
      , DirOptions
      , [ SubItemWithMergedOptions | Acc ]
      ).

merge_dir_options
  ( TitleMaybeWithOptions
  , DirOptions
  )
->
    case TitleMaybeWithOptions of

        [Char|_] = Title when is_integer(Char) ->
            { Title, DirOptions };

        { Title, SubItemDirOptions } ->
            { Title
            , SubItemDirOptions ++ DirOptions
            }
    end.


% vim: set fdm=marker:
% vim: set foldmarker={{-,}}-:
% vim: set nowrap:
