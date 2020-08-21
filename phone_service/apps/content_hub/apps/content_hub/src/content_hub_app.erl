%%%-------------------------------------------------------------------
%% @doc content_hub public API
%% @end
%%%-------------------------------------------------------------------

-module(content_hub_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    content_hub_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
