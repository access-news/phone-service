%%%-------------------------------------------------------------------
%% @doc content_storage_api public API
%% @end
%%%-------------------------------------------------------------------

-module(content_storage_api_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    content_storage_api_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
