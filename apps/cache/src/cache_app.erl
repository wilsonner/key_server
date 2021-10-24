%%%-------------------------------------------------------------------
%% @doc cache public API
%% @end
%%%-------------------------------------------------------------------

-module(cache_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    sc_store:init(),
    {ok, Sup} = cache_sup:start_link(),
    simple_cache:insert("Tom", "TomKey"),
    simple_cache:insert("Nancy", "NancyKey"),
    simple_cache:insert("Kitty", "KittyKey"),
    {ok, Sup}.

stop(_State) ->
    ok.

%% internal functions
