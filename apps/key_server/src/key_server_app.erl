%%%-------------------------------------------------------------------
%% @doc tcp_server public API
%% @end
%%%-------------------------------------------------------------------

-module(key_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok,Sup} = key_server_sup:start_link(),
    key_server:listen(8888),
    {ok,Sup}.


stop(_State) ->
    ok.

%% internal functions
