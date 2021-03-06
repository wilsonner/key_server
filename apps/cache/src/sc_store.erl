%%%-------------------------------------------------------------------
%%% @author qinweichen
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 5ζ 2021 8:09 δΈε
%%%-------------------------------------------------------------------
-module(sc_store).
-author("qinweichen").

%% API
-export([init/0,insert/2,lookup/1,delete/1]).

-define(TABLE_ID, ?MODULE).

init() ->
  ets:new(?TABLE_ID, [public, named_table]),
  ok.

insert(Key, Pid) ->
  ets:insert(?TABLE_ID, {Key, Pid}).

lookup(Key) ->
  case ets:lookup(?TABLE_ID, Key) of
    [{Key, Pid}] -> {ok, Pid};
    [] -> {error, not_found}
  end.

delete(Pid) ->
  ets:match_delete(?TABLE_ID, {'_', Pid}).
