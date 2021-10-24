%%%-------------------------------------------------------------------
%%% @author qinweichen
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 5月 2021 8:48 下午
%%%-------------------------------------------------------------------
-module(simple_cache).
-author("qinweichen").

%% API
-export([insert/2,delete/1,lookup/1]).

insert(Key, Value) ->
  case sc_store:lookup(Key) of
    {ok, Pid} ->
      sc_element:replace(Pid, Value);
    {error, not_found} ->
      {ok, Pid} = sc_element:create(Value),
      sc_store:insert(Key, Pid)
  end.

delete(Key) ->
  case sc_store:lookup(Key) of
    {ok, Pid} ->
      sc_element:delete(Pid);
    {error, not_found} ->
      ok
  end.

lookup(Key) ->
  case sc_store:lookup(Key) of
    {ok, Pid} ->
      sc_element:fetch(Pid);
    _ ->
      {error, not_found}
  end.





