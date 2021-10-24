%%%-------------------------------------------------------------------
%%% @author qinweichen
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 5月 2021 11:47 上午
%%%-------------------------------------------------------------------
-module(key_server).
-author("qinweichen").

%% API
-export([listen/1]).

listen(Port) ->
  erlang:spawn_link(
    fun() ->
      {ok, ListenSocket} = gen_tcp:listen(Port, [{mode,binary}]),
      loop(ListenSocket)
    end).


loop(ListenSocket) ->
  case gen_tcp:accept(ListenSocket) of
    {ok, Socket} ->
      case supervisor:start_child(key_server_sup, [Socket]) of
        {ok, ChildPid} ->
          gen_tcp:controlling_process(Socket, ChildPid);
        R ->
          io:format("supervisor:start_child fail ~w", [R])
      end,
      loop(ListenSocket);
    R ->
      io:format("tcp listen is fail ~w", [R])
  end.
