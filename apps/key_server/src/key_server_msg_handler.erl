%%%-------------------------------------------------------------------
%%% @author qinweichen
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 5月 2021 11:41 上午
%%%-------------------------------------------------------------------
-module(key_server_msg_handler).
-author("qinweichen").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(key_server_msg_handler_state, {address, port}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Args :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Socket) ->
  gen_server:start_link(?MODULE, [Socket], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #key_server_msg_handler_state{}} |
  {ok, State :: #key_server_msg_handler_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Socket]) ->
  {ok, {Address, Port}} = inet:peername(Socket),
  {ok, #key_server_msg_handler_state{address = Address, port = Port}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #key_server_msg_handler_state{}) ->
  {reply, Reply :: term(), NewState :: #key_server_msg_handler_state{}} |
  {reply, Reply :: term(), NewState :: #key_server_msg_handler_state{}, timeout() |
  hibernate} | {noreply, NewState :: #key_server_msg_handler_state{}} |
  {noreply, NewState :: #key_server_msg_handler_state{}, timeout() |
  hibernate} | {stop, Reason :: term(), Reply :: term(), NewState ::
  #key_server_msg_handler_state{}} | {stop, Reason :: term(), NewState ::
  #key_server_msg_handler_state{}}).
handle_call(_Request, _From, State = #key_server_msg_handler_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #key_server_msg_handler_state{}) ->
  {noreply, NewState :: #key_server_msg_handler_state{}} |
  {noreply, NewState :: #key_server_msg_handler_state{}, timeout() |
  hibernate} | {stop, Reason :: term(), NewState :: #key_server_msg_handler_state{}}).
handle_cast(_Request, State = #key_server_msg_handler_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #key_server_msg_handler_state{}) ->
  {noreply, NewState :: #key_server_msg_handler_state{}} |
  {noreply, NewState :: #key_server_msg_handler_state{}, timeout() |
  hibernate} | {stop, Reason :: term(), NewState :: #key_server_msg_handler_state{}}).
handle_info(Info, State) ->
  case Info of {tcp, Socket, RawData} ->
    <<Version:4, MsgId:4, Payload/binary>> = RawData,
    io:format("recv msg version:~p msgId:~p,payload:~p", [Version, MsgId, Payload]),

    QueryKey = binary_to_list(Payload),
    case simple_cache:lookup(QueryKey) of
      {ok, QueryValue} ->
        io:format("lookup key success key:~p value:~p", [QueryKey, QueryValue]),
        gen_tcp:send(Socket, io_lib:format("msg id:~p response:~s by key:~s",
          [MsgId, QueryValue, QueryKey])),
        {noreply, State};
      {error, not_found} ->
        gen_tcp:send(Socket, io_lib:format("msg id:~p response:~s by key:~s",
          [MsgId, "Not Found", QueryKey])),
        {noreply, State}
    end;
    timeout ->
      {stop, "timeout", State};
    {tcp_closed, _Socket} ->
      {stop, "tcp closed", State}
  end.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #key_server_msg_handler_state{}) -> term()).
terminate(Reason, _State = #key_server_msg_handler_state{address = Address,
  port = Port}) ->
  io:format("process exit reason:~p address:~p port:~p", [Reason, Address, Port]),
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State ::
#key_server_msg_handler_state{},
    Extra :: term()) ->
  {ok, NewState :: #key_server_msg_handler_state{}} | {error, Reason :: term
  ()}).
code_change(_OldVsn, State = #key_server_msg_handler_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
