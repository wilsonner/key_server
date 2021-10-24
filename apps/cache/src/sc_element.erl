%%%-------------------------------------------------------------------
%%% @author qinweichen
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 5月 2021 4:47 下午
%%%-------------------------------------------------------------------
-module(sc_element).
-author("qinweichen").

-behaviour(gen_server).

%% API
-export([start_link/2, create/2, create/1, fetch/1, replace/2, delete/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_LEASE_TIME, 60 * 60 * 24).

-record(sc_element_state, {value, lease_time, start_time}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Args :: term(), Args :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Value, LeaseTime) ->
  gen_server:start_link(?MODULE, [Value, LeaseTime], []).

create(Value, LeaseTime) ->
  cache_sup:start_child(Value, LeaseTime).

create(Value) ->
  cache_sup:start_child(Value, ?DEFAULT_LEASE_TIME).

fetch(Pid) ->
  gen_server:call(Pid, fetch).

replace(Pid, Value) ->
  gen_server:cast(Pid, {replace, Value}).

delete(Pid) ->
  gen_server:cast(Pid, delete).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #sc_element_state{}} | {ok, State :: #sc_element_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Value, LeaseTime]) ->
  Now = calendar:local_time(),
  StartTime = calendar:datetime_to_gregorian_seconds(Now),
  {ok, #sc_element_state{value = Value, lease_time = LeaseTime, start_time = StartTime}, time_left(StartTime, LeaseTime)}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #sc_element_state{}) ->
  {reply, Reply :: term(), NewState :: #sc_element_state{}} |
  {reply, Reply :: term(), NewState :: #sc_element_state{}, timeout() | hibernate} |
  {noreply, NewState :: #sc_element_state{}} |
  {noreply, NewState :: #sc_element_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #sc_element_state{}} |
  {stop, Reason :: term(), NewState :: #sc_element_state{}}).
handle_call(fetch, _From, State) ->
  #sc_element_state{value = Value, lease_time = LeaseTime, start_time = StartTime} = State,
  TimeLeft = time_left(StartTime, LeaseTime),
  {reply, {ok, Value}, State, TimeLeft};
handle_call(_Request, _From, State = #sc_element_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #sc_element_state{}) ->
  {noreply, NewState :: #sc_element_state{}} |
  {noreply, NewState :: #sc_element_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #sc_element_state{}}).
handle_cast({replace, Value}, State) ->
  #sc_element_state{lease_time = LeaseTime, start_time = StartTime} = State,
  TimeLeft = time_left(StartTime, LeaseTime),
  {noreply, State#sc_element_state{value = Value}, TimeLeft};
handle_cast(delete, State) ->
  {stop, normal, State};
handle_cast(_Request, State = #sc_element_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #sc_element_state{}) ->
  {noreply, NewState :: #sc_element_state{}} |
  {noreply, NewState :: #sc_element_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #sc_element_state{}}).
handle_info(timeout, State) ->
  {stop, normal, State};
handle_info(_Info, State = #sc_element_state{}) ->
  {noreply, State}.


%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #sc_element_state{}) -> term()).
terminate(_Reason, _State = #sc_element_state{}) ->
  sc_store:delete(self()),
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #sc_element_state{},
    Extra :: term()) ->
  {ok, NewState :: #sc_element_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #sc_element_state{}, _Extra) ->
  {ok, State}.

time_left(_StartTime, infinity) ->
  infinity;
time_left(StartTime, LeaseTime) ->
  Now = calendar:local_time(),
  CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
  TimeElapsed = CurrentTime - StartTime,
  case LeaseTime - TimeElapsed of
    Time when Time =< 0 -> 0;
    Time -> Time * 1000
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
