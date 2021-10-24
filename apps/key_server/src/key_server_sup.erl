%%%-------------------------------------------------------------------
%% @doc tcp_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(key_server_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 50,
                 period => 1},
    ChildSpecs = [
      #{
        id => key_server_msg_handler,
        start => {key_server_msg_handler, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [key_server_msg_handler]
      }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
