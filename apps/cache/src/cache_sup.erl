%%%-------------------------------------------------------------------
%% @doc cache top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(cache_sup).

-behaviour(supervisor).

-export([start_link/0,start_child/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Value, LeaseTime) ->
  supervisor:start_child(?SERVER, [Value, LeaseTime]).

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
    intensity => 0,
    period => 1},
  ChildSpecs = [
    #{
      id => sc_element,
      start => {sc_element, start_link, []},
      restart => transient,
      shutdown => brutal_kill,
      type => worker,
      modules => [sc_element]
    }
  ],
  {ok, {SupFlags, ChildSpecs}}.