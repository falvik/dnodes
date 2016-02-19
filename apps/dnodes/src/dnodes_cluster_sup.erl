-module(dnodes_cluster_sup).
-behaviour(supervisor).
-author("falvik").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
-spec start_link() ->
  {ok, pid()} | {ok, pid(), State :: term()} | {error, Reason ::term()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec init([]) -> {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.
init([]) ->
  {ok, {{one_for_one, 5, 10}, [
    {dnodes_nodes_sup,{dnodes_nodes_sup, start_link,[]}, permanent, infinity, supervisor,[]},
    {dnodes_cluster,{dnodes_cluster, start_link,[]}, permanent, 10, worker,[]}
  ]}}.

