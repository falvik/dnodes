-module(dnodes_nodes_sup).
-author("falvik").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-export([
  get_nodes/0,
  start_node/0
]).

-define(SERVER, ?MODULE).

-include("include.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() ->
  {ok, pid()} | {ok, pid(), State :: term()} | {error, Reason ::term()}.
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec get_nodes() ->
  [{  _,
      'restarting' | 'undefined' | pid(),
      'supervisor' | 'worker',
      'dynamic' | [atom() | tuple()]
  }].
get_nodes()->
  supervisor:which_children(?SERVER).

-spec start_node() -> {ok, {node_id(), pid()}}.
start_node()->
  Node_id = erlang:unique_integer(),
  Child = {Node_id, {dnodes_node, start_link, [Node_id]}, temporary, 10000, worker, []},
  {ok, Pid} = supervisor:start_child(?SERVER, Child),
  {ok, {Node_id, Pid}}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
-spec init([]) -> {ok, {supervisor:sup_flags(), []}}.
init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 10,
  MaxSecondsBetweenRestarts = 5,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  {ok, {SupFlags, []}}.