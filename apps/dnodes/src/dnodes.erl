-module(dnodes).
-author("falvik").

%% API
-export([start/0, stop/0]).
-export([nodes_info/0, add_node/0, kill_node/1, connect_nodes/2, disconnect/2]).

-define(APPS, [crypto, cowlib, ranch, cowboy, dnodes]).
-include("include.hrl").


%% ===================================================================
%% API functions
%% ===================================================================
-spec start() -> ok.
start() ->
  ok = ensure_started(?APPS),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", cowboy_static, {priv_file, dnodes, "index.html"}},
      {"/websocket", dnodes_ws_handler, []},
      {"/[...]", cowboy_static, {priv_dir, dnodes, ""}}
    ]}
  ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [{env, [{dispatch, Dispatch}]}]),
  ok.

-spec stop() -> ok.
stop() ->
  ok = stop_apps(lists:reverse(?APPS)).

-spec nodes_info()-> [{node_id(), Links :: [node_id()]}].
nodes_info() ->
  dnodes_cluster:nodes_info().

-spec add_node()-> node_id().
add_node()->
  dnodes_cluster:add_node().

-spec kill_node(node_id()) -> ok | not_found.
kill_node(Id)->
  dnodes_cluster:kill_node(Id).

-spec connect_nodes(node_id(), node_id()) -> ok | not_found.
connect_nodes(Target_id, Id) ->
  dnodes_cluster:connect_nodes(Target_id, Id).

-spec disconnect(node_id(), node_id()) -> ok | not_found.
disconnect(Id1, Id2)->
  dnodes_cluster:disconnect(Id1, Id2).

%% ===================================================================
%% Misc
%% ===================================================================

-spec ensure_started([Application :: atom()]) -> ok.
ensure_started([]) -> ok;
ensure_started([App | Apps]) ->
  case application:start(App) of
    ok ->
      ensure_started(Apps);
    {error, {already_started, App}} ->
      ensure_started(Apps)
  end.

-spec stop_apps([Application :: atom()]) -> ok.
stop_apps([]) -> ok;
stop_apps([App | Apps]) ->
  application:stop(App),
  stop_apps(Apps).


