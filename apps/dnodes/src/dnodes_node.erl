-module(dnodes_node).
-author("falvik").

-behaviour(gen_server).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

%% API
-export([
  start_link/1,
  get_links/1,
  kill_node/1,
  connect_nodes/2,
  connect_from/2,
  disconnect_from/2
]).

-include("include.hrl").
-type(state() :: #{id => node_id(), connect_list => [node_id()]}).

%% ===================================================================
%% Gen_server callbacks
%% ===================================================================

-spec init([node_id()]) -> {ok, #{id => node_id(), connect_list => []}}.
init([Node_id]) ->
  {ok, #{id => Node_id, connect_list => []}}.

%-spec handle_call
%    ({connect_from, node_id()}, _, state()) -> {reply, ok, state()};
%    (get_links, _, state())                 -> {reply, Links :: [node_id()], state()};
%    (_, _, state())                         -> {reply, ok, state()}.
-spec handle_call(_,_,state()) -> {reply,_ , state()}.
handle_call({connect_from, Id}, _From, State) ->
  Connect_list0 = maps:get(connect_list, State),
  Connect_list = [Id|Connect_list0],
  State2 = maps:put(connect_list, Connect_list, State),
  {reply, ok, State2};
handle_call(get_links, _From, State) ->
  Connections = maps:get(connect_list, State),
  {reply, Connections, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec handle_cast({connect_nodes | disconnect_from, node_id()}, state()) ->
  {noreply, state()}.
%-spec handle_cast(_, state()) ->
%  {noreply, state()}.
handle_cast({connect_nodes, Id}, State) ->
  case maps:get(id, State) of
    Id ->
      {noreply, State};
    _ ->
      case dnodes_cluster:connect_to(Id) of
        ok ->
          Connect_list0 = maps:get(connect_list, State),
          Connect_list = [Id|Connect_list0],
          State2 = maps:put(connect_list, Connect_list, State),
          {noreply, State2};
        _ ->
          {noreply, State}
      end
  end;
handle_cast({disconnect_from, Id}, State) ->
  Connect_list0 = maps:get(connect_list, State),
  Connect_list = lists:filter(
    fun(Id2)->
      Id2 =/= Id
    end,
    Connect_list0
  ),
  State2 = maps:put(connect_list, Connect_list, State),
  {noreply, State2};
handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(_, state()) -> {noreply, state()}.
handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(_, _) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(_, state(), _) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% ===================================================================
%% API
%% ===================================================================

-spec start_link(node_id()) -> {ok,pid()} | ignore | {error, Error :: term()}.
start_link(Node_id) ->
  gen_server:start_link(?MODULE, [Node_id], []).

-spec get_links(pid()) -> [node_id()].
get_links(Node_pid)->
  gen_server:call(Node_pid, get_links).

-spec kill_node(pid()) -> ok.
kill_node(Pid)->
  gen_server:stop(Pid, normal, 100).

-spec connect_nodes(pid(), node_id()) -> ok.
connect_nodes(Target_pid, Node_id)->
  gen_server:cast(Target_pid,{connect_nodes, Node_id}).

-spec connect_from(pid(), node_id()) -> ok.
connect_from(To_pid, From_id)->
gen_server:call(To_pid, {connect_from, From_id}).

-spec disconnect_from(pid(), node_id()) -> ok.
disconnect_from(Pid, Id)->
  gen_server:cast(Pid,{disconnect_from, Id}).