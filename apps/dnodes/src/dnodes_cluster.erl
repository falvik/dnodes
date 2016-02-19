-module(dnodes_cluster).
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
  start_link/0,
  nodes_info/0,
  add_node/0,
  kill_node/1,
  connect_nodes/2,
  connect_to/1,
  disconnect/2
]).

-define(SERVER, ?MODULE).
-include("include.hrl").
-record(state,{
  nodes = #{} :: nodes_map(),
  listeners = #{} :: #{pid() => reference()}
}).


%% ===================================================================
%% Gen_server callbacks
%% ===================================================================
-spec init([])->{ok, #state{}}.
init([]) ->
  Nodes_list = dnodes_nodes_sup:get_nodes(),
  To_map_list = lists:map(
    fun({Id, Node_pid, _,_})->
      Ref = erlang:monitor(process, Node_pid),
      Connections = dnodes_node:get_links(Node_pid),
      {Id, #node_info{pid= Node_pid, monitor=Ref, links=Connections}}
    end,
    Nodes_list),
    Nodes = maps:from_list(To_map_list),
    State = #state{nodes=Nodes},
  {ok, State}.

%-spec handle_call
%    (add_new_node, _, #state{})                           -> {reply, node_id() | Error, #state{}} when Error :: term();
%    (get_nodes_info, _, #state{})                         -> {reply, [{node_id(), Links :: [node_id()]}], #state{}};
%    ({kill_node, node_id()}, _, #state{})                 -> {reply, ok | not_found, #state{}};
%    ({connect_to, node_id()}, {pid(),_}, #state{})        -> {reply, ok | not_found, #state{}};
%    ({connect_nodes, node_id(), node_id()}, _, #state{})  -> {reply, ok | not_found, #state{}};
%    ({disconnect, node_id(), node_id()}, _, #state{})     -> {reply, ok | not_found, #state{}};
%    (register_listener, {pid(), _}, #state{})             -> {reply, ok, #state{}};
%    (_, _, #state{})                                      -> {reply, ok, #state{}}.
-spec handle_call(_, _, #state{}) -> {reply, ok, #state{}}.
handle_call(add_new_node, _From, State)->
  case dnodes_nodes_sup:start_node() of
    {ok,  {Id, Node_pid}} ->
      Ref=erlang:monitor(process,  Node_pid),
      Nodes0 = State#state.nodes,
      Nodes1 = maps:put(Id, #node_info{pid=Node_pid, monitor=Ref},Nodes0),
      State2 = State#state{nodes = Nodes1},
      Listeners = State#state.listeners,
      maps:map(
        fun(Pid, _Mon)->
          Pid ! update
        end,
        Listeners
      ),
      {reply,  Id, State2}
  end;
handle_call(get_nodes_info, _From, State)->
  Nodes = State#state.nodes,
  List = maps:map(
    fun(Id, Node_info)->
      {Id, Node_info#node_info.links}
    end,
    Nodes
  ),
  {reply,  maps:values(List), State};
handle_call({kill_node, Node_id}, _From, State)->
  Nodes = State#state.nodes,
  case maps:is_key(Node_id, Nodes) of
      true ->
        Node_info = maps:get(Node_id, Nodes),
        dnodes_node:kill_node(Node_info#node_info.pid),
        {reply,  ok, State};
      false ->
        {reply,  not_found, State}
  end;
handle_call({connect_to, Node_id}, {From_pid,_}, State)->
  Nodes = State#state.nodes,
  case maps:is_key(Node_id, Nodes) of
    true ->
      case get_id_by_pid(From_pid, Nodes) of
        not_found ->
          {reply, not_found, State};
        From_id ->
          Node_info = maps:get(Node_id, Nodes),
          Node_pid = Node_info#node_info.pid,
          {ok, Nodes1} = connect_from_to({From_pid, From_id}, {Node_pid, Node_id}, Nodes),
          State2 = State#state{nodes = Nodes1},
          Listeners = State#state.listeners,
          maps:map(
            fun(Pid, _Mon)->
              Pid ! update
            end,
            Listeners
          ),
          {reply, ok, State2}
      end;
    false ->
      {reply, not_found, State}
  end;
handle_call({connect_nodes, Target_id, Id}, _From, State) ->
  Nodes = State#state.nodes,
  case maps:is_key(Target_id, Nodes) and maps:is_key(Id, Nodes) of
    true ->
      Target_node_info = maps:get(Target_id, Nodes),
      Target_pid = Target_node_info#node_info.pid,
      dnodes_node:connect_nodes(Target_pid, Id),
      {reply, ok, State};
    false ->
      {reply, not_found, State}
  end;
handle_call({disconnect, Id1, Id2}, _From, State) ->
  Nodes = State#state.nodes,
  case maps:is_key(Id1, Nodes) and maps:is_key(Id2, Nodes) of
    true ->
      Node_info1 = maps:get(Id1, Nodes),
      Pid1 = Node_info1#node_info.pid,
      Links1 = Node_info1#node_info.links,
      Links11 = delete_all(Id2, Links1),
      Nodes2 = maps:put(Id1,Node_info1#node_info{links = Links11},Nodes),

      Node_info2 = maps:get(Id2, Nodes),
      Pid2 = Node_info2#node_info.pid,
      Links2 = Node_info2#node_info.links,
      Links21 = delete_all(Id1, Links2),
      Nodes3 = maps:put(Id2,Node_info2#node_info{links = Links21},Nodes2),

      dnodes_node:disconnect_from(Pid1, Id2),
      dnodes_node:disconnect_from(Pid2, Id1),

      State2 = State#state{nodes = Nodes3},
      Listeners = State#state.listeners,
      maps:map(
        fun(Pid, _Mon)->
          Pid ! update
        end,
        Listeners
      ),
      {reply, ok, State2};
    false ->
      {reply, not_found, State}
  end;
handle_call(register_listener, {Pid, _Tag}, State) ->
  Listeners = State#state.listeners,
  Mon = erlang:monitor(process, Pid),
  Listeners2 = maps:put(Pid,Mon, Listeners),
  State2 = State#state{listeners = Listeners2},
  {reply, ok, State2};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec handle_cast(_, #state{})->
  {noreply, #state{}}.
handle_cast(_Request, State) ->
  {noreply, State}.

%-spec handle_info({'DOWN', _, process, pid(), _}, #state{})->
%  {noreply, #state{}}.
%-spec handle_info(_, #state{})->
%  {noreply, #state{}}.
-spec handle_info({'DOWN', _, process, pid(), _}, #state{})->{noreply, #state{}}.
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
  Listeners = State#state.listeners,
  case maps:is_key(Pid, Listeners) of
    true ->
      Listeners2 = maps:remove(Pid, Listeners),
      State2 = State#state{listeners = Listeners2},
      {noreply, State2};
    false ->
      Nodes = State#state.nodes,
      Nodes2 = maps:filter(
        fun(_Id, Node_info)->
          Node_info#node_info.pid =/= Pid
        end,
        Nodes
      ),
      Down_id = get_id_by_pid(Pid, Nodes),
      Nodes3 = maps:map(
        fun(_Id, Node_info)->
          Links = Node_info#node_info.links,
          case lists:member(Down_id, Links) of
            true ->
              Pid_from = Node_info#node_info.pid,
              dnodes_node:disconnect_from(Pid_from, Down_id),
              Links2 = delete_all(Down_id, Links),
              Node_info#node_info{links = Links2};
            false ->
              Node_info
          end
        end,
        Nodes2
      ),
      Listeners = State#state.listeners,
      maps:map(
        fun(L_pid, _Mon)->
          L_pid ! update
        end,
        Listeners
      ),
      State2 = State#state{nodes = Nodes3},

      {noreply, State2}
  end;
handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(_, _) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(_, #state{}, _) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok,pid()} | ignore | {error, Error :: term()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec nodes_info() -> [{node_id(), Links :: [node_id()]}].
nodes_info() ->
  gen_server:call(?MODULE, get_nodes_info).

-spec add_node() -> node_id().
add_node()->
  gen_server:call(?MODULE, add_new_node).

-spec kill_node(node_id()) -> ok | not_found.
kill_node(Id)->
  gen_server:call(?MODULE, {kill_node, Id}).

-spec connect_nodes(node_id(), node_id()) -> ok | not_found.
connect_nodes(Target_id, Id) ->
  gen_server:call(?MODULE, {connect_nodes, Target_id, Id}).

% TODO remove implicit self() using
-spec connect_to(node_id()) -> ok | not_found.
connect_to(Id)->
  gen_server:call(?MODULE, {connect_to, Id}).

-spec disconnect(node_id(), node_id()) -> ok | not_found.
disconnect(Id1, Id2)->
  gen_server:call(?MODULE, {disconnect, Id1, Id2}).

%% ===================================================================
%% Misc
%% ===================================================================
%TODO check if unsuccessful
-spec connect_from_to({pid(), node_id()}, {pid(), node_id()}, nodes_map()) ->
  {ok, nodes_map()}.
connect_from_to({_From_pid, From_id}, {To_pid, To_id}, Nodes)->
  case dnodes_node:connect_from(To_pid, From_id) of
    ok ->
      From_node_info = maps:get(From_id, Nodes),
      From_links = From_node_info#node_info.links,
      From_node_info2 = From_node_info#node_info{links = [To_id|From_links]},
      Nodes2 = maps:put(From_id, From_node_info2, Nodes),

      To_node_info = maps:get(To_id, Nodes),
      To_links = To_node_info#node_info.links,
      To_node_info2 = To_node_info#node_info{links = [From_id|To_links]},
      Nodes3 = maps:put(To_id, To_node_info2, Nodes2),

      {ok, Nodes3}
  end.

-spec get_id_by_pid(pid(), nodes_map()) -> node_id() | not_found | no_return().
get_id_by_pid(Node_pid, Nodes)->
  Found = maps:filter(
    fun(_Id, Node_info)->
      Node_info#node_info.pid =:= Node_pid
    end,
    Nodes
  ),
  case maps:size(Found) of
    0 ->
      not_found;
    1 ->
      [Id] = maps:keys(Found),
      Id;
    _ ->
      exit(dublicate_nodes)
  end.

-spec delete_all(term(), list()) -> list().
delete_all(Elem, List)->
  lists:filter(
    fun(Elem1) ->
      Elem  =/= Elem1
    end,
    List
  ).