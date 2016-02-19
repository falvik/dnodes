-module(dnodes_ws_handler).
-author("falvik").

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

-type(task_id() :: integer()).

-spec init(Req, _) -> {cowboy_websocket, Req, #{mon => reference()}}.
init(Req, _Opts) ->
  ok = gen_server:call(dnodes_cluster, register_listener),
  Mon = erlang:monitor(process, dnodes_cluster),
  Opts2 = #{mon => Mon},
  {cowboy_websocket, Req, Opts2}.

-spec websocket_handle(_,_,_) -> {'ok',_,_} | {'reply',{'text',_},_,_}.
websocket_handle({text, Json}, Req, State) ->
  {struct, Tags_list} = mochijson2:decode(Json),
  {<<"command">>, Command} = lists:keyfind(<<"command">>, 1, Tags_list),
  {<<"id">>, Id} = lists:keyfind(<<"id">>, 1, Tags_list),
  execJson(Command, Id, Req, State,  Tags_list);
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

-spec websocket_info(_,_,_) -> {ok,_,_} | {stop, _, _}.
websocket_info({'DOWN', _Ref, process, _Pid, _Reason}, Req, State) ->
  {stop, Req, State};
websocket_info(update, Req, State) ->
  Pre_json = {struct, []},
  Json = mochijson2:encode(Pre_json),
  {reply, {text, Json}, Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

-spec execJson(binary(), Id :: task_id(), Req :: term(), State :: term(), Tags_list :: term()) ->
  {ok, Req, State} |  {reply, {text, Json :: term()}, Req, State}.
execJson(<<"deleteLink">>, _Id, Req, State, Tags_list)->
  {<<"nodeId1">>, Bin_node_id1} = lists:keyfind(<<"nodeId1">>, 1, Tags_list),
  {<<"nodeId2">>, Bin_node_id2} = lists:keyfind(<<"nodeId2">>, 1, Tags_list),
  Node_id1 = erlang:binary_to_integer(Bin_node_id1),
  Node_id2 = erlang:binary_to_integer(Bin_node_id2),
  dnodes:disconnect(Node_id1, Node_id2),
  {ok, Req, State};
execJson(<<"addLink">>, _Id, Req, State, Tags_list)->
  {<<"nodeId1">>, Bin_node_id1} = lists:keyfind(<<"nodeId1">>, 1, Tags_list),
  {<<"nodeId2">>, Bin_node_id2} = lists:keyfind(<<"nodeId2">>, 1, Tags_list),
  Node_id1 = erlang:binary_to_integer(Bin_node_id1),
  Node_id2 = erlang:binary_to_integer(Bin_node_id2),
  dnodes:connect_nodes(Node_id1, Node_id2),
  {ok, Req, State};
execJson(<<"deleteNode">>, _Id, Req, State, Tags_list)->
  {<<"nodeId">>, Bin_node_id} = lists:keyfind(<<"nodeId">>, 1, Tags_list),
  Node_id = erlang:binary_to_integer(Bin_node_id),
  dnodes:kill_node(Node_id),
  {ok, Req, State};
execJson(<<"newNode">>, _Id, Req, State, _Tags_list)->
  dnodes:add_node(),
  {ok, Req, State};
execJson(<<"nodes_info">>, Id, Req, State, _Tags_list)->
  Raw_info = dnodes:nodes_info(),
  Fun =
    fun(Link)->
      integer_to_binary(Link)
    end,
  Fun2 =
    fun({Node_id, Links})->
      Links2 = lists:map(
        Fun,
        Links
      ),
      {struct, [{<<"node_id">>,integer_to_binary(Node_id)},{<<"links">>, Links2}]}
    end,
  Info_list = lists:map(
    Fun2,
    Raw_info
  ),
  Pre_json = {struct, [{<<"data">>, Info_list}, {<<"id">>, Id}]},
  Json = mochijson2:encode(Pre_json),
  {reply, {text, Json}, Req, State}.


