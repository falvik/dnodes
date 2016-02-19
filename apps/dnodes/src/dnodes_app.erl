-module(dnodes_app).
-behaviour(application).
-author("falvik").

%% application callbacks
-export([start/2, stop/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(_, _) ->
    {ok, pid()} | {ok, pid(), State :: term()} | {error, Reason ::term()}.
start(_StartType, _StartArgs) ->
    dnodes_cluster_sup:start_link().

-spec stop(_) -> ok.
stop(_State) ->
    ok.
