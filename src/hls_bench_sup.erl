-module(hls_bench_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_client/3, stop_client/1, start_loader/1, stop_loader/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_client(Id, Url, Count) ->
	supervisor:start_child(hls_client_sup, [Id, Url, Count]).

stop_client(Pid) ->
	supervisor:terminate_child(hls_client_sup, Pid).

start_loader(Pid) ->
  supervisor:start_child(hls_loader_sup, [Pid]).

stop_loader(Pid) ->
  supervisor:terminate_child(hls_loader_sup, Pid).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([client]) ->
  {ok, {{simple_one_for_one, 5, 10}, [
    {undefined, {hls_client, start_link, []},
      temporary, 2000, worker, [hls_client]}
  ]}};

init([loader]) ->
  {ok, {{simple_one_for_one, 5, 10}, [
    {undefined, {hls_loader, start_link, []},
      temporary, 2000, worker, [hls_loader]}
  ]}};

init([]) ->
  Children = [
    {hls_client_sup,
      {supervisor, start_link, [{local, hls_client_sup}, ?MODULE, [client]]},
      permanent,
      infinity,
      supervisor,
      []
    },
    {hls_loader_sup,
      {supervisor, start_link, [{local, hls_loader_sup}, ?MODULE, [loader]]},
      permanent,
      infinity,
      supervisor,
      []
    },
    ?CHILD(hls_bench_server, worker)],
  {ok, {{one_for_one, 5, 10}, Children}}.

