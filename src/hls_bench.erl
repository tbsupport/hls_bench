-module(hls_bench).

-export([start/0, stop/0]).

start() ->
	ulitos_app:ensure_started(hls_bench).

stop() ->
	application:stop(hls_bench).