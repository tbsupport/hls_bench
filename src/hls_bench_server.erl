-module(hls_bench_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("log.hrl").

-define(Segments, 20).

-record(state, {
	feedback :: pid()
	}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, start/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start(Url, Count) ->
	gen_server:cast(?SERVER, {start, Url, Count, self()}).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({start, Url, Count, Pid}, State) ->
	erlang:monitor(process, Pid),
	start_workers(Url, Count),
    {noreply, State#state{feedback = Pid}};

handle_cast({_Msg}, State) ->
    {noreply, State}.

handle_info({'DOWN', _, process, Pid, _Reason}, #state{feedback = Pid} = State) ->
    {stop, normal, State};

handle_info({'DOWN', _, process, _Worker, _Reason}, #state{feedback = Pid} = State) ->
	Pid ! down,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

start_workers(_Url, 0) ->
	ok;

start_workers(Url, N) ->
	{ok, Worker} = hls_client:start(N, Url, ?Segments), 
	erlang:monitor(process, Worker),
	start_workers(Url, N - 1).

