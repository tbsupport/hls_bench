-module(hls_bench_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("log.hrl").

-record(state, {
	log :: file:fd(),
	feedback :: pid(),
	count :: non_neg_integer()
	}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, start/3, aggregate/2]).

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

start(Url, Workers, Segments) ->
	gen_server:cast(?SERVER, {start, Url, Workers, Segments, self()}).

aggregate(Delay, Segment) ->
	gen_server:cast(?SERVER, {aggregate, Delay, Segment}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({start, Url, Workers, Segments, Pid}, State) ->
	erlang:monitor(process, Pid),
	Filename = io_lib:format("~s~p", ["data/", Workers]),
	filelib:ensure_dir(Filename),
	{ok, Log} = file:open(Filename, [write]),
	start_workers(Url, Segments, Workers),
    {noreply, State#state{log = Log, feedback = Pid, count = Workers}};

handle_cast({aggregate, Delay, Segment}, #state{log = Log} = State) ->
	io:fwrite(Log, "~p	~p~n", [Delay, Segment]),
    {noreply, State};

handle_cast({_Msg}, State) ->
    {noreply, State}.

handle_info({'DOWN', _, process, Pid, _Reason}, #state{feedback = Pid} = State) ->
    {stop, normal, State};

handle_info({'DOWN', _, process, _Worker, _Reason}, #state{log = Log, count = 1, feedback = Pid} = State) ->
	file:close(Log),
	Pid ! down,
    {noreply, State};

handle_info({'DOWN', _, process, _Worker, _Reason}, #state{count = Count} = State) ->
    {noreply, State#state{count = Count - 1}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

start_workers(_Url, _Segments, 0) ->
	ok;

start_workers(Url, Segments, N) ->
	{ok, Worker} = hls_client:start(N, Url, Segments), 
	erlang:monitor(process, Worker),
	start_workers(Url, Segments, N - 1).

