-module(hls_loader).
-behaviour(gen_server).

-include("log.hrl").

-record(state, {
	owner :: pid(),
	queue = queue:new() :: queue:queue(),
	playlist :: binary(),
	load = false :: true | false
	}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/1, start_link/1, load_segment/3, load_playlist/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Pid) ->
    gen_server:start_link(?MODULE, [Pid], []).

start(Pid) ->
	hls_bench_sup:start_loader(Pid).

load_segment(Loader, Url, Duration) ->
	gen_server:cast(Loader, {segment, Url, Duration}).

load_playlist(Loader, Url) ->
	gen_server:cast(Loader, {playlist, Url}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Pid]) ->
	link(Pid),
    {ok, #state{owner = Pid}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({segment, Segment, Duration}, #state{queue = Queue, load = false} = State) ->
	NewQueue = queue:in({Segment, Duration}, Queue),
	{Url, _} = queue:get(NewQueue),
	Self = self(),
	spawn_link(fun() -> loader(segment, Url, Self) end),
    {noreply, State#state{queue = NewQueue, load = true}};

handle_cast({segment, Segment, Duration}, #state{queue = Queue, load = true} = State) ->
	NewQueue = queue:in({Segment, Duration}, Queue),
    {noreply, State#state{queue = NewQueue}};

handle_cast({playlist, Playlist}, #state{playlist = undefined, load = false} = State) ->
	Self = self(),
	spawn_link(fun() -> loader(playlist, Playlist, Self) end),
    {noreply, State#state{load = true}};

handle_cast({playlist, Playlist}, #state{playlist = undfined, load = true} = State) ->
    {noreply, State#state{playlist = Playlist}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({segment, _Body}, #state{owner = Owner, playlist = undefined, queue = Queue} = State) ->
	{Url, Duration} = queue:get(Queue),
	?D({downloaded, Url}),
	Owner ! {segment, Duration},
	NewQueue = queue:drop(Queue),
	case queue:is_empty(NewQueue) of
		true ->
			{noreply, State#state{queue = NewQueue, load = false}};
		false ->
			{NextUrl, _} = queue:get(NewQueue),
			Self = self(),
			spawn_link(fun() -> loader(segment, NextUrl, Self) end),
   			{noreply, State#state{queue = NewQueue}}
   	end;

handle_info({segment, _Body}, #state{owner = Owner, playlist = Playlist, queue = Queue} = State) ->
	{Url, Duration} = queue:get(Queue),
	?D({downloaded, Url}),
	Owner ! {segment, Duration},
	Self = self(),
	spawn_link(fun() -> loader(playlist, Playlist, Self) end),
	{noreply, State#state{playlist = undefined, queue = queue:drop(Queue)}};

handle_info({playlist, Playlist}, #state{owner = Owner, playlist = undefined, queue = Queue} = State) ->
	Owner ! {playlist, Playlist},
	case queue:is_empty(Queue) of
		true ->
			{noreply, State#state{queue = Queue, load = false}};
		false ->
			{Url, _} = queue:get(Queue),
			Self = self(),
			spawn_link(fun() -> loader(segment, Url, Self) end),
   			{noreply, State}
   	end;

handle_info({playlist, Playlist}, #state{owner = Owner, playlist = PlaylistUrl} = State) ->
	Owner ! {playlist, Playlist},
	Self = self(),
	spawn_link(fun() -> loader(playlist, PlaylistUrl, Self) end),
	{noreply, State#state{playlist = undefined}};

handle_info({error, Reason}, #state{owner = Owner} = State) ->
	Owner ! {error, Reason},
	{stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

loader(Type, Url, Pid) ->
	case hackney:get(Url, [], <<>>, []) of
		{ok, 200, _, ClientRef} ->
			case hackney:body(ClientRef) of
                {ok, Body} ->
                	Pid ! {Type, Body};
                {error, _Reason} ->
                  Pid ! {error, hackney_error}
              end;
		{ok, StatusCode, _, _} ->
			Pid ! {error, {http_error, StatusCode}};
		{error, Reason} ->
			Pid ! {error, Reason}
	end.

