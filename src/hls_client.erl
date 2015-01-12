-module(hls_client).
-behaviour(gen_server).

-include("log.hrl").

-record(state, {
	id :: non_neg_integer(),
	host :: binary(),
	url :: binary(),
	count :: non_neg_integer(),
	loader :: pid(),
	duration :: non_neg_integer(),
	queue = queue:new() :: queue:queue(),
	start = 0 :: non_neg_integer(),
	finish = 0 :: non_neg_integer(),
	seq = 0 :: non_neg_integer(),
	play = start ::  start | wait | play | idle
	}).

-record(playlist, {
	version :: non_neg_integer(),
	target_duration :: non_neg_integer(),
	seq :: non_neg_integer(),
	segments = [] :: [],
	finish = false :: true | false
	}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/3, start_link/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Id, Url, Count) ->
    gen_server:start_link(?MODULE, [Id, Url, Count], []).

start(Id, Url, Count) ->
	hls_bench_sup:start_client(Id, Url, Count).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Id, Url, Count]) ->
	{ok, Loader} = hls_loader:start(self()),
	self() ! playlist,
    {ok, #state{id = Id, host = host(Url), url = iolist_to_binary(Url), count = Count, loader = Loader}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(playlist, #state{loader = Loader, url = Url} = State) ->
	hls_loader:load_playlist(Loader, Url),
	{noreply, State};

handle_info({playlist, Playlist}, #state{host = Host, loader = Loader, start = Start, finish = Finish, seq = CurrentSeq} = State) ->
	case parse_playlist(Playlist) of
		#playlist{finish = true} ->
			?D("End of stream"),
			{stop, normal, State};
		#playlist{target_duration = TargetDuration, segments = Segments, seq = Seq} ->
			{NewFinish, NewCurrentSeq} = update_segments(Loader, Host, CurrentSeq, Finish, Segments, Seq),
			if 
				Finish - Start < 3 * TargetDuration ->
					erlang:send_after(TargetDuration bsr 1, self(), playlist);
				true ->
					erlang:send_after(TargetDuration, self(), playlist)
			end,
			NewState = State#state{duration = TargetDuration, finish = NewFinish, seq = NewCurrentSeq},
			{noreply, play(NewState)};
		_Else ->
			?D(_Else),
			{stop, normal, State}
	end;

handle_info({segment, Duration}, #state{queue = Queue} = State) ->
	NewState = State#state{queue = queue:in(Duration, Queue)},
	{noreply, play(NewState)};

handle_info(play, #state{id = Id, count = 0} = State) ->
	?D({Id, finish}),
	{stop, normal, State};

handle_info(play, #state{count = Count} = State) ->
	{noreply, play(State#state{count = Count - 1, play = wait})};

handle_info({error, Reason}, #state{id = Id} = State)	->
	?D({Id, error, Reason}),
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

play(#state{id = Id, queue = Segments, finish = Finish, start = Start, play = start, duration = Duration} = State) when Finish - Start >= 3 * Duration ->
	case queue:out(Segments) of
		{empty, _} ->
			State;
		{{value, SegDuration}, NewSegments} ->
			?D({Id, start}),
			erlang:send_after(SegDuration, self(), play),
			State#state{start = Start + SegDuration, play = play, queue = NewSegments}
   	end;

play(#state{queue = Segments, finish = Finish, start = Start, play = wait, duration = Duration} = State) when Finish - Start >= 3 * Duration ->
	case queue:out(Segments) of
		{empty, _} ->
			State#state{play = idle};
		{{value, SegDuration}, NewSegments} ->
			erlang:send_after(SegDuration, self(), play),
			State#state{start = Start + SegDuration, play = play, queue = NewSegments}
   	end;

play(#state{id = Id, queue = Segments, finish = Finish, start = Start, play = idle, duration = Duration} = State) when Finish - Start >= 3 * Duration ->
	case queue:out(Segments) of
		{empty, _} ->
			State#state{play = idle};
		{{value, SegDuration}, NewSegments} ->
			?D({Id, idle, Start}),
			erlang:send_after(SegDuration, self(), play),
			State#state{start = Start + SegDuration, play = play, queue = NewSegments}
   	end;

play(#state{play = wait} = State) ->
	State#state{play = idle};

play(#state{} = State) ->
	State.

host(URL) ->
	get_host(lists:reverse(URL)).

parse_playlist(BinPlaylist) ->
	Tags = lists:filter(fun(<<"">>) -> false; (_) -> true end, binary:split(BinPlaylist, [<<"#">>, <<"\n">>, <<",">>], [global])),
	Playlist = #playlist{segments = Segments} = parse_playlist(Tags, #playlist{}),
	Playlist#playlist{segments = lists:reverse(Segments)}.

parse_playlist([], Playlist) ->
	Playlist;

parse_playlist([<<"EXTM3U">> | Tags], Playlist) ->
	parse_playlist(Tags, Playlist);

parse_playlist([<<"EXT-X-TARGETDURATION:", Number/binary>> | Tags], Playlist) ->
	parse_playlist(Tags, Playlist#playlist{target_duration = 1000 * binary_to_integer(Number)});

parse_playlist([<<"EXT-X-VERSION:", Number/binary>> | Tags], Playlist) -> 
	parse_playlist(Tags, Playlist#playlist{version = binary_to_integer(Number)});	

parse_playlist([<<"EXT-X-MEDIA-SEQUENCE:", Number/binary>> | Tags], Playlist) -> 
	parse_playlist(Tags, Playlist#playlist{seq = binary_to_integer(Number)});	
 
parse_playlist([<<"EXTINF:", Number/binary>>, Segment | Tags], #playlist{segments = Segments} = Playlist) ->
	parse_playlist(Tags, Playlist#playlist{segments = [{Segment, round(1000 * binary_to_float(Number))} | Segments]});

parse_playlist([<<"EXT-X-ENDLIST">>], Playlist) ->
	Playlist#playlist{finish = true}.

get_host([$/ | Other]) ->
	Host = iolist_to_binary(lists:reverse(Other)),
	<<Host/binary, "/">>;

get_host([_ | Other]) ->
	get_host(Other).

update_segments(_Loader, _Host, CurrentSeq, Finish, [], _Seq) ->
	{Finish, CurrentSeq};

update_segments(Loader, Host, CurrentSeq, Finish, [{Segment, Duration} | Segments], Seq) when Seq > CurrentSeq ->
	hls_loader:load_segment(Loader, <<Host/binary, Segment/binary>>, Duration),
	update_segments(Loader, Host, Seq, Finish + Duration, Segments, Seq + 1);

update_segments(Loader, Host, CurrentSeq, Finish, [{_Segemnt, _Duration} | Segments], Seq) ->
	update_segments(Loader, Host, CurrentSeq, Finish, Segments, Seq + 1).
	