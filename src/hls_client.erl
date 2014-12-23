-module(hls_client).
-behaviour(gen_server).

-record(state, {
	host :: binary(),
	url :: binary(),
	count :: non_neg_integer()}).

-record(parser, {
	version :: non_neg_integer(),
	target_duration :: non_neg_integer(),
	sequence :: non_neg_integer(),
	streams :: []
	}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, host/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({URL, Count}) ->
    {ok, #state{host = host(URL), url = URL, count = Count}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_playlist(URL) ->
	case hackney:get(URL, [], <<>>, []) of
		{ok, 200, _, ClientRef} ->
			case hackney:body(ClientRef) of
                {ok, Body} ->
                  parse_playlist(Body),
                  ok;
                {error, _Reason} ->
                  {error, hackney_error}
              end;
		{ok, StatusCode, _, _} ->
			{error, StatusCode};
		{error, _} ->
			error
	end.

host(URL) ->
	get_host(lists:reverse(URL)).

parse_playlist(Playlist) ->
	Tags = lists:filter(fun(<<"">>) -> false; (_) -> true end, binary:split(Playlist, [<<"#">>, <<"\n">>])),
	parse_playlist(Tags, #parser{}).

parse_playlist([], Parser) ->
	Parser;

parse_playlist([<<"EXTM3U\n">> | Tags], Parser) ->
	parse_playlist(Tags, Parser);

parse_playlist([<<"EXT-X-TARGETDURATION:", Number/binary>> | Tags], Parser) ->
	parse_playlist(Tags, Parser#parser{target_duration = binary_to_integer(Number)});

parse_playlist([<<"EXT-X-VERSION:", Number/binary>> | Tags], Parser) -> 
	parse_playlist(Tags, Parser#parser{version = binary_to_integer(Number)});	

parse_playlist([<<"EXT-X-MEDIA-SEQUENCE:", Number/binary>> | Tags], Parser) -> 
	parse_playlist(Tags, Parser#parser{sequence = binary_to_integer(Number)});	
 
parse_playlist([<<"EXTINF:", Number/binary>>, Stream | Tags], #parser{streams = Streams} = Parser) ->
	parse_playlist(Tags, Parser#parser{streams = [{Stream, 1000 * binary_to_float(Number)} | Streams]}).

get_host([$/ | Other]) ->
	iolist_to_binary(lists:reverse(Other));

get_host([_ | Other]) ->
	get_host(Other).

	