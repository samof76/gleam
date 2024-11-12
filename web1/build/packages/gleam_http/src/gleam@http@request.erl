-module(gleam@http@request).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([to_uri/1, from_uri/1, get_header/2, set_header/3, prepend_header/3, set_body/2, map/2, path_segments/1, get_query/1, set_query/2, set_method/2, new/0, to/1, set_scheme/2, set_host/2, set_port/2, set_path/2, set_cookie/3, get_cookies/1, remove_cookie/2]).
-export_type([request/1]).

-type request(GGB) :: {request,
        gleam@http:method(),
        list({binary(), binary()}),
        GGB,
        gleam@http:scheme(),
        binary(),
        gleam@option:option(integer()),
        binary(),
        gleam@option:option(binary())}.

-file("/Users/louis/src/gleam/http/src/gleam/http/request.gleam", 31).
-spec to_uri(request(any())) -> gleam@uri:uri().
to_uri(Request) ->
    {uri,
        {some, gleam@http:scheme_to_string(erlang:element(5, Request))},
        none,
        {some, erlang:element(6, Request)},
        erlang:element(7, Request),
        erlang:element(8, Request),
        erlang:element(9, Request),
        none}.

-file("/Users/louis/src/gleam/http/src/gleam/http/request.gleam", 45).
-spec from_uri(gleam@uri:uri()) -> {ok, request(binary())} | {error, nil}.
from_uri(Uri) ->
    gleam@result:then(
        begin
            _pipe = erlang:element(2, Uri),
            _pipe@1 = gleam@option:unwrap(_pipe, <<""/utf8>>),
            gleam@http:scheme_from_string(_pipe@1)
        end,
        fun(Scheme) ->
            gleam@result:then(
                begin
                    _pipe@2 = erlang:element(4, Uri),
                    gleam@option:to_result(_pipe@2, nil)
                end,
                fun(Host) ->
                    Req = {request,
                        get,
                        [],
                        <<""/utf8>>,
                        Scheme,
                        Host,
                        erlang:element(5, Uri),
                        erlang:element(6, Uri),
                        erlang:element(7, Uri)},
                    {ok, Req}
                end
            )
        end
    ).

-file("/Users/louis/src/gleam/http/src/gleam/http/request.gleam", 76).
-spec get_header(request(any()), binary()) -> {ok, binary()} | {error, nil}.
get_header(Request, Key) ->
    gleam@list:key_find(erlang:element(3, Request), gleam@string:lowercase(Key)).

-file("/Users/louis/src/gleam/http/src/gleam/http/request.gleam", 87).
-spec set_header(request(GGL), binary(), binary()) -> request(GGL).
set_header(Request, Key, Value) ->
    Headers = gleam@list:key_set(
        erlang:element(3, Request),
        gleam@string:lowercase(Key),
        Value
    ),
    erlang:setelement(3, Request, Headers).

-file("/Users/louis/src/gleam/http/src/gleam/http/request.gleam", 104).
-spec prepend_header(request(GGO), binary(), binary()) -> request(GGO).
prepend_header(Request, Key, Value) ->
    Headers = [{gleam@string:lowercase(Key), Value} |
        erlang:element(3, Request)],
    erlang:setelement(3, Request, Headers).

-file("/Users/louis/src/gleam/http/src/gleam/http/request.gleam", 116).
-spec set_body(request(any()), GGT) -> request(GGT).
set_body(Req, Body) ->
    {request, Method, Headers, _, Scheme, Host, Port, Path, Query} = Req,
    {request, Method, Headers, Body, Scheme, Host, Port, Path, Query}.

-file("/Users/louis/src/gleam/http/src/gleam/http/request.gleam", 141).
-spec map(request(GGV), fun((GGV) -> GGX)) -> request(GGX).
map(Request, Transform) ->
    _pipe = erlang:element(4, Request),
    _pipe@1 = Transform(_pipe),
    set_body(Request, _pipe@1).

-file("/Users/louis/src/gleam/http/src/gleam/http/request.gleam", 161).
-spec path_segments(request(any())) -> list(binary()).
path_segments(Request) ->
    _pipe = erlang:element(8, Request),
    gleam@uri:path_segments(_pipe).

-file("/Users/louis/src/gleam/http/src/gleam/http/request.gleam", 167).
-spec get_query(request(any())) -> {ok, list({binary(), binary()})} |
    {error, nil}.
get_query(Request) ->
    case erlang:element(9, Request) of
        {some, Query_string} ->
            gleam@uri:parse_query(Query_string);

        none ->
            {ok, []}
    end.

-file("/Users/louis/src/gleam/http/src/gleam/http/request.gleam", 177).
-spec set_query(request(GHH), list({binary(), binary()})) -> request(GHH).
set_query(Req, Query) ->
    Pair = fun(T) ->
        gleam@string_builder:from_strings(
            [gleam@uri:percent_encode(erlang:element(1, T)),
                <<"="/utf8>>,
                gleam@uri:percent_encode(erlang:element(2, T))]
        )
    end,
    Query@1 = begin
        _pipe = Query,
        _pipe@1 = gleam@list:map(_pipe, Pair),
        _pipe@2 = gleam@list:intersperse(
            _pipe@1,
            gleam@string_builder:from_string(<<"&"/utf8>>)
        ),
        _pipe@3 = gleam@string_builder:concat(_pipe@2),
        _pipe@4 = gleam@string_builder:to_string(_pipe@3),
        {some, _pipe@4}
    end,
    erlang:setelement(9, Req, Query@1).

-file("/Users/louis/src/gleam/http/src/gleam/http/request.gleam", 200).
-spec set_method(request(GHL), gleam@http:method()) -> request(GHL).
set_method(Req, Method) ->
    erlang:setelement(2, Req, Method).

-file("/Users/louis/src/gleam/http/src/gleam/http/request.gleam", 207).
-spec new() -> request(binary()).
new() ->
    {request,
        get,
        [],
        <<""/utf8>>,
        https,
        <<"localhost"/utf8>>,
        none,
        <<""/utf8>>,
        none}.

-file("/Users/louis/src/gleam/http/src/gleam/http/request.gleam", 222).
-spec to(binary()) -> {ok, request(binary())} | {error, nil}.
to(Url) ->
    _pipe = Url,
    _pipe@1 = gleam@uri:parse(_pipe),
    gleam@result:then(_pipe@1, fun from_uri/1).

-file("/Users/louis/src/gleam/http/src/gleam/http/request.gleam", 230).
-spec set_scheme(request(GHS), gleam@http:scheme()) -> request(GHS).
set_scheme(Req, Scheme) ->
    erlang:setelement(5, Req, Scheme).

-file("/Users/louis/src/gleam/http/src/gleam/http/request.gleam", 236).
-spec set_host(request(GHV), binary()) -> request(GHV).
set_host(Req, Host) ->
    erlang:setelement(6, Req, Host).

-file("/Users/louis/src/gleam/http/src/gleam/http/request.gleam", 242).
-spec set_port(request(GHY), integer()) -> request(GHY).
set_port(Req, Port) ->
    erlang:setelement(7, Req, {some, Port}).

-file("/Users/louis/src/gleam/http/src/gleam/http/request.gleam", 248).
-spec set_path(request(GIB), binary()) -> request(GIB).
set_path(Req, Path) ->
    erlang:setelement(8, Req, Path).

-file("/Users/louis/src/gleam/http/src/gleam/http/request.gleam", 255).
-spec set_cookie(request(GIE), binary(), binary()) -> request(GIE).
set_cookie(Req, Name, Value) ->
    New_cookie_string = gleam@string:join([Name, Value], <<"="/utf8>>),
    {Cookies_string@2, Headers@1} = case gleam@list:key_pop(
        erlang:element(3, Req),
        <<"cookie"/utf8>>
    ) of
        {ok, {Cookies_string, Headers}} ->
            Cookies_string@1 = gleam@string:join(
                [Cookies_string, New_cookie_string],
                <<"; "/utf8>>
            ),
            {Cookies_string@1, Headers};

        {error, nil} ->
            {New_cookie_string, erlang:element(3, Req)}
    end,
    erlang:setelement(
        3,
        Req,
        [{<<"cookie"/utf8>>, Cookies_string@2} | Headers@1]
    ).

-file("/Users/louis/src/gleam/http/src/gleam/http/request.gleam", 274).
-spec get_cookies(request(any())) -> list({binary(), binary()}).
get_cookies(Req) ->
    {request, _, Headers, _, _, _, _, _, _} = Req,
    _pipe = Headers,
    _pipe@1 = gleam@list:filter_map(
        _pipe,
        fun(Header) ->
            {Name, Value} = Header,
            case Name of
                <<"cookie"/utf8>> ->
                    {ok, gleam@http@cookie:parse(Value)};

                _ ->
                    {error, nil}
            end
        end
    ),
    gleam@list:flatten(_pipe@1).

-file("/Users/louis/src/gleam/http/src/gleam/http/request.gleam", 292).
-spec remove_cookie(request(GIJ), binary()) -> request(GIJ).
remove_cookie(Req, Name) ->
    case gleam@list:key_pop(erlang:element(3, Req), <<"cookie"/utf8>>) of
        {ok, {Cookies_string, Headers}} ->
            New_cookies_string = begin
                _pipe = gleam@string:split(Cookies_string, <<";"/utf8>>),
                _pipe@4 = gleam@list:filter(
                    _pipe,
                    fun(Str) -> _pipe@1 = gleam@string:trim(Str),
                        _pipe@2 = gleam@string:split_once(_pipe@1, <<"="/utf8>>),
                        _pipe@3 = gleam@result:map(
                            _pipe@2,
                            fun(Tup) -> erlang:element(1, Tup) /= Name end
                        ),
                        gleam@result:unwrap(_pipe@3, true) end
                ),
                gleam@string:join(_pipe@4, <<";"/utf8>>)
            end,
            erlang:setelement(
                3,
                Req,
                [{<<"cookie"/utf8>>, New_cookies_string} | Headers]
            );

        {error, _} ->
            Req
    end.
