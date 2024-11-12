-module(web1_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0, hello_world_test/0]).

-spec main() -> nil.
main() ->
    gleeunit:main().

-spec hello_world_test() -> nil.
hello_world_test() ->
    Res = app@router:handle_request(wisp@testing:get(<<"/"/utf8>>, [])),
    _pipe = erlang:element(2, Res),
    gleeunit_ffi:should_equal(_pipe, 200),
    _pipe@1 = erlang:element(3, Res),
    gleeunit_ffi:should_equal(
        _pipe@1,
        [{<<"content-type"/utf8>>, <<"text/html; charset=utf-8"/utf8>>}]
    ),
    _pipe@2 = Res,
    _pipe@3 = wisp@testing:string_body(_pipe@2),
    gleeunit_ffi:should_equal(_pipe@3, <<"<h1>Hello wisp</h1>"/utf8>>).
