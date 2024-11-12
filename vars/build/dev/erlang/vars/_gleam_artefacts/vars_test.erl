-module(vars_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0, format_pair_test/0]).

-spec main() -> nil.
main() ->
    gleeunit:main().

-spec format_pair_test() -> nil.
format_pair_test() ->
    _pipe = internal:format_pair(<<"USER"/utf8>>, <<"msv"/utf8>>),
    gleeunit_ffi:should_equal(_pipe, <<"USER=msv"/utf8>>).
