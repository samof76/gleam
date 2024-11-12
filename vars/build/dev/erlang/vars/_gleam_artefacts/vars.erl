-module(vars).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).

-spec get(binary()) -> nil.
get(Name) ->
    Value = begin
        _pipe = envoy_ffi:get(Name),
        gleam@result:unwrap(_pipe, <<""/utf8>>)
    end,
    gleam@io:println(internal:format_pair(Name, Value)).

-spec main() -> nil.
main() ->
    case erlang:element(4, argv:load()) of
        [<<"get"/utf8>>, Name] ->
            get(Name);

        _ ->
            gleam@io:println(<<"Usage: vars get name"/utf8>>)
    end.
