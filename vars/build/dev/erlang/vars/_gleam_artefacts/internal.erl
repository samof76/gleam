-module(internal).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([format_pair/2]).

-spec format_pair(binary(), binary()) -> binary().
format_pair(Name, Value) ->
    <<<<Name/binary, "="/utf8>>/binary, Value/binary>>.
