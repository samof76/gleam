-module(gleam@regex).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([compile/2, from_string/1, check/2, split/2, scan/2, replace/3]).
-export_type([regex/0, match/0, compile_error/0, options/0]).

-type regex() :: any().

-type match() :: {match, binary(), list(gleam@option:option(binary()))}.

-type compile_error() :: {compile_error, binary(), integer()}.

-type options() :: {options, boolean(), boolean()}.

-spec compile(binary(), options()) -> {ok, regex()} | {error, compile_error()}.
compile(Pattern, Options) ->
    gleam_stdlib:compile_regex(Pattern, Options).

-spec from_string(binary()) -> {ok, regex()} | {error, compile_error()}.
from_string(Pattern) ->
    compile(Pattern, {options, false, false}).

-spec check(regex(), binary()) -> boolean().
check(Regex, String) ->
    gleam_stdlib:regex_check(Regex, String).

-spec split(regex(), binary()) -> list(binary()).
split(Regex, String) ->
    gleam_stdlib:regex_split(Regex, String).

-spec scan(regex(), binary()) -> list(match()).
scan(Regex, String) ->
    gleam_stdlib:regex_scan(Regex, String).

-spec replace(regex(), binary(), binary()) -> binary().
replace(Pattern, String, Substitute) ->
    gleam_stdlib:regex_replace(Pattern, String, Substitute).
