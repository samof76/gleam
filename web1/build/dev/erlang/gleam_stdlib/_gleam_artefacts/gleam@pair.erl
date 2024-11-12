-module(gleam@pair).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([first/1, second/1, swap/1, map_first/2, map_second/2, new/2]).

-spec first({AAA, any()}) -> AAA.
first(Pair) ->
    {A, _} = Pair,
    A.

-spec second({any(), AAD}) -> AAD.
second(Pair) ->
    {_, A} = Pair,
    A.

-spec swap({AAE, AAF}) -> {AAF, AAE}.
swap(Pair) ->
    {A, B} = Pair,
    {B, A}.

-spec map_first({AAG, AAH}, fun((AAG) -> AAI)) -> {AAI, AAH}.
map_first(Pair, Fun) ->
    {A, B} = Pair,
    {Fun(A), B}.

-spec map_second({AAJ, AAK}, fun((AAK) -> AAL)) -> {AAJ, AAL}.
map_second(Pair, Fun) ->
    {A, B} = Pair,
    {A, Fun(B)}.

-spec new(AAM, AAN) -> {AAM, AAN}.
new(First, Second) ->
    {First, Second}.
