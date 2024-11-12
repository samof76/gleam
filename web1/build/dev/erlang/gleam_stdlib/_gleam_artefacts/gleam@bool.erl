-module(gleam@bool).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export(['and'/2, 'or'/2, negate/1, nor/2, nand/2, exclusive_or/2, exclusive_nor/2, compare/2, to_int/1, to_string/1, guard/3, lazy_guard/3]).

-spec 'and'(boolean(), boolean()) -> boolean().
'and'(A, B) ->
    A andalso B.

-spec 'or'(boolean(), boolean()) -> boolean().
'or'(A, B) ->
    A orelse B.

-spec negate(boolean()) -> boolean().
negate(Bool) ->
    not Bool.

-spec nor(boolean(), boolean()) -> boolean().
nor(A, B) ->
    not (A orelse B).

-spec nand(boolean(), boolean()) -> boolean().
nand(A, B) ->
    not (A andalso B).

-spec exclusive_or(boolean(), boolean()) -> boolean().
exclusive_or(A, B) ->
    A /= B.

-spec exclusive_nor(boolean(), boolean()) -> boolean().
exclusive_nor(A, B) ->
    A =:= B.

-spec compare(boolean(), boolean()) -> gleam@order:order().
compare(A, B) ->
    case {A, B} of
        {true, true} ->
            eq;

        {true, false} ->
            gt;

        {false, false} ->
            eq;

        {false, true} ->
            lt
    end.

-spec to_int(boolean()) -> integer().
to_int(Bool) ->
    case Bool of
        false ->
            0;

        true ->
            1
    end.

-spec to_string(boolean()) -> binary().
to_string(Bool) ->
    case Bool of
        false ->
            <<"False"/utf8>>;

        true ->
            <<"True"/utf8>>
    end.

-spec guard(boolean(), BVH, fun(() -> BVH)) -> BVH.
guard(Requirement, Consequence, Alternative) ->
    case Requirement of
        true ->
            Consequence;

        false ->
            Alternative()
    end.

-spec lazy_guard(boolean(), fun(() -> BVI), fun(() -> BVI)) -> BVI.
lazy_guard(Requirement, Consequence, Alternative) ->
    case Requirement of
        true ->
            Consequence();

        false ->
            Alternative()
    end.
