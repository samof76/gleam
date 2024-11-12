-module(gleam@result).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([is_ok/1, is_error/1, map/2, map_error/2, flatten/1, 'try'/2, then/2, unwrap/2, lazy_unwrap/2, unwrap_error/2, unwrap_both/1, nil_error/1, 'or'/2, lazy_or/2, all/1, partition/1, replace/2, replace_error/2, values/1, try_recover/2]).

-spec is_ok({ok, any()} | {error, any()}) -> boolean().
is_ok(Result) ->
    case Result of
        {error, _} ->
            false;

        {ok, _} ->
            true
    end.

-spec is_error({ok, any()} | {error, any()}) -> boolean().
is_error(Result) ->
    case Result of
        {ok, _} ->
            false;

        {error, _} ->
            true
    end.

-spec map({ok, BXF} | {error, BXG}, fun((BXF) -> BXJ)) -> {ok, BXJ} |
    {error, BXG}.
map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, E} ->
            {error, E}
    end.

-spec map_error({ok, BXM} | {error, BXN}, fun((BXN) -> BXQ)) -> {ok, BXM} |
    {error, BXQ}.
map_error(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, Error} ->
            {error, Fun(Error)}
    end.

-spec flatten({ok, {ok, BXT} | {error, BXU}} | {error, BXU}) -> {ok, BXT} |
    {error, BXU}.
flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-spec 'try'({ok, BYB} | {error, BYC}, fun((BYB) -> {ok, BYF} | {error, BYC})) -> {ok,
        BYF} |
    {error, BYC}.
'try'(Result, Fun) ->
    case Result of
        {ok, X} ->
            Fun(X);

        {error, E} ->
            {error, E}
    end.

-spec then({ok, BYK} | {error, BYL}, fun((BYK) -> {ok, BYO} | {error, BYL})) -> {ok,
        BYO} |
    {error, BYL}.
then(Result, Fun) ->
    'try'(Result, Fun).

-spec unwrap({ok, BYT} | {error, any()}, BYT) -> BYT.
unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default
    end.

-spec lazy_unwrap({ok, BYX} | {error, any()}, fun(() -> BYX)) -> BYX.
lazy_unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default()
    end.

-spec unwrap_error({ok, any()} | {error, BZC}, BZC) -> BZC.
unwrap_error(Result, Default) ->
    case Result of
        {ok, _} ->
            Default;

        {error, E} ->
            E
    end.

-spec unwrap_both({ok, BZF} | {error, BZF}) -> BZF.
unwrap_both(Result) ->
    case Result of
        {ok, A} ->
            A;

        {error, A@1} ->
            A@1
    end.

-spec nil_error({ok, BZI} | {error, any()}) -> {ok, BZI} | {error, nil}.
nil_error(Result) ->
    map_error(Result, fun(_) -> nil end).

-spec 'or'({ok, BZO} | {error, BZP}, {ok, BZO} | {error, BZP}) -> {ok, BZO} |
    {error, BZP}.
'or'(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second
    end.

-spec lazy_or({ok, BZW} | {error, BZX}, fun(() -> {ok, BZW} | {error, BZX})) -> {ok,
        BZW} |
    {error, BZX}.
lazy_or(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second()
    end.

-spec all(list({ok, CAE} | {error, CAF})) -> {ok, list(CAE)} | {error, CAF}.
all(Results) ->
    gleam@list:try_map(Results, fun(X) -> X end).

-spec do_partition(list({ok, CAT} | {error, CAU}), list(CAT), list(CAU)) -> {list(CAT),
    list(CAU)}.
do_partition(Results, Oks, Errors) ->
    case Results of
        [] ->
            {Oks, Errors};

        [{ok, A} | Rest] ->
            do_partition(Rest, [A | Oks], Errors);

        [{error, E} | Rest@1] ->
            do_partition(Rest@1, Oks, [E | Errors])
    end.

-spec partition(list({ok, CAM} | {error, CAN})) -> {list(CAM), list(CAN)}.
partition(Results) ->
    do_partition(Results, [], []).

-spec replace({ok, any()} | {error, CBC}, CBF) -> {ok, CBF} | {error, CBC}.
replace(Result, Value) ->
    case Result of
        {ok, _} ->
            {ok, Value};

        {error, Error} ->
            {error, Error}
    end.

-spec replace_error({ok, CBI} | {error, any()}, CBM) -> {ok, CBI} | {error, CBM}.
replace_error(Result, Error) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, _} ->
            {error, Error}
    end.

-spec values(list({ok, CBP} | {error, any()})) -> list(CBP).
values(Results) ->
    gleam@list:filter_map(Results, fun(R) -> R end).

-spec try_recover(
    {ok, CBV} | {error, CBW},
    fun((CBW) -> {ok, CBV} | {error, CBZ})
) -> {ok, CBV} | {error, CBZ}.
try_recover(Result, Fun) ->
    case Result of
        {ok, Value} ->
            {ok, Value};

        {error, Error} ->
            Fun(Error)
    end.
