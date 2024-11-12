-module(gleam@list).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([length/1, reverse/1, is_empty/1, contains/2, first/1, rest/1, filter/2, filter_map/2, map/2, map2/3, index_map/2, try_map/2, drop/2, take/2, new/0, wrap/1, append/2, prepend/2, concat/1, flatten/1, flat_map/2, fold/3, count/2, group/2, map_fold/3, fold_right/3, index_fold/3, try_fold/3, fold_until/3, find/2, find_map/2, all/2, any/2, zip/2, strict_zip/2, unzip/1, intersperse/2, unique/1, sort/2, range/2, repeat/2, split/2, split_while/2, key_find/2, key_filter/2, pop/2, pop_map/2, key_pop/2, key_set/3, each/2, try_each/2, partition/2, permutations/1, window/2, window_by_2/1, drop_while/2, take_while/2, chunk/2, sized_chunk/2, reduce/2, scan/3, last/1, combinations/2, combination_pairs/1, transpose/1, interleave/1, shuffle/1]).
-export_type([continue_or_stop/1, sorting/0]).

-type continue_or_stop(AAP) :: {continue, AAP} | {stop, AAP}.

-type sorting() :: ascending | descending.

-spec count_length(list(any()), integer()) -> integer().
count_length(List, Count) ->
    case List of
        [_ | List@1] ->
            count_length(List@1, Count + 1);

        _ ->
            Count
    end.

-spec length(list(any())) -> integer().
length(List) ->
    erlang:length(List).

-spec do_reverse(list(AAZ), list(AAZ)) -> list(AAZ).
do_reverse(Remaining, Accumulator) ->
    case Remaining of
        [] ->
            Accumulator;

        [Item | Rest] ->
            do_reverse(Rest, [Item | Accumulator])
    end.

-spec reverse(list(AAW)) -> list(AAW).
reverse(List) ->
    lists:reverse(List).

-spec is_empty(list(any())) -> boolean().
is_empty(List) ->
    List =:= [].

-spec contains(list(ABF), ABF) -> boolean().
contains(List, Elem) ->
    case List of
        [] ->
            false;

        [First | _] when First =:= Elem ->
            true;

        [_ | Rest] ->
            contains(Rest, Elem)
    end.

-spec first(list(ABH)) -> {ok, ABH} | {error, nil}.
first(List) ->
    case List of
        [] ->
            {error, nil};

        [X | _] ->
            {ok, X}
    end.

-spec rest(list(ABL)) -> {ok, list(ABL)} | {error, nil}.
rest(List) ->
    case List of
        [] ->
            {error, nil};

        [_ | Rest] ->
            {ok, Rest}
    end.

-spec update_group(fun((ABQ) -> ABR)) -> fun((gleam@dict:dict(ABR, list(ABQ)), ABQ) -> gleam@dict:dict(ABR, list(ABQ))).
update_group(F) ->
    fun(Groups, Elem) -> case gleam@dict:get(Groups, F(Elem)) of
            {ok, Existing} ->
                gleam@dict:insert(Groups, F(Elem), [Elem | Existing]);

            {error, _} ->
                gleam@dict:insert(Groups, F(Elem), [Elem])
        end end.

-spec do_filter(list(ACE), fun((ACE) -> boolean()), list(ACE)) -> list(ACE).
do_filter(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            New_acc = case Fun(First) of
                true ->
                    [First | Acc];

                false ->
                    Acc
            end,
            do_filter(Rest, Fun, New_acc)
    end.

-spec filter(list(ACI), fun((ACI) -> boolean())) -> list(ACI).
filter(List, Predicate) ->
    do_filter(List, Predicate, []).

-spec do_filter_map(
    list(ACL),
    fun((ACL) -> {ok, ACN} | {error, any()}),
    list(ACN)
) -> list(ACN).
do_filter_map(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            New_acc = case Fun(First) of
                {ok, First@1} ->
                    [First@1 | Acc];

                {error, _} ->
                    Acc
            end,
            do_filter_map(Rest, Fun, New_acc)
    end.

-spec filter_map(list(ACT), fun((ACT) -> {ok, ACV} | {error, any()})) -> list(ACV).
filter_map(List, Fun) ->
    do_filter_map(List, Fun, []).

-spec do_map(list(ADA), fun((ADA) -> ADC), list(ADC)) -> list(ADC).
do_map(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            do_map(Rest, Fun, [Fun(First) | Acc])
    end.

-spec map(list(ADF), fun((ADF) -> ADH)) -> list(ADH).
map(List, Fun) ->
    do_map(List, Fun, []).

-spec do_map2(list(ADP), list(ADR), fun((ADP, ADR) -> ADT), list(ADT)) -> list(ADT).
do_map2(List1, List2, Fun, Acc) ->
    case {List1, List2} of
        {[], _} ->
            lists:reverse(Acc);

        {_, []} ->
            lists:reverse(Acc);

        {[A | As_], [B | Bs]} ->
            do_map2(As_, Bs, Fun, [Fun(A, B) | Acc])
    end.

-spec map2(list(ADJ), list(ADL), fun((ADJ, ADL) -> ADN)) -> list(ADN).
map2(List1, List2, Fun) ->
    do_map2(List1, List2, Fun, []).

-spec do_index_map(
    list(AEB),
    fun((AEB, integer()) -> AED),
    integer(),
    list(AED)
) -> list(AED).
do_index_map(List, Fun, Index, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            Acc@1 = [Fun(First, Index) | Acc],
            do_index_map(Rest, Fun, Index + 1, Acc@1)
    end.

-spec index_map(list(AEG), fun((AEG, integer()) -> AEI)) -> list(AEI).
index_map(List, Fun) ->
    do_index_map(List, Fun, 0, []).

-spec do_try_map(list(AEK), fun((AEK) -> {ok, AEM} | {error, AEN}), list(AEM)) -> {ok,
        list(AEM)} |
    {error, AEN}.
do_try_map(List, Fun, Acc) ->
    case List of
        [] ->
            {ok, lists:reverse(Acc)};

        [First | Rest] ->
            case Fun(First) of
                {ok, First@1} ->
                    do_try_map(Rest, Fun, [First@1 | Acc]);

                {error, Error} ->
                    {error, Error}
            end
    end.

-spec try_map(list(AEU), fun((AEU) -> {ok, AEW} | {error, AEX})) -> {ok,
        list(AEW)} |
    {error, AEX}.
try_map(List, Fun) ->
    do_try_map(List, Fun, []).

-spec drop(list(AFD), integer()) -> list(AFD).
drop(List, N) ->
    case N =< 0 of
        true ->
            List;

        false ->
            case List of
                [] ->
                    [];

                [_ | Rest] ->
                    drop(Rest, N - 1)
            end
    end.

-spec do_take(list(AFG), integer(), list(AFG)) -> list(AFG).
do_take(List, N, Acc) ->
    case N =< 0 of
        true ->
            lists:reverse(Acc);

        false ->
            case List of
                [] ->
                    lists:reverse(Acc);

                [First | Rest] ->
                    do_take(Rest, N - 1, [First | Acc])
            end
    end.

-spec take(list(AFK), integer()) -> list(AFK).
take(List, N) ->
    do_take(List, N, []).

-spec new() -> list(any()).
new() ->
    [].

-spec wrap(AFP) -> list(AFP).
wrap(Item) ->
    [Item].

-spec do_append(list(AFV), list(AFV)) -> list(AFV).
do_append(First, Second) ->
    case First of
        [] ->
            Second;

        [Item | Rest] ->
            do_append(Rest, [Item | Second])
    end.

-spec append(list(AFR), list(AFR)) -> list(AFR).
append(First, Second) ->
    lists:append(First, Second).

-spec prepend(list(AFZ), AFZ) -> list(AFZ).
prepend(List, Item) ->
    [Item | List].

-spec reverse_and_prepend(list(AGC), list(AGC)) -> list(AGC).
reverse_and_prepend(Prefix, Suffix) ->
    case Prefix of
        [] ->
            Suffix;

        [First | Rest] ->
            reverse_and_prepend(Rest, [First | Suffix])
    end.

-spec do_concat(list(list(AGG)), list(AGG)) -> list(AGG).
do_concat(Lists, Acc) ->
    case Lists of
        [] ->
            lists:reverse(Acc);

        [List | Further_lists] ->
            do_concat(Further_lists, reverse_and_prepend(List, Acc))
    end.

-spec concat(list(list(AGL))) -> list(AGL).
concat(Lists) ->
    do_concat(Lists, []).

-spec flatten(list(list(AGP))) -> list(AGP).
flatten(Lists) ->
    do_concat(Lists, []).

-spec flat_map(list(AGT), fun((AGT) -> list(AGV))) -> list(AGV).
flat_map(List, Fun) ->
    _pipe = map(List, Fun),
    flatten(_pipe).

-spec fold(list(AGY), AHA, fun((AHA, AGY) -> AHA)) -> AHA.
fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold(Rest, Fun(Initial, X), Fun)
    end.

-spec count(list(AAU), fun((AAU) -> boolean())) -> integer().
count(List, Predicate) ->
    fold(List, 0, fun(Acc, Value) -> case Predicate(Value) of
                true ->
                    Acc + 1;

                false ->
                    Acc
            end end).

-spec group(list(ABY), fun((ABY) -> ACA)) -> gleam@dict:dict(ACA, list(ABY)).
group(List, Key) ->
    fold(List, gleam@dict:new(), update_group(Key)).

-spec map_fold(list(ADW), ADY, fun((ADY, ADW) -> {ADY, ADZ})) -> {ADY,
    list(ADZ)}.
map_fold(List, Initial, Fun) ->
    _pipe = fold(
        List,
        {Initial, []},
        fun(Acc, Item) ->
            {Current_acc, Items} = Acc,
            {Next_acc, Next_item} = Fun(Current_acc, Item),
            {Next_acc, [Next_item | Items]}
        end
    ),
    gleam@pair:map_second(_pipe, fun lists:reverse/1).

-spec fold_right(list(AHB), AHD, fun((AHD, AHB) -> AHD)) -> AHD.
fold_right(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            Fun(fold_right(Rest, Initial, Fun), X)
    end.

-spec do_index_fold(
    list(AHE),
    AHG,
    fun((AHG, AHE, integer()) -> AHG),
    integer()
) -> AHG.
do_index_fold(Over, Acc, With, Index) ->
    case Over of
        [] ->
            Acc;

        [First | Rest] ->
            do_index_fold(Rest, With(Acc, First, Index), With, Index + 1)
    end.

-spec index_fold(list(AHH), AHJ, fun((AHJ, AHH, integer()) -> AHJ)) -> AHJ.
index_fold(List, Initial, Fun) ->
    do_index_fold(List, Initial, Fun, 0).

-spec try_fold(list(AHK), AHM, fun((AHM, AHK) -> {ok, AHM} | {error, AHN})) -> {ok,
        AHM} |
    {error, AHN}.
try_fold(List, Initial, Fun) ->
    case List of
        [] ->
            {ok, Initial};

        [First | Rest] ->
            case Fun(Initial, First) of
                {ok, Result} ->
                    try_fold(Rest, Result, Fun);

                {error, _} = Error ->
                    Error
            end
    end.

-spec fold_until(list(AHS), AHU, fun((AHU, AHS) -> continue_or_stop(AHU))) -> AHU.
fold_until(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [First | Rest] ->
            case Fun(Initial, First) of
                {continue, Next_accumulator} ->
                    fold_until(Rest, Next_accumulator, Fun);

                {stop, B} ->
                    B
            end
    end.

-spec find(list(AHW), fun((AHW) -> boolean())) -> {ok, AHW} | {error, nil}.
find(List, Is_desired) ->
    case List of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Is_desired(X) of
                true ->
                    {ok, X};

                _ ->
                    find(Rest, Is_desired)
            end
    end.

-spec find_map(list(AIA), fun((AIA) -> {ok, AIC} | {error, any()})) -> {ok, AIC} |
    {error, nil}.
find_map(List, Fun) ->
    case List of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Fun(X) of
                {ok, X@1} ->
                    {ok, X@1};

                _ ->
                    find_map(Rest, Fun)
            end
    end.

-spec all(list(AII), fun((AII) -> boolean())) -> boolean().
all(List, Predicate) ->
    case List of
        [] ->
            true;

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    all(Rest, Predicate);

                false ->
                    false
            end
    end.

-spec any(list(AIK), fun((AIK) -> boolean())) -> boolean().
any(List, Predicate) ->
    case List of
        [] ->
            false;

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    true;

                false ->
                    any(Rest, Predicate)
            end
    end.

-spec do_zip(list(AIM), list(AIO), list({AIM, AIO})) -> list({AIM, AIO}).
do_zip(One, Other, Acc) ->
    case {One, Other} of
        {[First_one | Rest_one], [First_other | Rest_other]} ->
            do_zip(Rest_one, Rest_other, [{First_one, First_other} | Acc]);

        {_, _} ->
            lists:reverse(Acc)
    end.

-spec zip(list(AIS), list(AIU)) -> list({AIS, AIU}).
zip(List, Other) ->
    do_zip(List, Other, []).

-spec strict_zip(list(AIX), list(AIZ)) -> {ok, list({AIX, AIZ})} | {error, nil}.
strict_zip(List, Other) ->
    case erlang:length(List) =:= erlang:length(Other) of
        true ->
            {ok, zip(List, Other)};

        false ->
            {error, nil}
    end.

-spec do_unzip(list({AJE, AJF}), list(AJE), list(AJF)) -> {list(AJE), list(AJF)}.
do_unzip(Input, One, Other) ->
    case Input of
        [] ->
            {lists:reverse(One), lists:reverse(Other)};

        [{First_one, First_other} | Rest] ->
            do_unzip(Rest, [First_one | One], [First_other | Other])
    end.

-spec unzip(list({AJL, AJM})) -> {list(AJL), list(AJM)}.
unzip(Input) ->
    do_unzip(Input, [], []).

-spec do_intersperse(list(AJQ), AJQ, list(AJQ)) -> list(AJQ).
do_intersperse(List, Separator, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Rest] ->
            do_intersperse(Rest, Separator, [X, Separator | Acc])
    end.

-spec intersperse(list(AJU), AJU) -> list(AJU).
intersperse(List, Elem) ->
    case List of
        [] ->
            List;

        [_] ->
            List;

        [X | Rest] ->
            do_intersperse(Rest, Elem, [X])
    end.

-spec unique(list(AJX)) -> list(AJX).
unique(List) ->
    case List of
        [] ->
            [];

        [X | Rest] ->
            [X | unique(filter(Rest, fun(Y) -> Y /= X end))]
    end.

-spec sequences(
    list(AKD),
    fun((AKD, AKD) -> gleam@order:order()),
    list(AKD),
    sorting(),
    AKD,
    list(list(AKD))
) -> list(list(AKD)).
sequences(List, Compare, Growing, Direction, Prev, Acc) ->
    Growing@1 = [Prev | Growing],
    case List of
        [] ->
            case Direction of
                ascending ->
                    [do_reverse(Growing@1, []) | Acc];

                descending ->
                    [Growing@1 | Acc]
            end;

        [New | Rest] ->
            case {Compare(Prev, New), Direction} of
                {gt, descending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {lt, ascending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {eq, ascending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {gt, ascending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [do_reverse(Growing@1, []) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end;

                {lt, descending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [do_reverse(Growing@1, []) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end;

                {eq, descending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [do_reverse(Growing@1, []) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end
            end
    end.

-spec merge_ascendings(
    list(ALA),
    list(ALA),
    fun((ALA, ALA) -> gleam@order:order()),
    list(ALA)
) -> list(ALA).
merge_ascendings(List1, List2, Compare, Acc) ->
    case {List1, List2} of
        {[], List} ->
            do_reverse(List, Acc);

        {List, []} ->
            do_reverse(List, Acc);

        {[First1 | Rest1], [First2 | Rest2]} ->
            case Compare(First1, First2) of
                lt ->
                    merge_ascendings(Rest1, List2, Compare, [First1 | Acc]);

                gt ->
                    merge_ascendings(List1, Rest2, Compare, [First2 | Acc]);

                eq ->
                    merge_ascendings(List1, Rest2, Compare, [First2 | Acc])
            end
    end.

-spec merge_ascending_pairs(
    list(list(AKO)),
    fun((AKO, AKO) -> gleam@order:order()),
    list(list(AKO))
) -> list(list(AKO)).
merge_ascending_pairs(Sequences, Compare, Acc) ->
    case Sequences of
        [] ->
            do_reverse(Acc, []);

        [Sequence] ->
            do_reverse([do_reverse(Sequence, []) | Acc], []);

        [Ascending1, Ascending2 | Rest] ->
            Descending = merge_ascendings(Ascending1, Ascending2, Compare, []),
            merge_ascending_pairs(Rest, Compare, [Descending | Acc])
    end.

-spec merge_descendings(
    list(ALF),
    list(ALF),
    fun((ALF, ALF) -> gleam@order:order()),
    list(ALF)
) -> list(ALF).
merge_descendings(List1, List2, Compare, Acc) ->
    case {List1, List2} of
        {[], List} ->
            do_reverse(List, Acc);

        {List, []} ->
            do_reverse(List, Acc);

        {[First1 | Rest1], [First2 | Rest2]} ->
            case Compare(First1, First2) of
                lt ->
                    merge_descendings(List1, Rest2, Compare, [First2 | Acc]);

                gt ->
                    merge_descendings(Rest1, List2, Compare, [First1 | Acc]);

                eq ->
                    merge_descendings(Rest1, List2, Compare, [First1 | Acc])
            end
    end.

-spec merge_descending_pairs(
    list(list(AKU)),
    fun((AKU, AKU) -> gleam@order:order()),
    list(list(AKU))
) -> list(list(AKU)).
merge_descending_pairs(Sequences, Compare, Acc) ->
    case Sequences of
        [] ->
            do_reverse(Acc, []);

        [Sequence] ->
            do_reverse([do_reverse(Sequence, []) | Acc], []);

        [Descending1, Descending2 | Rest] ->
            Ascending = merge_descendings(Descending1, Descending2, Compare, []),
            merge_descending_pairs(Rest, Compare, [Ascending | Acc])
    end.

-spec merge_all(
    list(list(AKK)),
    sorting(),
    fun((AKK, AKK) -> gleam@order:order())
) -> list(AKK).
merge_all(Sequences, Direction, Compare) ->
    case {Sequences, Direction} of
        {[], _} ->
            [];

        {[Sequence], ascending} ->
            Sequence;

        {[Sequence@1], descending} ->
            do_reverse(Sequence@1, []);

        {_, ascending} ->
            Sequences@1 = merge_ascending_pairs(Sequences, Compare, []),
            merge_all(Sequences@1, descending, Compare);

        {_, descending} ->
            Sequences@2 = merge_descending_pairs(Sequences, Compare, []),
            merge_all(Sequences@2, ascending, Compare)
    end.

-spec sort(list(AKA), fun((AKA, AKA) -> gleam@order:order())) -> list(AKA).
sort(List, Compare) ->
    case List of
        [] ->
            [];

        [X] ->
            [X];

        [X@1, Y | Rest] ->
            Direction = case Compare(X@1, Y) of
                lt ->
                    ascending;

                eq ->
                    ascending;

                gt ->
                    descending
            end,
            Sequences = sequences(Rest, Compare, [X@1], Direction, Y, []),
            merge_all(Sequences, ascending, Compare)
    end.

-spec tail_recursive_range(integer(), integer(), list(integer())) -> list(integer()).
tail_recursive_range(Start, Stop, Acc) ->
    case gleam@int:compare(Start, Stop) of
        eq ->
            [Stop | Acc];

        gt ->
            tail_recursive_range(Start, Stop + 1, [Stop | Acc]);

        lt ->
            tail_recursive_range(Start, Stop - 1, [Stop | Acc])
    end.

-spec range(integer(), integer()) -> list(integer()).
range(Start, Stop) ->
    tail_recursive_range(Start, Stop, []).

-spec do_repeat(ALN, integer(), list(ALN)) -> list(ALN).
do_repeat(Item, Times, Acc) ->
    case Times =< 0 of
        true ->
            Acc;

        false ->
            do_repeat(Item, Times - 1, [Item | Acc])
    end.

-spec repeat(ALQ, integer()) -> list(ALQ).
repeat(A, Times) ->
    do_repeat(A, Times, []).

-spec do_split(list(ALS), integer(), list(ALS)) -> {list(ALS), list(ALS)}.
do_split(List, N, Taken) ->
    case N =< 0 of
        true ->
            {lists:reverse(Taken), List};

        false ->
            case List of
                [] ->
                    {lists:reverse(Taken), []};

                [First | Rest] ->
                    do_split(Rest, N - 1, [First | Taken])
            end
    end.

-spec split(list(ALX), integer()) -> {list(ALX), list(ALX)}.
split(List, Index) ->
    do_split(List, Index, []).

-spec do_split_while(list(AMB), fun((AMB) -> boolean()), list(AMB)) -> {list(AMB),
    list(AMB)}.
do_split_while(List, F, Acc) ->
    case List of
        [] ->
            {lists:reverse(Acc), []};

        [First | Rest] ->
            case F(First) of
                false ->
                    {lists:reverse(Acc), List};

                _ ->
                    do_split_while(Rest, F, [First | Acc])
            end
    end.

-spec split_while(list(AMG), fun((AMG) -> boolean())) -> {list(AMG), list(AMG)}.
split_while(List, Predicate) ->
    do_split_while(List, Predicate, []).

-spec key_find(list({AMK, AML}), AMK) -> {ok, AML} | {error, nil}.
key_find(Keyword_list, Desired_key) ->
    find_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-spec key_filter(list({AMP, AMQ}), AMP) -> list(AMQ).
key_filter(Keyword_list, Desired_key) ->
    filter_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-spec do_pop(list(BEY), fun((BEY) -> boolean()), list(BEY)) -> {ok,
        {BEY, list(BEY)}} |
    {error, nil}.
do_pop(Haystack, Predicate, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Predicate(X) of
                true ->
                    {ok, {X, lists:append(lists:reverse(Checked), Rest)}};

                false ->
                    do_pop(Rest, Predicate, [X | Checked])
            end
    end.

-spec pop(list(AMX), fun((AMX) -> boolean())) -> {ok, {AMX, list(AMX)}} |
    {error, nil}.
pop(List, Is_desired) ->
    do_pop(List, Is_desired, []).

-spec do_pop_map(list(ANC), fun((ANC) -> {ok, ANE} | {error, any()}), list(ANC)) -> {ok,
        {ANE, list(ANC)}} |
    {error, nil}.
do_pop_map(List, Mapper, Checked) ->
    case List of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Mapper(X) of
                {ok, Y} ->
                    {ok, {Y, lists:append(lists:reverse(Checked), Rest)}};

                {error, _} ->
                    do_pop_map(Rest, Mapper, [X | Checked])
            end
    end.

-spec pop_map(list(ANM), fun((ANM) -> {ok, ANO} | {error, any()})) -> {ok,
        {ANO, list(ANM)}} |
    {error, nil}.
pop_map(Haystack, Is_desired) ->
    do_pop_map(Haystack, Is_desired, []).

-spec key_pop(list({ANV, ANW}), ANV) -> {ok, {ANW, list({ANV, ANW})}} |
    {error, nil}.
key_pop(List, Key) ->
    pop_map(
        List,
        fun(Entry) ->
            {K, V} = Entry,
            case K of
                K@1 when K@1 =:= Key ->
                    {ok, V};

                _ ->
                    {error, nil}
            end
        end
    ).

-spec key_set(list({AOB, AOC}), AOB, AOC) -> list({AOB, AOC}).
key_set(List, Key, Value) ->
    case List of
        [] ->
            [{Key, Value}];

        [{K, _} | Rest] when K =:= Key ->
            [{Key, Value} | Rest];

        [First | Rest@1] ->
            [First | key_set(Rest@1, Key, Value)]
    end.

-spec each(list(AOF), fun((AOF) -> any())) -> nil.
each(List, F) ->
    case List of
        [] ->
            nil;

        [First | Rest] ->
            F(First),
            each(Rest, F)
    end.

-spec try_each(list(AOI), fun((AOI) -> {ok, any()} | {error, AOL})) -> {ok, nil} |
    {error, AOL}.
try_each(List, Fun) ->
    case List of
        [] ->
            {ok, nil};

        [First | Rest] ->
            case Fun(First) of
                {ok, _} ->
                    try_each(Rest, Fun);

                {error, E} ->
                    {error, E}
            end
    end.

-spec do_partition(list(BHD), fun((BHD) -> boolean()), list(BHD), list(BHD)) -> {list(BHD),
    list(BHD)}.
do_partition(List, Categorise, Trues, Falses) ->
    case List of
        [] ->
            {lists:reverse(Trues), lists:reverse(Falses)};

        [First | Rest] ->
            case Categorise(First) of
                true ->
                    do_partition(Rest, Categorise, [First | Trues], Falses);

                false ->
                    do_partition(Rest, Categorise, Trues, [First | Falses])
            end
    end.

-spec partition(list(AOV), fun((AOV) -> boolean())) -> {list(AOV), list(AOV)}.
partition(List, Categorise) ->
    do_partition(List, Categorise, [], []).

-spec permutations(list(AOZ)) -> list(list(AOZ)).
permutations(List) ->
    case List of
        [] ->
            [[]];

        _ ->
            _pipe@3 = index_map(
                List,
                fun(I, I_idx) ->
                    _pipe = index_fold(
                        List,
                        [],
                        fun(Acc, J, J_idx) -> case I_idx =:= J_idx of
                                true ->
                                    Acc;

                                false ->
                                    [J | Acc]
                            end end
                    ),
                    _pipe@1 = lists:reverse(_pipe),
                    _pipe@2 = permutations(_pipe@1),
                    map(_pipe@2, fun(Permutation) -> [I | Permutation] end)
                end
            ),
            flatten(_pipe@3)
    end.

-spec do_window(list(list(APD)), list(APD), integer()) -> list(list(APD)).
do_window(Acc, List, N) ->
    Window = take(List, N),
    case erlang:length(Window) =:= N of
        true ->
            do_window([Window | Acc], drop(List, 1), N);

        false ->
            Acc
    end.

-spec window(list(APJ), integer()) -> list(list(APJ)).
window(List, N) ->
    case N =< 0 of
        true ->
            [];

        false ->
            _pipe = do_window([], List, N),
            lists:reverse(_pipe)
    end.

-spec window_by_2(list(APN)) -> list({APN, APN}).
window_by_2(List) ->
    zip(List, drop(List, 1)).

-spec drop_while(list(APQ), fun((APQ) -> boolean())) -> list(APQ).
drop_while(List, Predicate) ->
    case List of
        [] ->
            [];

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    drop_while(Rest, Predicate);

                false ->
                    [First | Rest]
            end
    end.

-spec do_take_while(list(APT), fun((APT) -> boolean()), list(APT)) -> list(APT).
do_take_while(List, Predicate, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    do_take_while(Rest, Predicate, [First | Acc]);

                false ->
                    lists:reverse(Acc)
            end
    end.

-spec take_while(list(APX), fun((APX) -> boolean())) -> list(APX).
take_while(List, Predicate) ->
    do_take_while(List, Predicate, []).

-spec do_chunk(list(AQA), fun((AQA) -> AQC), AQC, list(AQA), list(list(AQA))) -> list(list(AQA)).
do_chunk(List, F, Previous_key, Current_chunk, Acc) ->
    case List of
        [First | Rest] ->
            Key = F(First),
            case Key =:= Previous_key of
                false ->
                    New_acc = [lists:reverse(Current_chunk) | Acc],
                    do_chunk(Rest, F, Key, [First], New_acc);

                _ ->
                    do_chunk(Rest, F, Key, [First | Current_chunk], Acc)
            end;

        _ ->
            lists:reverse([lists:reverse(Current_chunk) | Acc])
    end.

-spec chunk(list(AQI), fun((AQI) -> any())) -> list(list(AQI)).
chunk(List, F) ->
    case List of
        [] ->
            [];

        [First | Rest] ->
            do_chunk(Rest, F, F(First), [First], [])
    end.

-spec do_sized_chunk(
    list(AQN),
    integer(),
    integer(),
    list(AQN),
    list(list(AQN))
) -> list(list(AQN)).
do_sized_chunk(List, Count, Left, Current_chunk, Acc) ->
    case List of
        [] ->
            case Current_chunk of
                [] ->
                    lists:reverse(Acc);

                Remaining ->
                    lists:reverse([lists:reverse(Remaining) | Acc])
            end;

        [First | Rest] ->
            Chunk = [First | Current_chunk],
            case Left > 1 of
                false ->
                    do_sized_chunk(
                        Rest,
                        Count,
                        Count,
                        [],
                        [lists:reverse(Chunk) | Acc]
                    );

                true ->
                    do_sized_chunk(Rest, Count, Left - 1, Chunk, Acc)
            end
    end.

-spec sized_chunk(list(AQU), integer()) -> list(list(AQU)).
sized_chunk(List, Count) ->
    do_sized_chunk(List, Count, Count, [], []).

-spec reduce(list(AQY), fun((AQY, AQY) -> AQY)) -> {ok, AQY} | {error, nil}.
reduce(List, Fun) ->
    case List of
        [] ->
            {error, nil};

        [First | Rest] ->
            {ok, fold(Rest, First, Fun)}
    end.

-spec do_scan(list(ARC), ARE, list(ARE), fun((ARE, ARC) -> ARE)) -> list(ARE).
do_scan(List, Accumulator, Accumulated, Fun) ->
    case List of
        [] ->
            lists:reverse(Accumulated);

        [First | Rest] ->
            Next = Fun(Accumulator, First),
            do_scan(Rest, Next, [Next | Accumulated], Fun)
    end.

-spec scan(list(ARH), ARJ, fun((ARJ, ARH) -> ARJ)) -> list(ARJ).
scan(List, Initial, Fun) ->
    do_scan(List, Initial, [], Fun).

-spec last(list(ARL)) -> {ok, ARL} | {error, nil}.
last(List) ->
    _pipe = List,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-spec combinations(list(ARP), integer()) -> list(list(ARP)).
combinations(Items, N) ->
    case N of
        0 ->
            [[]];

        _ ->
            case Items of
                [] ->
                    [];

                [First | Rest] ->
                    First_combinations = begin
                        _pipe = map(
                            combinations(Rest, N - 1),
                            fun(Com) -> [First | Com] end
                        ),
                        lists:reverse(_pipe)
                    end,
                    fold(
                        First_combinations,
                        combinations(Rest, N),
                        fun(Acc, C) -> [C | Acc] end
                    )
            end
    end.

-spec do_combination_pairs(list(ART)) -> list(list({ART, ART})).
do_combination_pairs(Items) ->
    case Items of
        [] ->
            [];

        [First | Rest] ->
            First_combinations = map(Rest, fun(Other) -> {First, Other} end),
            [First_combinations | do_combination_pairs(Rest)]
    end.

-spec combination_pairs(list(ARX)) -> list({ARX, ARX}).
combination_pairs(Items) ->
    _pipe = do_combination_pairs(Items),
    flatten(_pipe).

-spec transpose(list(list(ASE))) -> list(list(ASE)).
transpose(List_of_list) ->
    Take_first = fun(List) -> case List of
            [] ->
                [];

            [F] ->
                [F];

            [F@1 | _] ->
                [F@1]
        end end,
    case List_of_list of
        [] ->
            [];

        [[] | Rest] ->
            transpose(Rest);

        Rows ->
            Firsts = begin
                _pipe = Rows,
                _pipe@1 = map(_pipe, Take_first),
                flatten(_pipe@1)
            end,
            Rest@1 = transpose(
                map(Rows, fun(_capture) -> drop(_capture, 1) end)
            ),
            [Firsts | Rest@1]
    end.

-spec interleave(list(list(ASA))) -> list(ASA).
interleave(List) ->
    _pipe = transpose(List),
    flatten(_pipe).

-spec do_shuffle_pair_unwrap(list({float(), ASJ}), list(ASJ)) -> list(ASJ).
do_shuffle_pair_unwrap(List, Acc) ->
    case List of
        [] ->
            Acc;

        [Elem_pair | Enumerable] ->
            do_shuffle_pair_unwrap(
                Enumerable,
                [erlang:element(2, Elem_pair) | Acc]
            )
    end.

-spec do_shuffle_by_pair_indexes(list({float(), ASN})) -> list({float(), ASN}).
do_shuffle_by_pair_indexes(List_of_pairs) ->
    sort(
        List_of_pairs,
        fun(A_pair, B_pair) ->
            gleam@float:compare(
                erlang:element(1, A_pair),
                erlang:element(1, B_pair)
            )
        end
    ).

-spec shuffle(list(ASQ)) -> list(ASQ).
shuffle(List) ->
    _pipe = List,
    _pipe@1 = fold(_pipe, [], fun(Acc, A) -> [{rand:uniform(), A} | Acc] end),
    _pipe@2 = do_shuffle_by_pair_indexes(_pipe@1),
    do_shuffle_pair_unwrap(_pipe@2, []).
