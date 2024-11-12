-module(gleam@function).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2, apply1/2, apply2/3, apply3/4]).

-spec compose(fun((DPR) -> DPS), fun((DPS) -> DPT)) -> fun((DPR) -> DPT).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-spec curry2(fun((DPU, DPV) -> DPW)) -> fun((DPU) -> fun((DPV) -> DPW)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-spec curry3(fun((DPY, DPZ, DQA) -> DQB)) -> fun((DPY) -> fun((DPZ) -> fun((DQA) -> DQB))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-spec curry4(fun((DQD, DQE, DQF, DQG) -> DQH)) -> fun((DQD) -> fun((DQE) -> fun((DQF) -> fun((DQG) -> DQH)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-spec curry5(fun((DQJ, DQK, DQL, DQM, DQN) -> DQO)) -> fun((DQJ) -> fun((DQK) -> fun((DQL) -> fun((DQM) -> fun((DQN) -> DQO))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-spec curry6(fun((DQQ, DQR, DQS, DQT, DQU, DQV) -> DQW)) -> fun((DQQ) -> fun((DQR) -> fun((DQS) -> fun((DQT) -> fun((DQU) -> fun((DQV) -> DQW)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-spec flip(fun((DQY, DQZ) -> DRA)) -> fun((DQZ, DQY) -> DRA).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-spec identity(DRB) -> DRB.
identity(X) ->
    X.

-spec constant(DRC) -> fun((any()) -> DRC).
constant(Value) ->
    fun(_) -> Value end.

-spec tap(DRE, fun((DRE) -> any())) -> DRE.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.

-spec apply1(fun((DRG) -> DRH), DRG) -> DRH.
apply1(Fun, Arg1) ->
    Fun(Arg1).

-spec apply2(fun((DRI, DRJ) -> DRK), DRI, DRJ) -> DRK.
apply2(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

-spec apply3(fun((DRL, DRM, DRN) -> DRO), DRL, DRM, DRN) -> DRO.
apply3(Fun, Arg1, Arg2, Arg3) ->
    Fun(Arg1, Arg2, Arg3).