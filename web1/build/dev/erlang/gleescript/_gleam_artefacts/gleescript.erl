-module(gleescript).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([run/0, main/0]).
-export_type([config/0, erlang_option/0, never/0]).

-type config() :: {config, binary(), binary()}.

-type erlang_option() :: shebang |
    {comment, gleam@erlang@charlist:charlist()} |
    {emu_args, gleam@erlang@charlist:charlist()} |
    {archive,
        list({gleam@erlang@charlist:charlist(), bitstring()}),
        list(never())}.

-type never() :: any().

-spec snag_inspect_error({ok, CLX} | {error, any()}) -> {ok, CLX} |
    {error, snag:snag()}.
snag_inspect_error(Result) ->
    case Result of
        {ok, Value} ->
            {ok, Value};

        {error, Error} ->
            snag:error(gleam@string:inspect(Error))
    end.

-spec get_out_dir(list(binary())) -> binary().
get_out_dir(Args) ->
    Out = case Args of
        [<<"--out"/utf8>>, Folder] ->
            Folder;

        [<<"--out="/utf8, Folder/binary>>] ->
            Folder;

        _ ->
            <<"./"/utf8>>
    end,
    case gleam@string:last(Out) of
        {ok, <<"/"/utf8>>} ->
            Out;

        _ ->
            <<Out/binary, "/"/utf8>>
    end.

-spec load_config() -> {ok, config()} | {error, snag:snag()}.
load_config() ->
    Out_dir = get_out_dir(erlang:element(4, argv:load())),
    gleam@result:'try'(
        begin
            _pipe = simplifile:read(<<"gleam.toml"/utf8>>),
            _pipe@1 = snag_inspect_error(_pipe),
            snag:context(_pipe@1, <<"Failed to read gleam.toml"/utf8>>)
        end,
        fun(Text) ->
            gleam@result:'try'(
                begin
                    _pipe@2 = tom:parse(Text),
                    _pipe@3 = snag_inspect_error(_pipe@2),
                    snag:context(_pipe@3, <<"Failed to parse gleam.toml"/utf8>>)
                end,
                fun(Config) ->
                    gleam@result:'try'(
                        begin
                            _pipe@4 = tom:get_string(Config, [<<"name"/utf8>>]),
                            _pipe@5 = snag_inspect_error(_pipe@4),
                            snag:context(
                                _pipe@5,
                                <<"Failed to get package name from gleam.toml"/utf8>>
                            )
                        end,
                        fun(Package_name) ->
                            {ok, {config, Package_name, Out_dir}}
                        end
                    )
                end
            )
        end
    ).

-spec locate_beam_files() -> {ok, list(binary())} | {error, snag:snag()}.
locate_beam_files() ->
    gleam@result:'try'(
        begin
            _pipe = simplifile:get_files(<<"build/dev/erlang"/utf8>>),
            _pipe@1 = snag_inspect_error(_pipe),
            snag:context(_pipe@1, <<"Failed to read build directory"/utf8>>)
        end,
        fun(Files) -> _pipe@2 = Files,
            _pipe@3 = gleam@list:filter(
                _pipe@2,
                fun(F) ->
                    not gleam_stdlib:contains_string(F, <<"@@"/utf8>>) andalso (gleam@string:ends_with(
                        F,
                        <<".beam"/utf8>>
                    )
                    orelse gleam@string:ends_with(F, <<".app"/utf8>>))
                end
            ),
            {ok, _pipe@3} end
    ).

-spec run() -> {ok, nil} | {error, snag:snag()}.
run() ->
    gleam@result:'try'(
        begin
            _pipe = load_config(),
            snag:context(_pipe, <<"Failed to load configuration"/utf8>>)
        end,
        fun(Config) ->
            gleam@result:'try'(
                begin
                    _pipe@1 = locate_beam_files(),
                    snag:context(
                        _pipe@1,
                        <<"Failed to scan packages in build directory"/utf8>>
                    )
                end,
                fun(Files) ->
                    Emu_args = <<"-escript main gleescript_main_shim -env GLEESCRIPT_MAIN "/utf8,
                        (erlang:element(2, Config))/binary>>,
                    gleam@result:'try'(
                        gleam@list:try_map(
                            Files,
                            fun(F) ->
                                Name = unicode:characters_to_list(
                                    filepath:base_name(F)
                                ),
                                gleam@result:map(
                                    begin
                                        _pipe@2 = simplifile_erl:read_bits(F),
                                        _pipe@3 = snag_inspect_error(_pipe@2),
                                        snag:context(
                                            _pipe@3,
                                            <<"Failed to read "/utf8, F/binary>>
                                        )
                                    end,
                                    fun(Content) -> {Name, Content} end
                                )
                            end
                        ),
                        fun(Files@1) ->
                            gleam@result:'try'(
                                begin
                                    _pipe@4 = simplifile:create_directory_all(
                                        erlang:element(3, Config)
                                    ),
                                    _pipe@5 = snag_inspect_error(_pipe@4),
                                    snag:context(
                                        _pipe@5,
                                        <<<<"Failed to create "/utf8,
                                                (erlang:element(3, Config))/binary>>/binary,
                                            " directory"/utf8>>
                                    )
                                end,
                                fun(_) ->
                                    Result = escript:create(
                                        unicode:characters_to_list(
                                            <<(erlang:element(3, Config))/binary,
                                                (erlang:element(2, Config))/binary>>
                                        ),
                                        [shebang,
                                            {comment,
                                                unicode:characters_to_list(
                                                    <<""/utf8>>
                                                )},
                                            {emu_args,
                                                unicode:characters_to_list(
                                                    Emu_args
                                                )},
                                            {archive, Files@1, []}]
                                    ),
                                    _assert_subject = (gleam@dynamic:result(
                                        fun(Field@0) -> {ok, Field@0} end,
                                        fun(Field@0) -> {ok, Field@0} end
                                    ))(Result),
                                    {ok, Result@1} = case _assert_subject of
                                        {ok, _} -> _assert_subject;
                                        _assert_fail ->
                                            erlang:error(
                                                    #{gleam_error => let_assert,
                                                        message => <<"Assertion pattern match failed"/utf8>>,
                                                        value => _assert_fail,
                                                        module => <<"gleescript"/utf8>>,
                                                        function => <<"run"/utf8>>,
                                                        line => 77}
                                                )
                                    end,
                                    gleam@result:'try'(
                                        begin
                                            _pipe@6 = Result@1,
                                            _pipe@7 = snag_inspect_error(
                                                _pipe@6
                                            ),
                                            snag:context(
                                                _pipe@7,
                                                <<"Failed to build escript"/utf8>>
                                            )
                                        end,
                                        fun(_) ->
                                            Name@1 = <<(erlang:element(
                                                    3,
                                                    Config
                                                ))/binary,
                                                (erlang:element(2, Config))/binary>>,
                                            gleam@result:'try'(
                                                begin
                                                    _pipe@8 = simplifile_erl:set_permissions_octal(
                                                        Name@1,
                                                        8#777
                                                    ),
                                                    _pipe@9 = snag_inspect_error(
                                                        _pipe@8
                                                    ),
                                                    snag:context(
                                                        _pipe@9,
                                                        <<<<"Failed to make "/utf8,
                                                                Name@1/binary>>/binary,
                                                            " executable"/utf8>>
                                                    )
                                                end,
                                                fun(_) ->
                                                    gleam@io:println(
                                                        <<<<"  \x{001b}[95mGenerated\x{001b}[0m "/utf8,
                                                                (erlang:element(
                                                                    3,
                                                                    Config
                                                                ))/binary>>/binary,
                                                            (erlang:element(
                                                                2,
                                                                Config
                                                            ))/binary>>
                                                    ),
                                                    {ok, nil}
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-spec main() -> nil.
main() ->
    case run() of
        {ok, _} ->
            nil;

        {error, Error} ->
            gleam@io:println_error(snag:pretty_print(Error)),
            init:stop(1)
    end.
