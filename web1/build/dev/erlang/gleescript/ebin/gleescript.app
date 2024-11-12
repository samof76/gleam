{application, gleescript, [
    {vsn, "1.4.0"},
    {applications, [argv,
                    filepath,
                    gleam_erlang,
                    gleam_stdlib,
                    simplifile,
                    snag,
                    tom]},
    {description, "Bundle your Gleam-on-Erlang project into an escript, a single executable file!"},
    {modules, [gleescript]},
    {registered, []}
]}.
