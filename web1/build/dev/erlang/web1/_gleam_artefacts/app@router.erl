-module(app@router).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([handle_request/1]).

-spec handle_request(gleam@http@request:request(wisp@internal:connection())) -> gleam@http@response:response(wisp:body()).
handle_request(Req) ->
    app@web:middleware(
        Req,
        fun(_) ->
            Body = gleam@string_builder:from_string(
                <<"<h1>Hello wisp</h1>"/utf8>>
            ),
            wisp:html_response(Body, 200)
        end
    ).
