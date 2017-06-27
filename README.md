chat_client
=====

An OTP application

Build
-----

    $ rebar3 compile


Run
---

    $ rebar3 shell
    % In another shell, start the chat service which defaults to port 8080
    1> {ok, C} = chat_client:start_link("ws://localhost:8080/chat/foobar", []).

    % Check the state of the client:
    > chat_client:is_connected(C).
    {ok,true}

    % The client should automatically reconnect to the server in the event of transport error
    % We didn't get as far as an active disconnect, but killing the client process should suffice:
    > gen_fsm:stop(C).
    ok
