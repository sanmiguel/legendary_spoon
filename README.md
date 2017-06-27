chat_service
=====

An OTP application

Build
-----

    $ rebar3 compile

Run
---

    $ rebar3 shell
    1> application:ensure_all_started(chat_service).

    % To see room processes started:
    > supervisor:which_children(cs_rooms).

    % To see the members of a specific room by name (atom) e.g. 'foo':
    > cs_room:members(foo).
    {ok,[]}
