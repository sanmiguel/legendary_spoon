-module(cs_rooms).

-behaviour(supervisor).

-export(
   [
    start_link/0,
    create/1
   ]).

-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

create(Name) when is_list(Name) ->
    case supervisor:start_child(?MODULE, [Name]) of
        {error, {already_started, Pid}} ->
            {ok, Pid};
        {ok, Pid} -> {ok, Pid};
        {error, _} = Error ->
            Error
    end.

init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    RoomSpec = {somename, {cs_room, start_link, []},
                Restart, Shutdown, Type, [cs_room]},

    {ok, {SupFlags, [RoomSpec]}}.
