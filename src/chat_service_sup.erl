%%%-------------------------------------------------------------------
%% @doc chat_service top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chat_service_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Dispatch = cowboy_router:compile(
                 [{'_',
                   [
                    {"/chat/[:room]", cs_room_server, #{}}
                   ]}]),
    RanchOptions = [{port, application:get_env(chat_service, port, 8080)}, {nodelay, true}],
    CowboyEnv = [{env, [{dispatch, Dispatch},
                        {max_keepalive, 100},
                        {compress, false},
                        {timeout, 30000}]}],
    {ok, { {one_for_one, 5, 10},
           [
            ranch:child_spec(
              cs_room_server,
              50,
              ranch_tcp,
              RanchOptions,
              cowboy_protocol,
              CowboyEnv
             )
           ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
