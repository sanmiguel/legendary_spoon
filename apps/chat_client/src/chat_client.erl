-module(chat_client).

-behaviour(websocket_client).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
         start_link/2,
         is_connected/1
        ]).

%% Behaviour callbacks
-export(
   [
    init/1,
    onconnect/2,
    ondisconnect/2,
    websocket_handle/3,
    websocket_info/3,
    websocket_terminate/3
   ]).

-type state() :: #{}.

-spec start_link(URI::iolist(), Args::list()) ->
    {ok, pid()}
    | {error, Reason::term()}.
start_link(URI, Args) ->
    websocket_client:start_link(URI, ?MODULE, Args).

-spec is_connected(pid()) -> boolean().
is_connected(Pid) ->
    Pid ! { is_connected, self() },
    receive { is_connected, Pid, Bool } ->
                {ok, Bool }
    after 5000 ->
              {error, timeout}
    end.

-spec init(Args::list()) ->
    {reconnect, state()}.
init(_) ->
    {reconnect, #{connected => false}}.

-spec onconnect(WSReq :: websocket_req:req(),
                state()) ->
    {ok, state()}.
onconnect(_WSReq, #{}=St0) ->
    {ok, St0#{connected=>true}}.

-spec ondisconnect(Reason::term(), state()) ->
    {ok, state()}.
ondisconnect(Reason, #{}=St0) ->
    error_logger:info_msg("Client[~p] disconnected: ~p", [self(), Reason]),
    {ok, St0#{connected=>false}}.

-spec websocket_handle(WSFrame::term(),
                       websocket_req:req(),
                       state()) ->
    {ok, state()}.
websocket_handle(_Frame, _WSReq, #{}=St0) ->
    {ok, St0}.


-spec websocket_info(Info::any(), websocket_req:req(), state()) ->
    {ok, state()}.
websocket_info({is_connected, From}, _WSReq, #{connected:=Conn}=St0) ->
    From ! { is_connected, self(), Conn },
    {ok, St0};
websocket_info(Info, _WSReq, #{}=St0) ->
    error_logger:info_msg("Client[~p] received info [~p]", [self(), Info]),
    {ok, St0}.

-spec websocket_terminate(Reason :: term(), websocket_req:req(), state()) ->
    ok.
websocket_terminate(Reason, _WSReq, St0) ->
    error_logger:error_msg("Client[~p] terminated with [~p] while in state [~p]", [self(), Reason, St0]),
    ok.


-ifdef(TEST).

connect_test() ->
    {ok, Pid} = ?MODULE:start_link("ws://localhost:8080/chat/test_room", []),
    ?assertMatch({ok, true}, wait_for_connected(Pid, 10)).


supports_multi_user_test() ->
    Clients = [
               {ok, _} = ?MODULE:start_link("ws://localhost:8080/chat/test_room", [])
               || _ <- lists:seq(1, 5) ],
    [ ?assertMatch({ok, true}, wait_for_connected(C, 5))
      || {ok, C} <- Clients ].


wait_for_connected(_, 0) -> {ok, false};
wait_for_connected(Pid, Retries) ->
    case ?MODULE:is_connected(Pid) of
        {ok, true} -> {ok, true};
        {ok, false} ->
            timer:sleep(100),
            wait_for_connected(Pid, Retries-1);
        Other -> Other
    end.

-endif.
