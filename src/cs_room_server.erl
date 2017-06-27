-module(cs_room_server).

-export([
         init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3
        ]).

init(_, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_Transport, Req, _Opts) ->
    {ok, Req, #{}}.

websocket_handle(Frame, Req, State) ->
    error_logger:info_msg("Server[~p] received frame: ~p", [self(), Frame]),
    {ok, Req, State}.

websocket_info(Info, Req, State) ->
    error_logger:info_msg("Server[~p] received info: ~p", [self(), Info]),
    {ok, Req, State}.

websocket_terminate(Reason, _Req, _State) ->
    error_logger:error_msg("Server[~p] terminating with reason: ~p", [self(), Reason]),
    ok.
