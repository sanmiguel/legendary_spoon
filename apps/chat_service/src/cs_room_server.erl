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
    %% Pull room name from path as 'room'
    case cowboy_req:binding(room, Req) of
        undefined ->
            {error, unset_room};
        {RoomName, Req1} when is_binary(RoomName) ->
            %% Fetch PID for room name
            {ok, Room} = cs_rooms:create(binary_to_list(RoomName)),
            ok = cs_room:join(Room, self(), []),
            {ok, Req1, #{ room => Room }}
    end.

websocket_handle(Frame, Req, State) ->
    error_logger:info_msg("Server[~p] received frame: ~p", [self(), Frame]),
    {ok, Req, State}.

websocket_info(Info, Req, State) ->
    error_logger:info_msg("Server[~p] received info: ~p", [self(), Info]),
    {ok, Req, State}.

websocket_terminate(Reason, _Req, #{room := Room}=_State) ->
    error_logger:error_msg("Server[~p] terminating with reason: ~p", [self(), Reason]),
    ok = cs_room:leave(Room, self()).
