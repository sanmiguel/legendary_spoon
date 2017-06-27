-module(chat_client).

-behaviour(websocket_client).

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

-spec init(Args::list()) ->
    {reconnect, state()}.
init(_) ->
    {reconnect, #{}}.

-spec onconnect(WSReq :: websocket_req:req(),
                state()) ->
    {ok, state()}.
onconnect(_WSReq, #{}=St0) ->
    {ok, St0}.

-spec ondisconnect(Reason::term(), state()) ->
    {ok, state()}.
ondisconnect(Reason, #{}=St0) ->
    error_logger:info_msg("Client[~p] disconnected: ~p", [self(), Reason]),
    {ok, St0}.

-spec websocket_handle(WSFrame::term(),
                       websocket_req:req(),
                       state()) ->
    {ok, state()}.
websocket_handle(_Frame, _WSReq, #{}=St0) ->
    {ok, St0}.


-spec websocket_info(Info::any(), websocket_req:req(), state()) ->
    {ok, state()}.
websocket_info(Info, _WSReq, #{}=St0) ->
    error_logger:info_msg("Client[~p] received info [~p]", [self(), Info]),
    {ok, St0}.

-spec websocket_terminate(Reason :: term(), websocket_req:req(), state()) ->
    ok.
websocket_terminate(Reason, _WSReq, St0) ->
    error_logger:error_msg("Client[~p] terminated with [~p] while in state [~p]", [self(), Reason, St0]),
    ok.
