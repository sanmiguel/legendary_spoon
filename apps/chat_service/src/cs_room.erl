-module(cs_room).

-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export(
   [
    members/1,
    join/3,
    leave/2,
    broadcast/2
   ]).

-type room() :: string().

start_link(NameStr) when is_list(NameStr) ->
    Name = list_to_atom(NameStr),
    gen_server:start_link({local, Name}, ?MODULE, [Name, []], []).

-spec join(Room::atom() | pid(), Client::pid(), Opts::list()) ->
    {ok, Info :: term()}
    | {error, Reason :: term()}.
join(Room, Client, Opts) when is_pid(Room); is_atom(Room) ->
    gen_server:call(Room, {join, Client, Opts}).

-spec leave(Room :: room(), Client :: pid()) ->
    ok
    | {error, Reason :: term()}.
leave(Room, Client) ->
    gen_server:call(Room, {leave, Client}).

-spec members(Room :: pid() | atom()) ->
    {ok, list(pid())}
    | {error, Reason :: term()}.
members(Room) when is_pid(Room) ; is_atom(Room) ->
    gen_server:call(Room, list_members).

-spec broadcast(Room :: room(), Message :: binary()) ->
    ok.
broadcast(Room, Message) ->
    gen_server:call({?MODULE, Room}, {broadcast, Message}).

init([Name, Opts]) ->
    {ok, #{
       name => Name,
       clients => [],
       opts => Opts
      }}.

handle_call(list_members, _From, #{ clients := Cs }=St0) ->
    {reply, {ok, Cs}, St0};
handle_call({join, Client, Opts}, _From, #{ clients := Cs }=St0) ->
    error_logger:info_msg("Client[~p] joined ~s with opts ~p", [Client, maps:get(name, St0), Opts]),
    {reply, ok, St0#{ clients => [ Client | Cs ]}};
handle_call({leave, Client}, _From, #{ clients := Cs }=St0) ->
    error_logger:info_msg("Client[~p] leaving ~s", [Client, maps:get(name, St0)]),
    {reply, ok, St0#{ clients => Cs -- [Client] }};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.  
code_change(_OldVsn, State, _Extra) -> {ok, State}.

-ifdef(TEST).

room_members_basic_test() ->
    {ok, R} = cs_room:start_link("room_members_basic_test"),
    ?assertEqual({ok, []}, cs_room:members(R)).

room_join_test() ->
    {ok, R} = cs_room:start_link("Foo"),
    ok = cs_room:join(R, self(), []),
    ?assertEqual({ok, [self()]}, cs_room:members(R)).

-endif.
