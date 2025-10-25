%%%-------------------------------------------------------------------
%%% @doc Presence Tracker - Tracks user online/offline status
%%% @end
%%%-------------------------------------------------------------------
-module(presence_tracker).
-behaviour(gen_server).

-export([start_link/0, user_online/1, user_offline/1, get_presence/1, get_all_presence/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    presence :: #{binary() => {online | offline, integer()}}
}).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

user_online(UserId) ->
    gen_server:cast(?MODULE, {user_online, UserId}).

user_offline(UserId) ->
    gen_server:cast(?MODULE, {user_offline, UserId}).

get_presence(UserId) ->
    gen_server:call(?MODULE, {get_presence, UserId}).

get_all_presence() ->
    gen_server:call(?MODULE, get_all_presence).

%% Callbacks
init([]) ->
    io:format("🟢 Presence Tracker started~n"),
    {ok, #state{presence = #{}}}.

handle_call({get_presence, UserId}, _From, State) ->
    Presence = maps:get(UserId, State#state.presence, {offline, 0}),
    {reply, Presence, State};

handle_call(get_all_presence, _From, State) ->
    {reply, State#state.presence, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({user_online, UserId}, State) ->
    Timestamp = erlang:system_time(millisecond),
    Presence = State#state.presence,
    NewPresence = maps:put(UserId, {online, Timestamp}, Presence),
    
    io:format("🟢 User ~s is now ONLINE~n", [UserId]),
    
    %% Broadcast presence update
    PresenceMsg = #{
        type => <<"presence_update">>,
        userId => UserId,
        status => <<"online">>,
        timestamp => Timestamp
    },
    broadcast_presence(PresenceMsg),
    
    {noreply, State#state{presence = NewPresence}};

handle_cast({user_offline, UserId}, State) ->
    Timestamp = erlang:system_time(millisecond),
    Presence = State#state.presence,
    NewPresence = maps:put(UserId, {offline, Timestamp}, Presence),
    
    io:format("🔴 User ~s is now OFFLINE~n", [UserId]),
    
    %% Broadcast presence update
    PresenceMsg = #{
        type => <<"presence_update">>,
        userId => UserId,
        status => <<"offline">>,
        timestamp => Timestamp
    },
    broadcast_presence(PresenceMsg),
    
    {noreply, State#state{presence = NewPresence}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% Internal functions
broadcast_presence(PresenceMsg) ->
    %% TODO: Broadcast to all connected users or relevant contacts
    %% For now, just log
    ok.
