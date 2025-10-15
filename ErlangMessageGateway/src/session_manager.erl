%%%-------------------------------------------------------------------
%%% @doc Session Manager - Tracks active WebSocket connections
%%% @end
%%%-------------------------------------------------------------------
-module(session_manager).
-behaviour(gen_server).

-export([start_link/0, add_session/3, remove_session/2, get_sessions/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    sessions :: #{binary() => [{binary(), pid()}]}
}).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_session(UserId, DeviceId, Pid) ->
    gen_server:call(?MODULE, {add_session, UserId, DeviceId, Pid}).

remove_session(UserId, DeviceId) ->
    gen_server:call(?MODULE, {remove_session, UserId, DeviceId}).

get_sessions(UserId) ->
    gen_server:call(?MODULE, {get_sessions, UserId}).

%% Callbacks
init([]) ->
    io:format("👥 Session Manager started~n"),
    {ok, #state{sessions = #{}}}.

handle_call({add_session, UserId, DeviceId, Pid}, _From, State) ->
    Sessions = State#state.sessions,
    UserSessions = maps:get(UserId, Sessions, []),
    
    %% Monitor the process
    erlang:monitor(process, Pid),
    
    %% Add new session
    NewUserSessions = [{DeviceId, Pid} | lists:keydelete(DeviceId, 1, UserSessions)],
    NewSessions = maps:put(UserId, NewUserSessions, Sessions),
    
    io:format("➕ Session added: User ~s, Device ~s (~p active sessions)~n", 
              [UserId, DeviceId, length(NewUserSessions)]),
    
    {reply, ok, State#state{sessions = NewSessions}};

handle_call({remove_session, UserId, DeviceId}, _From, State) ->
    Sessions = State#state.sessions,
    UserSessions = maps:get(UserId, Sessions, []),
    NewUserSessions = lists:keydelete(DeviceId, 1, UserSessions),
    
    NewSessions = case NewUserSessions of
        [] -> maps:remove(UserId, Sessions);
        _ -> maps:put(UserId, NewUserSessions, Sessions)
    end,
    
    io:format("➖ Session removed: User ~s, Device ~s~n", [UserId, DeviceId]),
    {reply, ok, State#state{sessions = NewSessions}};

handle_call({get_sessions, UserId}, _From, State) ->
    Sessions = maps:get(UserId, State#state.sessions, []),
    {reply, Sessions, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% Remove crashed/disconnected session
    Sessions = State#state.sessions,
    NewSessions = maps:map(
        fun(_UserId, UserSessions) ->
            lists:filter(fun({_DeviceId, P}) -> P =/= Pid end, UserSessions)
        end,
        Sessions
    ),
    %% Remove empty user entries
    CleanedSessions = maps:filter(fun(_, V) -> V =/= [] end, NewSessions),
    {noreply, State#state{sessions = CleanedSessions}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
