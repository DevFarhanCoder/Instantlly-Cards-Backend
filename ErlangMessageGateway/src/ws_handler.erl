%%%-------------------------------------------------------------------
%%% @doc WebSocket handler for client connections
%%% Handles WebSocket upgrade, authentication, and message routing
%%% @end
%%%-------------------------------------------------------------------
-module(ws_handler).
-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2, 
         websocket_info/2, terminate/3]).

-record(state, {
    user_id :: binary(),
    device_id :: binary(),
    authenticated = false :: boolean()
}).

%% Cowboy callbacks
init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(_State) ->
    io:format("📱 New WebSocket connection~n"),
    {ok, #state{}}.

websocket_handle({text, Msg}, State) ->
    case jsx:decode(Msg, [return_maps]) of
        #{<<"type">> := <<"auth">>, <<"userId">> := UserId, <<"deviceId">> := DeviceId} ->
            handle_auth(UserId, DeviceId, State);
        Data when State#state.authenticated ->
            handle_message(Data, State);
        _ ->
            Reply = jsx:encode(#{
                type => <<"error">>,
                message => <<"Not authenticated">>
            }),
            {reply, {text, Reply}, State}
    end;

websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({message, Msg}, State) ->
    %% Received message from message_router to send to client
    Reply = jsx:encode(Msg),
    {reply, {text, Reply}, State};

websocket_info({presence, PresenceData}, State) ->
    %% Presence update
    Reply = jsx:encode(PresenceData),
    {reply, {text, Reply}, State};

websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, #state{user_id = UserId, device_id = DeviceId}) when UserId =/= undefined ->
    io:format("📴 User ~s disconnected (device: ~s)~n", [UserId, DeviceId]),
    session_manager:remove_session(UserId, DeviceId),
    presence_tracker:user_offline(UserId),
    ok;
terminate(_Reason, _Req, _State) ->
    ok.

%% Internal functions
handle_auth(UserId, DeviceId, State) ->
    io:format("🔐 Authenticating user: ~s (device: ~s)~n", [UserId, DeviceId]),
    
    %% Register session
    session_manager:add_session(UserId, DeviceId, self()),
    
    %% Update presence
    presence_tracker:user_online(UserId),
    
    %% Send authentication success
    Reply = jsx:encode(#{
        type => <<"auth_success">>,
        userId => UserId,
        deviceId => DeviceId,
        timestamp => erlang:system_time(millisecond)
    }),
    
    NewState = State#state{
        user_id = UserId,
        device_id = DeviceId,
        authenticated = true
    },
    
    {reply, {text, Reply}, NewState}.

handle_message(#{<<"type">> := <<"send_message">>} = Data, State) ->
    #{
        <<"messageId">> := MessageId,
        <<"receiverId">> := ReceiverId,
        <<"content">> := Content
    } = Data,
    
    MessageType = maps:get(<<"messageType">>, Data, <<"text">>),
    GroupId = maps:get(<<"groupId">>, Data, undefined),
    
    %% Create message payload
    Message = #{
        message_id => MessageId,
        sender_id => State#state.user_id,
        receiver_id => ReceiverId,
        group_id => GroupId,
        content => Content,
        message_type => MessageType,
        timestamp => erlang:system_time(millisecond),
        status => <<"sent">>
    },
    
    %% Route message
    message_router:route_message(Message),
    
    %% Send acknowledgment
    Ack = jsx:encode(#{
        type => <<"message_ack">>,
        messageId => MessageId,
        status => <<"sent">>,
        timestamp => erlang:system_time(millisecond)
    }),
    
    {reply, {text, Ack}, State};

handle_message(#{<<"type">> := <<"typing">>} = Data, State) ->
    #{<<"receiverId">> := ReceiverId} = Data,
    IsTyping = maps:get(<<"isTyping">>, Data, true),
    
    %% Notify receiver about typing status
    TypingMsg = #{
        type => <<"typing">>,
        senderId => State#state.user_id,
        isTyping => IsTyping
    },
    
    message_router:send_to_user(ReceiverId, TypingMsg),
    {ok, State};

handle_message(#{<<"type">> := <<"read_receipt">>} = Data, State) ->
    #{<<"messageId">> := MessageId} = Data,
    
    %% Update message status
    message_router:update_receipt(MessageId, <<"read">>, State#state.user_id),
    {ok, State};

handle_message(_Data, State) ->
    {ok, State}.
