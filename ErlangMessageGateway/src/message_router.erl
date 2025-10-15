%%%-------------------------------------------------------------------
%%% @doc Message Router - Routes messages to recipients
%%% @end
%%%-------------------------------------------------------------------
-module(message_router).
-behaviour(gen_server).

-export([start_link/0, route_message/1, send_to_user/2, update_receipt/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {}).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

route_message(Message) ->
    gen_server:cast(?MODULE, {route_message, Message}).

send_to_user(UserId, Message) ->
    gen_server:cast(?MODULE, {send_to_user, UserId, Message}).

update_receipt(MessageId, Status, UserId) ->
    gen_server:cast(?MODULE, {update_receipt, MessageId, Status, UserId}).

%% Callbacks
init([]) ->
    io:format("🚀 Message Router started~n"),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({route_message, Message}, State) ->
    #{
        message_id := MessageId,
        sender_id := SenderId,
        receiver_id := ReceiverId,
        content := Content,
        message_type := MessageType,
        timestamp := Timestamp
    } = Message,
    
    io:format("📨 Routing message ~s from ~s to ~s~n", [MessageId, SenderId, ReceiverId]),
    
    %% TODO: Save to PostgreSQL
    %% For now, just route to receiver
    
    %% Check if receiver is online
    case session_manager:get_sessions(ReceiverId) of
        [] ->
            io:format("📪 User ~s offline, queueing message~n", [ReceiverId]),
            %% TODO: Queue in Redis for offline delivery
            ok;
        Sessions ->
            %% Send to all active sessions
            DeliveryMsg = #{
                type => <<"new_message">>,
                messageId => MessageId,
                senderId => SenderId,
                receiverId => ReceiverId,
                content => Content,
                messageType => MessageType,
                timestamp => Timestamp,
                status => <<"delivered">>
            },
            [Pid ! {message, DeliveryMsg} || {_DeviceId, Pid} <- Sessions]
    end,
    
    {noreply, State};

handle_cast({send_to_user, UserId, Message}, State) ->
    case session_manager:get_sessions(UserId) of
        [] ->
            ok;
        Sessions ->
            [Pid ! {message, Message} || {_DeviceId, Pid} <- Sessions]
    end,
    {noreply, State};

handle_cast({update_receipt, MessageId, Status, UserId}, State) ->
    io:format("✅ Message ~s marked as ~s by ~s~n", [MessageId, Status, UserId]),
    %% TODO: Update in PostgreSQL
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
