%%%-------------------------------------------------------------------
%%% @doc Simple HTTP/WebSocket Server using gen_tcp
%%% Lightweight server that Node.js can connect to
%%% @end
%%%-------------------------------------------------------------------
-module(simple_websocket_server).
-export([start/0, start_link/0, stop/0]).
-export([accept_loop/1]).

start() ->
    start_link().

start_link() ->
    io:format("~n🌐 Starting WebSocket Server on port 8080...~n"),
    
    case gen_tcp:listen(8080, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]) of
        {ok, ListenSocket} ->
            io:format("✅ WebSocket Server listening on port 8080~n"),
            io:format("🔌 Node.js can now connect to ws://localhost:8080/ws~n~n"),
            
            % Start accepting connections in a new process
            Pid = spawn_link(?MODULE, accept_loop, [ListenSocket]),
            {ok, Pid};
        {error, Reason} ->
            io:format("❌ Failed to start server: ~p~n", [Reason]),
            io:format("   Port 8080 might be in use~n"),
            {error, Reason}
    end.

accept_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            io:format("📱 New connection from Node.js~n"),
            
            % Spawn a handler for this connection
            spawn(fun() -> handle_connection(Socket) end),
            
            % Continue accepting new connections
            accept_loop(ListenSocket);
        {error, Reason} ->
            io:format("Accept error: ~p~n", [Reason])
    end.

handle_connection(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("📩 Received HTTP request~n"),
            
            % Check if it's an HTTP request
            case binary:match(Data, <<"GET">>) of
                {_, _} ->
                    % Extract Sec-WebSocket-Key from headers
                    AcceptKey = case extract_websocket_key(Data) of
                        {ok, Key} ->
                            % RFC 6455: Concatenate key with magic GUID and hash with SHA-1
                            MagicString = <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>,
                            CombinedKey = <<Key/binary, MagicString/binary>>,
                            Hash = crypto:hash(sha, CombinedKey),
                            base64:encode(Hash);
                        error ->
                            % Fallback (should not happen with valid clients)
                            <<"s3pPLMBiTxaQ9kYGzzhZRbK+xOo=">>
                    end,
                    
                    % HTTP handshake - send WebSocket upgrade response
                    Response = [
                        <<"HTTP/1.1 101 Switching Protocols\r\n">>,
                        <<"Upgrade: websocket\r\n">>,
                        <<"Connection: Upgrade\r\n">>,
                        <<"Sec-WebSocket-Accept: ">>, AcceptKey, <<"\r\n">>,
                        <<"\r\n">>
                    ],
                    gen_tcp:send(Socket, Response),
                    io:format("✅ WebSocket handshake complete (key: ~s)~n", [AcceptKey]),
                    websocket_loop(Socket);
                nomatch ->
                    io:format("⚠️  Not an HTTP request~n"),
                    gen_tcp:close(Socket)
            end;
        {error, closed} ->
            io:format("🔌 Connection closed~n");
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end.

%% Extract Sec-WebSocket-Key from HTTP headers
extract_websocket_key(Data) ->
    case binary:split(Data, <<"\r\n">>, [global]) of
        Lines when is_list(Lines) ->
            find_websocket_key(Lines);
        _ ->
            error
    end.

find_websocket_key([]) ->
    error;
find_websocket_key([Line | Rest]) ->
    case binary:match(Line, <<"Sec-WebSocket-Key:">>) of
        {Pos, _Len} ->
            % Extract the key value (skip "Sec-WebSocket-Key: ")
            KeyStart = Pos + 19,
            KeyLen = byte_size(Line) - KeyStart,
            Key = binary:part(Line, KeyStart, KeyLen),
            % Trim whitespace
            {ok, string:trim(Key)};
        nomatch ->
            find_websocket_key(Rest)
    end.

websocket_loop(Socket) ->
    case gen_tcp:recv(Socket, 0, 30000) of
        {ok, Frame} ->
            case decode_websocket_frame(Frame) of
                {text, Payload} ->
                    io:format("📨 Received: ~s~n", [Payload]),
                    
                    % Parse JSON and route to message_router
                    handle_websocket_message(Payload, Socket),
                    
                    % Send acknowledgment
                    Response = encode_text_frame(<<"{\"type\":\"ack\"}">>),
                    gen_tcp:send(Socket, Response),
                    websocket_loop(Socket);
                {ping, _} ->
                    % Respond to ping with pong
                    Pong = <<16#8A, 0>>, % Pong frame with no payload
                    gen_tcp:send(Socket, Pong),
                    websocket_loop(Socket);
                {close, _} ->
                    io:format("🔌 Client requested close~n"),
                    gen_tcp:close(Socket);
                error ->
                    io:format("⚠️  Failed to decode frame~n"),
                    websocket_loop(Socket)
            end;
        {error, timeout} ->
            % Check for pending messages during timeout
            check_messages(Socket),
            websocket_loop(Socket);
        {error, closed} ->
            io:format("🔌 WebSocket closed~n");
        {error, Reason} ->
            io:format("WebSocket error: ~p~n", [Reason]),
            gen_tcp:close(Socket)
    end.

%% Check for messages from message_router
check_messages(Socket) ->
    receive
        {message, Msg} ->
            % Convert message map to JSON
            JSON = format_message_json(Msg),
            Response = encode_text_frame(JSON),
            gen_tcp:send(Socket, Response),
            io:format("📤 Sent message to client~n"),
            check_messages(Socket)
    after 0 ->
        ok
    end.

%% Format message as JSON
format_message_json(#{type := Type} = _Msg) ->
    % Simple JSON formatting
    TypeBin = atom_to_binary(Type),
    iolist_to_binary(["{\"type\":\"", TypeBin, "\"}"]).

%% Decode WebSocket frame (RFC 6455)
decode_websocket_frame(<<_Fin:1, _Rsv:3, Opcode:4, Mask:1, Len:7, Rest/binary>>) ->
    {PayloadLen, MaskAndPayload} = case Len of
        126 ->
            <<L:16, R/binary>> = Rest,
            {L, R};
        127 ->
            <<L:64, R/binary>> = Rest,
            {L, R};
        _ ->
            {Len, Rest}
    end,
    
    case Mask of
        1 ->
            % Client frames are masked
            <<MaskKey:32, Payload:PayloadLen/binary, _/binary>> = MaskAndPayload,
            UnmaskedPayload = unmask_payload(Payload, <<MaskKey:32>>),
            
            case Opcode of
                1 -> {text, UnmaskedPayload};      % Text frame
                2 -> {binary, UnmaskedPayload};    % Binary frame
                8 -> {close, UnmaskedPayload};     % Close frame
                9 -> {ping, UnmaskedPayload};      % Ping frame
                10 -> {pong, UnmaskedPayload};     % Pong frame
                _ -> error
            end;
        0 ->
            % Server frames should not be masked (but client might send unmasked - error)
            error
    end;
decode_websocket_frame(_) ->
    error.

%% Unmask WebSocket payload
unmask_payload(Payload, MaskKey) ->
    unmask_payload(Payload, MaskKey, 0, <<>>).

unmask_payload(<<>>, _MaskKey, _Index, Acc) ->
    Acc;
unmask_payload(<<Byte:8, Rest/binary>>, MaskKey, Index, Acc) ->
    <<_:Index/binary, MaskByte:8, _/binary>> = <<MaskKey/binary, MaskKey/binary, MaskKey/binary, MaskKey/binary>>,
    UnmaskedByte = Byte bxor MaskByte,
    unmask_payload(Rest, MaskKey, (Index + 1) rem 4, <<Acc/binary, UnmaskedByte>>).

%% Encode text frame for sending to client (server frames are NOT masked)
encode_text_frame(Payload) ->
    PayloadLen = byte_size(Payload),
    
    LenBytes = if
        PayloadLen =< 125 ->
            <<PayloadLen:7>>;
        PayloadLen =< 65535 ->
            <<126:7, PayloadLen:16>>;
        true ->
            <<127:7, PayloadLen:64>>
    end,
    
    % FIN=1, RSV=0, Opcode=1 (text), MASK=0 (server never masks)
    <<1:1, 0:3, 1:4, 0:1, LenBytes/bits, Payload/binary>>.

%% Handle WebSocket message (integrate with message_router)
handle_websocket_message(Payload, Socket) ->
    try
        % Parse JSON (simple parsing - in production use jsx library)
        io:format("🔍 Processing message: ~s~n", [Payload]),
        
        % Decode JSON manually (basic parser)
        case parse_json(Payload) of
            {ok, #{<<"type">> := <<"auth">>, <<"userId">> := UserId, <<"deviceId">> := DeviceId}} ->
                % Register session
                session_manager:add_session(UserId, DeviceId, self()),
                presence_tracker:user_online(UserId),
                io:format("✅ User ~s authenticated (device: ~s)~n", [UserId, DeviceId]),
                
                % Send auth success
                Response = encode_text_frame(<<"{\"type\":\"auth_success\",\"userId\":\"", UserId/binary, "\"}">>),
                gen_tcp:send(Socket, Response);
                
            {ok, #{<<"type">> := <<"send_message">>, <<"messageId">> := MsgId, 
                   <<"receiverId">> := ReceiverId, <<"content">> := Content}} ->
                % Route message through message_router
                Message = #{
                    message_id => MsgId,
                    receiver_id => ReceiverId,
                    content => Content,
                    timestamp => erlang:system_time(millisecond)
                },
                message_router:route_message(Message),
                io:format("📨 Message ~s routed to ~s~n", [MsgId, ReceiverId]);
                
            {ok, #{<<"type">> := <<"presence">>, <<"status">> := Status, <<"userId">> := UserId}} ->
                % Update presence
                case Status of
                    <<"online">> -> presence_tracker:user_online(UserId);
                    <<"offline">> -> presence_tracker:user_offline(UserId);
                    _ -> ok
                end;
                
            _ ->
                io:format("⚠️  Unknown message format~n")
        end
    catch
        _:Error ->
            io:format("⚠️  Error handling message: ~p~n", [Error])
    end.

%% Simple JSON parser (handles basic objects only)
parse_json(Bin) ->
    try
        % Remove whitespace and braces
        Trimmed = string:trim(Bin),
        Content = binary:part(Trimmed, 1, byte_size(Trimmed) - 2),
        
        % Split by comma
        Pairs = binary:split(Content, <<",">>, [global]),
        
        % Parse each key-value pair
        Map = lists:foldl(fun(Pair, Acc) ->
            case binary:split(Pair, <<":">>) of
                [Key, Value] ->
                    CleanKey = clean_json_string(Key),
                    CleanValue = clean_json_string(Value),
                    maps:put(CleanKey, CleanValue, Acc);
                _ -> Acc
            end
        end, #{}, Pairs),
        
        {ok, Map}
    catch
        _:_ -> error
    end.

%% Remove quotes and whitespace from JSON strings
clean_json_string(Bin) ->
    Trimmed = string:trim(Bin),
    Size = byte_size(Trimmed),
    if
        Size >= 2 ->
            case binary:at(Trimmed, 0) of
                $" -> binary:part(Trimmed, 1, Size - 2);
                _ -> Trimmed
            end;
        true -> Trimmed
    end.

stop() ->
    ok.
