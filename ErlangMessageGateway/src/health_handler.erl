%%%-------------------------------------------------------------------
%%% @doc Health check handler
%%% @end
%%%-------------------------------------------------------------------
-module(health_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
    {ok, AllPresence} = presence_tracker:get_all_presence(),
    OnlineUsers = length([U || {U, {online, _}} <- maps:to_list(AllPresence)]),
    
    Response = jsx:encode(#{
        status => <<"healthy">>,
        service => <<"erlang_message_gateway">>,
        version => <<"1.0.0">>,
        timestamp => erlang:system_time(millisecond),
        metrics => #{
            online_users => OnlineUsers,
            uptime_seconds => element(1, erlang:statistics(wall_clock)) div 1000
        }
    }),
    
    Req2 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Response, Req),
    
    {ok, Req2, State}.
