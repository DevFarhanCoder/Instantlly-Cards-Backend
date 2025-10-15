%%%-------------------------------------------------------------------
%%% @doc message_gateway top level supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(message_gateway_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
    ChildSpecs = [
        #{
            id => message_router,
            start => {message_router, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [message_router]
        },
        #{
            id => session_manager,
            start => {session_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [session_manager]
        },
        #{
            id => presence_tracker,
            start => {presence_tracker, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [presence_tracker]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
