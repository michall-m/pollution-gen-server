%%%-------------------------------------------------------------------
%% @doc pollution_gen_server public API
%% @end
%%%-------------------------------------------------------------------

-module(pollution_gen_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    pollution_gen_server_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
