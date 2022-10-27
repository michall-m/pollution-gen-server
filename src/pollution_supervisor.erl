%%%-------------------------------------------------------------------
%%% @author michalmisiak
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. maj 2021 15:40
%%%-------------------------------------------------------------------
-module(pollution_supervisor).
-author("michalmisiak").
-behaviour(supervisor).

%% API
-export([start_link/0, init/1]).

%%%% pollutionSupervisor ? pollution_supervisor, które nazewnictwo jest poprawne?
start_link() ->
  supervisor:start_link({local, pollutionSupervisor}, pollution_supervisor, []).

init(_Args) ->
  {ok, {
    #{strategy => one_for_one,
      intensity => 2,
      period => 3
    },
    [#{id => 'pollution_gen_server',
          start => {pollution_gen_server, start, []},
          restart => permanent,
          shutdown => 2000,
          type => worker,
          modules => [pollution_gen_server]} ,
     #{id => 'pollution_value_collector_gen_statem',
          start => {pollution_value_collector_gen_statem, start_link, []},
          restart => permanent,
          shutdown => 2000,
          type => worker,
          modules => [pollution_value_collector_gen_statem]} ]} }.



