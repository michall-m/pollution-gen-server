%%%-------------------------------------------------------------------
%%% @author michalmisiak
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. maj 2021 16:36
%%%-------------------------------------------------------------------
-module(pollution_value_collector_gen_statem).
-author("michalmisiak").
-behaviour(gen_statem).

%% API
-compile(export_all).

start_link() ->
  gen_statem:start_link({local, pollution_value_collector_gen_statem},
                         ?MODULE, [], []).
stop() ->
  gen_statem:stop(pollution_value_collector_gen_statem).

setStation(StationID) ->
  gen_statem:cast(?MODULE, {setStation, StationID}).

addValue(Date, Type, Value) ->
  gen_statem:cast(addValue, {Date, Type, Value}).

storeData() ->
  gen_statem:cast(?MODULE, flush_data).

init(_Args) ->
  {ok, unlocked, []}.

callback_mode() ->
  state_functions.

%% handlers
terminate(Reason, StateName, StateData) ->
  ok.

%% states
unlocked(_Event, {setStation, StationID}, _Data) ->
  {next_state, locked, #{stationID => StationID, measurements => []}}.

locked(_Event, {addValue, Date, Type, Value}, #{stationID := StationID, measurements := Measurements} = Data) ->
  {next_state, locked, Data#{measurements => [{Date, Type, Value} | Measurements]}};
locked(_Event, flush_data, #{stationID := StationID, measurements := Measurements} = Data) ->
  lists:foreach(fun({Date, Type, Value}) -> pollution_gen_server:addValue(StationID, Date, Type, Value) end, Measurements),
  {next_state, unlocked, []}.



