%%%-------------------------------------------------------------------
%%% @author michalmisiak
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. kwi 2021 11:23
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("michalmisiak").
-behaviour(gen_server).
-compile(export_all).

%% API


%% user interface
% start / stop
start() ->
  InitialMonitor = pollution_module:createMonitor(),
  start_link(InitialMonitor).

stop() ->
  gen_server:cast(pollution_gen_server, stop).

% pollution_module calls
addStation(Name, Coordinates) ->
  gen_server:call(pollution_gen_server, {addStation, Name, Coordinates}).

addValue(StationID, Date, Type, Value) ->
  gen_server:call(pollution_gen_server, {addValue, StationID, Date, Type, Value}).

getOneValue(StationID, Date, Type) ->
  gen_server:call(pollution_gen_server, {getOneValue,StationID, Date, Type}).

removeValue(StationID, Date, Type) ->
  gen_server:call(pollution_gen_server, {removeValue, StationID, Date, Type}).

getStationMean(StationID, Type) ->
  gen_server:call(pollution_gen_server, {getStationMean, StationID, Type}).

getDailyMean(Day, Type) ->
  gen_server:call(pollution_gen_server, {getDailyMean, Day, Type}).

getMinimumDistanceStations() ->
  gen_server:call(pollution_gen_server, {getMinimumDistanceStations}).

getTypesOfMeasurement() ->
  gen_server:call(pollution_gen_server, {getTypesOfMeasurement}).

getDailyAmountOfMeasurementTypes(Day) ->
  gen_server:call(pollution_gen_server, {getDailyAmountOfMeasurementTypes, Day}).

%% callbacks
% initial value
init(InitialMonitor) ->
  {ok, InitialMonitor}.

start_link(InitialMonitor) ->
  gen_server:start_link({local, pollution_gen_server}, pollution_gen_server, InitialMonitor, []).

start_link() ->
  start().

% terminate server
handle_cast(stop, PollutionMonitor) ->
  {stop, normal, PollutionMonitor}.

terminate(Reason, Value) ->
  io:format("Server: exit with value ~p~n", [Value]),
  Reason.

% crash
crash() ->
  1/0.

%  pollution_module functions
handle_call({addStation, Name, Coordinates}, _From, PollutionMonitor) ->
  case pollution_module:addStation(PollutionMonitor, Name, Coordinates) of
    {error, ErrorMessage} ->
      {reply, {error, ErrorMessage}, PollutionMonitor};
    UpdatedMonitor ->
      {reply, ok, UpdatedMonitor}
  end;

handle_call({addValue, StationID, Date, Type, Value}, _From, PollutionMonitor) ->
  case pollution_module:addValue(PollutionMonitor, StationID, Date, Type, Value) of
    {error, ErrorMessage} ->
      {reply, {error, ErrorMessage}, PollutionMonitor};
    UpdatedMonitor ->
      {reply, ok, UpdatedMonitor}
  end;


handle_call({removeValue, StationID, Date, Type}, _From, PollutionMonitor) ->
  case pollution_module:removeValue(PollutionMonitor, StationID, Date, Type) of
    {error, ErrorMessage} ->
      {reply, {error, ErrorMessage}, PollutionMonitor};
    UpdatedMonitor ->
      {reply, ok, UpdatedMonitor}
  end;

handle_call({getOneValue, StationID, Date, Type}, _From, PollutionMonitor) ->
  {reply, pollution_module:getOneValue(PollutionMonitor, StationID, Date, Type), PollutionMonitor};

handle_call({getStationMean, StationID, Type}, _From, PollutionMonitor) ->
  {reply, pollution_module:getStationMean(PollutionMonitor, StationID, Type), PollutionMonitor};

handle_call({getDailyMean, Day, Type}, _From, PollutionMonitor) ->
  {reply, pollution_module:getDailyMean(PollutionMonitor, Day, Type), PollutionMonitor};

handle_call({getMinimumDistanceStations}, _From, PollutionMonitor) ->
  {reply, pollution_module:getMinimumDistanceStations(PollutionMonitor), PollutionMonitor};

handle_call({getTypesOfMeasurement}, _From, PollutionMonitor) ->
  {reply, pollution_module:getTypesOfMeasurement(PollutionMonitor), PollutionMonitor};

handle_call({getDailyAmountOfMeasurementTypes, Day}, _From, PollutionMonitor) ->
  {reply, pollution_module:getDailyAmountOfMeasurementTypes(Day), PollutionMonitor}.






