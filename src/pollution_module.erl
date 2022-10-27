%%%-------------------------------------------------------------------
%%% @author michalmisiak
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. kwi 2021 11:40
%%%-------------------------------------------------------------------
-module(pollution_module).
-author("michalmisiak").

%% API
-export([createMonitor/0, addStation/3, addValue/5, identifyStation/2,
         removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3,
         getMinimumDistanceStations/1, getTypesOfMeasurement/1,
         getDailyAmountOfMeasurementTypes/2]).

%% records
-record(measurement, {type, value, date}).
-record(station, {name, loc, measurements}).
-record(monitor, {stations}).

createMonitor() -> #monitor{stations = []}.

addStation(TargetMonitor, Name, Coordinates) ->
  CurrentStations = TargetMonitor#monitor.stations,
  Qualify = lists:any(fun(#station{name = Sname, loc = Coords}) -> (Sname == Name) or (Coords == Coordinates) end, CurrentStations),
  case Qualify of
    true -> {error, "Station exists"};
    false ->
      UpdatedMonitor = #monitor{stations = [#station{name = Name, loc = Coordinates, measurements = []} | CurrentStations]},
      UpdatedMonitor
  end.

identifyStation(Monitor, {X, Y}) ->
  ThisStation = lists:filter(fun(#station{loc = {LX, LY}}) -> ((LX == X) and (LY == Y)) end, Monitor#monitor.stations),
  ThisStation;
identifyStation(Monitor, Name) ->
  ThisStation = lists:filter(fun(#station{name = Sname}) -> Sname == Name end, Monitor#monitor.stations),
  ThisStation.



addValue(Monitor, StationID, Date, Type, Value) ->
  [FoundStation | _ ] = identifyStation(Monitor, StationID),
  Measurements = FoundStation#station.measurements,
  NewMeasurement = #measurement{type = Type, value = Value, date = Date},
  Check = fun(Meas) -> Meas =:= NewMeasurement end,
  case lists:any(Check, Measurements) of
    true -> {error, "Measurement exists"};
    false ->
      UpdatedStation = FoundStation#station{measurements = [NewMeasurement | Measurements]},
      AnotherStations = fun (Stat) -> not (FoundStation =:= Stat) end,
      UpdatedMonitor = #monitor{stations = [UpdatedStation | lists:filter(AnotherStations, Monitor#monitor.stations)]},
      UpdatedMonitor
  end.


removeValue(Monitor, StationID, Date, Type) ->
  [FoundStation | _ ] = identifyStation(Monitor, StationID),
  AnotherMeasurements = fun(#measurement{date = Cdate, type = Ctype}) -> not ((Cdate == Date) and (Ctype == Type)) end,
  UpdatedMeasurements = lists:filter(AnotherMeasurements, FoundStation#station.measurements),
  UpdatedStation = FoundStation#station{measurements = UpdatedMeasurements},
  AnotherStations = fun(Stat) -> not Stat =:= FoundStation end,
  UpdatedMonitor = #monitor{stations = [UpdatedStation | lists:filter(AnotherStations, Monitor#monitor.stations)]},
  UpdatedMonitor.

getOneValue(Monitor, StationID, Date, Type) ->
  [FoundStation | _ ] = identifyStation(Monitor, StationID),
  ThisMeasurements = fun(#measurement{date = Cdate, type = Ctype}) -> (Cdate == Date) and (Ctype == Type) end,
  [OneMeas | _] = lists:filter(ThisMeasurements, FoundStation#station.measurements),
  OneMeas.

getStationMean(Monitor, StationID, Type) ->
  [FoundStation | _ ] = identifyStation(Monitor, StationID),
  Measurements = FoundStation#station.measurements,
  CheckType = fun(#measurement{type = Ctype}) -> Ctype == Type end,
  MeasurementsOfType = lists:filter(CheckType, Measurements),
  Sum = fun(#measurement{value = Val}, {S, I}) -> {S + Val, I + 1} end,
  {SumOfValues, NumberOfValues} = lists:foldl(Sum, {0,0}, MeasurementsOfType),
  SumOfValues/NumberOfValues.

getDailyMean(Monitor, Day, Type) ->
  CollectMeasurements = fun(#station{measurements = M}, Acc) -> M ++ Acc end,
  CollectedMeasurements = lists:foldl(CollectMeasurements, [], Monitor#monitor.stations),
  CheckTypeAndDate = fun(#measurement{type = Ctype, date = {Cday, _}}) -> (Ctype == Type) and (Cday == Day) end,
  MeasurementsOfType = lists:filter(CheckTypeAndDate, [], CollectedMeasurements),
  Sum = fun(#measurement{value = Val}, {S, I}) -> {S + Val, I + 1} end,
  {SumOfValues, NumberOfValues} = lists:foldl(Sum, {0,0}, MeasurementsOfType),
  SumOfValues/NumberOfValues.

getMinimumDistanceStations(Monitor) ->
  Stations = Monitor#monitor.stations,
  [First | Pairs] = [{X, Y} || X <- Stations, Y <- Stations, not (X =:= Y)],
  CalculateDistance = fun(#station{loc = {X1, Y1}}, #station{loc = {X2, Y2}}) ->
                          math:pow((X1 - X2), 2) + math:pow((Y1 - Y2), 2) end,
  CompareDistance = fun({S1, S2}, {O1, O2}) ->
                      case CalculateDistance(S1, S2) < CalculateDistance(O1, O2) of
                        true -> {S1, S2};
                        _ -> {O1, O2}
                      end
                     end,
  MinimumDistanceStations = lists:foldl(CompareDistance, First, Pairs),
  MinimumDistanceStations.


%Zarejestrowane typy pomiarów
getTypesOfMeasurement(Monitor) ->
  CollectMeasurements = fun(#station{measurements = M}, Acc) -> M ++ Acc end,
  CollectedMeasurements = lists:foldl(CollectMeasurements, [], Monitor#monitor.stations),
  Types = lists:usort([T || #measurement{type = T} <- CollectedMeasurements]),
  Types.


%Pomiary danego typu w danym dniu
getDailyAmountOfMeasurementTypes(Monitor, Day) ->
  CollectMeasurements = fun(#station{measurements = M}, Acc) -> M ++ Acc end,
  CollectedMeasurements = lists:foldl(CollectMeasurements, [], Monitor#monitor.stations),
  Types = getTypesOfMeasurement(Monitor),
  CheckTypeAndDate = fun(#measurement{type = Ctype, date = {Cday, _}}, Type, {Date, _}) -> (Ctype == Type) and (Cday == Date) end,
  TM = [{T, [X#measurement.value || X <- CollectedMeasurements, CheckTypeAndDate(X, T, Day)]} || T <- Types],
  TM.
