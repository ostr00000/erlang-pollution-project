-module(pollution).

-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4,
        getStationMean/3, getDailyMean/3, getHourlyStationData/3, test/1]).


% typeToDay -> DayToTime -> timeToVal
-record(station, {mapTypeToDay}).

-record(monitor, {posToName, nameToStation}).



createMonitor() -> #monitor{posToName=#{}, nameToStation=#{}}.



addStation(Name, Position={_, _}, Monitor) ->
    try maps:is_key(Name, Monitor#monitor.nameToStation) of
        true -> {error, {station_name_used, Name}};
        false -> try maps:is_key(Position, Monitor#monitor.posToName) of
            true -> {error, {station_position_used, Position}};
            false -> 
                NewStation = #station{mapTypeToDay=#{}},
                P = maps:put(Position, Name, Monitor#monitor.posToName),
                N = maps:put(Name, NewStation, Monitor#monitor.nameToStation),
                Monitor#monitor{posToName=P, nameToStation=N}
            catch
                error:E -> {error, E}        
            end
    catch
        error:E -> {error, E}
    end.



addValue(Pos={_,_}, DateTime, TypeMeasur, Value, Monitor) ->
    case getName(Pos, Monitor) of
        E={error, _} -> E;
        Name -> addValue(Name, DateTime, TypeMeasur, Value, Monitor)
    end;

addValue(Name, {DateMeasur, TimeMeasur}=DateTime, TypeMeasur, Value, Monitor) ->
    try maps:get(Name, Monitor#monitor.nameToStation) of Station ->
        Date = maps:get(TypeMeasur, Station#station.mapTypeToDay, #{}),
        Time = maps:get(DateMeasur, Date, #{}),
        case maps:is_key(TimeMeasur, Time) of
            true -> {error, {meansurment_exist, Name, DateTime, TypeMeasur, Value}};
            false -> 
                UpdateTime = maps:put(TimeMeasur, Value, Time),
                UpdateDate = maps:put(DateMeasur, UpdateTime, Date),
                UpdateType = maps:put(TypeMeasur, UpdateDate, Station#station.mapTypeToDay),
                NewStation = #station{mapTypeToDay=UpdateType},
                N = maps:put(Name, NewStation, Monitor#monitor.nameToStation),
                Monitor#monitor{nameToStation=N}
        end
    catch 
        error:E -> {error,{unknown_station, E}}
    end.



removeValue(Pos={_,_}, DateTime, TypeMeasur, Monitor) ->
    case getName(Pos, Monitor) of
        E={error, _} -> E;
        Name -> removeValue(Name, DateTime, TypeMeasur, Monitor)
    end;

removeValue(Name, {DateMeasur, TimeMeasur}, TypeMeasur, Monitor) ->
    case  getStation(Name, Monitor) of
        E={error, _} -> E;
        Station ->
            try maps:get(TypeMeasur, Station#station.mapTypeToDay) of Date ->
                try maps:get(DateMeasur, Date) of Time ->
                    UpdateTime = maps:remove(TimeMeasur, Time),
                    TimeSize = maps:size(UpdateTime),
                    if  TimeSize > 0 -> UpdateDate = maps:put(DateMeasur, UpdateTime, Date);
                        true -> UpdateDate = maps:remove(DateMeasur, Date)
                    end,
                    DateSize = maps:size(UpdateDate),
                    if  DateSize > 0 -> UpdateType = maps:put(TypeMeasur, UpdateDate, Station#station.mapTypeToDay);
                        true -> UpdateType = maps:remove(TypeMeasur, Station#station.mapTypeToDay)
                    end,
                    NewStation = #station{mapTypeToDay=UpdateType},
                    N = maps:put(Name, NewStation, Monitor#monitor.nameToStation),
                    Monitor#monitor{nameToStation=N}
                catch
                    error:E -> {error,{unknown_date, Date, E}}
                end
            catch
                error:E -> {error,{unknown_type, TypeMeasur, E}}
            end
    end.


getOneValue(TypeMeasur, DateTime, Pos={_,_}, Monitor) ->
    getOneValue(Pos, DateTime, TypeMeasur, Monitor);

getOneValue(Pos={_,_}, DateTime, TypeMeasur, Monitor) ->
    case getName(Pos, Monitor) of
        E={error, _} -> E;
        Name -> getOneValue(Name, DateTime, TypeMeasur, Monitor)
    end;

getOneValue(Name, {DateMeasur, TimeMeasur}, TypeMeasur, Monitor) ->
    case getStation(Name, Monitor) of
        E={error, _} -> E;        
        Station ->
            try maps:get(TypeMeasur, Station#station.mapTypeToDay) of Date ->
                try maps:get(DateMeasur, Date) of Time ->
                    try maps:get(TimeMeasur, Time) of
                        Val -> Val
                    catch
                        error:E -> {error, {unknown_time, TimeMeasur, E}}
                    end
                catch
                    error:E -> {error, {unknown_date, DateMeasur, E}}
                end
            catch
                error:E -> {error, {unknown_type, TypeMeasur, E}}
            end
    end.

getStationMean(TypeMeasur, Pos={_,_}, Monitor) ->
    getStationMean(Pos, TypeMeasur, Monitor);

getStationMean(Pos={_,_}, TypeMeasur, Monitor) ->
    case getName(Pos, Monitor) of
        E={error, _} -> E;
        Name -> getStationMean(Name, TypeMeasur, Monitor)
    end;

getStationMean(Name, TypeMeasur, Monitor) ->
    case getStation(Name, Monitor) of
        {error, _} -> 0.0;
        Station -> 
            try maps:get(TypeMeasur, Station#station.mapTypeToDay) of DateMap ->
                AverageDay = 
                    fun(TimeMap, {Sum, Counter}) ->
                        {NewSum, NewCounter} = sumValuesTimeMap(TimeMap),
                        {Sum+NewSum, Counter+NewCounter}
                    end,
                ListTimeMap = maps:values(DateMap),
                {Sum, Counter} = lists:foldl(AverageDay, {0, 0}, ListTimeMap),
                case Counter of
                    0 -> 0.0; % empty meansurments
                    _ -> Sum / Counter
                end
            catch
                error:_ -> 0.0
            end
    end.


getDailyMean(TypeMeasur, Date={_,_,_}, Monitor) ->
    getDailyMean(Date, TypeMeasur, Monitor);

getDailyMean(Date, TypeMeasur, Monitor) ->
    AverageStation = 
        fun(Station, Acc={Sum, Counter}) ->
            try maps:get(TypeMeasur, Station#station.mapTypeToDay) of DayMap -> 
                try maps:get(Date, DayMap) of
                    TimeMap -> {NewSum, NewCounter} = sumValuesTimeMap(TimeMap),
                               {Sum+NewSum, Counter+NewCounter}
                catch 
                    error:{badkey, _} -> Acc
                end
            catch
                error:{badkey, _} -> Acc
            end           
        end,
    try maps:values(Monitor#monitor.nameToStation) of Stations ->
        {Sum, Counter} = lists:foldl(AverageStation, {0, 0}, Stations),
        case Counter of
            0 -> {error, no_measurments};
            _ -> Sum / Counter
        end
    catch
        error:E -> {error, E}
    end.
    

getHourlyStationData(Pos={_,_}, TypeMeasur, Monitor) ->
    case getName(Pos, Monitor) of
        E={error, _} -> E;
        Name -> getHourlyStationData(Name, TypeMeasur, Monitor)
    end;

getHourlyStationData(Name, TypeMeasur, Monitor) ->
    case getStation(Name, Monitor) of
        E={error, _} -> E;
        Station ->
            try maps:get(TypeMeasur, Station#station.mapTypeToDay) of DateMap ->
                DataToAvrList = 
                    fun(Day, TimeMap, AccList) ->
                        AverageHour = 
                            fun({H, _, _}, Value, AccMap) ->
                                {Sum, Counter} = maps:get(H, AccMap, {0,0}),
                                NewVal = {Sum+Value, Counter+1},
                                maps:put(H, NewVal, AccMap)
                            end,
                        HourToArvTupleMap = maps:fold(AverageHour,#{},TimeMap),
                        MakeList = 
                            fun(H, {Sum, C}, Acc) -> 
                                [{{Day, {H, 0, 0}}, Sum / C}] ++ Acc
                            end,
                        DataTimeToAvrList = maps:fold(MakeList, [], HourToArvTupleMap),
                        DataTimeToAvrList ++ AccList
                    end,
                maps:fold(DataToAvrList, [], DateMap)
            catch
                error:E -> {error, {unknown_type, TypeMeasur, E}}
            end
    end.



sumValuesTimeMap(TimeMap) -> 
    Values = maps:values(TimeMap),
    lists:foldl(fun(A, {C, D}) -> {A+C,D+1} end, {0, 0}, Values).

getName(Pos, Monitor) -> 
    try maps:get(Pos, Monitor#monitor.posToName) of
        Val -> Val
    catch
        error:E -> {error, {unknown_position, Pos, E}}
    end.


getStation(StationName, Monitor) -> 
    try maps:get(StationName, Monitor#monitor.nameToStation) of
        Val -> Val
    catch
        error:E -> {error, {unknown_station, StationName, E}}
    end.


test(N) -> case N of
    0 -> createMonitor();
    1 -> A = test(0),
         B = addStation("stat", {1, 2}, A),
         C = addStation("test", {3, 4}, B),
         addStation("test2", {1, 4}, C);
         %addStation("stat", {5, 6}, C);
         %addStation("stts", {3, 4}, C);

    2 -> A = test(1),
         B = addValue({3, 4}, calendar:local_time(), "PM10", 60, A),
         C = addValue("test", calendar:local_time(), "PM2,5", 70, B),
         D = addValue("stat", calendar:local_time(), "PM2,5", 71, C),
         E = addValue("stat", calendar:local_time(), "PM10", 72, D),
         addValue("stat", calendar:local_time(), "temp", 20, E);
         %addValue({3, 4}, calendar:local_time(), "PM2,5", 80, E);
         %addValue("test", calendar:local_time(), "PM2,5", 90, E);
         %addValue({3, 4}, calendar:local_time(), "PM10", 100, E);
         %addValue("test", calendar:local_time(), "PM10", 110, E);

    3 -> A = test(2),
         io:format("before:~n~p~n~nafter:~n",[A]),
         B = removeValue({3, 4}, calendar:local_time(), "PM10", A),
         C = removeValue("test", calendar:local_time(), "PM10", B),
         removeValue("test", calendar:local_time(), "PM2,5", C);
         %removeValue("test2", calendar:local_time(), "PM10", C);

    4 -> A = test(2),
         io:format("~p~n",[A]),
         Res = getOneValue({3, 4}, calendar:local_time(), "PM10", A),
         Res = getOneValue("test", calendar:local_time(), "PM10", A),
         Ret = getOneValue("stat", calendar:local_time(), "PM10", A),
         io:format("~p~n", [Res]),
         io:format("~p~n", [Ret]),
         getOneValue({3, 4}, calendar:local_time(), "PM2,5", A);
         %getOneValue({1, 4}, calendar:local_time(), "PM10", A);

    5 -> A = test(1),
         {Day={Y, M, D}, Time} = calendar:local_time(),
         Inc = fun(X) -> X+1 end,
         B = generate("test", Day, calendar:time_to_seconds(Time), "temp", 10, 2, A, Inc, Inc),
         C = generate("test", {Y,M,D+1}, calendar:time_to_seconds(Time), "temp", 20, 3, B, Inc, Inc),
         generate("test2", Day, calendar:time_to_seconds(Time), "temp", 30, 4, C, Inc, Inc);

    6 -> A = test(5),
         io:format("~p~n", [A]),
         io:put_chars("1: 10+11,  2: 20+21+22  sum=84 avg=84/5=16,8\n"),
         getStationMean({3, 4}, "temp", A);

    7 -> A = test(5),
         io:format("~p~n", [A]),
         io:put_chars("1: 10+11,  2: 30+31+32+33 sum=147 avg=147/6=24,5\n"),
         {Day, _} = calendar:local_time(),
         getDailyMean(Day, "temp", A);
    
    8 -> A = test(1),
         {Day={Y, M, D}, Time} = calendar:local_time(),
         NextHalfHour = fun(X) -> (60*30+X) rem 86400 end,
         NextHour = fun(X) -> (3600+X) rem 86400 end,
         B = generate("test", Day, calendar:time_to_seconds(Time), "temp", 0, 3, A, fun(X) -> X+2 end, NextHalfHour),
         C = generate("test", Day, calendar:time_to_seconds(Time)+1, "temp", 20, 10, B, fun(X) -> X end, NextHour),
         E = generate("test", {Y, M, D+1}, calendar:time_to_seconds(Time), "temp", 0, 4, C, fun(X) -> X+1 end, NextHour),
          io:format("~p~n~n", [E]),
         getHourlyStationData("test", "temp", E);
    _ -> invalid_number
end.

generate(_, _, _, _, _, Num, Monitor, _, _) when Num==0 -> Monitor;

generate(Name, Day, TimeInSec, Type, StartVal, Num, Monitor, FunVal, FunTime) -> 
    NewMonitor = addValue(Name, {Day, calendar:seconds_to_time(TimeInSec)}, Type, StartVal, Monitor),
    generate(Name, Day, FunTime(TimeInSec), Type, FunVal(StartVal), Num-1, NewMonitor, FunVal, FunTime).

