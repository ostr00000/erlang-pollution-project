-module(my_pollution_test).

-include_lib("eunit/include/eunit.hrl").

getHourlyStationData_test() ->
    N = "testing station 0",
    Date = {2017, 5, 6},
    Time = calendar:datetime_to_gregorian_seconds({Date, {0, 0, 0}}),

    Add = fun(Val) -> fun(X) -> X + Val end end,
   
    MakeMeasur =
        fun Next(_, _, _, _, Monitor, 0, _, _) -> Monitor;
            Next(Name, TimeSec, Type, Val, Monitor, Iter, GetTime, GetVal) ->
                T =  calendar:gregorian_seconds_to_datetime(TimeSec),
                NewMonitor = pollution:addValue(Name, T, Type, Val, Monitor),
                NewTimeSec = GetTime(TimeSec),
                NewVal = GetVal(Val),
                Next(Name, NewTimeSec, Type, NewVal, NewMonitor, Iter - 1, GetTime, GetVal)
        end,

    A0 = pollution:createMonitor(),    
    A1 = pollution:addStation(N, {1, 2}, A0),
    A2 = pollution:addValue(N, {Date, {0, 10, 20}}, "temp", 15, A1),
    A3 = pollution:addStation("New Station", {3, 4}, A2),
    S0 = pollution:addValue("New Station", {Date, {0, 34, 17}}, "P10", -100, A3),
    
    % test 2 meansurments in 2 hours
    Expect1 =  [{{Date, {1,0,0}}, 12.0},
                {{Date, {0,0,0}}, 10.0}],
    S1 = MakeMeasur(N, Time, "P10", 10, S0, 2, Add(60*60), Add(2)),
    ?assertEqual(Expect1, pollution:getHourlyStationData(N, "P10", S1)),

    % test 2 meansurments in 1 hour
    Expect2 = [{{Date, {0,0,0}}, 11.0}],
    S2 = MakeMeasur(N, Time, "P10", 10, S0, 2, Add(5), Add(2)),
    ?assertEqual(Expect2, pollution:getHourlyStationData(N, "P10", S2)),

    % test 4 meansurments in 2 days
    Expect3 =  [{{{2017,5,7},{6,0,0}}, 16.0},
                {{Date, {20,0,0}}, 14.0},
                {{Date, {10,0,0}}, 12.0},
                {{Date, {0,0,0}}, 10.0}],
    S3 = MakeMeasur(N, Time, "P10", 10, S0, 4, Add(60*60*10), Add(2)),
    ?assertEqual(Expect3, pollution:getHourlyStationData(N, "P10", S3)),

    % test 4 meansurments in 2 hours
    Expect4 =  [{{Date, {1,0,0}}, 15.0},
                {{Date, {0,0,0}}, 11.0}],
    S4 = MakeMeasur(N, Time, "P10", 10, S0, 4, Add(60*30), Add(2)),
    ?assertEqual(Expect4, pollution:getHourlyStationData(N, "P10", S4)),
    
    % test 60 measurments in 1 hour 
    Expect5 =  [{{Date, {0,0,0}}, 69.0}], % (10+(10+2*(60-1)))/2 = 138/2 = 69 
    S5 = MakeMeasur(N, Time, "P10", 10, S0, 60, Add(1), Add(2)),
    ?assertEqual(Expect5, pollution:getHourlyStationData(N, "P10", S5)),
    
    % test 20 measurments in 4 hours
    Expect6 =  [{{Date, {3,0,0}}, (30+38)/2},
                {{Date, {2,0,0}}, (20+28)/2},
                {{Date, {1,0,0}}, (10+18)/2},
                {{Date, {0,0,0}}, ( 0+ 8)/2}],
    S6 = MakeMeasur(N, Time, "P10", 0, S0, 20, Add(60*12), Add(2)),
    ?assertEqual(Expect6, pollution:getHourlyStationData(N, "P10", S6)).

    % io:format("~p~n~n~p~n~n", [S6, pollution:getHourlyStationData(N, "P10", S6)]).    




