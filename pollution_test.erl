-module(pollution_test).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
 ?assert(true).

getStationMean_test() ->
%  Set up
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),
  Date1 = {{2017, 4, 1}, {1, 0, 51}},
  Date2 = {{2017, 4, 1}, {2, 0, 51}},
  Date3 = {{2017, 4, 1}, {3, 0, 51}},
  P2 = pollution:addValue({50.2345, 18.3445}, Date1, "PM10", 10, P1),
  P3 = pollution:addValue({50.2345, 18.3445}, Date1, "PM2.5", 15, P2),
  P4 = pollution:addValue({50.2345, 18.3445}, Date2, "PM10", 20, P3),
  P5 = pollution:addValue({50.2345, 18.3445}, Date2, "PM2.5", 12, P4),
  P6 = pollution:addValue({50.2345, 18.3445}, Date3, "PM10", 30, P5),
  P7 = pollution:addValue({50.2345, 18.3445}, Date3, "PM2.5", 3, P6),
  P8 = pollution:addStation("Aleja Mickiewicza", {5, 1}, P7),
  M = pollution:createMonitor(),


% Test cases
% Get a PM10 mean of an existing station
  Mean1 = pollution:getStationMean({50.2345, 18.3445}, "PM10", P8),
  ?assertEqual(20.0, Mean1),

% Get a PM2.5 mean of an existing station
  Mean2 = pollution:getStationMean({50.2345, 18.3445}, "PM2.5", P8),
  ?assertEqual(10.0, Mean2),

% Get a mean of an empty station
  Mean3 = pollution:getStationMean("Aleja Mickiewicza", "PM10", P8),
  ?assertEqual(0.0, Mean3),

% Get a mean of an empty monitor
  Mean4 = pollution:getStationMean("Aleja Slowackiego", "PM10", M),
  ?assertEqual(0.0, Mean4),

% Get a mean of a nonexistent station
  Mean5 = pollution:getStationMean("Aleja", "PM10", P8),
  ?assertEqual(0.0, Mean5),

% Get a mean of a wrong pollution type
  Mean6 = pollution:getStationMean("Aleja Mickiewicza", "PM100", P8),
  ?assertEqual(0.0, Mean6).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
addStation_test() ->
  % Initialising monitor:
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),
  P2 = pollution:addStation("Aleja Kochanowskiego", {18.3445, 00.0000}, P1),

  % Test cases:

  % adding station to an empty monitor
  ?assertEqual(pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),P1),
  ?assertNotMatch({error,_}, P1),

  % adding station to a non empty monitor
  ?assertEqual(pollution:addStation("Aleja Kochanowskiego", {18.3445, 00.0000}, P1),P2),
  ?assertNotMatch({error,_}, P2),

  % adding already existing station
  P3 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P1),
  ?assertMatch({error,_}, P3),

  % adding station with different coordinates but the same name
  P4 = pollution:addStation("Aleja Slowackiego", {50.0000, 18.0000}, P1),
  ?assertMatch({error,_}, P4),

  % adding station with different name but the same coordinates
  P5 = pollution:addStation("Aleja Mickiewicza", {50.2345, 18.3445}, P1),
  ?assertMatch({error,_}, P5).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
addValue_test() ->
    Name = "Test station",
    Pos = {1.0, 2.0},
    DateTime = {{2017, 5, 6}, {1, 2, 3}},
    M0 = pollution:createMonitor(),
    M1 = pollution:addStation(Name, Pos, M0),
    M2 = pollution:addStation("Another station", {3.0, 4.0}, M1),
    M3 = pollution:addValue(Pos, DateTime, "P10", 1, M2),

    % can't add 2 times value to the same station and with the same time
    ?assertMatch({error, _}, pollution:addValue(Pos, DateTime, "P10", 100, M3)),
    ?assertMatch({error, _}, pollution:addValue(Name, DateTime, "P10", 10, M3)),

    % can't add to not exist station
    ?assertMatch({error, _}, pollution:addValue({100.0, 200.0}, DateTime, "P10", 10, M3)),
    ?assertMatch({error, _}, pollution:addValue("not exist", DateTime, "P10", 10, M3)),

    %add value with new type
    M4 = pollution:addValue(Pos, DateTime, "temperature", 15, M3),
    ?assertNotMatch({error, _}, M4), 
    ?assertEqual(M4, pollution:addValue(Name, DateTime, "temperature", 15, M3)),

    %add value with new time
    NewTime1 = {{2017, 5, 6}, {10, 20, 30}},
    M5 = pollution:addValue(Pos, NewTime1, "P10", 1, M3),
    ?assertNotMatch({error, _}, M5), 
    ?assertEqual(M5, pollution:addValue(Name, NewTime1, "P10", 1, M3)),
    
    %add value with new date
    NewTime2 = {{2017, 1, 1}, {1, 2, 3}},
    M6 = pollution:addValue(Pos, NewTime2, "P10", 1, M3),
    ?assertNotMatch({error, _}, M6), 
    ?assertEqual(M6, pollution:addValue(Name, NewTime2, "P10", 1, M3)),

    %add default value to another station
    M7 = pollution:addValue("Another station", DateTime, "P10", 1, M3),
    ?assertNotMatch({error, _}, M7),
    ?assertEqual(M7, pollution:addValue({3.0, 4.0}, DateTime, "P10", 1, M3)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

removeValue_removesExistingValueDiffDates_test() ->
 M1 = pollution:createMonitor(),
 M2 = pollution:addStation("Station1", {1, 2}, M1),
 M3 = pollution:addStation("Station2", {2, 3}, M2),
 M4 = pollution:addValue("Station1", {{2017, 04, 11},{20, 0, 0}}, "PM10", 6, M3),
 M5 = pollution:addValue("Station1", {{2017, 04, 11},{19, 0, 0}}, "PM10", 6, M4),
 M6 = pollution:removeValue("Station1", {{2017, 04, 11},{19, 0, 0}}, "PM10", M5),
 ?assertEqual(M4, M6).

removeValue_removesExistingValueDiffTypes_test() ->
 M1 = pollution:createMonitor(),
 M2 = pollution:addStation("Station1", {1, 2}, M1),
 M3 = pollution:addStation("Station2", {2, 3}, M2),
 M4 = pollution:addValue("Station1", {{2017, 04, 11},{20, 0, 0}}, "PM10", 6, M3),
 M5 = pollution:addValue("Station1", {{2017, 04, 11},{20, 0, 0}}, "PM25", 6, M4),
 M6 = pollution:removeValue("Station1", {{2017, 04, 11},{20, 0, 0}}, "PM25", M5),
 ?assertEqual(M4, M6).

removeValue_removesExistingValueDiffStations_test() ->
 M1 = pollution:createMonitor(),
 M2 = pollution:addStation("Station1", {1, 2}, M1),
 M3 = pollution:addStation("Station2", {2, 3}, M2),
 M4 = pollution:addValue("Station1", {{2017, 04, 11},{20, 0, 0}}, "PM10", 6, M3),
 M5 = pollution:addValue("Station2", {{2017, 04, 11},{20, 0, 0}}, "PM10", 6, M4),
 M6 = pollution:removeValue({2,3}, {{2017, 04, 11},{20, 0, 0}}, "PM10", M5),
 ?assertEqual(M4, M6).

removeValue_doesNotRemoveNotExistingValue_test() ->
 M1 = pollution:createMonitor(),
 M2 = pollution:addStation("Station1", {1, 2}, M1),
 M3 = pollution:addStation("Station2", {2, 3}, M2),
 M4 = pollution:addValue("Station1", {{2017, 04, 11},{20, 0, 0}}, "PM10", 6, M3),
 M5 = pollution:addValue("Station2", {{2017, 04, 11},{20, 0, 0}}, "PM10", 6, M4),
 M6 = pollution:removeValue("Station1", {{2017, 04, 11},{19, 0, 0}}, "PM10", M5),
 ?assertEqual(M5, M6).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getDailyMean_test()->
        P = pollution:createMonitor(),
	P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),
	P2 = pollution:addStation("Aleja Mickiewicza", {18.3445, 00.0000}, P1),
	P3 = pollution:addStation("Aleja Krasickiego", {18.3445, 01.0000}, P2),
	Date1 = {{2017, 4, 1},{1,1,1}},
	Date2 = {{2017, 4, 2},{1,1,1}},
	Date11={2017, 4, 1},
	Date21={2017, 4, 2},
	P4 = pollution:addValue("Aleja Slowackiego", Date1, "PM10", 10, P3),
	P5 = pollution:addValue("Aleja Slowackiego", Date2, "PM10", 100, P4),
	P6 = pollution:addValue("Aleja Krasickiego", Date1, "PM10", 20, P5),
	P7 = pollution:addValue("Aleja Mickiewicza", Date1, "PM2.5", 30, P6),
	P8 = pollution:addValue("Aleja Mickiewicza", Date2, "PM10", 101, P7),
    ?assertEqual(15.0, pollution:getDailyMean(Date11,"PM10",P8)),
    ?assertEqual(30.0, pollution:getDailyMean(Date11,"PM2.5",P8)),
    ?assertEqual(100.5, pollution:getDailyMean(Date21,"PM10",P8)).

