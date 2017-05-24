# erlang-pollution-project

Pollution project can collect data from many stations. Stations can measure quality of air on various ways.

### PollutionData.ex 
to parse input file

 - importLinesFromCSV - read file and parse strings to required structs
  
        () -> [{ {{year, month, day},{hour, minutes, seconds=0}}, {cordX, cordY}, val}]
      
 - identifyStations - find and create name based on cords for all stations
  
       (outputFrom_importLinesFromCSV) -> map
      
 - addDataToServer - add data parsed data (server need to be started before)
  
        () -> time_of_adding
        (outputFrom_importLinesFromCSV, outputFrom_identifyStations) -> {station_result, measurments_result}
      
 - findError - find all error that appear while adding to server
  
        ({station_result, measurments_result}) -> {station_errors, measurments_errors}
  
### commands.txt 
 commands to paste into elixir console, compile all modules, run server and add all measurments

### meanTimeOfAdd 
 bash script to compile all files and run script "timeOfAdd"

### my_pollution_test.erl 
 tests for module pollution for getHourlyStationData:

      getHourlyStationData_test()

### pollution.csv 
  file with data of measurments

### pollution.erl 
main file with functions:
 - createMonitor/0 - create and return new monitor
 
        () -> monitor
        
 - addStation/3 - add new station to monitor
 
        (name, cords, monitor) -> monitor
        
 - addValue/5 - add new measurement to monitor 
 
        (cords, dataTime, type, value, monitor) -> monitor
        (name, dataTime, type, value, monitor) -> monitor
        
 - removeValue/4 - remove measurement from monitor 
 
        (cords, dataTime, type, monitor) -> monitor
        (name, dataTime, type, monitor) -> monitor
        
 - getOneValue/4 - return value from station with given dataTime and type
 
        (type, dataTime, cords, monitor) -> monitor
        (cords, dataTime, type, monitor) -> monitor
        (name, dataTime, type, monitor) -> monitor
        
 - getStationMean/3 - return mean value of all meansurments from station with given type
 
        (type, cords, monitor) -> monitor
        (cords, type, monitor) -> monitor
        (name, type, monitor) -> monitor
        
 - getDailyMean/3 - return mean value of all meansurments from all station with given day and type 
 
        (type, day, monitor) -> monitor
        (day, type, monitor) -> monitor
        
 - getHourlyStationData/3 - return list of mean values in every hour from given station and type
 
        (cords, type, monitor) -> monitor
        (name, type, monitor) -> monitor
      
### pollutionSupervisor.erl 
start supervisor for server, should be used command: 

      start_link_shell()
      
### pollution_test.erl 
tests for module pollution for
 - getStationMean: 

        getStationMean_test()
    
 - addStation: 
    
        addStation_test()
    
 - addValue:
  
        addValue_test()
    
 - removeValue:
   
        removeValue_removesExistingValueDiffDates_test()
        removeValue_removesExistingValueDiffTypes_test()
        removeValue_removesExistingValueDiffStations_test() 
        removeValue_doesNotRemoveNotExistingValue_test()

-  getDailyMean:
  
        getDailyMean_test()
  
### rPollution.erl
server for pollution monitor
 - start_link/0 - start server (for supervisor),
 - addStation/2,
 - addValue/4,
 - removeValue/3,
 - getOneValue/3,
 - getStationMean/2,
 - getDailyMean/2,
 - getHourlyStationData/2,
 - stop/0,
 - getStruct/0 - return full structure of monitor (for debug),
 - crash/0 - div by 0,
 - tests/0 - for test if rPollution work
  
### time.txt 
first line contain time of adding all stations and measurments from file pollution.csv to server, also there are sample results of some functions
  
### timeOfAdd 
script for run server, parse and add data to server and print time of adding
