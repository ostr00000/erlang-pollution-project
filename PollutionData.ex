
defmodule PollutionData do
    def importLinesFromCSV() do
        case File.read("pollution.csv") do
            {:ok, body} -> 
                IO.puts "ok"
                dataList = String.split(body)
                conv = fn(line) ->
                    toNumber = fn(type, string) -> 
                        tuple = type.parse(string)
                        elem(tuple, 0)
                    end
                    toInt = fn(string) -> toNumber.(Integer, string) end
                    [date, time, cordX, cordY, val] = String.split(line, ",")
                    [day, month, year] = String.split(date, "-")
                    [hour, minute] = String.split(time, ":")
                    date = {toInt.(year), toInt.(month), toInt.(day)}
                    time = {toInt.(hour), toInt.(minute), 0}
                    cords = {toNumber.(Float, cordX), toNumber.(Float, cordY)}
                    {{date, time}, cords, toInt.(val)}
                end
                Enum.map(dataList, conv)
            {:error, reason} -> IO.puts "error #{reason}"
        end
    end

    def identifyStations(listOfMeasurments) do 
        addStation = fn {_, cords={x,y}, _}, acc -> Map.put(acc, cords, "station_#{x}_#{y}") end
        Enum.reduce(listOfMeasurments, %{}, addStation)
    end

    def addDataToServer() do
        m = PollutionData.importLinesFromCSV()
        s = PollutionData.identifyStations(m)
        fn -> PollutionData.addDataToServer(m, s) end |> :timer.tc #|> elem(0)
    end

    def addDataToServer(listOfMeasurments, mapOfStations) do
        Enum.map(mapOfStations, fn({cords, name}) -> :rPollution.addStation(name, cords) end)
        Enum.map(listOfMeasurments, fn({dateTime, cords, val}) -> :rPollution.addValue(cords, dateTime, "PM10", val) end)
    end

end
