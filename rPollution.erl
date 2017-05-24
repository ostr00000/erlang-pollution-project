%%%-------------------------------------------------------------------
%%% @author Student
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. maj 2017 10:42
%%%-------------------------------------------------------------------
-module(rPollution).
-author("Student").

-behaviour(gen_server).

%% API
-export([start_link/0, 
        addStation/2,
        addValue/4,
        removeValue/3,
        getOneValue/3,
        getStationMean/2,
        getDailyMean/2,
        getHourlyStationData/2,
        stop/0,getStruct/0,
        tests/0, crash/0]).

%% gen_server callbacks
-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
crash() ->
  F = fun(A) -> A div A end,
  F(0).

tests() ->
    DAY = {2013, 12, 1},
    io:format("~p~n",[rPollution:stop()]),
    io:format("~p~n",[rPollution:start_link()]),
    io:format("~p~n",[rPollution:addStation("ala",{1, 2})]),
    io:format("should be error = ~p~n",[rPollution:addStation("kot",{1, 2})]),
    io:format("~p~n",[rPollution:addStation("kot",{1, 3})]),
    io:format("~p~n",[rPollution:addValue("ala", {DAY, {10, 21, 10}}, "temp", 21)]),
    io:format("should be error = ~p~n",[rPollution:addValue("ala", {DAY, {10, 21, 10}}, "temp", 21)]),
    io:format("~p~n",[rPollution:addValue("ala", {DAY, {10, 34, 10}}, "temp", 22)]),
    io:format("~p~n",[rPollution:addValue("ala", {DAY, {10, 43, 10}}, "temp", 23)]),
    io:format("~p~n",[rPollution:addValue("ala", {DAY, {10, 57, 10}}, "temp", 24)]),
    io:format("~p~n",[rPollution:addValue("ala", {DAY, {12, 57, 10}}, "temp", 28)]),
    io:format("~p~n",[rPollution:addValue("kot", {DAY, {10, 57, 10}}, "temp", 25)]),
    io:format("~p~n",[rPollution:addValue("kot", {DAY, {10, 52, 10}}, "temp", 35)]),
    io:format("~p~n",[rPollution:addValue("kot", {DAY, {10, 57, 10}}, "aqua", 5)]),
    io:format("~p~n",[rPollution:addValue("kot", {{2013, 11, 1}, {10, 57, 10}}, "temp", 15)]),
    io:format("expected val = 25, recieved: ~p~n",[rPollution:getOneValue("kot", {DAY, {10, 57, 10}}, "temp")]),
    io:format("should be error = ~p~n",[rPollution:getOneValue("kot", {DAY, {10, 57, 10}}, "P10")]),
    io:format("expected val = 25, recieved: ~p~n",[rPollution:getStationMean("kot", "temp")]),
    io:format("expected val = 25, recieved: ~p~n",[rPollution:getDailyMean(DAY, "temp")]),
    io:format("~p~n",[rPollution:getHourlyStationData("ala", "temp")]),
    true.


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  io:format("~p (~p) server start\n",[{global, ?MODULE},self()]),
  gen_server:start_link({local, ?SERVER}, ?MODULE, pollution:createMonitor(), []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

stop() -> gen_server:cast(?MODULE, stop).

addStation(Name, Pos) -> 
    gen_server:call(?MODULE, {addStation, Name, Pos}).
addValue(Name, DateTime, Type, Val) -> 
    gen_server:call(?MODULE, {addValue, Name, DateTime, Type, Val}).
removeValue(Name, DateTime, Type) -> 
    gen_server:call(?MODULE, {removeValue, Name, DateTime, Type}).
getOneValue(Name, DateTime, Type) ->
    gen_server:call(?MODULE, {getOneValue, Name, DateTime, Type}).
getStationMean(Name, Type) ->
    gen_server:call(?MODULE, {getStationMean, Name, Type}).
getDailyMean(Date, Type) ->
    gen_server:call(?MODULE, {getDailyMean, Date, Type}).
getHourlyStationData(Name, Type) ->
    gen_server:call(?MODULE, {getHourlyStationData, Name, Type}).
getStruct() -> gen_server:call(?MODULE, getStruct).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init(S) ->
  {ok, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

is_error(ReturnedVal, {ret, OldVal}) -> {reply, ReturnedVal, OldVal};
is_error({error, _}=Err, OldVal) -> {reply, Err, OldVal};
is_error(NewVal, _) -> {reply, ok, NewVal}.

handle_call(getStruct,_From, State) ->{reply, State,State};

handle_call({addStation, Name, Pos}, _From, State) ->
    is_error(pollution:addStation(Name, Pos, State), State);

handle_call({addValue, Name, DateTime, Type, Val}, _From, State) ->
    is_error(pollution:addValue(Name, DateTime, Type, Val, State), State);

handle_call({removeValue, Name, DateTime, Type}, _From, State) ->
    is_error(pollution:removeValue(Name, DateTime, Type, State), State);

handle_call({getOneValue, Name, DateTime, Type}, _From, State) ->
    is_error(pollution:getOneValue(Name, DateTime, Type, State), {ret, State});

handle_call({getStationMean, Name, Type}, _From, State) ->
    is_error(pollution:getStationMean(Name, Type, State), {ret, State});

handle_call({getDailyMean, Date, Type}, _From, State) ->
    is_error(pollution:getDailyMean(Date, Type, State), {ret, State});

handle_call({getHourlyStationData, Name, Type}, _From, State) ->
    is_error(pollution:getHourlyStationData(Name, Type, State), {ret, State}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_cast(stop ,State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



