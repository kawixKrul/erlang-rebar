%%%-------------------------------------------------------------------
%%% @author michalkawa
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(pollution_gen_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3, add_station/2, add_value/4, remove_value/3, get/0, get_daily_deviation/2,
  get_daily_mean/2, get_station_mean/2, get_one_value/3, crash/0]).

-define(SERVER, ?MODULE).

-record(pollution_gen_server_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, pollution:create_monitor()}.

add_station(Name, Cords) ->
  gen_server:cast(local, {add_station, Name, Cords}).

add_value(Key, Date, Type, Value) ->
  gen_server:cast(local, {add_value, Key, Date, Type, Value}).

remove_value(Key, Date, Type) ->
  gen_server:cast(local, {remove_value, Key, Date, Type}).

get() ->
  gen_server:call(local , get).

get_one_value(Key, Date, Type) ->
  gen_server:call(local, {get_one_value, Key, Date, Type}).

get_station_mean(Key, Type) ->
  gen_server:call(local, {get_station_mean, Key, Type}).

get_daily_mean(Type, Day) ->
  gen_server:call(local, {get_daily_mean, Type, Day}).

get_daily_deviation(Type, Day) ->
  gen_server:call(local, {get_daily_deviation, Type, Day}).

handle_call(Args, _From, Monitor) ->
  Result = case Args of
             get ->
               Monitor;
             {get_one_value, Key, Date, Type} ->
               pollution:get_one_value(Key, Date, Type, Monitor);
             {get_station_mean, Key, Type} ->
               pollution:get_station_mean(Key, Type, Monitor);
             {get_daily_mean, Type, Day} ->
               pollution:get_daily_mean(Type, Day, Monitor);
             {get_daily_deviation, Type, Day} ->
               pollution:get_daily_deviation(Type, Day, Monitor);
             _ ->
               {error, bad_call}
           end,

  case Result of
    {error, Msg} ->
      print_error(Msg), {noreply, Monitor};
    _ ->
      {reply, Result, Monitor}
  end.

handle_cast(Args, Monitor) ->
  Result = case Args of
             {add_station, Name, Cords} ->
               pollution:add_station(Name, Cords, Monitor);
             {add_value, Key, Date, Type, Value} ->
               pollution:add_value(Key, Date, Type, Value, Monitor);
             {remove_value, Key, Date, Type} ->
               pollution:remove_value(Key, Date, Type, Monitor);
             _ ->
               {error, bad_cast}
           end,

  case Result of
    {error, Msg} ->
      print_error(Msg), {noreply, Monitor};
    _ ->
      {reply, Result, Monitor}
  end.

handle_info(_Info, State = #pollution_gen_server_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #pollution_gen_server_state{}) ->
  ok.

code_change(_OldVsn, State = #pollution_gen_server_state{}, _Extra) ->
  {ok, State}.

print_error(Error) ->
  io:format("ERROR: ~w. ~n", [Error]),
  ok.

crash() ->
  no:exist(),
  ok.
%%%===================================================================
%%% Internal functions
%%%===================================================================
