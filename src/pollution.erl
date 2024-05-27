%%%-------------------------------------------------------------------
%%% @author michalkawa
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. mar 2024 19:07
%%%-------------------------------------------------------------------
-module(pollution).
-author("michalkawa").

%% API
-export([find_station/2, create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4, get_station_mean/3, get_daily_mean/3, get_daily_deviation/3]).

create_monitor() -> #{}.

add_station(Name, Cords, Monitor) ->
  Exists = fun({N, C}, _, Acc) -> Name =:= N orelse C =:= Cords orelse Acc end,

  case maps:fold(Exists, false, Monitor) of
    true -> {error, exist};
    false -> maps:put({Name, Cords}, #{}, Monitor)
  end.

find_station(Key, Monitor) ->
  SearchByKey = fun ({Name, Cords}, Val, _) when Key =:= Name orelse Key =:= Cords -> {{Name, Cords}, Val};
    (_, _, Acc) -> Acc end,

  case maps:fold(SearchByKey, none, Monitor) of
    {K, V} -> {ok, K, V};
    none -> {error, not_found}
  end.

add_value(Key, Date, Type, Value, Monitor) ->
  case find_station(Key, Monitor) of
    {ok, K, V} -> case maps:is_key({Date, Type}, V) of
                    true -> {error, exist};
                    false -> New = maps:put({Date, Type}, Value, V),
                      Monitor#{K => New}
                  end;
    {error, not_found} -> {error, not_found}
  end.

remove_value(Key, Date, Type, Monitor) ->
  case find_station(Key, Monitor) of
    {ok, K, V} -> case maps:is_key({Date, Type}, V) of
                     true -> New = maps:remove({Date, Type}, V),
                       Monitor#{K => New};
                     false -> {error, not_found}
                  end;
    {error, not_found} -> {error, not_found}
  end.

get_one_value(Key, Date, Type, Monitor) ->
  case find_station(Key, Monitor) of
    {ok, _, V} -> case maps:is_key({Date, Type}, V) of
                    true -> maps:get({Date, Type}, V);
                    false -> {error, not_found}
                  end;
    {error, not_found} -> {error, not_found}
  end.

get_station_mean(Key, Type, Monitor) ->
  case find_station(Key, Monitor) of
    {ok, _, V} -> Pollutions = [X || {{_, T}, X} <- maps:to_list(V), T =:= Type],
      case length(Pollutions) of
        0 -> {error, zero_division};
        _ -> lists:sum(Pollutions)/length(Pollutions)
      end;
    {error, not_found} -> {error, not_found}
  end.

get_daily_mean(Type, Day, Monitor) ->
  Pollutions = [V || {_, X} <- maps:to_list(Monitor), {{{D, _}, T}, V} <- maps:to_list(X), D =:= Day, T =:= Type],
  case length(Pollutions) of
    0 -> {error, zero_division};
    _ -> lists:sum(Pollutions)/length(Pollutions)
  end.

get_daily_deviation(Type, Day, Monitor) ->
  Pollutions = [V || {_, X} <- maps:to_list(Monitor), {{{D, _}, T}, V} <- maps:to_list(X), D =:= Day, T =:= Type],
  case length(Pollutions) of
    0 -> {error, zero_division};
    _ -> Mean = lists:sum(Pollutions)/length(Pollutions),
      math:sqrt(lists:sum([math:pow(Mean-P, 2) || P <- Pollutions])/length(Pollutions))
  end.