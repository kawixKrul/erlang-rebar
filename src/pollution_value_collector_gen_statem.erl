%%%-------------------------------------------------------------------
%%% @author michalkawa
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. maj 2024 09:11
%%%-------------------------------------------------------------------
-module(pollution_value_collector_gen_statem).
-author("michalkawa").

-behaviour(gen_statem).

%% API
-export([start_link/0]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0, set_station/1, add_value/2, add_value/3, store_data/0]).

-define(SERVER, ?MODULE).

-record(pollution_value_collector_gen_statem_state, {}).
-record(state, {monitor, key=none, readings=[]}).
-record(reading, {type, value, date}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link() ->
    Monitor = pollution:create_monitor(),
    gen_statem:start_link({local, ?SERVER}, ?MODULE, Monitor, []).

%%%===================================================================
%%% USER API
%%%===================================================================

set_station(Key) ->
    gen_statem:cast(?SERVER, {set_station, Key}).

add_value(Type, Value) ->
    gen_statem:cast(?SERVER, {add_value, Type, Value}).
add_value(Type, Value, Date) ->
    gen_statem:cast(?SERVER, {add_value, Type, Value, Date}).

store_data() ->
    gen_statem:cast(?SERVER, {store_data}).


station_not_set(_EventType, {set_station, Key}, State = #state) ->
    case pollution:find_station(Key, State#state.monitor) of
        {error, Msg} ->
            print_error(Msg);
        _Station ->
            {next_state, station_set, State#state{key = Key}}
    end.

station_set(_EventType, {add_value, Type, Value, Date}, State = #state) ->
    Reading = #reading(type = Type, value = Value, date = Date),
    ReadingList = [Reading | State#state.readings],
    {keep_state, State#state{readings = ReadingList}}.
station_set(_EventType, {store_data}, State = #state) ->
    commit_data(State#state.readings, State#state.key, State#state.monitor),
    {next_state, station_not_set, #state{monitor = State#state.monitor}}.


commit_data([], _Key, _Monitor) -> ok.
commit_data([Reading = #reading | Tail], Key, Monitor) ->
    [Date, Type, Value] = [Reading#reading.date, Reading#reading.type, Reading#reading.value],
    case pollution:add_value(Key, Date, Type, Value, Monitor) of
        {error, Msg} ->
            print_error(Msg),
            commit_data(Tail, Key, Monitor);
        NewMonitor ->
            commit_data(Tail, Key, NewMonitor)
    end.
%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init(Monitor) ->
  {ok, station_not_set, #state{monitor = Monitor}}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
  handle_event_function.

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
state_name(_EventType, _EventContent, State = #pollution_value_collector_gen_statem_state{}) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.

%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #pollution_value_collector_gen_statem_state{}) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State = #pollution_value_collector_gen_statem_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #pollution_value_collector_gen_statem_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

print_error(Error) ->
    io:format("ERROR: ~w. ~n", [Error]),
    ok.