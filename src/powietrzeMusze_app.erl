%%%-------------------------------------------------------------------
%% @doc powietrzeMusze public API
%% @end
%%%-------------------------------------------------------------------

-module(powietrzeMusze_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    powietrzeMusze_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
