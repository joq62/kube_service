%%%-------------------------------------------------------------------
%% @doc test_divi public API
%% @end
%%%-------------------------------------------------------------------

-module(test_divi_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    test_divi_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
