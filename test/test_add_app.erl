%%%-------------------------------------------------------------------
%% @doc test_add public API
%% @end
%%%-------------------------------------------------------------------

-module(test_add_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    test_add_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
