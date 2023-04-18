%%%-------------------------------------------------------------------
%% @doc ops public API
%% @end
%%%-------------------------------------------------------------------

-module(tests_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    tests_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
