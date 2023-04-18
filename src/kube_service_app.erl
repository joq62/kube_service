%%%-------------------------------------------------------------------
%% @doc kube_service public API
%% @end
%%%-------------------------------------------------------------------

-module(kube_service_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    kube_service_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
