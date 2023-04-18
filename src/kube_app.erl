%%%-------------------------------------------------------------------
%% @doc main public API
%% @end
%%%-------------------------------------------------------------------

-module(kube_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("Dbg node + cookie ~p~n",[{node(),erlang:get_cookie()}]),
    kube_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
