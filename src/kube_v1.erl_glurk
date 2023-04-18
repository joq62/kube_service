%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 15 Mar 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(kube).

-include("log.api").

-define(SERVER,kube_server).

%% API
-export([
	 %% host controller
	 start_host_controller/1,
	 stop_host_controller/1,
	 is_started_host_controller/1,

	 start_host_controller/2,
	 stop_host_controller/2,
	 is_started_host_controller/2,
	 
	 %% provider 
	 load_provider/2,
	 load_provider/3,
	 
	 start_provider/2,
	 start_provider/3,

	 unload_provider/2,
	 unload_provider/3,

	 stop_provider/2,
	 stop_provider/3,

	 is_loaded_provider/2,
	 is_loaded_provider/3,

	 is_started_provider/2,
	 is_started_provider/3,
	 
	 %% 

	 orchestrate_result/1,
	 orchestrate_result/2,
	 ping/0,
	 ping/1

	]).


%% Leader API
-export([
	 
	]).

-export([
	 start/0,
	 stop/0,
	 start/1,
	 stop/1
	]).


%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
orchestrate_result(Result,{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp})->
    io:format("Time: ~p~n",[calendar:now_to_datetime(erlang:timestamp())]),
    io:format("Sender: ~p~n",[{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp}]),
    io:format("Input: ~p~n",[{?MODULE,?FUNCTION_NAME,[Result]}]),
    T1=os:system_time(millisecond),
    Result=orchestrate_result(Result),
    T2=os:system_time(millisecond),
    io:format("Output: ~p~n",[{?MODULE,?FUNCTION_NAME,[Result]}]),
    io:format("Exection time (ms): ~p~n~n",[T2-T1]),
    Result.
    
orchestrate_result(Result)->
    gen_server:cast(?SERVER,{orchestrate_result,Result}).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
load_provider(ProviderSpec,HostSpec,{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp})->
    io:format("Time: ~p~n",[calendar:now_to_datetime(erlang:timestamp())]),
    io:format("Sender: ~p~n",[{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp}]),
    io:format("Input: ~p~n",[{?MODULE,?FUNCTION_NAME,[ProviderSpec,HostSpec]}]),
    T1=os:system_time(millisecond),
    Result=load_provider(ProviderSpec,HostSpec),
    T2=os:system_time(millisecond),
    io:format("Output: ~p~n",[{?MODULE,?FUNCTION_NAME,[Result]}]),
    io:format("Exection time (ms): ~p~n~n",[T2-T1]),
    Result.

load_provider(ProviderSpec,HostSpec)->
    gen_server:call(?SERVER, {load_provider,ProviderSpec,HostSpec},infinity).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
unload_provider(ProviderSpec,HostSpec,{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp})->
    io:format("Time: ~p~n",[calendar:now_to_datetime(erlang:timestamp())]),
    io:format("Sender: ~p~n",[{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp}]),
    io:format("Input: ~p~n",[{?MODULE,?FUNCTION_NAME,[ProviderSpec,HostSpec]}]),
    T1=os:system_time(millisecond),
    Result=unload_provider(ProviderSpec,HostSpec),
    T2=os:system_time(millisecond),
    io:format("Output: ~p~n",[{?MODULE,?FUNCTION_NAME,[Result]}]),
    io:format("Exection time (ms): ~p~n~n",[T2-T1]),
    Result.

unload_provider(ProviderSpec,HostSpec)->
    gen_server:call(?SERVER, {unload_provider,ProviderSpec,HostSpec},infinity).

%% Not fixed


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

start_provider(ProviderSpec,HostSpec,{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp})->
    io:format("Time: ~p~n",[calendar:now_to_datetime(erlang:timestamp())]),
    io:format("Sender: ~p~n",[{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp}]),
    io:format("Input: ~p~n",[{?MODULE,?FUNCTION_NAME,[ProviderSpec,HostSpec]}]),
    T1=os:system_time(millisecond),
    Result=start_provider(ProviderSpec,HostSpec),
    T2=os:system_time(millisecond),
    io:format("Output: ~p~n",[{?MODULE,?FUNCTION_NAME,[Result]}]),
    io:format("Exection time (ms): ~p~n~n",[T2-T1]),
    Result.

start_provider(ProviderSpec,HostSpec)->
    gen_server:call(?SERVER, {start_provider,ProviderSpec,HostSpec},infinity).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

stop_provider(ProviderSpec,HostSpec,{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp})->
    io:format("Time: ~p~n",[calendar:now_to_datetime(erlang:timestamp())]),
    io:format("Sender: ~p~n",[{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp}]),
    io:format("Input: ~p~n",[{?MODULE,?FUNCTION_NAME,[ProviderSpec,HostSpec]}]),
    T1=os:system_time(millisecond),
    Result=stop_provider(ProviderSpec,HostSpec),
    T2=os:system_time(millisecond),
    io:format("Output: ~p~n",[{?MODULE,?FUNCTION_NAME,[Result]}]),
    io:format("Exection time (ms): ~p~n~n",[T2-T1]),
    Result.

stop_provider(ProviderSpec,HostSpec)->
    gen_server:call(?SERVER, {stop_provider,ProviderSpec,HostSpec},infinity).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
is_started_provider(ProviderSpec,HostSpec,{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp})->
    ?LOG_NOTICE("is_started_test",[ProviderSpec,HostSpec]),
    io:format("Time: ~p~n",[calendar:now_to_datetime(erlang:timestamp())]),
    io:format("Sender: ~p~n",[{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp}]),
    io:format("Input: ~p~n",[{?MODULE,?FUNCTION_NAME,[ProviderSpec,HostSpec]}]),
    T1=os:system_time(millisecond),
    Result=is_started_provider(ProviderSpec,HostSpec),
    T2=os:system_time(millisecond),
    io:format("Output: ~p~n",[{?MODULE,?FUNCTION_NAME,[Result]}]),
    io:format("Exection time (ms): ~p~n~n",[T2-T1]),
    Result.
is_started_provider(ProviderSpec,HostSpec)->
    gen_server:call(?SERVER, {is_started_provider,ProviderSpec,HostSpec},infinity).    
 
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
is_loaded_provider(ProviderSpec,HostSpec,{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp})->
    io:format("Time: ~p~n",[calendar:now_to_datetime(erlang:timestamp())]),
    io:format("Sender: ~p~n",[{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp}]),
    io:format("Input: ~p~n",[{?MODULE,?FUNCTION_NAME,[ProviderSpec,HostSpec]}]),
    T1=os:system_time(millisecond),
    Result=is_loaded_provider(ProviderSpec,HostSpec),
    T2=os:system_time(millisecond),
    io:format("Output: ~p~n",[{?MODULE,?FUNCTION_NAME,[Result]}]),
    io:format("Exection time (ms): ~p~n~n",[T2-T1]),
    Result.
is_loaded_provider(ProviderSpec,HostSpec)->
    gen_server:call(?SERVER, {is_loaded_provider,ProviderSpec,HostSpec},infinity).    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

start_host_controller(HostSpec,{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp})->
    ?LOG_NOTICE("testing",[HostSpec]),
    io:format("Time: ~p~n",[calendar:now_to_datetime(erlang:timestamp())]),
    io:format("Sender: ~p~n",[{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp}]),
    io:format("Input: ~p~n",[{?MODULE,?FUNCTION_NAME,[HostSpec]}]),
    T1=os:system_time(millisecond),
    Result=start_host_controller(HostSpec),
    T2=os:system_time(millisecond),
    io:format("Output: ~p~n",[{?MODULE,?FUNCTION_NAME,[Result]}]),
    io:format("Exection time (ms): ~p~n~n",[T2-T1]),
    Result.

start_host_controller(HostSpec)->
    gen_server:call(?SERVER, {start_host_controller,HostSpec},infinity).

    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

stop_host_controller(HostSpec,{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp})->
   ?LOG_NOTICE("logging",[HostSpec]),
    io:format("Time: ~p~n",[calendar:now_to_datetime(erlang:timestamp())]),
    io:format("Sender: ~p~n",[{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp}]),
    io:format("Input: ~p~n",[{?MODULE,?FUNCTION_NAME,[HostSpec]}]),
    T1=os:system_time(millisecond),
    Result=stop_host_controller(HostSpec),
    T2=os:system_time(millisecond),
    io:format("Output: ~p~n",[{?MODULE,?FUNCTION_NAME,[Result]}]),
    io:format("Exection time (ms): ~p~n~n",[T2-T1]),
    Result.

stop_host_controller(HostSpec)->
    gen_server:call(?SERVER, {stop_host_controller,HostSpec},infinity).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
is_started_host_controller(HostSpec,{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp})->
    io:format("Time: ~p~n",[calendar:now_to_datetime(erlang:timestamp())]),
    io:format("Sender: ~p~n",[{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp}]),
    io:format("Input: ~p~n",[{?MODULE,?FUNCTION_NAME,[HostSpec]}]),
    T1=os:system_time(millisecond),
    Result=is_started_host_controller(HostSpec),
    T2=os:system_time(millisecond),
    io:format("Output: ~p~n",[{?MODULE,?FUNCTION_NAME,[Result]}]),
    io:format("Exection time (ms): ~p~n~n",[T2-T1]),
    Result.
is_started_host_controller(HostSpec)->
    gen_server:call(?SERVER, {is_started_host_controller,HostSpec},infinity).    
 

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start({SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp})->
   ?LOG_NOTICE("Start gen_server",[]),
    io:format("Time: ~p~n",[calendar:now_to_datetime(erlang:timestamp())]),
    io:format("Sender: ~p~n",[{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp}]),
    io:format("Input: ~p~n",[{?MODULE,?FUNCTION_NAME,[]}]),
    T1=os:system_time(millisecond),
    Result=start(),
    T2=os:system_time(millisecond),
    io:format("Output: ~p~n",[{?MODULE,?FUNCTION_NAME,[Result]}]),
    io:format("Exection time (ms): ~p~n~n",[T2-T1]),
    Result.

start()->
    gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
stop({SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp})->
    io:format("Time: ~p~n",[calendar:now_to_datetime(erlang:timestamp())]),
    io:format("Sender: ~p~n",[{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp}]),
    io:format("Input: ~p~n",[{?MODULE,?FUNCTION_NAME,[]}]),
    T1=os:system_time(millisecond),
    Result=stop(),
    T2=os:system_time(millisecond),
    io:format("Output: ~p~n",[{?MODULE,?FUNCTION_NAME,[Result]}]),
    io:format("Exection time (ms): ~p~n~n",[T2-T1]),
    Result.

stop()->
    gen_server:call(?SERVER, {stop},infinity).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
ping({SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp})->
    ?LOG_NOTICE("ping",[]),
    io:format("Time: ~p~n",[calendar:now_to_datetime(erlang:timestamp())]),
    io:format("Sender: ~p~n",[{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp}]),
    io:format("Input: ~p~n",[{?MODULE,?FUNCTION_NAME,[]}]),
    T1=os:system_time(millisecond),
    Result=ping(),
    T2=os:system_time(millisecond),
    io:format("Output: ~p~n",[{?MODULE,?FUNCTION_NAME,[Result]}]),
    io:format("Exection time (ms): ~p~n~n",[T2-T1]),
    Result.

ping()->
    gen_server:call(?SERVER, {ping},infinity).


%%%===================================================================
%%% Internal functions
%%%===================================================================

