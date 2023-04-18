%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 18 Jan 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(orchestrate).

-include("log.api").

-define(SleepInterval,60*1000).
-define(LockTimeout, 3*60*1000).

%% API
-export([
	 start/1,
	 start/2
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start(LockId)->
    start(LockId,?SleepInterval).

start(LockId,SleepInterval)->
    Result=case sd:call(dbetcd,db_lock,try_lock,[LockId,?LockTimeout],5000) of
	       {error,Reason}->
		   {error,["Failed calling dbetcd,db_lock,try_lock: ",Reason,LockId,?LockTimeout,?MODULE,?FUNCTION_NAME,?LINE]};
	       {badrpc,Reason}->
		   {error,["badrpc Failed calling dbetcd,db_lock,try_lock: ",Reason,LockId,?LockTimeout,?MODULE,?FUNCTION_NAME,?LINE]};
	       locked ->
		   timer:sleep(SleepInterval),
		   locked;
	       {ok,TransactionId} ->
		   ResultHostController=check_and_start_host_controllers(),
		   ResultProvider=check_and_start_providers(),
		   timer:sleep(SleepInterval),
		   sd:call(dbetcd,db_lock,unlock,[LockId,TransactionId],5000),
		   {ok,ResultHostController,[not_implmented]}
	   end,
    rpc:cast(node(),kube,orchestrate_result,[Result]).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------


%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
check_and_start_providers()->
    Result=case sd:call(dbetcd,db_provider_spec,get_all_id,[],5000) of
	       {error,Reason}->
		   {error,["Failed calling dbetcd,db_host_spec,read,get_all_id: ",Reason,?MODULE,?FUNCTION_NAME,?LINE]};
	       {badrpc,Reason}->
		   {error,["badrpc Failed calling dbetcd,db_host_spec,read,get_all_id: ",Reason,?MODULE,?FUNCTION_NAME,?LINE]};
	       AllProviderSpecs->
		 %  io:format("AllProviderSpecs ~p~n",[{AllProviderSpecs,?MODULE,?FUNCTION_NAME,?LINE}]),
		   case sd:call(dbetcd,db_host_spec,get_all_id,[],5000) of
		       {error,Reason}->
			   {error,["Failed calling dbetcd,db_host_spec,read,get_all_id: ",Reason,?MODULE,?FUNCTION_NAME,?LINE]};
		       {badrpc,Reason}->
			   {error,["badrpc Failed calling dbetcd,db_host_spec,read,get_all_id: ",Reason,?MODULE,?FUNCTION_NAME,?LINE]};
		       AllHostSpecs->
		%	   io:format("AllHostSpecs ~p~n",[{AllHostSpecs,?MODULE,?FUNCTION_NAME,?LINE}]),
			   ProviderAndHostSpecList=[{ProviderSpec,HostSpec}||ProviderSpec<-AllProviderSpecs,
									     HostSpec<-AllHostSpecs],
			   io:format("ProviderAndHostSpecList ~p~n",[{ProviderAndHostSpecList,?MODULE,?FUNCTION_NAME,?LINE}]),

			   MissingProviders=[{ProviderSpec,HostSpec}||{ProviderSpec,HostSpec}<-ProviderAndHostSpecList,
						       false== kube:is_started_provider(ProviderSpec,HostSpec)],
			   R=map_start_provider(MissingProviders),
			   R
		   end
	   end,
    ?LOG_NOTICE("start_provider result ",[Result]),
    Result.

map_start_provider(MissingProviders)->
    io:format(" map_start_provider MissingProviders ~p~n",[MissingProviders]),
    F1 = fun start_provider/2,
    F2 = fun provider_result/3,
    R=mapreduce:start(F1,F2,[],MissingProviders),
    io:format(" map_start_provider R ~p~n",[R]),
    R.



start_provider(Pid,{ProviderSpec,HostSpec})->
    io:format("Pid,ProviderSpec,HostSpec ~p~n",[{Pid,ProviderSpec,HostSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
 %   Result=case kube:load_provider(ProviderSpec,HostSpec) of
    Result=case lib_provider:load(ProviderSpec,HostSpec) of
	       {error,Reason}->
		   {error,Reason};
	       {ok,_ProviderNode,_App}->
	%	   kube:start_provider(ProviderSpec,HostSpec)
		   lib_provider:start(ProviderSpec,HostSpec)
	   end,
    Pid!{start_provider,Result}.

provider_result(start_provider,Vals,Acc)->
    [Vals|Acc].    


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
check_and_start_host_controllers()->
    Result=case sd:call(dbetcd,db_host_spec,get_all_id,[],5000) of
	       {error,Reason}->
		   {error,["Failed calling dbetcd,db_host_spec,read,get_all_id: ",Reason,?MODULE,?FUNCTION_NAME,?LINE]};
	       {badrpc,Reason}->
		   {error,["badrpc Failed calling dbetcd,db_host_spec,read,get_all_id: ",Reason,?MODULE,?FUNCTION_NAME,?LINE]};
	       AllHostSpecs->
		   MissingHostControllers=[HostSpec||HostSpec<-AllHostSpecs,
						     false==lib_host:is_started_host_controller(HostSpec)],
		   R=map_start_host_controller(MissingHostControllers),
		   ?LOG_NOTICE("start_host_controller result ",[R]),
		   R
		   
	   end,
    Result.

map_start_host_controller(HostSpecList)->
    F1 = fun start_host/2,
    F2 = fun host_result/3,
    [R]=mapreduce:start(F1,F2,[],HostSpecList),
    io:format(" R ~p~n",[R]),
    R.



start_host(Pid,HostSpec)->
    Pid!{start_host_controller,lib_host:start_host_controller(HostSpec)}.

host_result(start_host_controller,Vals,Acc)->
    [Vals|Acc].
    

