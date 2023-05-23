%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 18 Jan 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(orchestrate2).
 
-include("log.api").

-define(SleepInterval,30*1000).
-define(LockTimeout, 2*?SleepInterval).
-define(InfraDeploymentSpec,"infra").
-define(InfraApps,[dbetcd_appl,kube_appl]).
-define(Type,"provider").

%% API
-export([
	 create_deployments/1,
	 start_infra_providers/0,

	 update_deployment/1,
	 start/1,
	 start/2,
	 start/3,

	 is_wanted_state/0
	]).
% io:format("X ~p~n",[{X,?MODULE,?FUNCTION_NAME,?LINE}]),
%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_deployments(DeploymentSpec)->    
    {DeploymentSpec,ProviderDeployments}=sd:call(dbetcd_appl,db_deployment_spec,read,[DeploymentSpec],5000),
    Result=[lib_provider2:create_deployment(ProviderSpec,HostSpec,?Type)||{ProviderSpec,HostSpec}<-ProviderDeployments],
    Result.
    


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_infra_providers()->
    InfraDeployIds=get_infra_deploy(),
    io:format("InfraDeployIds ~p~n",[{InfraDeployIds,?MODULE,?FUNCTION_NAME,?LINE}]),
    DeploymentStatus_Id=[{lib_provider2:is_deployed(DeploymentId),DeploymentId}||DeploymentId<-InfraDeployIds],
    StartResult=[{rpc:call(node(),lib_provider2,load_start_ssh,[DeploymentId],5000),DeploymentId}||{false,DeploymentId}<-DeploymentStatus_Id],
    
    StartResult.
  
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------



%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

get_infra_deploy()->
    AllDeployId=sd:call(dbetcd_appl,db_deploy,get_all_id,[],5000),
    DeployId_ProviderSpecResult=[{DeployId,sd:call(dbetcd_appl,db_deploy,read,[provider_spec,DeployId],5000)}||DeployId<-AllDeployId],
    get_infra_deploy(DeployId_ProviderSpecResult).

get_infra_deploy(L)->
    get_infra_deploy(L,[]).
get_infra_deploy([],Acc)->
    Acc;
	
get_infra_deploy([{DeployId,{ok,ProviderSpec}}|T],Acc)->
    {ok,App}=sd:call(dbetcd_appl,db_provider_spec,read,[app,ProviderSpec],5000),
    NewAcc=case lists:member(App,?InfraApps) of
	       false->
		   Acc;
	       true ->
		   [DeployId|Acc]
	   end,
    get_infra_deploy(T,NewAcc).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_providers()->
    
    ok.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

    
update_deployment(Glurk)->

    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
is_wanted_state()->
     [{"production",WantedState}]=sd:call(dbetcd_appl,db_deployment_spec,read_all,[],5000),
    DeployResult=[{ProviderSpec,HostSpec}||{ProviderSpec,HostSpec}<-WantedState,
					   true=/=lib_provider:is_started(ProviderSpec,HostSpec)],
    Result=case DeployResult of
	       []->
		   true;
	       NotDeployed ->
		   {false,NotDeployed}
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start(LockId,WantedState)->
    start(LockId,WantedState,?SleepInterval).

start(LockId,WantedState,SleepInterval)->
    Result=case sd:call(dbetcd_appl,db_lock,try_lock,[LockId,?LockTimeout],5000) of
	       {error,Reason}->
		   ?LOG_NOTICE("Failed calling dbetcd,db_lock,try_lock: ",[]),
		   {error,["Failed calling dbetcd,db_lock,try_lock: ",Reason,LockId,?LockTimeout,?MODULE,?FUNCTION_NAME,?LINE]};
	       {badrpc,Reason}->
		   ?LOG_NOTICE("badrpc Failed calling dbetcd,db_lock,try_lock: ",[]),
		   {error,["badrpc Failed calling dbetcd,db_lock,try_lock: ",Reason,LockId,?LockTimeout,?MODULE,?FUNCTION_NAME,?LINE]};
	       locked ->
		   ?LOG_NOTICE("Locked  ",[]),
		   timer:sleep(SleepInterval),
		   locked;
	       {ok,TransactionId} ->
		   ?LOG_NOTICE("Un Locked  ",[]),
		   {ok,StartControllers}=controllers(WantedState),
		   {ok,StartProviders}=providers(WantedState),
		 %  timer:sleep(SleepInterval),
		   sd:call(dbetcd_appl,db_lock,unlock,[LockId,TransactionId],5000),
		   {ok,StartControllers,StartProviders}
	   end,

    timer:sleep(SleepInterval),
    rpc:cast(node(),kube,orchestrate_result,[Result]).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start(WantedState)->
    {ok,StartControllers}=controllers(WantedState),
    {ok,StartProviders}=providers(WantedState),
    timer:sleep(?SleepInterval),
    rpc:cast(node(),kube,orchestrate_result,[{ok,StartControllers,StartProviders}]).

    

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
controllers(WantedState)->
    WantedHostSpecs=list_rm_duplicates(WantedState),
    MissingControlles=[HostSpec||HostSpec<-WantedHostSpecs,
				 false==lib_host:is_controller_started(HostSpec)],
    StartControllers=[{lib_host:start_controller(HostSpec),HostSpec}||HostSpec<-MissingControlles],    
    {ok,StartControllers}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
list_rm_duplicates(L)->
    list_rm_duplicates(L,[]).
list_rm_duplicates([],Acc)->
    Acc;
list_rm_duplicates([{_,HostSpec}|T],Acc)->
    NewAcc=case lists:member(HostSpec,Acc) of
	       true->
		   Acc;
	       false ->
		   [HostSpec|Acc]
	   end,
    list_rm_duplicates(T,NewAcc).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
providers(WantedState)->
    StartMissingProviders=providers(WantedState,[]),
    {ok,StartMissingProviders}.

providers([],Acc)->
    Acc;
providers([{ProviderSpec,HostSpec}|T],Acc)->
    NewAcc=case lib_host:is_controller_started(HostSpec) of
	       false->
		   [{error,["host controller not started ",HostSpec,?MODULE,?LINE]}|Acc];
	       true->
		   Loaded=lib_provider:is_loaded(ProviderSpec,HostSpec),
		   Started=lib_provider:is_started(ProviderSpec,HostSpec),
		   case {Loaded,Started} of
		       {false,_}->
			   case lib_provider:load(ProviderSpec,HostSpec) of
			       {ok,ProviderSpec,HostSpec,_ProviderNode,_ProviderApp}->
				   [{lib_provider:start(ProviderSpec,HostSpec),ProviderSpec,HostSpec}|Acc];
			       {error,Reason}->
				   [{error,[ProviderSpec,HostSpec,Reason,?MODULE,?LINE]}|Acc]
			   end;
		       {true,false}->
			   [{lib_provider:start(ProviderSpec,HostSpec),ProviderSpec,HostSpec}|Acc];
		       {true,true}->
			   Acc
		   end
	   end,
    providers(T,NewAcc).
    
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

