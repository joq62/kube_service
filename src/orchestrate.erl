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

-define(SleepInterval,30*1000).
-define(LockTimeout, 2*?SleepInterval).

%% API
-export([
	 start/1,
	 start/2,
	 start/3
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start(LockId,WantedState)->
    start(LockId,WantedState,?SleepInterval).

start(LockId,WantedState,SleepInterval)->
    Result=case sd:call(dbetcd,db_lock,try_lock,[LockId,?LockTimeout],5000) of
	       {error,Reason}->
		   {error,["Failed calling dbetcd,db_lock,try_lock: ",Reason,LockId,?LockTimeout,?MODULE,?FUNCTION_NAME,?LINE]};
	       {badrpc,Reason}->
		   {error,["badrpc Failed calling dbetcd,db_lock,try_lock: ",Reason,LockId,?LockTimeout,?MODULE,?FUNCTION_NAME,?LINE]};
	       locked ->
		   timer:sleep(SleepInterval),
		   locked;
	       {ok,TransactionId} ->
		   {ok,StartControllers}=controllers(WantedState),
		   {ok,StartProviders}=providers(WantedState),
		   timer:sleep(SleepInterval),
		   sd:call(dbetcd,db_lock,unlock,[LockId,TransactionId],5000),
		   {ok,StartControllers,StartProviders}
	   end,
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
    Loaded=lib_provider:is_loaded(ProviderSpec,HostSpec),
    Started=lib_provider:is_started(ProviderSpec,HostSpec),
    NewAcc=case {Loaded,Started} of
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
	   end,
    providers(T,NewAcc).
    
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

