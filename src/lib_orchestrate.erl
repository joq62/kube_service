%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-module(lib_orchestrate).

-export([
	 candidates/1,
	 get_candidates/3,
	 host_controller_update/0,
	 provider_update/0
	]).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
host_controller_update()->
    MissingHostControllers=[HostSpec||HostSpec<-db_host_spec:get_all_id(),
				      false==kube:is_started_host_controller(HostSpec)],
    Result=case MissingHostControllers of
	       []->
		   {ok,["desired state"]};
	       _->
		   [{HostSpec,kube:start_host_controller(HostSpec)}||HostSpec<-MissingHostControllers]
	   end,
    Result.
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
provider_update()->
    ProvidersToStart=[{ProviderSpec,get_candidates(ProviderSpec)}||ProviderSpec<-db_provider_spec:get_all_id()],
    Result=case ProvidersToStart of
	       []->
		   {ok,["desired state"]};
	       _-> 
		   [stop_provider(ProviderSpec,HostSpecList)||{ProviderSpec,HostSpecList}<-ProvidersToStart],
		   [unload_provider(ProviderSpec,HostSpecList)||{ProviderSpec,HostSpecList}<-ProvidersToStart],
		   [load_provider(ProviderSpec,HostSpecList)||{ProviderSpec,HostSpecList}<-ProvidersToStart],
		   R4=[start_provider(ProviderSpec,HostSpecList)||{ProviderSpec,HostSpecList}<-ProvidersToStart],   		   
		   {ok,["Start providers ",R4]}
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_provider(ProviderSpec,HostSpecList)->
    start_provider(ProviderSpec,HostSpecList,[]).
start_provider(_ProviderSpec,[],Acc)->
    Acc;
start_provider(ProviderSpec,[HostSpec|T],Acc)->
    Result=kube:start_provider(ProviderSpec,HostSpec),
    start_provider(ProviderSpec,T,[{Result,ProviderSpec,HostSpec}|Acc]).
 
 
load_provider(ProviderSpec,HostSpecList)->
    load_provider(ProviderSpec,HostSpecList,[]).
load_provider(_ProviderSpec,[],Acc)->
    Acc;
load_provider(ProviderSpec,[HostSpec|T],Acc)->
    Result=kube:load_provider(ProviderSpec,HostSpec),
    load_provider(ProviderSpec,T,[{Result,ProviderSpec,HostSpec}|Acc]).
 


stop_provider(ProviderSpec,HostSpecList)->
    stop_provider(ProviderSpec,HostSpecList,[]).
stop_provider(_ProviderSpec,[],Acc)->
    Acc;
stop_provider(ProviderSpec,[HostSpec|T],Acc)->
    Result=kube:stop_provider(ProviderSpec,HostSpec),
    stop_provider(ProviderSpec,T,[{Result,ProviderSpec,HostSpec}|Acc]).
 
unload_provider(ProviderSpec,HostSpecList)->
    unload_provider(ProviderSpec,HostSpecList,[]).
unload_provider(_ProviderSpec,[],Acc)->
    Acc;
unload_provider(ProviderSpec,[HostSpec|T],Acc)->
    Result=kube:unload_provider(ProviderSpec,HostSpec),
    unload_provider(ProviderSpec,T,[{Result,ProviderSpec,HostSpec}|Acc]).
 
    

    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
candidates(ProviderSpec)->
    Result=case db_provider_spec:member(ProviderSpec) of
	       false->
		   {error,["eexists ",ProviderSpec]};
	       true->
		   {ok,Num}=db_provider_spec:read(num,ProviderSpec),
		   {ok,Affinity}=db_provider_spec:read(affinity,ProviderSpec),
		   get_candidates(Affinity,ProviderSpec,Num)	   
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
get_candidates(ProviderSpec)->
    {ok,Num}=db_provider_spec:read(num,ProviderSpec),
    {ok,Affinity}=db_provider_spec:read(affinity,ProviderSpec),
    get_candidates(Affinity,ProviderSpec,Num).

get_candidates([all_hosts],ProviderSpec,_Num)->
    TotalNumPossibleProviders=erlang:length(db_host_spec:get_all_id()),
    
    MissingProviders=TotalNumPossibleProviders-erlang:length([true||HostSpec<-db_host_spec:get_all_id(),
								    true==kube:is_started_provider(ProviderSpec,HostSpec)]),
    Result=case MissingProviders of
	       0->
		   [];
	       _->
		   L0=[{HostSpec,db_host_spec:read(hostname,HostSpec)}||HostSpec<-db_host_spec:get_all_id()],
		   HostSpecNameList=[{HostSpec,HostName}||{HostSpec,{ok,HostName}}<-L0,
							  false==kube:is_started_provider(ProviderSpec,HostSpec)],
		   HostNameLength=[{HostName,erlang:length(AppList)}||{_Node,HostName,AppList}<-sd:all()],
		   HostSpecLength=change_to_host_spec(HostNameLength,HostSpecNameList,[]),
		   SumList=sum(HostSpecLength,[]),
		   SortedHostSpecLength=qsort(SumList),
		   Candidates=[HostSpec||{HostSpec,_}<-SortedHostSpecLength],
		   Candidates
	   end,
    Result;
		 

get_candidates([any_host],ProviderSpec,Num)->
    TotalNumPossibleProviders=Num,
    MissingProviders=TotalNumPossibleProviders-erlang:length([true||HostSpec<-db_host_spec:get_all_id(),
					      true==kube:is_started_provider(ProviderSpec,HostSpec)]),
    Result=case MissingProviders of
	       0->
		   [];
	       _->
		   L0=[{HostSpec,db_host_spec:read(hostname,HostSpec)}||HostSpec<-db_host_spec:get_all_id()],
		   HostSpecNameList=[{HostSpec,HostName}||{HostSpec,{ok,HostName}}<-L0,
							  false==kube:is_started_provider(ProviderSpec,HostSpec)],
		   HostNameLength=[{HostName,erlang:length(AppList)}||{_Node,HostName,AppList}<-sd:all()],
		   HostSpecLength=change_to_host_spec(HostNameLength,HostSpecNameList,[]),
		   SumList=sum(HostSpecLength,[]),
		   SortedHostSpecLength=qsort(SumList),
		   Candidates=[HostSpec||{HostSpec,_}<-SortedHostSpecLength],
		   lists:sublist(Candidates,MissingProviders)
	   end,
    Result;

get_candidates(HostList,ProviderSpec,Num)->
    TotalNumPossibleProviders=Num,
    MissingProviders=TotalNumPossibleProviders-erlang:length([true||HostSpec<-db_host_spec:get_all_id(),
					      true==kube:is_started_provider(ProviderSpec,HostSpec)]),
    Result=case MissingProviders of
	       0->
		   [];
	       _->			       
		   L0=[{HostSpec,db_host_spec:read(hostname,HostSpec)}||HostSpec<-HostList],
		   HostSpecNameList=[{HostSpec,HostName}||{HostSpec,{ok,HostName}}<-L0,
							  false==kube:is_started_provider(ProviderSpec,HostSpec)],
		   HostNameLength=[{HostName,erlang:length(AppList)}||{_Node,HostName,AppList}<-sd:all()],
		   HostSpecLength=change_to_host_spec(HostNameLength,HostSpecNameList,[]),
		   SumList=sum(HostSpecLength,[]),
		   SortedHostSpecLength=qsort(SumList),
		   SortedHostSpecLength=qsort(SumList),
		   Candidates=[HostSpec||{HostSpec,_}<-SortedHostSpecLength],
		   lists:sublist(Candidates,MissingProviders)
	   end,
    Result.



%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
change_to_host_spec([],_HostSpecNameList,Acc)->
    Acc;
change_to_host_spec([{HostName,N}|T],HostSpecNameList,Acc)->
    Result=[{HostSpec,N}||{HostSpec,XHostName}<-HostSpecNameList,
				 HostName==XHostName],    
    change_to_host_spec(T,HostSpecNameList,lists:append(Result,Acc)).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
sum([],SumList)->
    SumList;
sum([{HostName,N}|T],Acc) ->
    NewAcc=case lists:keyfind(HostName,1,Acc) of
	       false->
		   
		   [{HostName,N}|Acc];
	       {HostName,N_Acc} ->
		   lists:keyreplace(HostName,1, Acc, {HostName,N+N_Acc})
	   end,
    sum(T,NewAcc).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
qsort([{HostName,PIn}|T])->
    qsort([{H1,PX} || {H1,PX} <- T, PX < PIn]) ++
	[{HostName,PIn}] ++
	qsort([{H1,PX} || {H1,PX} <- T, PX >= PIn]);
qsort([]) -> [].
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
