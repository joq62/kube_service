%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point   
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is§
%%% -------------------------------------------------------------------
-module(basic).   
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->

    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    {ok,HostName}=net:gethostname(),
    [N0,N1,N2]=setup(),
    

    ok=load_test([N0,N1,N2],HostName),
    ok=start_1_test([N0,N1,N2],HostName),
  
    
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
 
    init:stop(),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
start_1_test([N0,N1,N2],HostName)->

    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    ok=rpc:call(N0,application,start,[test_add],5000),
    ok=rpc:call(N1,application,start,[test_divi],5000),
    ok=rpc:call(N2,application,start,[test_add],5000),
    ok=rpc:call(N2,application,start,[test_divi],5000),

    ['n0@c50','n2@c50']=lists:sort(sd:get_node(test_add)),
    ['n1@c50','n2@c50']=lists:sort(sd:get_node(test_divi)),  
    []=sd:get_node(glurk), 
    [{'n0@c50',"c50"},{'n2@c50',"c50"}]=lists:sort(sd:get_node_on_host(test_add,HostName)),
    [{'n1@c50',"c50"},{'n2@c50',"c50"}]=lists:sort(sd:get_node_on_host(test_divi,HostName)),
    []=sd:get_node_on_host(test_divi,"glurk_hostname"),
    []=sd:get_node_on_host(glurk,HostName),
    []=sd:get_node_on_host(glurk,"glurk_hostname"),
    M=test_add,
    F=add,
    A=[20,22],
    T=5000,
    42=sd:call(test_add,M,F,A,T),
    {error,["No node available for app : ",glurk,sd,_]}=sd:call(glurk,M,F,A,T),
    {badrpc,_}=sd:call(test_add,glurk,F,A,T),
    {badrpc,_}=sd:call(test_add,M,glurk,A,T),
    {badrpc,_}=sd:call(test_add,M,F,[a,34],T),
    
    true=sd:cast(test_add,M,F,A),
    io:format(" sd:all ~p~n",[sd:all()]),

    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
  
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
load_test([N0,N1,N2],HostName)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=rpc:call(N0,application,load,[test_add],5000),
    ok=rpc:call(N1,application,load,[test_divi],5000),
    ok=rpc:call(N2,application,load,[test_add],5000),
    ok=rpc:call(N2,application,load,[test_divi],5000),

    []=sd:get_node(test_add),
    []=sd:get_node(test_divi),  
    []=sd:get_node(glurk), 
    []=sd:get_node_on_host(test_add,HostName),
    []=sd:get_node_on_host(test_divi,HostName),
    []=sd:get_node_on_host(test_divi,"glurk_hostname"),
    []=sd:get_node_on_host(glurk,HostName),
    []=sd:get_node_on_host(glurk,"glurk_hostname"),
    M=test_add,
    F=add,
    A=[20,22],
    T=5000,
    {error,["No node available for app : ",test_add,sd,_]}=sd:call(test_add,M,F,A,T),
    {error,["No node available for app : ",glurk,sd,_]}=sd:call(glurk,M,F,A,T),
    {error,["No node available for app : ",test_add,sd,_]}=sd:call(test_add,glurk,F,A,T),
    {error,["No node available for app : ",test_add,sd,_]}=sd:call(test_add,M,glurk,A,T),
    {error,["No node available for app : ",test_add,sd,_]}=sd:call(test_add,M,F,[a,34],T),
    {error,["No node available for app : ",test_add,sd,_]}=sd:cast(test_add,M,F,A),

   
    ok.


%call(App,M,F,A,T)
%cast(App,M,F,A)
%all(),
%get(WantedApp)
%get_host(WantedApp,WantedHost)

%get(WantedApp,WantedNode)
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
call_cast_test()->
    
    ok.



setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=rpc:call(node(),test_nodes,start_nodes,[],5000),
    [N0,N1,N2]=test_nodes:get_nodes(),
    rpc:call(N0,code,add_patha,["test_ebin"],5000),
    rpc:call(N1,code,add_patha,["test_ebin"],5000),
    rpc:call(N2,code,add_patha,["test_ebin"],5000),
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    [N0,N1,N2].
