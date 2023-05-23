%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(all2).      
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define(DeploymentSpec,"test").
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
   
    ok=setup(),
 %   ok=test1(),
    ok=test2(),
    ok=test3(),

    io:format("Test OK !!! ~p~n",[?MODULE]),
    timer:sleep(2000),
    init:stop(),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
test3()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    kuk=orchestrate2:start_infra_providers(),
    
    ok.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
test2()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    file:del_dir_r("adder_unique_1_provider"),
    file:del_dir_r("adder_unique_2_provider"),

    {"test",ProviderDeployments}=sd:call(dbetcd_appl,db_deployment_spec,read,[?DeploymentSpec],5000),
    [{"adder","c50"},{"adder","c50"},{"adder","c50"},
     {"dbetcd_appl","c50"},{"dbetcd_appl","c50"},
     {"divi","c50"},
     {"test_appl","c50"},{"test_appl","c50"}
    ]=lists:sort(ProviderDeployments),
    
    {ok,DeploymentId_1}=lib_provider2:create_deployment("adder","c50","provider","adder_unique_1_provider"),
    {
     "adder_unique_1_provider",
     "adder",
     "adder_unique_1_provider",
     "adder_unique_1_provider",
     adder_unique_1_provider@c50,
     "c50",
     {_,_}
    }=sd:call(dbetcd_appl,db_deploy,read,[DeploymentId_1],5000),
    {ok,DeploymentId_2}=lib_provider2:create_deployment("adder","c50","provider","adder_unique_2_provider"),
    {
     "adder_unique_2_provider",
     "adder",
     "adder_unique_2_provider",
     "adder_unique_2_provider",
     adder_unique_2_provider@c50,
     "c50",
     {_,_}
    }=sd:call(dbetcd_appl,db_deploy,read,[DeploymentId_2],5000),
    ["adder_unique_1_provider","adder_unique_2_provider"]=lists:sort(sd:call(dbetcd_appl,db_deploy,get_all_id,[],5000)),

    ok=lib_provider2:load_start("adder_unique_1_provider"),
    [adder_unique_1_provider@c50]=sd:get_node(adder),
    42=sd:call(adder,adder,add,[20,22],5000),
    
    ok=lib_provider2:load_start("adder_unique_2_provider"),
    [adder_unique_1_provider@c50,adder_unique_2_provider@c50]=lists:sort(sd:get_node(adder)),
    42=sd:call(adder,adder,add,[20,22],5000),
    
    ok=lib_provider2:stop_unload("adder_unique_1_provider"),
    [adder_unique_2_provider@c50]=sd:get_node(adder),
    42=sd:call(adder,adder,add,[20,22],5000),
    
    ok=lib_provider2:stop_unload("adder_unique_2_provider"),
    []=sd:get_node(adder),
    {error,["No node available for app : ",adder,sd,_]}=sd:call(adder,adder,add,[20,22],5000),

    true=sd:call(dbetcd_appl,db_deploy,member,["adder_unique_1_provider"],5000),
    true=sd:call(dbetcd_appl,db_deploy,member,["adder_unique_2_provider"],5000),

    ok=lib_provider2:delete_deployment("adder_unique_1_provider"),
    false=sd:call(dbetcd_appl,db_deploy,member,["adder_unique_1_provider"],5000),
    true=sd:call(dbetcd_appl,db_deploy,member,["adder_unique_2_provider"],5000),

    ok=lib_provider2:delete_deployment("adder_unique_2_provider"),
    false=sd:call(dbetcd_appl,db_deploy,member,["adder_unique_1_provider"],5000),
    false=sd:call(dbetcd_appl,db_deploy,member,["adder_unique_2_provider"],5000),
    ok.

    

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
test1()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    %% create
    file:del_dir_r("adder_1_provider"),
    file:del_dir_r("adder_2_provider"),
    {ok,"adder_1_provider","adder",adder_1_provider@c50,
     "adder_1_provider",
     "c50",{_,_},_
    }=lib_provider2:new("adder","adder_1_provider","c50"),

    42=sd:call(adder,adder,add,[20,22],6000),

    lib_provider2:new("adder","adder_2_provider","c50"),
    [
     'adder_1_provider@c50',
     'adder_2_provider@c50'
    ]=lists:sort(sd:get_node(adder)),
    
    {badrpc,_}=lib_provider2:delete("glurk"),
    ok=lib_provider2:delete("adder_1_provider"),
     [
      'adder_2_provider@c50'
     ]=lists:sort(sd:get_node(adder)),
    
    

    ok.
    

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
   
    a_cookie=erlang:get_cookie(),
    ok=application:start(dbetcd_appl),
    pong=dbetcd:ping(),

    pong=common:ping(),
    pong=sd:ping(),
    pong=log:ping(),
 %   {ok,_}=kube:start_link(),
 %   pong=kube:ping(),
    []=sd:call(dbetcd_appl,db_deploy,read_all,[],6000),
    ok.
