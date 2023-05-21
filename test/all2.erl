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


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
   
    ok=setup(),
    ok=test1(),

    io:format("Test OK !!! ~p~n",[?MODULE]),
    timer:sleep(2000),
    init:stop(),
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
     {_,_},_
    }=lib_provider2:new("adder","adder_1_provider"),
    42=sd:call(adder,adder,add,[20,22],6000),
    lib_provider2:new("adder","adder_2_provider"),
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
