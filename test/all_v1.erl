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
-module(all_v1).   
 
-export([start/1]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start(_Args)->
    ok=setup(),
    ok=application:start(common),
    pong=common:ping(),
    ok=map_test(),
    ok=list_len(),
    ok=list_duplicates(),
 %   ok=local_vm_test:start(),
    ok=ssh_vm_test:start(),
    io:format("TEST OK there you go!! ~p~n",[?MODULE]),
    timer:sleep(1000),
    init:stop(),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
list_duplicates()->
    A=[a,b,2,b,"b",3,4,2,a,a,"c",5,"b",{1,2,3},{1,2,3}],
    [a,b,2,"b",3,4,"c",5,{1,2,3}]=list_duplicates:remove(A),
    io:format("TEST OK! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
list_len()->
    L=[1,a,{34,z},"b",'kalle',<<"a">>],
    6=list_length:start(L),
    io:format("TEST OK! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
map_test()->
    F1 = fun square/2,
    F2 = fun sum/3,
    L=[1,2,3,4,5,6,7,8,9],
    [{[1,4,9,16,25,36,49,64,81],sqr}]=mapreduce:start(F1,F2,[],L),
    io:format("TEST OK! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok.

square(Pid,Tal)->
    Pid!{sqr,Tal*Tal}.
sum(Key,Vals,Acc)->
    [{Vals,Key}|Acc].
    
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

setup()->
  
    % Simulate host
  %  R=rpc:call(node(),test_nodes,start_nodes,[],2000),
%    [Vm1|_]=test_nodes:get_nodes(),

%    Ebin="ebin",
 %   true=rpc:call(Vm1,code,add_path,[Ebin],5000),
 
   % R.
    ok.
