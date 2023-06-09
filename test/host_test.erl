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
-module(host_test).      
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(DBETCD,dbetcd_appl).

-define(HostSpecs,["c200","c201"]).
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=setup(),
    ok=start_stop_controllers(),


    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start_stop_controllers()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    [C200,C201]=?HostSpecs,
    [false,false]=[kube:is_controller_started(HostSpec)||HostSpec<-[C200,C201]],

    io:format("Start C200 and C201 ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    %% start C200
    ok=kube:start_controller(C200),
    [true,false]=[kube:is_controller_started(HostSpec)||HostSpec<-[C200,C201]],
     %% start C201
    ok=kube:start_controller(C201),
    [true,true]=[kube:is_controller_started(HostSpec)||HostSpec<-[C200,C201]],

    io:format("Stop C200 and C201 ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    %% stop C200
    ok=kube:stop_controller(C200),
    [false,true]=[kube:is_controller_started(HostSpec)||HostSpec<-[C200,C201]],
    %% stop C201
    ok=kube:stop_controller(C201),
    [false,false]=[kube:is_controller_started(HostSpec)||HostSpec<-[C200,C201]],
    io:format("Restart C200 and C201 ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    %% start C200
    ok=kube:start_controller(C200),
    [true,false]=[kube:is_controller_started(HostSpec)||HostSpec<-[C200,C201]],
     %% start C201
    ok=kube:start_controller(C201),
    [true,true]=[kube:is_controller_started(HostSpec)||HostSpec<-[C200,C201]],
    io:format("Re stop C200 and C201 ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    %% stop C200
    ok=kube:stop_controller(C200),
    [false,true]=[kube:is_controller_started(HostSpec)||HostSpec<-[C200,C201]],
    %% stop C201
    ok=kube:stop_controller(C201),
    [false,false]=[kube:is_controller_started(HostSpec)||HostSpec<-[C200,C201]],
       
    
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    [C200,C201]=?HostSpecs,
    {ok,ControllerNodeC200}=sd:call(?DBETCD,db_host_spec,read,[connect_node,C200],5000),
    rpc:call(ControllerNodeC200,init,stop,[],5000),
    {ok,ControllerNodeC201}=sd:call(?DBETCD,db_host_spec,read,[connect_node,C201],5000),
    rpc:call(ControllerNodeC201,init,stop,[],5000),
    timer:sleep(2000),
    ok.
