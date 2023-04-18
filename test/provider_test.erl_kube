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
-module(provider_test).      
  
-export([start/1]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start(Node)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=setup(Node),
    ok=read_specs_test(Node),
  
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
read_specs_test(Node)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    AllProviders=lists:sort(rpc:call(Node,db_provider_spec,get_all_id,[],5000)),
%    io:format("AllProviders ~p~n",[{AllProviders,?MODULE,?FUNCTION_NAME}]),
    true=lists:member("kube",AllProviders),

   {"kube","kube","0.1.0","kube",kube,"kube","kube","a_cookie",
    "https://github.com/joq62/kube.git"," -pa kube/lib/*/ebin ",
    "tar -xvf kube/kube-0.1.0.tar.gz -C kube ",
    {application,start,[kube],20000},
    1,
    [all_hosts]}=rpc:call(Node,db_provider_spec,read,["kube"],5000),
    
    {ok,"kube"}=rpc:call(Node,db_provider_spec,read,[spec,"kube"],5000),
    {ok,"kube"}=rpc:call(Node,db_provider_spec,read,[appl_name,"kube"],5000),
    {ok,"0.1.0"}=rpc:call(Node,db_provider_spec,read,[vsn,"kube"],5000),
    {ok,"kube"}=rpc:call(Node,db_provider_spec,read,[app_name,"kube"],5000),
    {ok,kube}=rpc:call(Node,db_provider_spec,read,[app,"kube"],5000),
    {ok,"kube"}=rpc:call(Node,db_provider_spec,read,[dir,"kube"],5000),
    {ok,"kube"}=rpc:call(Node,db_provider_spec,read,[node_name,"kube"],5000),
    {ok,"a_cookie"}=rpc:call(Node,db_provider_spec,read,[cookie,"kube"],5000),
   {ok," -pa kube/lib/*/ebin "}=rpc:call(Node,db_provider_spec,read,[pa_args,"kube"],5000),
    {ok,"https://github.com/joq62/kube.git"}=rpc:call(Node,db_provider_spec,read,[git_path,"kube"],5000),
    {ok,"tar -xvf kube/kube-0.1.0.tar.gz -C kube "}=rpc:call(Node,db_provider_spec,read,[tar_cmd,"kube"],5000),
    {ok,{application,start,[kube],20000}}=rpc:call(Node,db_provider_spec,read,[start_cmd,"kube"],5000),
    {ok,1}=rpc:call(Node,db_provider_spec,read,[num,"kube"],5000),
    {ok, [all_hosts]}=rpc:call(Node,db_provider_spec,read,[affinity,"kube"],5000),
    
 
    {error,[eexist,"glurk",db_provider_spec,_]}=rpc:call(Node,db_provider_spec,read,[dir,"glurk"],5000),
    {error,['Key eexists',glurk,"kube",db_provider_spec,_]}=rpc:call(Node,db_provider_spec,read,[glurk,"kube"],5000),
 
       ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


setup(Node)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
       
    pong=rpc:call(Node,dbetcd,ping,[],5000),
   
    ok.
