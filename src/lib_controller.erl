%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lib_controller).
 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(ConfigFile_ToBeChanged,"config/sys.config").

-define(DBETCD,dbetcd_appl).


%% External exports
-export([
	 is_deployed/1,
	 create_deployment/3,
	 create_deployment/4,
	 load_start/1,
	 stop_unload/1,
	 delete_deployment/1,

	 new/2,
	 new/3,
	 delete/1,
	 local_delete/1,
	 unique_node_name/2,
	 deployment_id/1,
	 dir/1
	]).


%io:format("DeploymentId ~p~n",[{DeploymentId,?MODULE,?FUNCTION_NAME,?LINE}]),
%% ====================================================================
%% External functions
%% ====================================================================
is_deployed(DeploymentId)->
    {ok,Node}=sd:call(dbetcd_appl,db_deploy,read,[node,DeploymentId],5000),
    {ok,ProviderSpec}=sd:call(dbetcd_appl,db_deploy,read,[provider_spec,DeploymentId],5000),
    {ok,App}=sd:call(dbetcd_appl,db_provider_spec,read,[app,ProviderSpec],5000),
    Result=case net_adm:ping(Node) of
	       pang->
		   false;
	       pong->
		   case rpc:call(Node,App,ping,[],5000) of
		       pong->
			   true;
		       _ ->
			   false
		   end
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_deployment(ProviderSpec,HostSpec,Type)->
    {ok,AppName}=sd:call(?DBETCD,db_provider_spec,read,[app_name,ProviderSpec],5000),
    {ok,Unique}=unique_node_name(AppName,Type),
    create_deployment(ProviderSpec,HostSpec,Type,Unique).

create_deployment(ProviderSpec,HostSpec,_Type,Unique)->
    DeploymentId=Unique,
    NodeName=Unique,
    Dir=Unique,
    {ok,HostName}=sd:call(?DBETCD,db_host_spec,read,[hostname,HostSpec],5000),
    Node=list_to_atom(Unique++"@"++HostName),
    CreationTime={date(),time()},
    {atomic,ok}=sd:call(?DBETCD,db_deploy,create,[DeploymentId,ProviderSpec,NodeName,Dir,Node,HostSpec,CreationTime],2*5000),
    {ok,DeploymentId}.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
delete_deployment(DeploymentId)->
    {atomic,ok}=sd:call(?DBETCD,db_deploy,delete,[DeploymentId],2*5000),
    ok.

    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
load_start(DeploymentId)->
    {ok,MyHostName}=inet:gethostname(),
    {ok,HostSpec}=sd:call(dbetcd_appl,db_deploy,read,[host_spec,DeploymentId],5000),
    {ok,HostName}=sd:call(dbetcd_appl,db_host_spec,read,[hostname,HostSpec],5000),
    case MyHostName==HostName of
	true->
	    local_load_start(DeploymentId);
	false ->
	    ssh_load_start(DeploymentId)
    end.

local_load_start(DeploymentId)->
    CookieStr=atom_to_list(erlang:get_cookie()),
    {ok,Dir}=sd:call(dbetcd_appl,db_deploy,read,[dir,DeploymentId],5000),
    %% TBD PaArgs needs to adjusted in db_provider_Spec 
    PaArgs="-pa "++Dir++"/ebin"++" "++" -config "++Dir++"/config/sys.config",
    EnvArgs=" ",
    ErlArgs=PaArgs++" "++"-setcookie "++CookieStr++" "++EnvArgs,
   
    % Need to create everything befor start the vm becaudse of config
    %% Create Dir
    ok=rpc:call(node(),file,make_dir,[Dir],5000),
    %% Clone
    {ok,ProviderSpec}=sd:call(dbetcd_appl,db_deploy,read,[provider_spec,DeploymentId],5000),
    {ok,GitPath}=sd:call(?DBETCD,db_provider_spec,read,[git_path,ProviderSpec],5000),
    CloneInfo=rpc:call(node(),os,cmd,["git clone "++GitPath++" "++Dir],5000),
    %% Create ErlangVm
    {ok,HostSpec}=sd:call(dbetcd_appl,db_deploy,read,[host_spec,DeploymentId],5000),
    {ok,HostName}=sd:call(dbetcd_appl,db_host_spec,read,[hostname,HostSpec],5000),
    {ok,NodeName}=sd:call(dbetcd_appl,db_deploy,read,[node_name,DeploymentId],5000),
 %   io:format("HostName,NodeName,ErlArgs ~p~n",[{HostName,NodeName,ErlArgs,?MODULE,?FUNCTION_NAME,?LINE}]),
    {ok,ProviderNode}=slave:start(HostName,NodeName,ErlArgs),
 
   %% Create Dir
 %   ok=rpc:call(ProviderNode,file,make_dir,[Dir],5000),
    %% Clone
%    CloneInfo=rpc:call(ProviderNode,os,cmd,["git clone "++GitPath++" "++Dir],5000),
    %% Load App
    {ok,App}=sd:call(?DBETCD,db_provider_spec,read,[app,ProviderSpec],5000),
    ok=rpc:call(ProviderNode,application,load,[App],5000),
    %% start App
    ok=rpc:call(ProviderNode,application,start,[App],5000),
    pong=net_adm:ping(ProviderNode),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
ssh_load_start(DeploymentId)->
    
    CookieStr=atom_to_list(erlang:get_cookie()),
    {ok,Dir}=sd:call(dbetcd_appl,db_deploy,read,[dir,DeploymentId],5000),
    %% TBD PaArgs needs to adjusted in db_provider_Spec 
    PaArgs="-pa "++Dir++"/ebin"++" "++" -config "++Dir++"/config/sys.config",
    EnvArgs=" ",
    ErlArgs=PaArgs++" "++"-setcookie "++CookieStr++" "++EnvArgs,
   


    % Need to create everything befor start the vm becaudse of config
    %% Create Dir
    ok=rpc:call(node(),file,make_dir,[Dir],5000),
    %% Clone
    {ok,ProviderSpec}=sd:call(dbetcd_appl,db_deploy,read,[provider_spec,DeploymentId],5000),
    {ok,GitPath}=sd:call(?DBETCD,db_provider_spec,read,[git_path,ProviderSpec],5000),
    CloneInfo=rpc:call(node(),os,cmd,["git clone "++GitPath++" "++Dir],5000),
    %% Create ErlangVm
    {ok,HostSpec}=sd:call(dbetcd_appl,db_deploy,read,[host_spec,DeploymentId],5000),
    {ok,HostName}=sd:call(dbetcd_appl,db_host_spec,read,[hostname,HostSpec],5000),
    {ok,NodeName}=sd:call(dbetcd_appl,db_deploy,read,[node_name,DeploymentId],5000),
 %   io:format("HostName,NodeName,ErlArgs ~p~n",[{HostName,NodeName,ErlArgs,?MODULE,?FUNCTION_NAME,?LINE}]),
    {ok,ProviderNode}=slave:start(HostName,NodeName,ErlArgs),
 
   %% Create Dir
 %   ok=rpc:call(ProviderNode,file,make_dir,[Dir],5000),
    %% Clone
%    CloneInfo=rpc:call(ProviderNode,os,cmd,["git clone "++GitPath++" "++Dir],5000),
    %% Load App
    {ok,App}=sd:call(?DBETCD,db_provider_spec,read,[app,ProviderSpec],5000),
    ok=rpc:call(ProviderNode,application,load,[App],5000),
    %% start App
    ok=rpc:call(ProviderNode,application,start,[App],5000),
    pong=net_adm:ping(ProviderNode),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
stop_unload(DeploymentId)->
    {ok,ProviderNode}=sd:call(?DBETCD,db_deploy,read,[node,DeploymentId],2*5000),
    {ok,ProviderDir}=sd:call(?DBETCD,db_deploy,read,[dir,DeploymentId],2*5000),
    ok=slave:stop(ProviderNode),
    ok=file:del_dir_r(ProviderDir),
    pang=net_adm:ping(ProviderNode),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
new(ProviderSpec,HostSpec)->
    %% Create an unique node name
    %% AppId==ProvideId, Type==provider, used when deleting 
    {ok,AppName}=sd:call(?DBETCD,db_provider_spec,read,[app_name,ProviderSpec],5000),
    Type="provider",
    NodeName=unique_node_name(AppName,Type),
    rpc:call(node(),?MODULE,new,[ProviderSpec,HostSpec,NodeName],5*5000).

new(ProviderSpec,HostSpec,NodeName)->
    {ok,HostName}=inet:gethostname(),
    {ok,App}=sd:call(?DBETCD,db_provider_spec,read,[app,ProviderSpec],5000),
    {ok,GitPath}=sd:call(?DBETCD,db_provider_spec,read,[git_path,ProviderSpec],5000),
    
    Dir=NodeName,
    DeploymentId=NodeName,
    CreationTime={date(),time()},
    CookieStr=atom_to_list(erlang:get_cookie()),
  
   %% TBD PaArgs needs to adjusted in db_provider_Spec 
    PaArgs="-pa "++Dir++"/ebin"++" "++" -config "++Dir++"/config/sys.config",
    EnvArgs=" ",
    ErlArgs=PaArgs++" "++"-setcookie "++CookieStr++" "++EnvArgs,
   
    % Need to create everything befor start the vm becaudse of config
    %% Create Dir
    ok=rpc:call(node(),file,make_dir,[Dir],5000),
    %% Clone
    CloneInfo=rpc:call(node(),os,cmd,["git clone "++GitPath++" "++Dir],5000),
    %% Create ErlangVm
    io:format("HostName,NodeName,ErlArgs ~p~n",[{HostName,NodeName,ErlArgs,?MODULE,?FUNCTION_NAME,?LINE}]),
    {ok,ProviderNode}=slave:start(HostName,NodeName,ErlArgs),
 
   %% Create Dir
 %   ok=rpc:call(ProviderNode,file,make_dir,[Dir],5000),
    %% Clone
%    CloneInfo=rpc:call(ProviderNode,os,cmd,["git clone "++GitPath++" "++Dir],5000),
    %% Load App
    ok=rpc:call(ProviderNode,application,load,[App],5000),
    %% start App
    ok=rpc:call(ProviderNode,application,start,[App],5000),
    %% update dbase
    {atomic,ok}=sd:call(?DBETCD,db_deploy,create,[DeploymentId,ProviderSpec,NodeName,Dir,ProviderNode,HostSpec,CreationTime],2*5000),

    {ok,DeploymentId,ProviderSpec,NodeName,Dir,ProviderNode,HostSpec,CreationTime,CloneInfo}.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
delete(DeploymentId)->
    rpc:call(node(),?MODULE,local_delete,[DeploymentId],5*5000).
local_delete(DeploymentId)->
    true=sd:call(?DBETCD,db_deploy,member,[DeploymentId],2*5000),
    {ok,ProviderNode}=sd:call(?DBETCD,db_deploy,read,[node,DeploymentId],2*5000),
    {ok,ProviderDir}=sd:call(?DBETCD,db_deploy,read,[dir,DeploymentId],2*5000),
    ok=slave:stop(ProviderNode),
    ok=file:del_dir_r(ProviderDir),
    pang=net_adm:ping(ProviderNode),
    {atomic,ok}=sd:call(?DBETCD,db_deploy,delete,[DeploymentId],2*5000),
    ok.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
unique_node_name(AppId,Type)->
    Unique=integer_to_list(os:system_time(),36),
    {ok,AppId++"_"++Unique++"_"++Type}.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
deployment_id(Node)->
    NodeStr=atom_to_list(Node),
    [DeploymentId,_HostName]=string:tokens(NodeStr,"@"),
    {ok,DeploymentId}.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
dir(Node)->
    NodeStr=atom_to_list(Node),
    [Dir,_HostName]=string:tokens(NodeStr,"@"),
    {ok,Dir}.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
