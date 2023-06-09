%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lib_provider).
 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(ConfigFile_ToBeChanged,"config/sys.config").

-define(DBETCD,dbetcd_appl).


%% External exports
-export([
	 wanted_state_from_file/1,
	 create_deployment_from_file/1,
	 delete_deployment_from_file/1,

	 load/2,
	 start/2,
	 unload/2,
	 stop/2,
	 is_started/2,
	 is_node_started/2,
	 is_loaded/2,
	 is_stopped/2
	]).


%% ====================================================================
%% External functions
%% ====================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

wanted_state_from_file(FullPathFile)->
    Result=case file:consult(FullPathFile) of
	       {error,Reason}->
		   {error,["couldnt read file ",FullPathFile,Reason,?MODULE,?LINE]};
	       {ok,[{deployment_spec,_SpecId,List}]}->
		   {ok,add_state_info(List,[])}
	   end,    
    Result.
add_state_info([],Acc)->
    Acc;
add_state_info([{ProviderSpec,HostSpec}|T],Acc)->
    NewAcc=case sd:call(?DBETCD,db_provider_spec,member,[ProviderSpec],5000) of
	       false->
		   [{error,[eexists,ProviderSpec,?MODULE,?LINE]}|Acc];
	       true ->
		   case sd:call(?DBETCD,db_provider_spec,read,[app,ProviderSpec],5000) of
		       {error,Reason}->
			   [{error,["error reading provider spec",ProviderSpec,Reason,?MODULE,?LINE]}|Acc];
		       {ok,App}->
			   [{ProviderSpec,HostSpec,App}|Acc]
		   end
	   end,
    add_state_info(T,NewAcc).
		   

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_deployment_from_file(FullPathFile)->
 %   io:format("Dbg ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE,FullPathFile}]),
    Result=case file:consult(FullPathFile) of
	       {error,Reason}->
		   {error,["couldnt read file ",FullPathFile,Reason,?MODULE,?LINE]};
	       {ok,[{deployment_spec,_SpecId,List}]}->
		   deploy(List,[])
	   end,    
    Result.

deploy([],Acc)->
    Acc;
deploy([{ProviderSpec,HostSpec}|T],Acc)->
   % io:format("deploy ~p~n",[{ProviderSpec,HostSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
    NewAcc=case is_loaded(ProviderSpec,HostSpec) of
	       false->
		   case load(ProviderSpec,HostSpec) of
		       {ok,ProviderSpec,HostSpec,ProviderNode,App}->
			   case start(ProviderSpec,HostSpec) of
			       ok->
				   [{ok,ProviderSpec,HostSpec,ProviderNode,App,?MODULE,?LINE}|Acc];
			       {error,Reason}->
				   [{error,Reason,ProviderSpec,HostSpec,?MODULE,?LINE}|Acc]
			   end;
		       {error,Reason}->
			   [{error,Reason,ProviderSpec,HostSpec,?MODULE,?LINE}|Acc]
		   end;
	       true->
		   [{error,already_loaded,ProviderSpec,HostSpec,?MODULE,?LINE}|Acc]
	   end,
    deploy(T,NewAcc).
 
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
delete_deployment_from_file(FullPathFile)->
  %  io:format("Dbg ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE,FullPathFile}]),
    Result=case file:consult(FullPathFile) of
	       {error,Reason}->
		   {error,["couldnt read file ",FullPathFile,Reason,?MODULE,?LINE]};
	       {ok,[{deployment_spec,_SpecId,List}]}->
		   delete(List,[])
	   end,    
    Result.
delete([],Acc)->
    Acc;
delete([{ProviderSpec,HostSpec}|T],Acc)->
   % io:format("delete ~p~n",[{ProviderSpec,HostSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
    NewAcc=case is_loaded(ProviderSpec,HostSpec) of
	       true->
		   case stop(ProviderSpec,HostSpec) of
		       ok->
			   case unload(ProviderSpec,HostSpec) of
			       ok->
				   [{ok,ProviderSpec,HostSpec,?MODULE,?LINE}|Acc];
			       {error,Reason}->
				   [{error,Reason,ProviderSpec,HostSpec,?MODULE,?LINE}|Acc]
			   end;
		       {error,Reason}->
			   [{error,Reason,ProviderSpec,HostSpec,?MODULE,?LINE}|Acc]
		   end;
	       false->
		   [{error,not_loaded,ProviderSpec,HostSpec,?MODULE,?LINE}|Acc]
	   end,
    delete(T,NewAcc).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
load(ProviderSpec,HostSpec)->
    ProviderExists=sd:call(?DBETCD,db_provider_spec,member,[ProviderSpec],5000),
    HostSpecExists=sd:call(?DBETCD,db_host_spec,member,[HostSpec],5000),
    IsStarted=lib_host:is_controller_started(HostSpec),
    Result=case {ProviderExists,HostSpecExists,IsStarted} of
	       {false,_,_}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]};
	       {_,false,_}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]};
	       {true,true,false}->
		   {error,["host controller not started ",HostSpec]};
	       {true,true,true}->
		   {ok,HostNode}=sd:call(?DBETCD,db_host_spec,read,[connect_node,HostSpec],5000),
		   %% 
		   {ok,ProviderDir}=sd:call(?DBETCD,db_provider_spec,read,[dir,ProviderSpec],5000),
		   case rpc:call(HostNode,file,del_dir_r,[ProviderDir],5000) of
		       {badrpc,Reason}->
			   {error,[badrpc,Reason,ProviderSpec,HostSpec,?MODULE,?FUNCTION_NAME,?LINE]};
		       _ ->
			   {ok,GitPath}=sd:call(?DBETCD,db_provider_spec,read,[git_path,ProviderSpec],5000),
			   case rpc:call(HostNode,os,cmd,["git clone "++GitPath],2*55*1000) of
			       {badrpc,Reason}->
				   {error,[badrpc,Reason,ProviderSpec,HostSpec,?MODULE,?FUNCTION_NAME,?LINE]};
			       {error,Reason}->
				   {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
			       _->
				   {ok,TarCmd}=sd:call(?DBETCD,db_provider_spec,read,[tar_cmd,ProviderSpec],5000),
				   case rpc:call(HostNode,os,cmd,[TarCmd],20*1000) of
				       {badrpc,Reason}->
					   {error,[badrpc,Reason,ProviderSpec,HostSpec,?MODULE,?FUNCTION_NAME,?LINE]};
				       {error,Reason}->
					   {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
				       _-> 
					   %% start slave
					   {ok,ProviderNodeName}=sd:call(?DBETCD,db_provider_spec,read,[node_name,ProviderSpec],5000),
					   {ok,CookieStr}=sd:call(?DBETCD,db_provider_spec,read,[cookie,ProviderSpec],5000),
					   {ok,PaArgs}=sd:call(?DBETCD,db_provider_spec,read,[pa_args,ProviderSpec],5000),
				%	   {ok,ConfigFile}=sd:call(?DBETCD,db_provider_spec,read,[config_file,ProviderSpec],5000),
					   {ok,HostName}=sd:call(?DBETCD,db_host_spec,read,[hostname,HostSpec],5000),
					   EnvArgs=" ",
				%	   Args=PaArgs++" "++"-setcookie "++CookieStr++" "++EnvArgs++" "++"-config "++?ConfigFile_ToBeChanged,
					   Args=PaArgs++" "++"-setcookie "++CookieStr++" "++EnvArgs,
					   ProviderNode=list_to_atom(ProviderNodeName++"@"++HostName),
					   rpc:call(ProviderNode,init,stop,[],5000),
					  
					   case vm:check_stopped_node(ProviderNode) of
					       false->
						   {error,["Failed to stop host controller node ",ProviderNode,?MODULE,?FUNCTION_NAME,?LINE]};
					       true->
						   io:format("HostNode,HostName,ProviderNodeName,Args ~p~n",[{rpc:call(HostNode,erlang,get_cookie,[],5000),HostNode,HostName,ProviderNodeName,Args,?MODULE,?FUNCTION_NAME,?LINE}]),	   
						   case rpc:call(HostNode,slave,start,[HostName,ProviderNodeName,Args],10*1000) of
						       {badrpc,Reason}->
							   {error,[badrpc,Reason,ProviderSpec,HostSpec,?MODULE,?FUNCTION_NAME,?LINE]};
						       {error,Reason}->
							   {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
						       {ok,ProviderNode}->
							   {ok,App}=sd:call(?DBETCD,db_provider_spec,read,[app,ProviderSpec],5000),
							   case rpc:call(ProviderNode,code,is_loaded,[App],10*1000)of
							       {badrpc,Reason}->
								   {error,[badrpc,Reason,ProviderSpec,HostSpec,?MODULE,?FUNCTION_NAME,?LINE]};
							       {file,_}->
								   {error,["Already loaded",App,?MODULE,?FUNCTION_NAME,?LINE]};
							       false->
								   case rpc:call(ProviderNode,application,load,[App],10*1000)of
								       {badrpc,Reason}->
									   {error,[badrpc,Reason,ProviderSpec,HostSpec,?MODULE,?FUNCTION_NAME,?LINE]};
								       {error,Reason}->
									   {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
								       ok->
									   case is_loaded(ProviderSpec,HostSpec) of
									       false->
										  {error,["Not loaded  ",App,?MODULE,?FUNCTION_NAME,?LINE]};
									       true->
										  {ok,ProviderSpec,HostSpec,ProviderNode,App}
									   end
								   end
							   end
						   end
					   end
				   end
			   end
		   end
	   end,
   % io:format("Load result  ~p~n",[{Result,?MODULE,?FUNCTION_NAME}]),
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
stop(ProviderSpec,HostSpec)->
    ProviderExists=sd:call(?DBETCD,db_provider_spec,member,[ProviderSpec],5000),
    HostSpecExists=sd:call(?DBETCD,db_host_spec,member,[HostSpec],5000),
    IsStarted=lib_host:is_controller_started(HostSpec),
    Result=case {ProviderExists,HostSpecExists,IsStarted} of
	       {false,_,_}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]};
	       {_,false,_}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]};
	       {true,true,false}->
		   {error,["host controller not started ",HostSpec]};
	       {true,true,true}->
		   case is_stopped(ProviderSpec,HostSpec) of
		       true->
			   {error,["Already stopped provider ",ProviderSpec,HostSpec]};
		       false->
			   {ok,ProviderNodeName}=sd:call(?DBETCD,db_provider_spec,read,[node_name,ProviderSpec],5000),
			   {ok,HostName}=sd:call(?DBETCD,db_host_spec,read,[hostname,HostSpec],5000),
			   ProviderNode=list_to_atom(ProviderNodeName++"@"++HostName),
			   {ok,App}=sd:call(?DBETCD,db_provider_spec,read,[app,ProviderSpec],5000),
			   case rpc:call(ProviderNode,application,stop,[App],10*1000)of
			       {badrpc,Reason}->
				   {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
			       {error,Reason}->
				   {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
			       ok->
				   ok
			   end
		   end
	   end,		       
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
unload(ProviderSpec,HostSpec)->
    ProviderExists=sd:call(?DBETCD,db_provider_spec,member,[ProviderSpec],5000),
    HostSpecExists=sd:call(?DBETCD,db_host_spec,member,[HostSpec],5000),
    IsStarted=lib_host:is_controller_started(HostSpec),
    Result=case {ProviderExists,HostSpecExists,IsStarted} of
	       {false,_,_}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]};
	       {_,false,_}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]};
	       {true,true,false}->
		   {error,["host controller not started ",HostSpec]};
	       {true,true,true}->
		   {ok,ProviderNodeName}=sd:call(?DBETCD,db_provider_spec,read,[node_name,ProviderSpec],5000),
		   {ok,HostName}=sd:call(?DBETCD,db_host_spec,read,[hostname,HostSpec],5000),
		   ProviderNode=list_to_atom(ProviderNodeName++"@"++HostName),
		   {ok,App}=sd:call(?DBETCD,db_provider_spec,read,[app,ProviderSpec],5000),
		   case rpc:call(ProviderNode,application,unload,[App],10*1000) of
		       {badrpc,Reason}->
			   {error,["Failed to unload app",badrpc,App,ProviderSpec,HostSpec,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
		       {error,Reason}->
			   {error,["Failed to unload app ",ProviderSpec,HostSpec,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
		       ok->
			   {ok,HostNode}=sd:call(?DBETCD,db_host_spec,read,[connect_node,HostSpec],5000),
			   {ok,ProviderDir}=sd:call(?DBETCD,db_provider_spec,read,[dir,ProviderSpec],5000),
			   case rpc:call(HostNode,file,del_dir_r,[ProviderDir],10*1000)of
			       {badrpc,Reason}->
				   {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
			       {error,Reason}->
				   {error,["Failed to delete ProviderDir",ProviderDir, Reason,?MODULE,?FUNCTION_NAME,?LINE]};
			       ok->
				   case rpc:call(HostNode,slave,stop,[ProviderNode],10*1000) of
				       {badrpc,Reason}->
					   {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
				       {error,Reason}->
					   {error,["Failed to stop ProviderNode",ProviderNode, Reason,?MODULE,?FUNCTION_NAME,?LINE]};
				       ok->
					   case vm:check_stopped_node(ProviderNode) of
					       true->
						   ok;
					       false ->
						   {error,["Failed to stop ProviderNode",ProviderNode,?MODULE,?FUNCTION_NAME,?LINE]}
					   end
				   end
			   end
		   end
	   end,		       
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start(ProviderSpec,HostSpec)->
    ProviderExists=sd:call(?DBETCD,db_provider_spec,member,[ProviderSpec],5000),
    HostSpecExists=sd:call(?DBETCD,db_host_spec,member,[HostSpec],5000),
    IsStarted=lib_host:is_controller_started(HostSpec),
    Result=case {ProviderExists,HostSpecExists,IsStarted} of
	       {false,_,_}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]};
	       {_,false,_}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]};
	       {true,true,false}->
		   {error,["host controller not started ",HostSpec]};
	       {true,true,true}->
		   {ok,App}=sd:call(?DBETCD,db_provider_spec,read,[app,ProviderSpec],5000),
		   {ok,ProviderNodeName}=sd:call(?DBETCD,db_provider_spec,read,[node_name,ProviderSpec],5000),
		   {ok,HostName}=sd:call(?DBETCD,db_host_spec,read,[hostname,HostSpec],5000),
		   ProviderNode=list_to_atom(ProviderNodeName++"@"++HostName),
		   case is_loaded(ProviderSpec,HostSpec) of
		       false->
			   {error,["Not loaded  ",App,ProviderSpec,HostSpec,?MODULE,?FUNCTION_NAME,?LINE]};
		       true->	  
			   case rpc:call(ProviderNode,application,start,[App],30*1000)of
			       {badrpc,Reason}->
				   {error,[badrpc,Reason,ProviderSpec,HostSpec,?MODULE,?FUNCTION_NAME,?LINE]};
			       {error,Reason}->
				   {error,[Reason,ProviderSpec,HostSpec,?MODULE,?FUNCTION_NAME,?LINE]};
			       ok->
				   ok
			   end
		   end
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
is_loaded(ProviderSpec,HostSpec)->
    ProviderExists=sd:call(?DBETCD,db_provider_spec,member,[ProviderSpec],5000),
    HostSpecExists=sd:call(?DBETCD,db_host_spec,member,[HostSpec],5000),
    IsStarted=lib_host:is_controller_started(HostSpec),
    Result=case {ProviderExists,HostSpecExists,IsStarted} of
	       {false,_,_}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]};
	       {_,false,_}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]};
	       {true,true,false}->
		   {error,["host controller not started ",HostSpec]};
	       {true,true,true}->
		   {ok,App}=sd:call(?DBETCD,db_provider_spec,read,[app,ProviderSpec],5000),
		   {ok,ProviderNodeName}=sd:call(?DBETCD,db_provider_spec,read,[node_name,ProviderSpec],5000),
		   {ok,HostName}=sd:call(?DBETCD,db_host_spec,read,[hostname,HostSpec],5000),
		   ProviderNode=list_to_atom(ProviderNodeName++"@"++HostName),
		   case rpc:call(ProviderNode,application,loaded_applications,[],10*1000) of
		       {badrpc,_Reason}->
			   false;
		       LoadedList->
			   lists:keymember(App,1,LoadedList)
		   end
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
is_started(ProviderSpec,HostSpec)->
  %  io:format("ProviderSpec ,ProviderSpec  ~p~n",[{ProviderSpec ,HostSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
    ProviderExists=sd:call(?DBETCD,db_provider_spec,member,[ProviderSpec],5000),
    HostSpecExists=sd:call(?DBETCD,db_host_spec,member,[HostSpec],5000),
    IsStarted=lib_host:is_controller_started(HostSpec),
    Result=case {ProviderExists,HostSpecExists,IsStarted} of
	       {false,_,_}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]},
		   false;
	       {_,false,_}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]},
		   false;
	       {true,true,false}->
		   {error,["host controller not started ",HostSpec]},
		   false;
	       {true,true,true}->
		   {ok,ProviderNodeName}=sd:call(?DBETCD,db_provider_spec,read,[node_name,ProviderSpec],5000),
		   {ok,HostName}=sd:call(?DBETCD,db_host_spec,read,[hostname,HostSpec],5000),
		   {ok,App}=sd:call(?DBETCD,db_provider_spec,read,[app,ProviderSpec],5000),
		   ProviderNode=list_to_atom(ProviderNodeName++"@"++HostName),
		   case is_node_started(ProviderSpec,HostSpec) of
		       false->
			   %{error,["provider node not started",ProviderNode,?MODULE,?FUNCTION_NAME,?LINE]},
			   false;
		       true->
		%	   io:format("  ~p~n",[{ProviderNode,App ,?MODULE,?FUNCTION_NAME,?LINE}]),
		%	   io:format("ProviderNode,App   ~p~n",[{ProviderNode,App ,?MODULE,?FUNCTION_NAME,?LINE}]),
			   case rpc:call(ProviderNode,App,ping,[],3*5000) of
			       {badrpc,_Reason}->
				  % {error,[badrpc,Reason,ProviderNode,App,?MODULE,?FUNCTION_NAME,?LINE]};
				   false;
			       pong->
				   true;
			       pang->
				   false
			   end
		   end
	   end,
 %   io:format("Result, ProviderSpec ,ProviderSpec  ~p~n",[{Result,ProviderSpec ,HostSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
is_node_started(ProviderSpec,HostSpec)->
  %  io:format("ProviderSpec ,ProviderSpec  ~p~n",[{ProviderSpec ,HostSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
    ProviderExists=sd:call(?DBETCD,db_provider_spec,member,[ProviderSpec],5000),
    HostSpecExists=sd:call(?DBETCD,db_host_spec,member,[HostSpec],5000),
    IsStarted=lib_host:is_controller_started(HostSpec),
    Result=case {ProviderExists,HostSpecExists,IsStarted} of
	       {false,_,_}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]},
		   false;
	       {_,false,_}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]},
		   false;
	       {true,true,false}->
		   {error,["host controller not started ",HostSpec]},
		   false;
	       {true,true,true}->
	%	   io:format("ProviderExists,HostSpecExists ~p~n",[{ProviderExists,HostSpecExists,?MODULE,?FUNCTION_NAME,?LINE}]),
		   {ok,ProviderNodeName}=sd:call(?DBETCD,db_provider_spec,read,[node_name,ProviderSpec],5000),
		  
		   {ok,HostName}=sd:call(?DBETCD,db_host_spec,read,[hostname,HostSpec],5000),
		  
		   {ok,HostNode}=sd:call(?DBETCD,db_host_spec,read,[connect_node,HostSpec],5000),  

		%   io:format("HostNode,ProviderNodeName,HostName ~p~n",[{HostNode,ProviderNodeName,HostName,?MODULE,?FUNCTION_NAME,?LINE}]), 
 
		   ProviderNode=list_to_atom(ProviderNodeName++"@"++HostName),
		 %  io:format("ProviderNode ,HostNode  ~p~n",[{ProviderNode ,HostNode,?MODULE,?FUNCTION_NAME,?LINE}]),
		   case rpc:call(HostNode,net_adm,ping,[ProviderNode],5000) of
		       {badrpc,_Reason}->
			   %% log
			   false;
		       pong->
			   true;
		       pang->
			   false
		   end
	   end,
 %   io:format("Result, ProviderSpec ,ProviderSpec  ~p~n",[{Result,ProviderSpec ,HostSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
is_stopped(ProviderSpec,HostSpec)->
    ProviderExists=sd:call(?DBETCD,db_provider_spec,member,[ProviderSpec],5000),
    HostSpecExists=sd:call(?DBETCD,db_host_spec,member,[HostSpec],5000),
    IsStarted=lib_host:is_controller_started(HostSpec),
    Result=case {ProviderExists,HostSpecExists,IsStarted} of
	       {false,_,_}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]};
	       {_,false,_}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]};
	       {true,true,false}->
		   {error,["host controller not started ",HostSpec]};
	       {true,true,true}->
		   {ok,ProviderNodeName}=sd:call(?DBETCD,db_provider_spec,read,[node_name,ProviderSpec],5000),
		   {ok,HostName}=sd:call(?DBETCD,db_host_spec,read,[hostname,HostSpec],5000),
		   {ok,HostNode}=sd:call(?DBETCD,db_host_spec,read,[connect_node,HostSpec],5000),  
		   ProviderNode=list_to_atom(ProviderNodeName++"@"++HostName),
		   case rpc:call(HostNode,net_adm,ping,[ProviderNode],5000) of
		       {badrpc,_Reason}->
			   %% log
			   true;
		       pong->
			   false;
		       pang->
			   true
		   end
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
