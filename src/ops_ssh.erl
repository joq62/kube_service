%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(ops_ssh).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%%---------------------------------------------------------------------
%% Records for test
%%
-define(DBETCD,dbetcd_appl).

-define(IsDir(Dir),"test -d "++Dir++" && echo true || echo false").
%% --------------------------------------------------------------------
%-compile(export_all).
-export([
	 check_stopped_node/1,
	 check_started_node/1,
	 call/3,
	 is_dir/2	 
	 
	]).
	 	 
-export([
	 delete/1,
	 delete/2,
	 create/5
	]).
	 
%  io:format("SshResult ~p~n",[{SshResult,?MODULE,?FUNCTION_NAME,?LINE}]),
%% ====================================================================
%% External functions
%% ====================================================================


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------	 
call(HostSpec,LinuxCmd,TimeOut)->
    {ok,Ip}=sd:call(?DBETCD,db_host_spec,read,[local_ip,HostSpec],5000),
    {ok,SshPort}=sd:call(?DBETCD,db_host_spec,read,[ssh_port,HostSpec],5000),
    {ok,Uid}=sd:call(?DBETCD,db_host_spec,read,[uid,HostSpec],5000),
    {ok,Pwd}=sd:call(?DBETCD,db_host_spec,read,[passwd,HostSpec],5000),
    %% Need to do a process otherwise message are left for next call
    rpc:call(node(),my_ssh,ssh_send,[Ip,SshPort,Uid,Pwd,LinuxCmd,TimeOut],TimeOut+1000).
    
    
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------	
create(HostSpec,NodeName,Cookie,PaArgs,EnvArgs)->
    TimeOut=7000,    
    {ok,HostName}=sd:call(?DBETCD,db_host_spec,read,[hostname,HostSpec],5000),
    Node=list_to_atom(NodeName++"@"++HostName),
    rpc:call(Node,init,stop,[],5000),
    true=check_stopped_node(100,Node,false),
    LinuxCmd="erl -sname "++NodeName++" -setcookie a_cookie "++" -detached ",
    Result=case call(HostSpec,LinuxCmd,TimeOut) of
	       {badrpc,Reason}->
		   {error,[{?MODULE,?LINE," ",badrpc,Reason}]};
	       SshResult->
		   case check_started_node(50,Node,false) of
		       false->
			   rpc:call(Node,init,stop,[],5000),
			   {error,[{?MODULE,?LINE," ",couldnt_connect,node(),erlang:get_cookie(),Node,
				    HostName,NodeName,Cookie,PaArgs,EnvArgs}]};
		       true->
			   {ok,Node}
		   end
	   end,
    Result.


 
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
is_dir(HostSpec,Dir)->
    TimeOut=5000,
    case call(HostSpec,?IsDir(Dir),TimeOut) of
	["false"]->
	    false;
	["true"] ->
	    true;
	ok ->
	    true;
	Reason ->
	    io:format(" Reason ~p~n",[Reason]),
	    {error,[Reason]}
    end.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------	 
check_stopped_node(Node)->
    check_stopped_node(100,Node,false).

check_stopped_node(_N,_Node,true)->
    true;
check_stopped_node(0,_Node,Boolean) ->
    Boolean;
check_stopped_node(N,Node,_) ->
 
    Boolean=case net_adm:ping(Node) of
		pong->
		    timer:sleep(500),
		    false;
		pang->
		    true
	    end,
    check_stopped_node(N-1,Node,Boolean).

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------	 
check_started_node(Node)->
    check_started_node(100,Node,false).

check_started_node(_N,_Node,true)->
  %  io:format("Dbg calling node,Node ~p~n",[{node(),Node,?MODULE,?FUNCTION_NAME,?LINE}]),
    true;
check_started_node(0,_Node,Boolean) ->
  %  io:format("Dbg calling node,Node ~p~n",[{node(),Node,?MODULE,?FUNCTION_NAME,?LINE}]),
    Boolean;
check_started_node(N,Node,_) ->
    io:format("Dbg calling node,Node ~p~n",[{node(),Node,erlang:get_cookie(),?MODULE,?FUNCTION_NAME,?LINE}]),
    Boolean=case net_adm:ping(Node) of
		pang->
		    timer:sleep(100),
		    false;
		pong->
		    true
	    end,
    check_started_node(N-1,Node,Boolean).

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------	       
delete(Node)->
    slave:stop(Node).

delete(Node,Dir)->
    rpc:call(Node,os,cmd,["rm -rf "++Dir]),
    slave:stop(Node).




