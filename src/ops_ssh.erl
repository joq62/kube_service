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

-define(IsDir(Dir),"test -d "++Dir++" && echo true || echo false").
%% --------------------------------------------------------------------
%-compile(export_all).
-export([
	 create_dir/2,
	 delete_dir/2,
	 call/3,
	 is_dir/2	 
	 
	]).
	 	 
-export([
	 delete/1,
	 delete/2,
	 create/5,
	 create/6,
	 create/7
	]).
	 

%% ====================================================================
%% External functions
%% ====================================================================


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------	 
call(HostSpec,LinuxCmd,TimeOut)->
    {ok,Ip}=sd:call(dbetcd,db_host_spec,read,[local_ip,HostSpec],5000),
    {ok,SshPort}=sd:call(dbetcd,db_host_spec,read,[ssh_port,HostSpec],5000),
    {ok,Uid}=sd:call(dbetcd,db_host_spec,read,[uid,HostSpec],5000),
    {ok,Pwd}=sd:call(dbetcd,db_host_spec,read,[passwd,HostSpec],5000),
    Result=my_ssh:ssh_send(Ip,SshPort,Uid,Pwd,LinuxCmd,TimeOut),
        
    Result.
    
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------	


create(HostSpec,NodeName,Cookie,PaArgs,EnvArgs)->
    create(HostSpec,NodeName,Cookie,PaArgs,EnvArgs,7000).    

create(HostSpec,NodeName,Cookie,PaArgs,EnvArgs,TimeOut)->    
  %  io:format(" ~p~n",[{?MODULE,?LINE,?FUNCTION_NAME,HostName,Cookie,NodeName,PaArgs,EnvArgs,TimeOut}]),
    {ok,HostName}=sd:call(dbetcd,db_host_spec,read,[hostname,HostSpec],5000),
    {ok,Ip}=sd:call(dbetcd,db_host_spec,read,[local_ip,HostSpec],5000),
    {ok,SshPort}=sd:call(dbetcd,db_host_spec,read,[ssh_port,HostSpec],5000),
    {ok,Uid}=sd:call(dbetcd,db_host_spec,read,[uid,HostSpec],5000),
    {ok,Pwd}=sd:call(dbetcd,db_host_spec,read,[passwd,HostSpec],5000),
    create(HostName,NodeName,Cookie,PaArgs,EnvArgs,
	   {Ip,SshPort,Uid,Pwd},TimeOut).
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------	
create(HostName,NodeName,Cookie,PaArgs,EnvArgs,
       {Ip,SshPort,Uid,Pwd},TimeOut)->
   
 %   io:format("HostName,NodeName,Cookie,PaArgs,EnvArgs,
%Ip,SshPort,Uid,Pwd,TimeOut ~p~n",[{HostName,NodeName,Cookie,PaArgs,EnvArgs,
%Ip,SshPort,Uid,Pwd,TimeOut,erlang:get_cookie(),?MODULE,?LINE}]),
    
    Node=list_to_atom(NodeName++"@"++HostName),
    rpc:call(Node,init,stop,[],5000),
    true=check_stopped_node(100,Node,false),
    Args=PaArgs++" "++"-setcookie "++Cookie++" "++EnvArgs,
    Msg="erl -sname "++NodeName++" "++Args++" ",
    Result=case rpc:call(node(),my_ssh,ssh_send,[Ip,SshPort,Uid,Pwd,Msg,TimeOut],TimeOut+1000) of
	       % {badrpc,timeout}-> retry X times       
	       {badrpc,Reason}->
		   {error,[{?MODULE,?LINE," ",badrpc,Reason}]};
	       ok->
		   {Node,rpc:call(Node,erlang,get_cookie,[],5000)};
		  % case check_started_node(100,Node,false) of
		   %    false->
		%	   rpc:call(Node,init,stop,[],5000),
		%	   {error,[{?MODULE,?LINE," ",couldnt_connect,node(),erlang:get_cookie(),Node,
		%		    HostName,NodeName,Cookie,PaArgs,EnvArgs,
		%		    Ip,SshPort,Uid,Pwd}]};
		 %      true->
		%	   {ok,Node}
		 %  end;
	       Error ->
		     {error,[unmatched_signal,Error,?MODULE,?LINE]}
	   end,
    Result.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
delete_dir(HostSpec,Dir)->
 %   {ok,HostName}=sd:call(dbetcd,db_host_spec,read,[hostname,HostSpec],5000),
    {ok,Ip}=sd:call(dbetcd,db_host_spec,read,[local_ip,HostSpec],5000),
    {ok,SshPort}=sd:call(dbetcd,db_host_spec,read,[ssh_port,HostSpec],5000),
    {ok,Uid}=sd:call(dbetcd,db_host_spec,read,[uid,HostSpec],5000),
    {ok,Pwd}=sd:call(dbetcd,db_host_spec,read,[passwd,HostSpec],5000),
    TimeOut=5000,
    SshDeleteDir=my_ssh:ssh_send(Ip,SshPort,Uid,Pwd,"rm -rf "++Dir,TimeOut),
    io:format("SshDeleteDir,HostSpec,Dir ~p~n",[{SshDeleteDir,HostSpec,Dir,?MODULE,?LINE}]),
    
%    case ops_ssh:is_dir(HostName,Dir) of
%	false->
%	    {ok,Dir};
%	true ->
%	    {error,["failed to delete ",HostName,Dir,?MODULE,?FUNCTION_NAME,?LINE]}
 %   end.
    {ok,Dir}.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
create_dir(HostSpec,Dir)->
    {ok,Ip}=sd:call(dbetcd,db_host_spec,read,[local_ip,HostSpec],5000),
    {ok,SshPort}=sd:call(dbetcd,db_host_spec,read,[ssh_port,HostSpec],5000),
    {ok,Uid}=sd:call(dbetcd,db_host_spec,read,[uid,HostSpec],5000),
    {ok,Pwd}=sd:call(dbetcd,db_host_spec,read,[passwd,HostSpec],5000),
    TimeOut=5000,
    my_ssh:ssh_send(Ip,SshPort,Uid,Pwd,"rm -rf "++Dir,TimeOut),
    timer:sleep(2000),
    my_ssh:ssh_send(Ip,SshPort,Uid,Pwd,"mkdir "++Dir,TimeOut),
    timer:sleep(5000),
    case ops_ssh:is_dir(Dir,{Ip,SshPort,Uid,Pwd,TimeOut}) of
	true->
	    {ok,Dir};
	false ->
	    {error,["failed to create ",Dir,?MODULE,?FUNCTION_NAME,?LINE]}
    end.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
is_dir(HostSpec,Dir)->
    {ok,Ip}=sd:call(dbetcd,db_host_spec,read,[local_ip,HostSpec],5000),
    {ok,SshPort}=sd:call(dbetcd,db_host_spec,read,[ssh_port,HostSpec],5000),
    {ok,Uid}=sd:call(dbetcd,db_host_spec,read,[uid,HostSpec],5000),
    {ok,Pwd}=sd:call(dbetcd,db_host_spec,read,[passwd,HostSpec],5000),
    TimeOut=5000,
    case my_ssh:ssh_send(Ip,SshPort,Uid,Pwd,?IsDir(Dir),TimeOut) of
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
check_started_node(_N,Node,true)->
    io:format("Dbg calling node,Node ~p~n",[{node(),Node,?MODULE,?FUNCTION_NAME,?LINE}]),
    true;
check_started_node(0,Node,Boolean) ->
    io:format("Dbg calling node,Node ~p~n",[{node(),Node,?MODULE,?FUNCTION_NAME,?LINE}]),
    Boolean;
check_started_node(N,Node,_) ->
    io:format("Dbg calling node,Node ~p~n",[{node(),Node,?MODULE,?FUNCTION_NAME,?LINE}]),
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




