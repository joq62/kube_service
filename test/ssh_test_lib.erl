%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 31 May 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(ssh_test_lib).

%% API
-export([
	 start_node/3,
	 stop_node/2,
	 is_running/2
	 
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_node(NodeName,HostSpec,Args)->
    {ok,HostName}=db_host_spec:read(hostname,HostSpec),
    Cookie=atom_to_list(erlang:get_cookie()),
  
    Node=list_to_atom(NodeName++"@"++HostName),
    rpc:call(Node,init,stop,[],5000),
    true=ops_ssh:check_stopped_node(Node),

    ERlCmd="erl -sname "++NodeName++" "++"-setcookie "++Cookie++" "++Args,
    _ErlR=ssh_server:send_msg(HostSpec,ERlCmd,5*5000),
    true=ops_ssh:check_started_node(Node),
    true=net_kernel:connect_node(Node),
    {ok,Node}.
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
stop_node(NodeName,HostSpec)->
    {ok,HostName}=db_host_spec:read(hostname,HostSpec),
    Node=list_to_atom(NodeName++"@"++HostName),
    rpc:call(Node,init,stop,[],5000),
    ops_ssh:check_stopped_node(Node).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
is_running(NodeName,HostSpec)->
    {ok,HostName}=db_host_spec:read(hostname,HostSpec),
    Node=list_to_atom(NodeName++"@"++HostName),
    net_kernel:connect_node(Node).
	   




%%%===================================================================
%%% Internal functions
%%%===================================================================
