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
-module(dist_test).      
    
 
-export([start/0

	]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    ok=setup(),
    ok=test_1(),
   
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    timer:sleep(2000),
  % init:stop(),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
test_1()->
  io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    AllNodes=test_nodes:get_nodes(),
    [N1,N2,N3]=AllNodes,
    ['n0@c50','n1@c50','n2@c50']=AllNodes,
    [rpc:call(N1,net_adm,ping,[N2],3000)||N1<-AllNodes,
					  N2<-AllNodes,
					  N1/=N2],
  
    

  
  %% N1 ---------------------------------------------------------------------------------
    ok=rpc:call(N1,application,load,[tests],5000),
    ok=rpc:call(N1,application,start,[tests],5000),
    pong=rpc:call(N1,dbetcd,ping,[],5000),
    pong=rpc:call(N1,common,ping,[],5000),
    pong=rpc:call(N1,sd,ping,[],5000),
    pong=rpc:call(N1,log,ping,[],5000),
    ['n0@c50']=lists:sort(rpc:call(N1,mnesia,system_info,[db_nodes],5000)),
    {atomic,ok}=rpc:call(N1,db_lock,create,[{db_lock,schedule}],5000),
   % rpc:call(N1,mnesia,system_info,[],5000),
    [schedule]=rpc:call(N1,db_lock,read_all,[],5000),
    {ok,TransActionsId_1}=rpc:call(N1,db_lock,try_lock,[schedule,1000],5000),
    timer:sleep(500),
    locked=rpc:call(N1,db_lock,try_lock,[schedule,1000],5000),
    timer:sleep(600),
    {ok,TransActionsId_2}=rpc:call(N1,db_lock,try_lock,[schedule,1000],5000),
    locked=rpc:call(N1,db_lock,try_lock,[schedule,1000],5000),
    ok=rpc:call(N1,db_lock,unlock,[schedule,TransActionsId_2],5000),
    io:format("N1 dist OK! ~p~n",[{?MODULE,?LINE}]),
    

    %% N2 -----------------------------------------------------------------------------
    ok=rpc:call(N2,application,load,[tests],5000),
    ok=rpc:call(N2,application,start,[tests],5000),
    pong=rpc:call(N2,dbetcd,ping,[],5000),
    pong=rpc:call(N2,common,ping,[],5000),
    pong=rpc:call(N2,sd,ping,[],5000),
    pong=rpc:call(N2,log,ping,[],5000),
    ['n1@c50','n2@c50']=lists:sort(rpc:call(N2,mnesia,system_info,[db_nodes],5000)),
    
    [schedule]=rpc:call(N2,db_lock,read_all,[],5000),
    {ok,TransActionsId_20}=rpc:call(N2,db_lock,try_lock,[schedule,1000],5000),
    timer:sleep(500),
    locked=rpc:call(N1,db_lock,try_lock,[schedule,1000],5000),
    locked=rpc:call(N2,db_lock,try_lock,[schedule,1000],5000),
    timer:sleep(600),
    {ok,TransActionsId_21}=rpc:call(N2,db_lock,try_lock,[schedule,1000],5000),
    locked=rpc:call(N1,db_lock,try_lock,[schedule,1000],5000),
    locked=rpc:call(N2,db_lock,try_lock,[schedule,1000],5000),
    ok=rpc:call(N2,db_lock,unlock,[schedule,TransActionsId_21],5000),
    io:format("N2 dbetcd OK! ~p~n",[{?MODULE,?LINE}]),


  %% N3 -----------------------------------------------------------------------------------
    ok=rpc:call(N3,application,load,[tests],5000),
    ok=rpc:call(N3,application,start,[tests],5000),
    pong=rpc:call(N3,dbetcd,ping,[],5000),
    pong=rpc:call(N3,common,ping,[],5000),
    pong=rpc:call(N3,sd,ping,[],5000),
    pong=rpc:call(N3,log,ping,[],5000),
    ['c1@c50','c2@c50','c3@c50','do_test@c50']=lists:sort(rpc:call(N3,mnesia,system_info,[db_nodes],5000)),
    
    [schedule]=rpc:call(N3,db_lock,read_all,[],5000),
    {ok,TransActionsId_30}=rpc:call(N3,db_lock,try_lock,[schedule,1000],5000),
    timer:sleep(500),
    locked=rpc:call(N1,db_lock,try_lock,[schedule,1000],5000),
    locked=rpc:call(N2,db_lock,try_lock,[schedule,1000],5000),
    locked=rpc:call(N3,db_lock,try_lock,[schedule,1000],5000),
    timer:sleep(600),
    {ok,TransActionsId_31}=rpc:call(N3,db_lock,try_lock,[schedule,1000],5000),
    locked=rpc:call(N1,db_lock,try_lock,[schedule,1000],5000),
    locked=rpc:call(N2,db_lock,try_lock,[schedule,1000],5000),
    locked=rpc:call(N3,db_lock,try_lock,[schedule,1000],5000),
    ok=rpc:call(N3,db_lock,unlock,[schedule,TransActionsId_31],5000),
    io:format("N3 dbetcd OK! ~p~n",[{?MODULE,?LINE}]),
  
 %% N4 -----------------------------------------------------------------------------------------
   %% kill N3 
    io:format("kill N3  ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    [schedule]=rpc:call(N3,db_lock,read_all,[],5000),
    {ok,TransActionsId_40}=rpc:call(N3,db_lock,try_lock,[schedule,3000],5000),
    rpc:call(N3,init,stop,[],5000),
    true=vm:check_stopped_node(N3),
    locked=rpc:call(N1,db_lock,try_lock,[schedule,3000],5000),
    locked=rpc:call(N2,db_lock,try_lock,[schedule,3000],5000),
    
    %% restart N3 
    {ok,N3}=test_nodes:start_slave("c3"),
    [rpc:call(N3,net_adm,ping,[N],5000)||N<-AllNodes],
    {ok,Cwd}=rpc:call(N3,file,get_cwd,[],5000),
    Ebin=filename:join(Cwd,"test_ebin"),
    true=rpc:call(N3,code,add_patha,[Ebin],5000), 
    ok=rpc:call(N3,application,start,[tests],5000), 
    pong=rpc:call(N3,dbetcd,ping,[],5000),
    io:format("N3 restarted  ~p~n",[{?MODULE,?LINE}]),

    locked=rpc:call(N1,db_lock,try_lock,[schedule,3000],5000),
    locked=rpc:call(N2,db_lock,try_lock,[schedule,3000],5000),
    locked=rpc:call(N3,db_lock,try_lock,[schedule,3000],5000),
    timer:sleep(3000),
    {ok,TransActionsId_41}=rpc:call(N3,db_lock,try_lock,[schedule,1000],5000),
    locked=rpc:call(N1,db_lock,try_lock,[schedule,1000],5000),
    locked=rpc:call(N2,db_lock,try_lock,[schedule,1000],5000),
    locked=rpc:call(N3,db_lock,try_lock,[schedule,1000],5000),
    ok=rpc:call(N3,db_lock,unlock,[schedule,TransActionsId_41],5000),
    
    io:format("restart N3 OK!  ~p~n",[{?MODULE,?LINE}]),

  

  init:stop(),
    timer:sleep(2000),

  ok.


%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok=test_nodes:start_nodes(),
    {ok,Cwd}=file:get_cwd(),
    TestEbin=filename:join(Cwd,"test_ebin"),
    true=filelib:is_dir(TestEbin),
    [true,true,true]=[rpc:call(N,code,add_patha,[TestEbin],5000)||N<-test_nodes:get_nodes()],    

   
    ok.
