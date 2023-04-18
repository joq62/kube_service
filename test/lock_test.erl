%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lock_test).   
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% External exports
-export([start/0]).



%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:tes cases
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),

    ok=setup(),
    ok=lock_test_1(),
 
    io:format("End testing  SUCCESS!! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
%    init:stop(),
%    timer:sleep(3000),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
lock_test_1()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),

    %% db_lock timeout set to
    LockTimeOut=3000,   %% 3 Seconds
    [{error,[eexists,glurk]}]=db_lock:try_lock(glurk,LockTimeOut),

    {ok,TransActionsId_1}=db_lock:try_lock(schedule,LockTimeOut),
    timer:sleep(1*1000),
    locked=db_lock:try_lock(schedule,LockTimeOut),

    {error,[eexists,glurk]}=db_lock:unlock(glurk,TransActionsId_1),
    {error,["eexists Transactions id",glurk,_]}=db_lock:unlock(schedule,glurk),

    ok=db_lock:unlock(schedule,TransActionsId_1),
    {ok,TransActionsId_2}=db_lock:try_lock(schedule,LockTimeOut),
    locked=db_lock:try_lock(schedule,LockTimeOut),

    timer:sleep(3*1000),
    {ok,TransActionsId_3}=db_lock:try_lock(schedule,LockTimeOut),
    locked=db_lock:try_lock(schedule,LockTimeOut),
  
%    io:format("TransActionsId_1 ~p~n",[{TransActionsId_1,?MODULE,?FUNCTION_NAME,?LINE}]),
%    io:format("TransActionsId_2 ~p~n",[{TransActionsId_2,?MODULE,?FUNCTION_NAME,?LINE}]),
%    io:format("TransActionsId_3 ~p~n",[{TransActionsId_3,?MODULE,?FUNCTION_NAME,?LINE}]),
    ok. 
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),

    {atomic,ok}=db_lock:create({db_lock,schedule}),
    [{schedule,na,0,na}]=db_lock:read_all_info(),
    [schedule] =db_lock:read_all(),
    ok.
