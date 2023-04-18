%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 18 Jan 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(orchestrate).

-define(SleepInterval,60*1000).
%% API
-export([
	 start/1,
	 start/2
	]).

-export([
	 start/1,
	 start/2
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start(LeaderPid)->
    start(LeaderPid,?SleepInterval).

start(LeaderPid,SleepInterval)->
  %  sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"start orchestrate  ",[node()]]),
    timer:sleep(SleepInterval),
    Result=case leader:am_i_leader(LeaderPid,node(),5000) of
	       false->
	%	   sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"am_i_leader",[false,node()]]),
		   [ok,ok,ok,ok];
	       true->
%		   sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"am_i_leader",[true,node()]]),
		   orchistrate()
	   end,
%    sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"end orchestrate  ",[node()]]),
    rpc:cast(node(),kube,orchestrate_result,Result).


orchistrate()->
 %   sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"start orchestrate ",[node()]]),
    ResultStartHostNodes=rpc:call(node(),host_server,start_nodes,[],15*1000),
 %   sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"ResultStartParents ",[ResultStartParents]]),

   
    [ResultStartHostNodes].
    
   

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
