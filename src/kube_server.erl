%%% -------------------------------------------------------------------
%%% Author  : joqerlang
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(kube_server).
 
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("log.api").
%% --------------------------------------------------------------------

 
%% gen_server callbacks



-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%-------------------------------------------------------------------
-record(state,{
	       lock_id
	      }).


%% ====================================================================
%% External functions
%% ====================================================================

       

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
%init(ClusterSpec) -> 
init([]) -> 
    io:format("Debug ~p~n",[{?MODULE,?LINE}]),
    LockId=?MODULE,
    {atomic,ok}=sd:call(dbetcd,db_lock,create,[LockId,0],5000),
   
   % sd:cast(log,log,notice,[?MODULE,?FUNCTION_NAME,?LINE,node(),"server start",[]]),
    {ok, #state{lock_id=LockId},0
    }.   
 

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
handle_call({load_provider,ProviderSpec,HostSpec},_From, State) ->
    Reply= rpc:call(node(),lib_provider,load,[ProviderSpec,HostSpec],2*60*1000),
    {reply, Reply, State};

handle_call({start_provider,ProviderSpec,HostSpec},_From, State) ->
    Reply=rpc:call(node(),lib_provider,start,[ProviderSpec,HostSpec],60*1000),
    {reply, Reply, State};

handle_call({stop_provider,ProviderSpec,HostSpec},_From, State) ->
      Reply=rpc:call(node(),lib_provider,stop,[ProviderSpec,HostSpec],60*1000),
    {reply, Reply, State};

handle_call({unload_provider,ProviderSpec,HostSpec},_From, State) ->
    Reply=rpc:call(node(),lib_provider,unload,[ProviderSpec,HostSpec],60*1000),
    {reply, Reply, State};

handle_call({is_loaded_provider,ProviderSpec,HostSpec},_From, State) ->
    Reply=rpc:call(node(),lib_provider,is_loaded,[ProviderSpec,HostSpec],60*1000),
    {reply, Reply, State};

handle_call({is_started_provider,ProviderSpec,HostSpec},_From, State) ->
    Reply=rpc:call(node(),lib_provider,is_started,[ProviderSpec,HostSpec],60*1000),
    {reply, Reply, State};

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

handle_call({start_host_controller,HostSpec},_From, State) ->
    Reply=rpc:call(node(),lib_host,start_host_controller,[HostSpec],30*1000),
    {reply, Reply, State};

handle_call({stop_host_controller,HostSpec},_From, State) ->
    Reply=rpc:call(node(),lib_host,stop_host_controller,[HostSpec],30*1000),
    {reply, Reply, State};

handle_call({is_started_host_controller,HostSpec},_From, State) ->
    Reply=rpc:call(node(),lib_host,is_started_host_controller,[HostSpec],30*1000),
    {reply, Reply, State};

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------



handle_call({ping},_From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call(Request, From, State) ->
    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Unmatched signal",[Request,From]]),
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast({orchestrate_result,Result}, State) ->
    case Result of
	 {error,Reason}->
	    ?LOG_WARNING("Error orchestrate_result ",[Reason]);
	{ok,ResultHostController,ResultProviders}->
	    ?LOG_NOTICE("orchestrate_result ",[ResultHostController,ResultProviders])
    end,
    spawn(fun()->orchestrate:start(State#state.lock_id) end),
    {noreply, State};



handle_cast(Msg, State) ->
    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Unmatched signal",[Msg]]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |

%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(timeout, State) ->
    spawn(fun()->orchestrate:start(State#state.lock_id) end),
    {noreply, State};

handle_info(Info, State) ->
    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Unmatched signal",[Info]]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
