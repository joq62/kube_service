%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(kube).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("log.api").


%% API
-export([
	 start_orchestrate/1,
	 orchestrate_result/1,
	 is_wanted_state/0
	 ]).

-export([
%% Host controller
	 start_controller/1,
	 stop_controller/1,
	 is_controller_started/1,
%% provider
	 create_provider/2,
	 delete_provider/2,
	 wanted_state_from_file/1,

	 create_deployment_from_file/1,
	 delete_deployment_from_file/1,
	 

	 is_provider_loaded/2,
	 load_provider/2,
	 unload_provider/2,
	 is_provider_started/2,
	 is_provider_node_started/2,
	 start_provider/2,
	 stop_provider/2,
%	 is_provider_stopped/2,
%	 
%	 is_provider_unloaded/2,

%	 where_is_provider/1,
%log

	 ping/0	 
	]).
-export([
	 start_link/0,
	 stop/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).
-define(LockId,?MODULE).

-record(state, {orchestrate_started,
		wanted_state,
		lock_id=?LockId,
		is_wanted_state}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_orchestrate(WantedState)->
    gen_server:cast(?SERVER,{start_orchestrate,WantedState}).
orchestrate_result(StartResult)->
    gen_server:cast(?SERVER,{orchestrate_result,StartResult}).
is_wanted_state()->
    gen_server:call(?SERVER,{is_wanted_state},infinity).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_deployment_from_file(FullPathFile)->
    gen_server:call(?SERVER, {create_deployment_from_file,FullPathFile},infinity).
delete_deployment_from_file(FullPathFile)->
    gen_server:call(?SERVER, {delete_deployment_from_file,FullPathFile},infinity).
wanted_state_from_file(FullPathFile)->
    gen_server:call(?SERVER, {wanted_state_from_file,FullPathFile},infinity).


create_provider(ProviderSpec,HostSpec)->
    gen_server:call(?SERVER, {create_provider,ProviderSpec,HostSpec},infinity).
delete_provider(ProviderSpec,HostSpec)->
    gen_server:call(?SERVER, {delete_provider,ProviderSpec,HostSpec},infinity).

    

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_provider(ProviderSpec,HostSpec)->
    gen_server:call(?SERVER, {start_provider,ProviderSpec,HostSpec},infinity).
stop_provider(ProviderSpec,HostSpec)->
    gen_server:call(?SERVER, {stop_provider,ProviderSpec,HostSpec},infinity).
is_provider_started(ProviderSpec,HostSpec)->
    gen_server:call(?SERVER, {is_provider_started,ProviderSpec,HostSpec},infinity).

is_provider_node_started(ProviderSpec,HostSpec)->
    gen_server:call(?SERVER, {is_provider_node_started,ProviderSpec,HostSpec},infinity).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
load_provider(ProviderSpec,HostSpec)->
    gen_server:call(?SERVER, {load_provider,ProviderSpec,HostSpec},infinity).
unload_provider(ProviderSpec,HostSpec)->
    gen_server:call(?SERVER, {unload_provider,ProviderSpec,HostSpec},infinity).
is_provider_loaded(ProviderSpec,HostSpec)->
    gen_server:call(?SERVER, {is_provider_loaded,ProviderSpec,HostSpec},infinity).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_controller(HostSpec)->
    gen_server:call(?SERVER, {start_controller,HostSpec},infinity).
stop_controller(HostSpec)->
    gen_server:call(?SERVER, {stop_controller,HostSpec},infinity).
is_controller_started(HostSpec)->
    gen_server:call(?SERVER, {is_controller_started,HostSpec},infinity).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


stop()-> gen_server:call(?SERVER, {stop},infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.

init([]) ->

    %% Init lock 
    case lists:member(?LockId,sd:call(dbetcd_appl,db_lock,read_all,[],5000)) of
	true->
	    no_action;
	false ->
	    {atomic,ok}=sd:call(dbetcd_appl,db_lock,create,[?LockId,0],5000)
    end,
    
    %% check initial state
    IsWantedState=case orchestrate:is_wanted_state() of
		      true->
			  true;
		      {false,NotDeployed}->
			  false
		  end,
 
    %% start orchestrate automatic
    
    
    [{"production",WantedState}]=sd:call(dbetcd_appl,db_deployment_spec,read_all,[],5000),
    spawn(fun()->orchestrate:start(?LockId,WantedState) end),
      
    
    ?LOG_NOTICE("Server started test pattern ",[WantedState]),
       
    {ok, #state{orchestrate_started=true,
		wanted_state=WantedState,
		is_wanted_state=IsWantedState}}.

%%--------------------------------------------------------------------
%% @doc2
%% @spec
%% @end
%%--------------------------------------------------------------------


handle_call({create_deployment_from_file,FullPathFile}, _From, State) ->
    Reply=lib_provider:create_deployment_from_file(FullPathFile),
    {reply, Reply, State};

handle_call({delete_deployment_from_file,FullPathFile}, _From, State) ->
    Reply=lib_provider:delete_deployment_from_file(FullPathFile),
    {reply, Reply, State};

handle_call({wanted_state_from_file,FullPathFile}, _From, State) ->
    Reply=lib_provider:wanted_state_from_file(FullPathFile),
    {reply, Reply, State};

handle_call({create_provider,ProviderSpec,HostSpec}, _From, State) ->
    Reply=lib_provider:create(ProviderSpec,HostSpec),
    {reply, Reply, State};
handle_call({delete_provider,ProviderSpec,HostSpec}, _From, State) ->
    Reply=lib_provider:delete(ProviderSpec,HostSpec),
    {reply, Reply, State};


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
handle_call({start_provider,ProviderSpec,HostSpec}, _From, State) ->
    Reply=lib_provider:start(ProviderSpec,HostSpec),
    {reply, Reply, State};
handle_call({stop_provider,ProviderSpec,HostSpec}, _From, State) ->
    Reply=lib_provider:stop(ProviderSpec,HostSpec),
    {reply, Reply, State};

handle_call({is_provider_started,ProviderSpec,HostSpec}, _From, State) ->
    Reply=lib_provider:is_started(ProviderSpec,HostSpec),
    {reply, Reply, State};

handle_call({is_provider_node_started,ProviderSpec,HostSpec}, _From, State) ->
    Reply=lib_provider:is_node_started(ProviderSpec,HostSpec),
    {reply, Reply, State};
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
handle_call({load_provider,ProviderSpec,HostSpec}, _From, State) ->
    Reply=lib_provider:load(ProviderSpec,HostSpec),
    {reply, Reply, State};
handle_call({unload_provider,ProviderSpec,HostSpec}, _From, State) ->
    Reply=lib_provider:unload(ProviderSpec,HostSpec),
    {reply, Reply, State};

handle_call({is_provider_loaded,ProviderSpec,HostSpec}, _From, State) ->
    Reply=lib_provider:is_loaded(ProviderSpec,HostSpec),
    {reply, Reply, State};

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
handle_call({start_controller,HostSpec}, _From, State) ->
    Reply=lib_host:start_controller(HostSpec),
    {reply, Reply, State};
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
handle_call({stop_controller,HostSpec}, _From, State) ->
    Reply=lib_host:stop_controller(HostSpec),
    {reply, Reply, State};
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
handle_call({is_controller_started,HostSpec}, _From, State) ->
    Reply=lib_host:is_controller_started(HostSpec),
    {reply, Reply, State};

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
handle_call({is_wanted_state}, _From, State) ->
    Reply=orchestrate:is_wanted_state(),
    {reply, Reply, State};
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
handle_call({ping}, _From, State) ->
    Reply=pong,
    {reply, Reply, State};


handle_call(UnMatchedSignal, From, State) ->
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal, From,?MODULE,?LINE}]),
    Reply = {error,[unmatched_signal,UnMatchedSignal, From]},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({start_orchestrate,WantedState},#state{orchestrate_started=false}=State) ->
    spawn(fun()->orchestrate:start(?LockId,WantedState) end),
    NewState=State#state{orchestrate_started=true,
			 wanted_state=WantedState},
    
    {noreply, NewState};

handle_cast({start_orchestrate,_WantedState},#state{orchestrate_started=true}=State) ->
    io:format("start_orchestrate true ~p~n",[{no_action,?MODULE,?LINE}]),
    {noreply, State};

handle_cast({orchestrate_result,StartResult},State) ->
    NewIsWantedState=case orchestrate:is_wanted_state() of
			 true->
			     case State#state.is_wanted_state of 
				 true->
				     true;
				 false->
				     ?LOG_NOTICE("Wanted State ",[]),
				     false
			     end;
			 {false,_NotDeployed}->
			     case State#state.is_wanted_state of 
				 true->
				     ?LOG_NOTICE("Not Wanted State ",[]),
				     false;
				 false->
				     false
			     end
		     end,
    
    io:format("orchestrate_result ~p~n",[{StartResult,?MODULE,?LINE}]),
    spawn(fun()->orchestrate:start(?LockId,State#state.wanted_state) end),
    {noreply, State#state{is_wanted_state=NewIsWantedState}};
   


handle_cast(UnMatchedSignal, State) ->
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.
handle_info({_A,_B,_C}, State) ->
 %%   io:format("A,B,C ~p~n",[{A,B,C,?MODULE,?LINE}]),
    {noreply, State};



handle_info(Info, State) ->
    io:format("unmatched_signal ~p~n",[{Info,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
