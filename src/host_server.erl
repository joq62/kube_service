%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(host_server).
 
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("log.api").

%% --------------------------------------------------------------------
-define(HeartbeatTime,20*1000).

 -define(CookieStr,"a_cookie").
-define(SERVER,?MODULE).
%% External exports

-export([
	 start_controller/1,
	 stop_controller/1,
	 is_started_controller/1
	]).



-export([
	 
	 start_nodes/0,
	 create_node/1,
	 desired_nodes/0,
	 active_nodes/0,
	 stopped_nodes/0

	]).
-export([
	 ping/0
	]).


-export([
	 start/0,
	 stop/0
	]).


%% gen_server callbacks



-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%-------------------------------------------------------------------
-record(state,{
	       all_hosts,
	       cookie_str
	      }).


%% ====================================================================
%% External functions
%% ====================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_controller(HostSpec) ->
    gen_server:call(?SERVER, {start_controller,HostSpec},infinity).
stop_controller(HostSpec) ->
    gen_server:call(?SERVER, {stop_controller,HostSpec},infinity).

is_started_controller(HostSpec) ->
    gen_server:call(?SERVER, {is_started_controller,HostSpec},infinity).

	    
%% call
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).
%%---------------------------------------------------------------------
start_nodes()->
    gen_server:call(?MODULE,{start_nodes},infinity).
    
create_node(HostSpec)->
    gen_server:call(?MODULE,{create_node,HostSpec},infinity).

desired_nodes()->
    gen_server:call(?MODULE,{desired_nodes},infinity).
active_nodes()->
    gen_server:call(?MODULE,{active_nodes},infinity).
stopped_nodes()->
    gen_server:call(?MODULE,{stopped_nodes},infinity).


%%----------------------------------------------------------------------------

ping() ->
    gen_server:call(?MODULE, {ping}).
%% cast


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
init([]) -> 
    AllHosts=sd:call(dbetcd,db_host_spec,get_all_id,[],5000),
    CookieStr=atom_to_list(erlang:get_cookie()),
    io:format("CookieStr ~p~n",[CookieStr]),
    ?LOG_NOTICE("server start",[AllHosts,CookieStr]),
    {ok, #state{all_hosts=AllHosts,
		cookie_str=?CookieStr}}.   
 

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

handle_call({start_controller,HostSpec},_From, State) ->
    {ok,Node}=sd:call(dbetcd,db_host_spec,read,[connect_node,HostSpec],5000),
    rpc:call(Node,init,stop,[]),
    timer:sleep(3000),
    PaArgs=" ",
    EnvArgs="  ",
    CookieStr=State#state.cookie_str,
    {ok,NodeName}=db_host_spec:read(connect_node_name,HostSpec),
    Reply=lib_kube:ssh_create_node(HostSpec,NodeName,CookieStr,PaArgs,EnvArgs),
    {reply, Reply, State};

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

handle_call({start_nodes},_From, State) ->
    Reply=lib_host:start_nodes(State#state.all_hosts,State#state.cookie_str),
    {reply, Reply, State};

handle_call({create_node,HostSpec},_From, State) ->
 %   Reply={glurk},
    Reply=lib_host:create_node(HostSpec,State#state.cookie_str),
    {reply, Reply, State};

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
handle_info({ssh_cm,_,_}, State) ->
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
