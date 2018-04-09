%% -*- erlang -*-
%%
%% Common runtime environment for distributed programming languages
%%
%% Copyright 2015-2018 Jörgen Brandt
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @author Jörgen Brandt <joergen.brandt@onlinehome.de>
%% @version 0.1.5
%% @copyright 2015-2018 Jörgen Brandt
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( cre_master ).
-behavior( gen_server ).

%%====================================================================
%% Exports
%%====================================================================

%% API functions
-export( [start_link/0, start_link/1, add_worker/2, worker_result/4,
          cre_request/4, stop/1, cre_info/1, node_info/1, task_info/1] ).

%% gen_server callback functions
-export( [code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
          terminate/2] ).



%%====================================================================
%% Record definitions
%%====================================================================

-record( cre_state, { subscr_map = #{},     % maps app to list of client pid, i pairs
                      idle_lst   = [],      % list of idle worker pids
                      busy_map   = #{},     % maps app to worker pid
                      queue      = [],      % list of apps that cannot be scheduled
                      cache      = #{} } ). % maps app to delta

%%====================================================================
%% API functions
%%====================================================================

%% @doc Starts an anonymous CRE instance.
%%
%%      Returns `{ok, Pid}' on success where `Pid' is the process id of the
%%      newly created process.
%%
%% @see start_link/1
%%
start_link() ->
  gen_server:start_link( ?MODULE, [], [] ).


%% @doc Starts a named CRE instance.
%%
%%      Returns `{ok, Pid}' on success where `Pid' is the process id of the
%%      newly created process.
%%
%% @see start_link/0
%%
start_link( CreName ) ->
  gen_server:start_link( CreName, ?MODULE, [], [] ).


%% @doc Registers a worker process with a given CRE instance.
%%
%%      Takes the name of a CRE instance `CreName' and the name of a worker
%%      instance `WorkerName' and adds the worker to the worker pool of the CRE.
%%      The presence of workers is a precondition for the CRE to send out demand
%%      or perform work. A CRE without workers, thus, can accept clients but can
%%      never make progress.
%%
add_worker( CreName, WorkerName ) ->
  gen_server:cast( CreName, {add_worker, WorkerName} ).


%% @doc Sends the result of a previously computed application to the CRE.
%%
%%      When a worker has computed the result of an application that has
%%      previously been requested from it the worker sends the result back to
%%      the CRE using this function.
%%
worker_result( CreName, WorkerName, A, Delta ) ->
  gen_server:cast( CreName, {worker_result, WorkerName, A, Delta} ).


%% @doc Requests the computation of an application from a given CRE intance.
%%
%%      When a client with the name `ClientName' that has received demand has
%%      generated an application `A' belonging to a program with the program
%%      identifier `I' it uses this function to send the application to the CRE
%%      instance with the name `CreName'.
%%
cre_request( CreName, ClientName, I, A ) ->
  gen_server:cast( CreName, {cre_request, ClientName, I, A} ).


%% @doc Stops the CRE instance.
%%
stop( CreName ) ->
  gen_server:stop( CreName ).


cre_info( CreName ) ->
  {ok, Info} = gen_server:call( CreName, cre_info ),
  Info.
  
node_info( CreName ) ->
  {ok, Info} = gen_server:call( CreName, node_info ),
  Info.

task_info( CreName ) ->
  {ok, Info} = gen_server:call( CreName, task_info ),
  Info.


%%====================================================================
%% gen_server callback functions
%%====================================================================

code_change( _OldVsn, CreState, _Extra ) -> {ok, CreState}.
terminate( _Reason, _CreState )          -> ok.

init( _Arg ) ->
  process_flag( trap_exit, true ),
  {ok, #cre_state{}}.


handle_cast( {add_worker, P}, CreState ) ->

  #cre_state{ idle_lst = IdleLst, busy_map = BusyMap } = CreState,

  % extract pid because link/1 cannot deal with registered names
  Pid =
    if
      is_pid( P ) -> P;
      true        -> whereis( P )
    end,

  error_logger:info_report(
    [{info, "new worker"},
     {application, cre},
     {cre_master_pid, self()},
     {worker_pid, Pid},
     {nworker, length( IdleLst )+maps:size( BusyMap )+1}] ),


  true = link( Pid ),

  CreState1 = CreState#cre_state{ idle_lst = [Pid|IdleLst] },

  CreState2 = attempt_progress( CreState1 ),

  {noreply, CreState2};

handle_cast( {worker_result, P, A, Delta}, CreState ) ->

  Pid =
    if
      is_pid( P ) -> P;
      true        -> whereis( P )
    end,

  #cre_state{ subscr_map = SubscrMap,
              idle_lst   = IdleLst,
              busy_map   = BusyMap,
              cache      = Cache } = CreState,

  F =
    fun( {Q, I} ) ->
      cre_client:cre_reply( Q, I, A, Delta )
    end,

  case maps:get( A, BusyMap, undefined ) of

    Pid ->
      lists:foreach( F, maps:get( A, SubscrMap ) ),
      CreState1 = CreState#cre_state{ subscr_map = maps:remove( A, SubscrMap ),
                                      idle_lst   = [Pid|IdleLst],
                                      busy_map   = maps:remove( A, BusyMap ),
                                      cache      = Cache#{ A => Delta } },
      CreState2 = attempt_progress( CreState1 ),
      {noreply, CreState2};

    _ ->
      {noreply, CreState}

  end;

handle_cast( {cre_request, Q, I, A}, CreState ) ->

  #cre_state{ subscr_map = SubscrMap,
              busy_map   = BusyMap,
              queue      = Queue,
              cache      = Cache } = CreState,

  case maps:is_key( A, Cache ) of

    true ->
      cre_client:cre_reply( Q, I, A, maps:get( A, Cache ) ),
      {noreply, CreState};

    false ->
      SubscrMap1 = SubscrMap#{ A => [{Q, I}|maps:get( A, SubscrMap, [] )] },
      case lists:member( A, Queue ) orelse maps:is_key( A, BusyMap ) of

        true ->
          {noreply, CreState#cre_state{ subscr_map = SubscrMap1 }};

        false ->
          Queue1 = [A|Queue],
          CreState1 = CreState#cre_state{ subscr_map = SubscrMap1,
                                          queue      = Queue1 },
          CreState2 = attempt_progress( CreState1 ),
          {noreply, CreState2}

      end

  end;

handle_cast( _Request, CreState ) -> {noreply, CreState}.


handle_info( {'EXIT', P, _Reason}, CreState ) ->

  #cre_state{ idle_lst = IdleLst,
              busy_map = BusyMap,
              queue    = Queue } = CreState,

  Pid =
    if
      is_pid( P ) -> P;
      true        -> whereis( P )
    end,


  case lists:member( Pid, IdleLst ) of

    % an idle worker died
    true ->

      error_logger:info_report(
        [{info, "idle worker down"},
         {application, cre},
         {cre_master_pid, self()},
         {worker_pid, Pid},
         {nworker, length( IdleLst )+maps:size( BusyMap )-1}] ),

      CreState1 = CreState#cre_state{ idle_lst = IdleLst--[Pid] },

      {noreply, CreState1};

    false ->
      case lists:keyfind( Pid, 2, maps:to_list( BusyMap ) ) of

        % a busy worker died    
        {A, Pid} ->

          error_logger:info_report(
            [{info, "busy worker down"},
             {application, cre},
             {cre_master_pid, self()},
             {worker_pid, Pid},
             {nworker, length( IdleLst )+maps:size( BusyMap )-1}] ),

          CreState1 = CreState#cre_state{ queue    = [A|Queue],
                                          busy_map = maps:remove( A, BusyMap ) },
          CreState2 = attempt_progress( CreState1 ),
          {noreply, CreState2};

        % some other linked process died
        false ->

          error_logger:info_report(
            [{info, "exit signal received"},
             {application, cre},
             {cre_master_pid, self()},
             {from_pid, Pid}] ),

          {stop, exit, CreState}
      end

  end;

handle_info( _Info, CreState ) -> {noreply, CreState}.






handle_call( cre_info, _From, CreState ) ->

  #cre_state{ idle_lst = IdleLst,
              busy_map = BusyMap } = CreState,

  NIdle = length( IdleLst ),
  NBusy = maps:size( BusyMap ),
  N     = NBusy+NIdle,

  Ratio =
    case N of
      0 -> 0.0;
      _ -> NBusy/N
    end,

  CreInfoMap = #{ total_load => Ratio, n_wrk => N},

  {reply, {ok, CreInfoMap}, CreState};

handle_call( node_info, _From, CreState ) ->

  #cre_state{ idle_lst = IdleLst,
              busy_map = BusyMap } = CreState,

  PidLst = IdleLst++maps:values( BusyMap ),

  F =
    fun( Pid, Acc ) ->
      Node = node( Pid ),
      L = maps:get( Node, Acc, [] ),
      Acc#{ Node => [Pid|L] }
    end,

  NodeWrkMap = lists:foldl( F, #{}, PidLst ),

  IsBusy =
    fun( Pid ) ->
      not lists:member( Pid, IdleLst )
    end,

  G =
    fun( Node, L ) ->
      NWrk = length( L ),
      NBusy  = length( lists:filter( IsBusy, L ) ),
      NodeLoad = NBusy/NWrk,
      #{ node => Node, n_wrk => NWrk, node_load => NodeLoad }
    end,

  NodeInfoLst = maps:values( maps:map( G, NodeWrkMap ) ),

  {reply, {ok, NodeInfoLst}, CreState};

handle_call( task_info, _From, CreState ) ->

  #cre_state{ cache    = Cache,
              busy_map = BusyMap,
              queue    = Queue } = CreState,

  BinaryToHexString =
    fun( X ) when is_binary( X ) ->
      list_to_binary( 
        lists:flatten(
          [io_lib:format( "~2.16.0b", [B] ) || <<B>> <= X] ) )
    end,

  FormatQueuedTask =
    fun

      % if apps have the form of Cuneiform applications, we can format them
      ( #{ app_id := AppId, lambda := #{ lambda_name := LambdaName } } )
      when is_binary( AppId ),
           is_binary( LambdaName ) ->
        #{ app_id      => AppId,
           lambda_name => LambdaName };

      % generic apps are represented with the last seven digits of their sha224
      ( App ) ->
        Hash = crypto:hash( sha224, io_lib:format( "~w", [App] ) ),
        B = BinaryToHexString( Hash ),
        #{ app_id      => B,
           lambda_name => <<"na">> }

    end,

  FormatActiveTask =
    fun( App, Pid ) ->
      M = FormatQueuedTask( App ),
      M#{ node => atom_to_binary( node( Pid ), utf8 ) }
    end,

  FormatCompleteTask =
    fun( App ) ->
      M = FormatQueuedTask( App ),
      #{ App := R } = Cache,
      case R of

        #{ stat := #{ node := Node }, result := #{ status := Status } }
        when is_binary( Node ),
             Status =:= <<"ok">> orelse Status =:= <<"error">> ->
          M#{ node => Node, status => Status };
        
        _ ->
          M#{ node => <<"na">>, status => <<"ok">> }
          
      end
    end,

  QueuedLst = [FormatQueuedTask( App ) || App <- Queue],
  ActiveLst = [FormatActiveTask( App, Pid ) || {App, Pid} <- maps:to_list( BusyMap )],
  CompleteLst = [FormatCompleteTask( App ) || App <- maps:keys( Cache )],

  SortFormattedTask =
    fun( M1, M2 ) ->
      #{ app_id := AppId1, lambda_name := LambdaName1 } = M1,
      #{ app_id := AppId2, lambda_name := LambdaName2 } = M2,
      if
        LambdaName1 =/= LambdaName2 -> LambdaName1 =< LambdaName2;
        true                        -> AppId1 =< AppId2
      end
    end,

  SortFormattedActiveTask =
    fun( M1, M2 ) ->

      #{ app_id := AppId1, lambda_name := LambdaName1, node := Node1 } = M1,
      #{ app_id := AppId2, lambda_name := LambdaName2, node := Node2 } = M2,

      if

        Node1 =/= Node2 ->
          Node1 =< Node2;

        true ->
          if

            LambdaName1 =/= LambdaName2 ->
              LambdaName1 =< LambdaName2;

            true ->
              AppId1 =< AppId2

          end

      end

    end,

  TaskInfoMap = #{ queued_lst   => lists:sort( SortFormattedTask, QueuedLst ),
                   active_lst   => lists:sort( SortFormattedActiveTask, ActiveLst ),
                   complete_lst => lists:sort( SortFormattedTask, CompleteLst ) },

  {reply, {ok, TaskInfoMap}, CreState};

handle_call( _Request, _From, CreState ) ->
  {reply, {error, bad_msg}, CreState}.




%%====================================================================
%% Internal functions
%%====================================================================

-spec attempt_progress( CreState :: #cre_state{} ) -> #cre_state{}.

attempt_progress( CreState ) ->

  #cre_state{ idle_lst   = IdleLst,
              busy_map   = BusyMap,
              queue      = Queue } = CreState,


  case Queue of

    [] ->
      CreState;

    [A|Queue1] ->
      case IdleLst of

        [] ->
          CreState;

        [_|_] ->

          Pid = lib_combin:pick_from( IdleLst ),
          IdleLst1 = IdleLst--[Pid],
          BusyMap1 = BusyMap#{ A => Pid },

          cre_worker:worker_request( Pid, A ),

          CreState#cre_state{ idle_lst = IdleLst1,
                              busy_map = BusyMap1,
                              queue    = Queue1 }

      end

  end.