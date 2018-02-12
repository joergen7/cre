%% -*- erlang -*-
%%
%% A common runtime environment (CRE) for distributed workflow languages.
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
%% @version 0.1.2
%% @copyright 2015-2018 Jörgen Brandt
%%
%% @doc A module implementing the behavior of the common runtime environment
%%      (CRE).
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
          cre_request/4, stop/1] ).

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

%%====================================================================
%% gen_server callback functions
%%====================================================================

code_change( _OldVsn, CreState, _Extra )  -> {ok, CreState}.
handle_call( _Request, _From, CreState ) -> {reply, {error, bad_msg}, CreState}.
terminate( _Reason, _CreState )           -> ok.

init( _Arg ) ->
  process_flag( trap_exit, true ),
  {ok, #cre_state{}}.


handle_cast( {add_worker, P}, CreState ) ->

  error_logger:info_report(
    [{info, "registering new worker"},
     {application, cre},
     {cre_master_pid, self()},
     {worker_pid, P}] ),

  true = link( P ),

  #cre_state{ idle_lst = IdleLst } = CreState,

  CreState1 = CreState#cre_state{ idle_lst = [P|IdleLst] },

  CreState2 = attempt_progress( CreState1 ),

  {noreply, CreState2};

handle_cast( {worker_result, P, A, Delta}, CreState ) ->

  #cre_state{ subscr_map = SubscrMap,
              idle_lst   = IdleLst,
              busy_map   = BusyMap,
              cache      = Cache } = CreState,

  F =
    fun( {Q, I} ) ->
      cre_client:cre_reply( Q, I, A, Delta )
    end,

  case maps:get( A, BusyMap, undefined ) of

    P ->
      lists:foreach( F, maps:get( A, SubscrMap ) ),
      CreState1 = CreState#cre_state{ subscr_map = maps:remove( A, SubscrMap ),
                                      idle_lst   = [P|IdleLst],
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

  error_logger:info_report(
    [{info, "worker down"},
     {application, cre},
     {cre_master_pid, self()},
     {worker_pid, P}] ),

  #cre_state{ idle_lst = IdleLst,
              busy_map = BusyMap,
              queue    = Queue } = CreState,

  

  case lists:member( P, IdleLst ) of

    true ->
      CreState1 = CreState#cre_state{ idle_lst = IdleLst--[P] },
      {noreply, CreState1};

    false ->
      {A, P} = lists:keyfind( P, 2, maps:to_list( BusyMap ) ),
      CreState1 = CreState#cre_state{ queue    = [A|Queue],
                                      busy_map = maps:remove( A, BusyMap ) },
      CreState2 = attempt_progress( CreState1 ),
      {noreply, CreState2}

  end;

handle_info( _Info, CreState ) -> {noreply, CreState}.

%%====================================================================
%% Internal functions
%%====================================================================

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

          P = lib_combin:pick_from( IdleLst ),
          IdleLst1 = IdleLst--[P],
          BusyMap1 = BusyMap#{ A => P },

          cre_worker:worker_request( P, A ),

          CreState#cre_state{ idle_lst = IdleLst1,
                              busy_map = BusyMap1,
                              queue    = Queue1 }

      end

  end.