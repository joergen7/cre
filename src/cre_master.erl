%% -*- erlang -*-
%%
%% A common runtime environment (CRE) for distributed workflow languages.
%%
%% Copyright 2015-2017 Jörgen Brandt
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
%% @version 0.1.0
%% @copyright 2015-2017 Jörgen Brandt
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
-behaviour( gen_pnet ).

%%====================================================================
%% Exports
%%====================================================================

-export( [code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
          terminate/2, trigger/3] ).

-export( [place_lst/0, trsn_lst/0, init_marking/2, preset/1, is_enabled/3,
          fire/3] ).

-export( [start_link/0, start_link/1, add_worker/2, worker_result/4,
          add_client/2, cre_request/4] ).

%%====================================================================
%% Macro definitions
%%====================================================================

-define( DEMAND_FACTOR, 2 ).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Starts an anonymous CRE instance.
%%
%%      Returns `{ok, Pid}` on success where `Pid` is the process id of the
%%      newly created process.
%%
%% @see start_link/1
%%
start_link() ->
  gen_pnet:start_link( ?MODULE, [], [] ).


%% @doc Starts a named CRE instance.
%%
%%      Returns `{ok, Pid}` on success where `Pid` is the process id of the
%%      newly created process.
%%
%% @see start_link/0
%%
start_link( CreName ) ->
  gen_pnet:start_link( CreName, ?MODULE, [], [] ).


%% @doc Registers a worker process with a given CRE instance.
%%
%%      Takes the name of a CRE instance `CreName` and the name of a worker
%%      instance `WorkerName` and adds the worker to the worker pool of the CRE.
%%      The presence of workers is a precondition for the CRE to send out demand
%%      or perform work. A CRE without workers, thus, can accept clients but can
%%      never make progress.
%%
add_worker( CreName, WorkerName ) ->
  gen_pnet:cast( CreName, {add_worker, WorkerName} ).


%% @doc Sends the result of a previously computed application to the CRE.
%%
%%      When a worker has computed the result of an application that has
%%      previously been requested from it the worker sends the result back to
%%      the CRE using this function.
%%
worker_result( CreName, WorkerName, A, Delta ) ->
  gen_pnet:cast( CreName, {worker_result, WorkerName, A, Delta} ).


%% @doc Registers a client process with a given CRE instance.
%%
%%      Takes the name of a CRE instance `CreName` and the name of a client
%%      instance `ClientName` and adds the client to the client pool of the CRE.
%%
add_client( CreName, ClientName ) ->
  gen_pnet:cast( CreName, {add_client, ClientName} ).

%% @doc Requests the computation of an application from a given CRE intance.
%%
%%      When a client with the name `ClientName` that has received demand has
%%      generated an application `A` belonging to a program with the program
%%      identifier `I` it uses this function to send the application to the CRE
%%      instance with the name `CreName`.
%%
cre_request( CreName, ClientName, I, A ) ->
  gen_pnet:cast( CreName, {cre_request, ClientName, I, A} ).


%%====================================================================
%% Interface callback functions
%%====================================================================

code_change( _OldVsn, NetState, _Extra )  -> {ok, NetState}.
handle_call( _Request, _From, _NetState ) -> {reply, {error, bad_msg}}.
terminate( _Reason, _NetState )           -> ok.

init( [] ) ->
  process_flag( trap_exit, true ),
  {ok, gen_pnet:new( ?MODULE, [] )}.

handle_cast( {add_worker, P}, _ ) ->
  {noreply, #{}, #{ 'AddWorker' => [P] }};

handle_cast( {worker_result, P, A, Delta}, _ ) ->
  {noreply, #{}, #{ 'WorkerResult' => [{{P, A}, Delta}] }};

handle_cast( {add_client, Q}, _ ) ->
  {noreply, #{}, #{ 'AddClient' => [Q] }};

handle_cast( {cre_request, Q, I, A}, _ ) ->
  {noreply, #{}, #{ 'CreRequest' => [{{Q, I}, A}] }};

handle_cast( _Request, _NetState ) ->
  noreply.


handle_info( {'EXIT', FromPid, _}, NetState ) ->

  AddClient  = gen_pnet:get_ls( 'AddClient', NetState ),
  ClientPool = gen_pnet:get_ls( 'ClientPool', NetState ),
  AddWorker  = gen_pnet:get_ls( 'AddWorker', NetState ),
  WorkerPool = gen_pnet:get_ls( 'WorkerPool', NetState ),
  BusyWorker = gen_pnet:get_ls( 'BusyWorker',NetState ),

  QLst = AddClient++ClientPool,
  PLst = AddWorker++WorkerPool++BusyWorker,

  ExitClient = case lists:member( FromPid, QLst ) of
    true  -> [FromPid];
    false -> []
  end,

  ExitWorker = case lists:member( FromPid, PLst ) of
    true  -> [FromPid];
    false -> []
  end,

  {noreply, #{}, #{ 'ExitClient' => ExitClient, 'ExitWorker' => ExitWorker }};

handle_info( _Info, _NetState ) ->
  noreply.


trigger( 'Demand', Q, _ ) ->
  cre_client:demand( Q ),
  drop;

trigger( 'CreReply', {{Q, I}, A, Delta}, _ ) ->
  cre_client:cre_reply( Q, I, A, Delta ),
  drop;

trigger( 'WorkerRequest', {P, A}, _ ) ->
  cre_worker:worker_request( P, A ),
  drop;

trigger( _Place, _Token, _NetState ) ->
  pass.

%%====================================================================
%% Petri net callback functions
%%====================================================================

place_lst() ->
 [
  % client interface
  'AddClient', 'ExitClient', 'Demand', 'CreRequest', 'CreReply',

  % worker interface
  'AddWorker', 'ExitWorker', 'WorkerRequest', 'WorkerResult',

  % demand cycle
  'ClientPool', 'BadClient', 'DemandPool', 'SentDemand', 'BusyDemand',

  % cache cylce
  'Introduced', 'Released', 'Guard', 'Cache',

  % invocation cycle
  'Allowed', 'WorkerPool', 'BusyWorker', 'Surplus'
 ].

trsn_lst() ->
  [
   % demand cycle
   link_client, remove_client, send_demand, recover_demand, introduce, address,

   % cache cycle
   allow, lookup,

   % invocation cycle
   link_worker, remove_worker, reallow, schedule, release, remove_demand
  ].

init_marking( 'Guard', _ ) -> [[]];
init_marking( _Place, _UsrInfo ) -> [].

preset( link_client )    -> ['AddClient'];
preset( remove_client )  -> ['ClientPool', 'ExitClient'];
preset( send_demand )    -> ['DemandPool', 'ClientPool'];
preset( recover_demand ) -> ['SentDemand', 'BadClient'];
preset( introduce )      -> ['SentDemand', 'CreRequest'];
preset( address )        -> ['Released', 'BusyDemand'];
preset( allow )          -> ['Introduced', 'Guard'];
preset( lookup )         -> ['Introduced', 'Cache'];
preset( link_worker )    -> ['AddWorker'];
preset( remove_worker )  -> ['ExitWorker', 'WorkerPool'];
preset( reallow )        -> ['ExitWorker', 'BusyWorker'];
preset( schedule )       -> ['Allowed', 'WorkerPool'];
preset( release )        -> ['WorkerResult', 'BusyWorker'];
preset( remove_demand )  -> ['Surplus', 'DemandPool'].

is_enabled( link_client,    _,                                                              _ ) -> true;
is_enabled( remove_client,  #{ 'ClientPool' := [Q], 'ExitClient' := [Q] },                  _ ) -> true;
is_enabled( send_demand,    _,                                                              _ ) -> true;
is_enabled( recover_demand, #{ 'SentDemand' := [Q], 'BadClient' := [Q] },                   _ ) -> true;
is_enabled( introduce,      #{ 'SentDemand' := [Q], 'CreRequest' := [{{Q, _}, _}] },        _ ) -> true;
is_enabled( address,        #{ 'Released' := [{A, _}], 'BusyDemand' := [{_, A}] },          _ ) -> true;
is_enabled( allow,          #{ 'Introduced' := [A], 'Guard' := [Alst] },                    _ ) -> not lists:member( A, Alst );
is_enabled( lookup,         #{ 'Introduced' := [A], 'Cache' := [{A, _}] },                  _ ) -> true;
is_enabled( link_worker,    _,                                                              _ ) -> true;
is_enabled( remove_worker,  #{ 'ExitWorker' := [P], 'WorkerPool' := [P] },                  _ ) -> true;
is_enabled( reallow,        #{ 'ExitWorker' := [P], 'BusyWorker' := [{P, _}] },             _ ) -> true;
is_enabled( schedule,       _,                                                              _ ) -> true;
is_enabled( release,        #{ 'WorkerResult' := [{{P, A}, _}], 'BusyWorker' := [{P, A}] }, _ ) -> true;
is_enabled( remove_demand,  _,                                                              _ ) -> true;
is_enabled( _Trsn,          _,                                                              _ ) -> false.


fire( link_client, #{ 'AddClient' := [Q] }, _ ) ->
  true = link( Q ),
  {produce, #{ 'ClientPool' => [Q] }};

fire( remove_client, #{ 'ClientPool' := [Q],
                        'ExitClient' := [Q] }, _ ) ->
  {produce, #{ 'BadClient' => [Q] }};

fire( send_demand, #{ 'DemandPool' := [unit],
                      'ClientPool' := [Q] }, _ ) ->
  {produce, #{ 'ClientPool' => [Q],
               'Demand'     => [unit],
               'SentDemand' => [Q] }};

fire( recover_demand, #{ 'SentDemand' := [Q],
                         'BadClient'  := [Q]}, _ ) ->
  {produce, #{ 'BadClient'  => [Q],
               'DemandPool' => [unit] }};

fire( introduce, #{ 'SentDemand' := [Q],
                    'CreRequest' := [{{Q, I}, A}] }, _ ) ->
  {produce, #{ 'Introduced' => [A],
               'BusyDemand' => [{{Q, I}, A}] }};

fire( address, #{ 'Released'   := [{A, Delta}],
                  'BusyDemand' := [{{Q, I}, A}] }, _ ) ->
  {produce, #{ 'CreReply'   => [{{Q, I}, A, Delta}],
               'DemandPool' => [unit] }};

fire( allow, #{ 'Introduced' := [A],
                'Guard'      := [Alst] }, _ ) ->
  {produce, #{ 'Allowed' => [A],
               'Guard'   => [[A|Alst]] }};

fire( lookup, #{ 'Introduced' := [A],
                 'Cache'      := [{A, Delta}] }, _ ) ->
  {produce, #{ 'Released' => [{A, Delta}],
               'Cache'    => [{A, Delta}] }};

fire( link_worker, #{ 'AddWorker' := [P] }, _ ) ->
  {produce, #{ 'WorkerPool' => [P],
               'DemandPool' => lists:duplicate( ?DEMAND_FACTOR, unit ) }};

fire( remove_worker, #{ 'ExitWorker' := [P],
                        'WorkerPool' := [P] }, _ ) ->
  {produce, #{ 'Surplus' => lists:duplicate( ?DEMAND_FACTOR, unit ) }};

fire( reallow, #{ 'ExitWorker' := [P],
                  'BusyWorker' := [{P, A}] }, _ ) ->
  {produce, #{ 'Surplus' => lists:duplicate( ?DEMAND_FACTOR, unit ),
               'Allowed' => [A] }};

fire( schedule, #{ 'Allowed'    := [A],
                   'WorkerPool' := [P] }, _ ) ->
  {produce, #{ 'WorkerRequest' => [{P, A}],
               'BusyWorker'    => [{P, A}] }};

fire( release, #{ 'WorkerResult' := [{{P, A}, Delta}],
                  'BusyWorker'   := [{P, A}] }, _ ) ->
  {produce, #{ 'WorkerPool' => [P],
               'Cache'      => [{A, Delta}],
               'Released'   => [{A, Delta}] }};

fire( remove_demand, _, _ ) ->
  {produce, #{}}.


