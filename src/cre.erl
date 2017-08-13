%% -*- erlang -*-
%%
%% A common runtime environment for distributed workflow languages.
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
%% @doc
%%
%%
%% @end
%% -------------------------------------------------------------------


-module( cre ).
-behaviour( gen_pnet ).

%%====================================================================
%% Exports
%%====================================================================

-export( [code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
          terminate/2, trigger/3] ).

-export( [place_lst/0, trsn_lst/0, init_marking/2, preset/1, is_enabled/3,
          fire/3] ).

-export( [start_link/0] ).

%%====================================================================
%% Macro definitions
%%====================================================================

-define( DEMAND_FACTOR, 2 ).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  gen_pnet:start_link( ?MODULE, [], [] ).


%%====================================================================
%% Interface callback functions
%%====================================================================

code_change( _OldVsn, NetState, _Extra ) -> {ok, NetState}.

handle_call( _Request, _From, _NetState ) -> {reply, {error, bad_msg}}.

handle_cast( _Request, _NetState ) -> noreply.

handle_info( _Request, _NetState ) -> noreply.

init( [] ) ->

  {ok, gen_pnet:new( ?MODULE, [] )}.

terminate( _Reason, _NetState ) -> ok.

trigger( _Place, _Token, _NetState ) -> pass.


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
is_enabled( _Trsn, _Mode, _UsrInfo ) -> false.


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


