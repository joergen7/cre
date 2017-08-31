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
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( cre_worker ).
-behavior( gen_pnet ).

%%====================================================================
%% Exports
%%====================================================================

-export( [code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
          terminate/2, trigger/3] ).

-export( [place_lst/0, trsn_lst/0, init_marking/2, preset/1, is_enabled/3,
          fire/3] ).

-export( [start_link/3] ).

%%====================================================================
%% Includes
%%====================================================================


%%====================================================================
%% Callback definitions
%%====================================================================

-callback do_stagein( A :: _, F :: _, UsrInfo :: _ ) -> ok | {error, enoent}.

-callback do_stageout( A :: _, F :: _, UsrInfo :: _ ) -> ok | {error, enoent}.

-callback init( WrkArg :: _ ) -> UsrInfo :: _.

-callback run( A :: _, UsrInfo :: _ ) -> {ok, R :: _} | {error, Reason :: _}.

-callback stagein_lst( A :: _ ) -> [F :: _].

-callback stageout_lst( A :: _, R :: _ ) -> [F :: _].


%%====================================================================
%% Record definitions
%%====================================================================

-record( wrk_state, {cre_name, wrk_mod, usr_info} ).

%%====================================================================
%% API functions
%%====================================================================

start_link( CreName, WrkMod, WrkArg ) ->
  gen_pnet:start_link( ?MODULE, {CreName, WrkMod, WrkArg}, [] ).


%%====================================================================
%% Interface callback functions
%%====================================================================

code_change( _OldVsn, NetState, _Extra ) -> {ok, NetState}.

handle_call( _Request, _From, _NetState ) -> {reply, {error, bad_msg}}.

handle_cast( {worker_request, A}, _NetState ) ->
  {noreply, #{}, #{ 'WorkerRequest' => [A] }};

handle_cast( _Request, _NetState ) -> noreply.

handle_info( _Request, _NetState ) -> noreply.

init( {CreName, WrkMod, WrkArg} ) ->

  UsrInfo = WrkMod:init( WrkArg ),

  WrkState =  #wrk_state{ cre_name = CreName,
                          wrk_mod  = WrkMod,
                          usr_info = UsrInfo },

  ok = cre_master:add_worker( CreName, self() ),

  {ok, gen_pnet:new( ?MODULE, WrkState )}.

terminate( shutdown, _NetState ) -> shutdown;
terminate( _Reason, _NetState ) -> ok.

trigger( 'WorkerOk', {A, Ra}, NetState ) ->
  #wrk_state{ cre_name = CreName } = gen_pnet:get_usr_info( NetState ),
  cre_master:worker_ok( CreName, self(), A, Ra ),
  drop;

trigger( 'WorkerError', {A, Ea}, NetState ) ->
  #wrk_state{ cre_name = CreName } = gen_pnet:get_usr_info( NetState ),
  cre_master:return_error( CreName, self(), A, Ea ),
  drop;

trigger( _Place, _Token, _NetState ) -> pass.


%%====================================================================
%% Petri net callback functions
%%====================================================================

place_lst() ->
  ['WorkerRequest', 'Stagein', 'StageinOk', 'StageinError', 'PreSync', 'Result',
   'Stageout', 'StageoutOk', 'StageoutError', 'PostSync', 'WorkerOk',
   'WorkerError'].


trsn_lst() ->
  [prep_stagein, do_stagein, sync_inok, sync_inerror, run, prep_stageout,
   do_stageout, sync_outok, sync_outerror, ret_ok, ret_posterror, ret_preerror].


init_marking( _Place, _UsrInfo ) -> [].


preset( prep_stagein )  -> ['WorkerRequest'];
preset( do_stagein )    -> ['Stagein'];
preset( sync_inok )     -> ['StageinOk', 'PreSync'];
preset( sync_inerror )  -> ['StageinError', 'PreSync'];
preset( run )           -> ['PreSync'];
preset( prep_stageout ) -> ['Result'];
preset( do_stageout )   -> ['Stageout'];
preset( sync_outok )    -> ['StageoutOk', 'PostSync'];
preset( sync_outerror ) -> ['StageoutError', 'PostSync'];
preset( ret_ok )        -> ['PostSync'];
preset( ret_posterror ) -> ['PostSync'];
preset( ret_preerror )  -> ['PreSync'].


is_enabled( prep_stagein, _, _ )   -> true;
is_enabled( do_stagein,   _, _ )   -> true;
is_enabled( sync_inok,    _, _ )   -> true;
is_enabled( sync_inerror, _, _ )   -> true;
is_enabled( prep_stageout, _, _ )  -> true;
is_enabled( do_stageout, _, _ )    -> true;
is_enabled( sync_outok, _, _ )     -> true;
is_enabled( sync_outerror, _ , _ ) -> true;

is_enabled( ret_preerror, #{ 'PreSync' := [{A, F1, F2}] },
                          #wrk_state{ wrk_mod = WrkMod } )
when length( F2 ) > 0 ->
  F1uF2 = ordsets:union( F1, F2 ),
  Fa = ordsets:from_list( WrkMod:stagein_lst( A ) ),
  Fa =:= F1uF2;

is_enabled( ret_preerror, _, _ ) -> false;

is_enabled( run, #{ 'PreSync' := [{A, F1, []}] }, 
                 #wrk_state{ wrk_mod = WrkMod } ) ->
  Fa = ordsets:from_list( WrkMod:stagein_lst( A ) ),
  Fa =:= F1;

is_enabled( run, _, _ ) -> false;

is_enabled( ret_posterror, #{ 'PostSync' := [{A, Ra, F1, F2}] },
                           #wrk_state{ wrk_mod = WrkMod } )
when length( F2 ) > 0 ->
  F1uF2 = ordsets:union( F1, F2 ),
  Fa = ordsets:from_list( WrkMod:stageout_lst( A, Ra ) ),
  Fa =:= F1uF2;

is_enabled( ret_posterror, _, _ ) -> false;

is_enabled( ret_ok, #{ 'PostSync' := [{A, Ra, F1, []}] },
                    #wrk_state{ wrk_mod = WrkMod } ) ->
  Fa = ordsets:from_list( WrkMod:stageout_lst( A, Ra ) ),
  Fa =:= F1;

is_enabled( ret_ok, _, _ ) -> false.


fire( prep_stagein, #{ 'Start' := [A] }, #wrk_state{ wrk_mod = WrkMod } ) ->
  Fa = WrkMod:stagein_lst( A ),
  {produce, #{ 'Stagein' => [{A, F} || F <- Fa], 'PreSync' => [{A, [], []}] }};

fire( do_stagein, #{ 'Stagein' := [{A, F}] },
                  #wrk_state{ wrk_mod = WrkMod, usr_info = UsrInfo } ) ->
  case WrkMod:do_stagein( A, F, UsrInfo ) of
    ok              -> {produce, #{ 'StageinOk' => [{A, F}] }};
    {error, enoent} -> {produce, #{ 'StageinError' => [{A, F}] }}
  end;

fire( sync_inok, #{ 'StageinOk' := [{A, F}], 'PreSync' := [{A, F1, F2}] },
                 _WrkState ) ->
  {produce, #{ 'PreSync' => [{A, ordsets:add_element( F, F1 ), F2}] }};

fire( sync_inerror, #{ 'StageinError' := [{A, F}], 'PreSync' := [{A, F1, F2}] },
                    _WrkState ) ->
  {produce, #{ 'PreSync' => [{A, F1, ordsets:add_element( F, F2 )}] }};

fire( ret_preerror, #{ 'PreSync' := [{A, _F1, F2}] }, _WrkState ) ->
  {produce, #{ 'WorkerError' => [{A, {stagein, F2}}] }};

fire( run, #{ 'PreSync' := [{A, _Fa, []}] },
      #wrk_state{ wrk_mod = WrkMod, usr_info = UsrInfo } ) ->
  case WrkMod:run( A, UsrInfo ) of
    {ok, Ra}        -> {produce, #{ 'Result' => [{A, Ra}] }};
    {error, Reason} -> {produce, #{ 'WorkerError' => [{A, {run, Reason}}] }}
  end;

fire( prep_stageout, #{ 'Result' := [{A, Ra}]}, #wrk_state{ wrk_mod = WrkMod } ) ->
  Fra = WrkMod:stageout_lst( A, Ra ),
  {produce, #{ 'Stageout' => [{A, F} || F <- Fra], 'PostSync' => [{A, Ra, [], []}] }};

fire( do_stageout, #{ 'Stageout' := [{A, F}] },
                   #wrk_state{ wrk_mod = WrkMod, usr_info = UsrInfo } ) ->
  case WrkMod:do_stageout( A, F, UsrInfo ) of
    ok              -> {produce, #{ 'StageoutOk' => [{A, F}] }};
    {error, enoent} -> {produce, #{ 'StageoutError' => [{A, F}] }}
  end;

fire( sync_outok, #{ 'StageoutOk' := [{A, F}],
                     'PostSync'   := [{A, Ra, F1, F2}] }, _WrkState ) ->
  {produce, #{ 'PostSync' => [{A, Ra, ordsets:add_element( F, F1 ), F2}] }};

fire( sync_outerror, #{ 'StageoutError' := [{A, F}],
                        'PostSync'      := [{A, Ra, F1, F2}] },
                     _WrkState ) ->
  {produce, #{ 'PostSync' => [{A, Ra, F1, ordsets:add_element( F, F2 )}] }};

fire( ret_posterror, #{ 'PostSync' := [{A, _Ra, _F1, F2}] }, _WrkState ) ->
  {produce, #{ 'Error' => [{A, {stageout, F2}}] }};

fire( ret_ok, #{ 'PostSync' := [{A, Ra, _Fa, []}] }, _WrkState ) ->
  {produce, #{ 'WorkerOk' => [{A, Ra}] }}.