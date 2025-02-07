%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jörgen Brandt <joergen@cuneiform-lang.org>
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
%% @author Jörgen Brandt <joergen@cuneiform-lang.org>
%% @copyright 2015
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module(cre_worker).
-behavior(gen_pnet).

%%====================================================================
%% Exports
%%====================================================================

-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2,
         trigger/3]).

-export([place_lst/0,
         trsn_lst/0,
         init_marking/2,
         preset/1,
         is_enabled/3,
         fire/3]).

-export([start_link/3, start_link/4, worker_request/2, stop/1]).

%%====================================================================
%% Includes
%%====================================================================

%%====================================================================
%% Callback definitions
%%====================================================================


-callback init(InitArg :: _) -> UsrInfo :: _.

-callback prepare_case(A :: _, UsrInfo :: _) -> ok.

-callback stagein_lst(A :: _, UsrInfo :: _) -> [F :: _].

-callback do_stagein(A :: _, F :: _, UsrInfo :: _) -> ok | {error, enoent}.

-callback run(A :: _, UsrInfo :: _) -> {ok, R :: _} | {error, Reason :: _}.

-callback stageout_lst(A :: _, R :: _, UsrInfo :: _) -> [F :: _].

-callback do_stageout(A :: _, F :: _, UsrInfo :: _) -> ok | {error, enoent}.

-callback error_to_expr(A :: _,
                        Reason :: {stagein | stageout, [_]} | {run, _},
                        UsrInfo :: _) -> _.

-callback cleanup_case(A :: _, R :: _, UsrInfo :: _) -> R1 :: _.

%%====================================================================
%% Record definitions
%%====================================================================

-record(wrk_state, {cre_name, wrk_mod, usr_info}).

%%====================================================================
%% API functions
%%====================================================================


start_link(CreName, WrkMod, WrkArg) ->
    gen_pnet:start_link(?MODULE, {CreName, WrkMod, WrkArg}, []).


start_link(WrkName, CreName, WrkMod, WrkArg) ->
    gen_pnet:start_link(WrkName, ?MODULE, {CreName, WrkMod, WrkArg}, []).


worker_request(WrkName, A) ->
    gen_pnet:cast(WrkName, {worker_request, A}).


stop(WrkName) ->
    gen_pnet:stop(WrkName).


%%====================================================================
%% Interface callback functions
%%====================================================================


code_change(_OldVsn, NetState, _Extra) -> {ok, NetState}.


handle_call(_Request, _From, _NetState) -> {reply, {error, bad_msg}}.


handle_cast({worker_request, A}, NetState) ->

    WrkState = gen_pnet:get_usr_info(NetState),
    #wrk_state{wrk_mod = WrkMod, usr_info = UsrInfo} = WrkState,

    ok = WrkMod:prepare_case(A, UsrInfo),

    {noreply, #{'WorkerRequest' => [A]}};

handle_cast(_Request, _NetState) -> noreply.


handle_info(_Request, _NetState) -> noreply.


init({CreName, WrkMod, WrkArg}) ->

    UsrInfo = WrkMod:init(WrkArg),

    WrkState = #wrk_state{
                 cre_name = CreName,
                 wrk_mod = WrkMod,
                 usr_info = UsrInfo
                },

    ok = cre_master:add_worker(CreName, self()),

    WrkState.


terminate(_Reason, _NetState) -> ok.


trigger('WorkerResult', {A, R}, NetState) ->

    WrkState = gen_pnet:get_usr_info(NetState),
    #wrk_state{
      cre_name = CreName,
      wrk_mod = WrkMod,
      usr_info = UsrInfo
     } = WrkState,

    R1 = WrkMod:cleanup_case(A, R, UsrInfo),
    cre_master:worker_result(CreName, self(), A, R1),

    drop;

trigger(_Place, _Token, _NetState) -> pass.


%%====================================================================
%% Petri net callback functions
%%====================================================================


place_lst() ->
    ['WorkerRequest', 'Stagein', 'StageinOk', 'StageinError', 'PreSync', 'Result',
     'Stageout', 'StageoutOk', 'StageoutError', 'PostSync', 'Error',
     'WorkerResult'].


trsn_lst() ->
    [prep_stagein, do_stagein, sync_inok, sync_inerror, run, prep_stageout,
     do_stageout, sync_outok, sync_outerror, ret_posterror, ret_preerror,
     return_ok, return_error].


init_marking(_Place, _UsrInfo) -> [].


preset(prep_stagein) -> ['WorkerRequest'];
preset(do_stagein) -> ['Stagein'];
preset(sync_inok) -> ['StageinOk', 'PreSync'];
preset(sync_inerror) -> ['StageinError', 'PreSync'];
preset(run) -> ['PreSync'];
preset(prep_stageout) -> ['Result'];
preset(do_stageout) -> ['Stageout'];
preset(sync_outok) -> ['StageoutOk', 'PostSync'];
preset(sync_outerror) -> ['StageoutError', 'PostSync'];
preset(ret_preerror) -> ['PreSync'];
preset(ret_posterror) -> ['PostSync'];
preset(return_ok) -> ['PostSync'];
preset(return_error) -> ['Error'].


is_enabled(prep_stagein, _, _) -> true;
is_enabled(do_stagein, _, _) -> true;
is_enabled(sync_inok, _, _) -> true;
is_enabled(sync_inerror, _, _) -> true;
is_enabled(prep_stageout, _, _) -> true;
is_enabled(do_stageout, _, _) -> true;
is_enabled(sync_outok, _, _) -> true;
is_enabled(sync_outerror, _, _) -> true;
is_enabled(return_error, _, _) -> true;

is_enabled(ret_preerror,
           #{'PreSync' := [{A, F1, F2}]},
           #wrk_state{
             wrk_mod = WrkMod,
             usr_info = UsrInfo
            })
  when length(F2) > 0 ->
    F1uF2 = ordsets:union(F1, F2),
    Fa = ordsets:from_list(WrkMod:stagein_lst(A, UsrInfo)),
    Fa =:= F1uF2;

is_enabled(run,
           #{'PreSync' := [{A, F1, []}]},
           #wrk_state{wrk_mod = WrkMod, usr_info = UsrInfo}) ->
    Fa = ordsets:from_list(WrkMod:stagein_lst(A, UsrInfo)),
    Fa =:= F1;

is_enabled(ret_posterror,
           #{'PostSync' := [{A, Ra, F1, F2}]},
           #wrk_state{
             wrk_mod = WrkMod,
             usr_info = UsrInfo
            })
  when length(F2) > 0 ->
    F1uF2 = ordsets:union(F1, F2),
    Fa = ordsets:from_list(WrkMod:stageout_lst(A, Ra, UsrInfo)),
    Fa =:= F1uF2;

is_enabled(return_ok,
           #{'PostSync' := [{A, Ra, F1, []}]},
           #wrk_state{wrk_mod = WrkMod, usr_info = UsrInfo}) ->
    Fa = ordsets:from_list(WrkMod:stageout_lst(A, Ra, UsrInfo)),
    Fa =:= F1;

is_enabled(_, _, _) -> false.


fire(prep_stagein,
     #{'WorkerRequest' := [A]},
     #wrk_state{wrk_mod = WrkMod, usr_info = UsrInfo}) ->
    Fa = ordsets:from_list(WrkMod:stagein_lst(A, UsrInfo)),
    {produce, #{'Stagein' => [ {A, F} || F <- Fa ], 'PreSync' => [{A, [], []}]}};

fire(do_stagein,
     #{'Stagein' := [{A, F}]},
     #wrk_state{wrk_mod = WrkMod, usr_info = UsrInfo}) ->
    case WrkMod:do_stagein(A, F, UsrInfo) of
        ok -> {produce, #{'StageinOk' => [{A, F}]}};
        {error, enoent} -> {produce, #{'StageinError' => [{A, F}]}}
    end;

fire(sync_inok,
     #{'StageinOk' := [{A, F}], 'PreSync' := [{A, F1, F2}]},
     _WrkState) ->
    {produce, #{'PreSync' => [{A, ordsets:add_element(F, F1), F2}]}};

fire(sync_inerror,
     #{'StageinError' := [{A, F}], 'PreSync' := [{A, F1, F2}]},
     _WrkState) ->
    {produce, #{'PreSync' => [{A, F1, ordsets:add_element(F, F2)}]}};

fire(ret_preerror, #{'PreSync' := [{A, _F1, F2}]}, _WrkState) ->
    {produce, #{'Error' => [{A, {stagein, F2}}]}};

fire(run,
     #{'PreSync' := [{A, _Fa, []}]},
     #wrk_state{wrk_mod = WrkMod, usr_info = UsrInfo}) ->
    case WrkMod:run(A, UsrInfo) of
        {ok, Ra} -> {produce, #{'Result' => [{A, Ra}]}};
        {error, Reason} -> {produce, #{'Error' => [{A, {run, Reason}}]}}
    end;

fire(prep_stageout,
     #{'Result' := [{A, Ra}]},
     #wrk_state{wrk_mod = WrkMod, usr_info = UsrInfo}) ->
    Fra = ordsets:from_list(WrkMod:stageout_lst(A, Ra, UsrInfo)),
    {produce, #{
                'Stageout' => [ {A, F} || F <- Fra ],
                'PostSync' => [{A, Ra, [], []}]
               }};

fire(do_stageout,
     #{'Stageout' := [{A, F}]},
     #wrk_state{wrk_mod = WrkMod, usr_info = UsrInfo}) ->
    case WrkMod:do_stageout(A, F, UsrInfo) of
        ok -> {produce, #{'StageoutOk' => [{A, F}]}};
        {error, enoent} -> {produce, #{'StageoutError' => [{A, F}]}}
    end;

fire(sync_outok,
     #{
       'StageoutOk' := [{A, F}],
       'PostSync' := [{A, Ra, F1, F2}]
      },
     _WrkState) ->
    {produce, #{'PostSync' => [{A, Ra, ordsets:add_element(F, F1), F2}]}};

fire(sync_outerror,
     #{
       'StageoutError' := [{A, F}],
       'PostSync' := [{A, Ra, F1, F2}]
      },
     _WrkState) ->
    {produce, #{'PostSync' => [{A, Ra, F1, ordsets:add_element(F, F2)}]}};

fire(ret_posterror, #{'PostSync' := [{A, _Ra, _F1, F2}]}, _WrkState) ->
    {produce, #{'Error' => [{A, {stageout, F2}}]}};

fire(return_ok, #{'PostSync' := [{A, Ra, _Fa, []}]}, _WrkState) ->
    {produce, #{'WorkerResult' => [{A, Ra}]}};

fire(return_error,
     #{'Error' := [{A, E}]},
     #wrk_state{wrk_mod = WrkMod, usr_info = UsrInfo}) ->
    Ra = WrkMod:error_to_expr(A, E, UsrInfo),
    {produce, #{'WorkerResult' => [{A, Ra}]}}.
