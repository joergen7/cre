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
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( logic_worker ).
-behaviour( cre_worker ).


%%====================================================================
%% Exports
%%====================================================================

-export( [do_stagein/3, do_stageout/3, init/1, run/2, stagein_lst/2,
          stageout_lst/3, error_to_expr/3] ).
-export( [start_link/0, start_link/1, start_link/2, stop/1] ).


%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  {ok, CreName} = cre:pid(),
  start_link( CreName ).

start_link( CreName ) ->
  cre_worker:start_link( CreName, ?MODULE, [] ).

start_link( WrkName, CreName ) ->
  cre_worker:start_link( WrkName, CreName, ?MODULE, [] ).

stop( WrkName ) ->
  cre_worker:stop( WrkName ).

%%====================================================================
%% CRE worker callback functions
%%====================================================================

-spec do_stagein( A :: _, F :: _, UsrInfo :: _ ) -> ok | {error, enoent}.

do_stagein( _A, _F, _UsrInfo ) -> error( unused ).


-spec do_stageout( A :: _, F :: _, UsrInfo :: _ ) -> ok | {error, enoent}.

do_stageout( _A, _F, _UsrInfo ) -> error( unused ).


-spec init( WrkArg :: _ ) -> UsrInfo :: _.

init( _WrkArg ) -> [].


-spec run( A :: _, UsrInfo :: _ ) -> {ok, R :: _} | {error, Reason :: _}.

run( {'not', X}, _ )      -> {ok, not X};
run( {'and', X1, X2}, _ ) -> {ok, X1 andalso X2};
run( {'or', X1, X2}, _ )  -> {ok, X1 orelse X2}.


-spec stagein_lst( A :: _, UsrInfo :: _ ) -> [F :: _].

stagein_lst( _A, _UsrInfo ) -> [].


-spec stageout_lst( A :: _, R :: _, UsrInfo :: _ ) -> [F :: _].

stageout_lst( _A, _R, _UsrInfo ) -> [].


-spec error_to_expr( A, Reason, UsrInfo ) -> _
when A       :: _,
     Reason  :: {stagein | stageout, [_]} | {run, _},
     UsrInfo :: _.

error_to_expr( _A, _Reason, _UsrInfo ) -> error( unused ).