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

-module( default_client ).
-behaviour( cre_client ).


%%====================================================================
%% Exports
%%====================================================================

-export( [start_link/0, start_link/1, start_link/2, eval/2, stop/1] ).

-export( [init/1, is_value/2, step/2] ).


%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  {ok, CreName} = cre:pid(),
  start_link( CreName ).

start_link( CreName ) ->
  cre_client:start_link( CreName, ?MODULE, [] ).

start_link( ClientName, CreName ) ->
  cre_client:start_link( ClientName, CreName, ?MODULE, [] ).

eval( ClientName, T ) ->
  cre_client:eval( ClientName, T ).

stop( ClientName ) ->
  cre_client:stop( ClientName ).


%%====================================================================
%% CRE client callback functions
%%====================================================================

-spec init( InitArg :: _ ) -> UsrInfo :: _.

init( _InitArg ) -> [].


-spec is_value( E :: _, UsrInfo :: _ ) -> boolean().

is_value( _E, _UsrInfo ) -> true.


-spec step( {Q, C, E}, UsrInfo ) -> {ok, {Q1, C1, E1}} | norule
when Q       :: [_],
     C       :: [{_, _}],
     E       :: _,
     UsrInfo :: _,
     Q1      :: [_],
     C1      :: [{_, _}],
     E1      :: _.

step( _, _UsrInfo ) -> norule.