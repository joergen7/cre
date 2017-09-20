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
%% @version 0.1.1
%% @copyright 2015-2017 Jörgen Brandt
%%
%%
%%
%%
%%
%% @end
%% --------------------------------------------------------------------

-module( logic_client ).
-behaviour( cre_client ).


%%====================================================================
%% Exports
%%====================================================================

-export( [init/1, is_value/2, step/2] ).
-export( [start_link/0, start_link/1, start_link/2, eval/2, stop/1] ).

%%====================================================================
%% Language definition
%%====================================================================

-type e() :: boolean()
           | {'not', e()}
           | {'and', e(), e()}
           | {'or', e(), e()}
           | {fut, e()}.

-type ctx() :: hole
             | {'not', ctx()}
             | {'and', ctx(), e()}
             | {'and', e(), ctx()}
             | {'or', ctx(), e()}
             | {'or', e(), ctx()}.

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
%% CRE worker callback functions
%%====================================================================

-spec init( Arg :: _ ) -> UsrInfo :: _.

init( _InitArg ) -> [].


-spec is_value( E :: e(), UsrInfo :: _ ) -> boolean().

is_value( E, _UsrInfo ) -> is_boolean( E ).


-spec step( {Q, C, E}, UsrInfo ) -> {ok, {Q1, C1, E1}} | norule
when Q       :: [_],
     C       :: [{_, _}],
     E       :: _,
     UsrInfo :: _,
     Q1      :: [_],
     C1      :: [{_, _}],
     E1      :: _.

step( {Q, [], E}, _UsrInfo ) ->
  case find_context( E ) of
  	{ok, {Ctx, TNext}}   -> {ok, {[TNext|Q], [], in_hole( Ctx, {fut, TNext} )}};
  	{error, nocontext} -> norule
  end;

step( {Q, [{A, Delta}|C1], E}, _UsrInfo ) ->
  {ok, {Q, C1, subst_fut( E, A, Delta )}}.


%%====================================================================
%% Internal functions
%%====================================================================

in_hole( hole, T )            -> T;
in_hole( {'not', E}, T )      -> {'not', in_hole( E, T )};
in_hole( {'and', E1, E2}, T ) -> {'and', in_hole( E1, T ), in_hole( E2, T )};
in_hole( {'or', E1, E2}, T )  -> {'or', in_hole( E1, T ), in_hole( E2, T )};
in_hole( T, _ )               -> T.


find_context( T ) ->
  case find_context( T, hole ) of
  	[]    -> {error, nocontext};
  	[H|_] -> {ok, H}
  end.

find_context( T, _E ) when is_boolean( T ) -> [];
find_context( {fut, _T}, _E )              -> [];

find_context( {'not', T}, E ) when is_boolean( T ) ->
  [{E, {'not', T}}];

find_context( {'not', T}, E ) ->
  find_context( T, in_hole( E, {'not', hole} ) );

find_context( {Op, T1, T2}, E ) when is_boolean( T1 ), is_boolean( T2 ) ->
  [{E, {Op, T1, T2}}];

find_context( {Op, T1, T2}, E ) ->
  find_context( T1, in_hole( E, {Op, hole, T2} ) )++
  find_context( T2, in_hole( E, {Op, T1, hole} ) ).


subst_fut( {'not', T}, A, V ) ->
  {'not', subst_fut( T, A, V )};

subst_fut( {'and', T1, T2}, A, V ) ->
  {'and', subst_fut( T1, A, V ), subst_fut( T2, A, V )};

subst_fut( {'or', T1, T2}, A, V ) ->
  {'or', subst_fut( T1, A, V ), subst_fut( T2, A, V )};

subst_fut( {fut, A}, A, V ) -> V;
subst_fut( T, _A, _V )      -> T.
