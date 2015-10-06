% The Cuneiform Runtime Environment is an interpreter of the functional
% programming language Cuneiform.
%
% Copyright 2013-2015 Jörgen Brandt, Marc Bux, and Ulf Leser
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.



-module( cuneiform_csem ).
-export( [eval/5, is_final_list/1] ).

-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).
-endif.

-import( lists, [flatmap/2, flatten/1, foldl/3, nth/2, reverse/1, usort/1, zipwith/3] ).
-import( maps, [get/2, get/3, is_key/2, merge/2, put/3, remove/2, values/1] ).



%% eval/5
%
eval( CompoundExpr, Rho, CreateTicket, Global, Fin )
when is_list( CompoundExpr ),
     is_map( Rho ),
     is_function( CreateTicket ),
     is_map( Global ),
     is_map( Fin ) ->
     
  Y = step( CompoundExpr, Rho, CreateTicket, Global, Fin ),
  
  case Y =:= CompoundExpr of
    true -> Y;
    false -> eval( Y, Rho, CreateTicket, Global, Fin )
  end.

  
%% step/5
%
step( CompoundExpr, Rho, CreateTicket, Global, Fin ) ->
  flatmap( fun( E ) -> step_single( E, Rho, CreateTicket, Global, Fin ) end, CompoundExpr ).

%% step_single/5
%
step_single( Str={str, _, _}, _Rho, _CreateTicket, _Global, _Fin ) -> [Str];

step_single( {var, Line, Id}, Rho, _CreateTicket, Global, _Fin ) ->
     
  case is_key( Id, Global ) of
  
    true ->
      get( Id, Global );
    
    false ->
      case is_key( Id, Rho ) of 
        true -> get( Id, Rho );
        false -> error( {sem, unbound_var, var, Line} )
      end
  end;
  
step_single( Lam={lam, _, _, _, _}, _, _, _, _ ) -> [Lam];

step_single( Select={select, _Line, Channel, Ticket={ticket, _, _, _, _}}, _Rho, _CreateTicket, _Global, Fin ) ->
  get( {Channel, Ticket}, Fin, [Select] );

step_single( {cnd, Loc, Condition, ThenExpr, ElseExpr}, Rho, CreateTicket, Global, Fin ) ->
  
  C = step( Condition, Rho, CreateTicket, Global, Fin ),
  
  case C of
    [] -> ElseExpr;
    [_|_] ->
      case is_final_list( C ) of
        true -> ThenExpr;
        false -> [{cnd, Loc, C, ThenExpr, ElseExpr}]
      end
  end;
  
step_single( {app, Line, _, [], _}, _, _, _, _ ) -> error( {sem, nil_tasklist, app, Line} );
  
step_single( {app, AppLoc, Channel, LamList, Binding}, Rho, CreateTicket, Global, Fin ) ->


  Binding1 = maps:map( fun( _, V ) ->
                         step( V, Rho, CreateTicket, Global, Fin )
                       end,
                       Binding ),
  LamList1 = step( LamList, Rho, CreateTicket, Global, Fin ),
     
  case is_enumerable_list( LamList ) of
  
    false ->                               % step lambda list
      [{app, AppLoc, Channel, LamList1, Binding1}];
      
    true ->                                % lambda compound expression is final

      case is_enumerable_map( Binding ) of
  
        false ->                           % step binding map
        
          [{app, AppLoc, Channel, LamList, Binding1}];
      
        true ->                            % binding map is final     
              
          [Lam|_] = LamList,
          {lam, LamLoc, LamName, Sign, Body} = Lam,
          {sign, OutList, CorrelList, InList} = Sign,
          
          case CorrelList of
  
            [_|_] ->                       % task correlation must be resolved
            
              EnumList = uncorr( CorrelList, Binding, Binding, [] ),
              InList1 = [{param, Name, false} || Name <- CorrelList]++InList,
              zipwith(
                fun( {lam, L, N, _, B}, Enum ) ->
                  L1 = {lam, L, N, {sign, OutList, [], InList1}, B},
                  {app, AppLoc, Channel, [L1], Enum}
                end,
                LamList, EnumList );

            [] ->                          % no task correlation
            
              case length( LamList ) > 1 of
              
                true ->                    % resolve multiple lambdas
                  [{app, AppLoc, Channel, [L], Binding} || L <- LamList];
                    
                false ->                   % single lambda

                  {Sign1, EnumList} = step_enum( Sign, Binding, CreateTicket, Global, Fin ),
                  
                  case length( EnumList ) > 1 orelse Sign1 =/= Sign of
                    
                    true ->                % step_enum could enumerate or alter signature                
                      [{app, AppLoc, Channel, [{lam, LamLoc, LamName, Sign1, Body}], E} || E <- EnumList];
                        
                    false ->               % step_enum could neither enumerate
                                           % nor alter signature
                      
                      case is_final_single( Lam ) of
                      
                        false ->
                          [{app, AppLoc, Channel, LamList1, Binding1}];
                          
                        true ->
                        
                          case is_final_map( Binding ) of
                          
                            false ->
                              [{app, AppLoc, Channel, LamList1, Binding1}];
                              
                            true ->
                              step_app( AppLoc, Channel, Lam, Binding, CreateTicket, Global, Fin )
                              
                          end
                      end
                  end                  
              end
          end          
      end    
  end.


%% step_app/7
%
step_app( AppLoc, Channel, {lam, _Loc, _Name, Sign, ForBody={forbody, _, _}}, Binding, CreateTicket, _Global, _Fin ) ->
  [{select, AppLoc, Channel, apply( CreateTicket, [AppLoc, Sign, ForBody, Binding] )}];
  
step_app( AppLoc,
          Channel,
          {lam, LamLoc, LamName, Sign={sign, OutvarList, _, _}, {natbody, BodyMap}},
          Enum,
          CreateTicket,
          Global, Fin ) ->
     
  {param, {name, OutvarName, _}, _} = nth( Channel, OutvarList ),
  Ctx = merge( BodyMap, Enum ), 
  Expr = get( OutvarName, Ctx ),
  X = step( Expr, Ctx, CreateTicket, Global, Fin ),
  case is_final_list( X ) of
    true -> X;
    false ->
      BodyMap1 = put( OutvarName, X, BodyMap ),
      Lam1 = [{lam, LamLoc, LamName, Sign, {natbody, BodyMap1}}],
      [{app, AppLoc, Channel, Lam1, Enum}]
  end.
  
%% step_enum/5
%
step_enum( Sign={sign, _, [], InList}, Binding, CreateTicket, Global, Fin ) ->
  step_enum( Sign, Binding, CreateTicket, Global, Fin, InList, [] ).

  
%% step_enum/7
%
step_enum( Sign, Binding, _CreateTicket, _Global, _Fin, [], _Acc ) ->
  {Sign, [Binding]};

step_enum( Sign, Binding, CreateTicket, Global, Fin, [Param={param, {name, ParamName, _}, false}|R], Acc ) ->

  Value = get( ParamName, Binding ),
  case length( Value ) > 1 of
  
    true ->
      {Sign, [put( ParamName, [V], Binding ) || V <- Value]};

    false ->
      step_enum( Sign, Binding, CreateTicket, Global, Fin, R, [Param|Acc] )
          
     
  end;

step_enum( Sign, Binding, CreateTicket, Global, Fin, [Param={param, {name, _, _}, true}|R], Acc ) ->
  step_enum( Sign, Binding, CreateTicket, Global, Fin, R, [Param|Acc] );
      
  
step_enum( {sign, OutList, [], _}, Binding, _, _, _, [{correl, NameList=[_,_|_]}|R], Acc ) ->

  InList1 = reverse( Acc )++[{param, Name, false} || Name <- NameList]++R,
  Sign1 = {sign, OutList, [], InList1},
  {Sign1, uncorr( NameList, Binding, Binding, [] )};
  
step_enum( {sign, OutList, [], _}, Binding, _, _, _, [{comb, cnr, {name, ParamName, IsFile}, AliasList=[_,_|_]}|R], Acc ) ->

  InList1 = reverse( Acc )++[{param, {name, Alias, IsFile}, false} || Alias <- AliasList]++R,
  Sign1 = {sign, OutList, [], InList1},
  {Sign1, uncnr( ParamName, AliasList, Binding, Binding )}.
  
  
  
  
%% is_enumerable_list/1
%
is_enumerable_list( [] ) -> true;

is_enumerable_list( [H|T] ) ->
  is_enumerable_single( H ) andalso is_enumerable_list( T ).
  
%% is_enumerable_single/1
%
is_enumerable_single( {str, _, _} ) -> true;
is_enumerable_single( {lam, _, _, _, _} ) -> true;
is_enumerable_single( {var, _, _} ) -> false;
is_enumerable_single( {app, _, _, _, _} ) -> false;
is_enumerable_single( {cnd, _, _, _, _} ) -> false;

is_enumerable_single( {select, _, Channel, {ticket, _, {sign, OutList, _, _}, _, _}} ) ->

  {param, _, IsList} = nth( Channel, OutList ),
  not IsList.
  
%% is_enumerable_map/1
%
is_enumerable_map( Binding ) ->
  is_enumerable_list( flatten( values( Binding ) ) ).
  
  
  
%% is_final_list/1
%
is_final_list( [] ) -> true;

is_final_list( [H|T] ) when is_tuple( H ) ->
  is_final_single( H ) andalso is_final_list( T ).

%% is_final_map/1
%
is_final_map( Binding ) when is_map( Binding ) ->
  is_final_list( flatten( values( Binding ) ) ).
  


%% is_final_single/1
%
is_final_single( {str, _, _} ) -> true;
is_final_single( {lam, _, _, _, _} ) -> true;
is_final_single( {var, _, _} ) -> false;
is_final_single( {cnd, _, _, _, _} ) -> false;
is_final_single( {select, _, _, _} ) -> false;
is_final_single( {app, _, _, _, _} ) -> false.



%% uncnr/4
%
uncnr( ParamName, AliasList, C, BindingMap ) ->
  N = length( AliasList ),
  Ce = get( ParamName, BindingMap ),
  CombList = comb_no_replace( N, Ce ),
  BindingMap0 = remove( ParamName, BindingMap ),
  [for_comb( Comb, AliasList, C, BindingMap0 ) || Comb <- CombList].


%% uncorr/4
%
% @spec uncorr( Correl::list(), C::map(), BindingMap::map(), Accum::list() ) ->
%         list()
%
% @doc Resolves correlated parameter set. Returns a list of singular bindings
%      derived from the original binding map.
%
%      Correl is a list containing elements of type param(). C is the
%      preliminary Binding, which is to be extended. BindingMap is the original
%      binding map. Accum is an internal accumulator which should be set to the
%      empty list [].
%
uncorr( Correl=[{name, ParamName, _}|_], C, BindingMap, Accum )
when is_list( ParamName ),
     is_map( C ),
     is_map( BindingMap ),
     is_list( Accum ) ->
  
  First = get( ParamName, BindingMap ),
  case First of
  
    [] ->
      Accum;
      
    [_|_] ->

      {C1, BindingMap1} = foldl( fun poplast/2, {C, BindingMap}, Correl ),
      uncorr( Correl, C, BindingMap1, [C1|Accum] )
  end.

%% comb_no_replace/2
%
% @spec comb_no_replace( N::pos_integer(), List::list() ) -> list()
%
comb_no_replace( N, List )
when is_integer( N ), N > 0,
     is_list( List ), length( List ) >= N ->
  cnr( N, [], List ).



  
  
%% cnr/3
%
cnr( 0, Acc=[_|_], _ )
when is_list( Acc ) ->
  [reverse( Acc )];

cnr( N, _, List )
when is_integer( N ), N > length( List ) ->
  [];

cnr( N, Acc, [H|T] )
when is_integer( N ), N > 0,
     is_list( Acc ) ->
  A = cnr( N-1, [H|Acc], T ),
  B = cnr( N, Acc, T ),
  A++B.
  
%% for_comb/4
%
for_comb( [], [], SingBinding, _ ) ->
  SingBinding;

for_comb( _Comb=[Ch|Ct], _AliasList=[Ah|At], SingBinding, BindingMap ) ->
  SingBinding1 = put( Ah, [Ch], SingBinding ),
  for_comb( Ct, At, SingBinding1, BindingMap ).
  
  
  
%% poplast/2
%
% @spec poplast( Param::param(), MapPair::{LastMap::map(), RestMap::map()} ) ->
%         {LastMap1::map(), RestMap1::map()}
%
% @doc Swaps the last element of a bound value in one map and appends it in
%      another.
%
poplast( {name, N, _}, {LastMap, RestMap} ) ->
  [H|T] = reverse( get( N, RestMap ) ),
  {put( N, [H], LastMap ), put( N, T, RestMap )}.
  
  
  
  
  
-ifdef( TEST ).


-define( CREATE_TICKET, fun( Line, Sign, ForBody, Binding ) -> {ticket, Line, Sign, ForBody, Binding} end ).


setup_ticketsrc() -> {fun cuneiform_csem:eval/5, ?CREATE_TICKET}.

teardown_ticketsrc( E ) -> {ok, E}.


eval_test_() ->
  {foreach, fun setup_ticketsrc/0,
            fun teardown_ticketsrc/1,
            [ 
      fun nil_should_eval_itself/1
     ,fun str_should_eval_itself/1
     ,fun lam_should_eval_itself/1
     ,fun undef_var_should_fail/1
     ,fun var_def_in_rho_should_eval_to_bound_value/1
     ,fun var_def_in_global_should_eval_to_bound_value/1
     ,fun global_should_bind_closer_than_rho/1
     ,fun def_var_should_cascade_binding/1
     ,fun def_var_should_cascade_binding_twice/1
     ,fun unfinished_ticket_should_eval_to_itself/1
     ,fun finished_ticket_should_eval_to_value/1
     ,fun identity_fn_should_eval_arg/1
     ,fun multiple_output_should_be_bindable/1
     ,fun app_should_ignore_calling_context/1
     ,fun app_should_hand_down_global_context/1
     ,fun binding_should_override_body/1
     ,fun app_with_empty_task_list_should_fail/1
     ,fun cross_product_should_be_derivable_from_signature/1
     ,fun dot_product_should_be_derivable_from_signature/1
     ,fun aggregate_should_consume_list_as_whole/1
     ,fun task_correlation_should_work/1
     ,fun cnd_false_should_eval_else_expr/1
     ,fun cnd_evaluates_condition_before_decision1/1
     ,fun cnd_evaluates_condition_before_decision2/1
     ,fun cnd_evaluates_only_on_final_condition/1
     ,fun cnd_evaluates_then_expr/1
     ,fun cnd_evaluates_else_expr/1
     ,fun foreign_app_with_cnd_param_is_left_untouched/1
     ,fun foreign_app_with_select_param_is_left_untouched/1
     ,fun comb_param_works/1
     ,fun cascading_app_does_not_break_enum/1
     ,fun app_task_param_is_evaluated/1
     ,fun app_non_final_result_preserves_app/1
     ,fun app_non_final_result_preserves_app_with_new_lam/1
     ,fun nested_app_undergoes_reduction/1
     ,fun lam_can_be_result_of_cnd/1
     ,fun deferring_lam_does_not_defer_param_evaluation/1
     ,fun recursion_terminates/1
     ,fun app_select_param_is_enumerated/1
    ]}.

nil_should_eval_itself( {Eval, CreateTicket} ) ->
  ?_assertEqual( [], apply( Eval, [[], #{}, CreateTicket, #{}, #{}] ) ).

str_should_eval_itself( {Eval, CreateTicket} ) ->
  E = [{str, 10, "bla"}],
  ?_assertEqual( E, apply( Eval, [E, #{}, CreateTicket, #{}, #{}] ) ).
  
lam_should_eval_itself( {Eval, CreateTicket} ) ->
  Sign = {sign, [{param, {name, "out", false}, false}], [], []},
  Body = {natbody, #{"out" => [{str, "blub"}]}},
  E = [{lam, 10, "f", Sign, Body}],
  ?_assertEqual( E, apply( Eval, [E, #{}, CreateTicket, #{}, #{}] ) ).
  
undef_var_should_fail( {Eval, CreateTicket} ) ->
  E = [{var, 10, "x"}],
  ?_assertError( {sem, unbound_var, var, 10}, apply( Eval, [E, #{}, CreateTicket, #{}, #{}] ) ).
  
var_def_in_rho_should_eval_to_bound_value( {Eval, CreateTicket} ) ->
  E = [{str, 10, "blub"}],
  X = apply( Eval, [[{var, 10, "x"}], #{"x" => E}, CreateTicket, #{}, #{}] ),
  ?_assertEqual( E, X ).
  
var_def_in_global_should_eval_to_bound_value( {Eval, CreateTicket} ) ->
  F = [{lam, 10, "f", {sign, [{param, {name, "out", false}, false}], [], []}, {forbody, bash, "blub"}}],
  X = apply( Eval, [[{var, 10, "f"}], #{}, CreateTicket, #{"f" => F}, #{}] ),
  ?_assertEqual( F, X ).
  
global_should_bind_closer_than_rho( {Eval, CreateTicket} ) ->
  F1 = [{lam, 10, "f", {sign, [{param, {name, "out", false}, false}], [], []}, {forbody, bash, "blub"}}],
  F2 = [{lam, 11, "f", {sign, [{param, {name, "out", false}, false}], [], []}, {forbody, bash, "bla"}}],
  X = apply( Eval, [[{var, 12, "f"}], #{"f" => F1}, CreateTicket, #{"f" => F2}, #{}] ),
  ?_assertEqual( F2, X ).

def_var_should_cascade_binding( {Eval, CreateTicket} ) ->
  E = [{str, 10, "blub"}],
  X = apply( Eval,
             [[{var, 11, "x"}], #{"x" => [{var, 12, "y"}], "y" => E},
              CreateTicket, #{}, #{}] ),
  ?_assertEqual( E, X ).
  
def_var_should_cascade_binding_twice( {Eval, CreateTicket} ) ->
  A = [{str, 10, "A"}],
  Rho = #{"x" => [{var, 11, "y"}], "y" => [{var, 12, "z"}], "z" => A},
  ?_assertEqual( A, apply( Eval, [[{var, 13, "x"}], Rho, CreateTicket, #{}, #{}] ) ).

unfinished_ticket_should_eval_to_itself( {Eval, CreateTicket} ) ->
  Sign = {sign, [{param, {name, "out", false}, false}], [], []},
  Ticket = {ticket, 10, Sign, {forbody, bash, "blub"}, #{}},
  E = [{select, 10, 1, Ticket}],
  X = apply( Eval, [E, #{}, CreateTicket, #{}, #{}] ),
  ?_assertEqual( E, X ).
  
finished_ticket_should_eval_to_value( {Eval, CreateTicket} ) ->
  Sign = {sign, [{param, {name, "out", false}, false}], [], []},
  Ticket = {ticket, 10, Sign, {forbody, bash, "blub"}, #{}},
  E = [{select, 10, 1, Ticket}],
  F = [{str, 10, "blub"}],
  X = apply( Eval, [E, #{}, CreateTicket, #{}, #{{1, Ticket} => F}] ),
  ?_assertEqual( F, X ).
    
identity_fn_should_eval_arg( {Eval, CreateTicket} ) ->
  E = [{str, 10, "bla"}],
  Sign = {sign, [{param, {name, "out", false}, false}],
                [], [{param, {name, "inp", false}, false}]},
  Body = {natbody, #{"out" => [{var, 11, "inp"}]}},
  LamList = [{lam, 12, "f", Sign, Body}],
  F = [{app, 13, 1, LamList, #{"inp" => E}}],
  ?_assertEqual( E, apply( Eval, [F, #{}, CreateTicket, #{}, #{}] ) ).
  
multiple_output_should_be_bindable( {Eval, CreateTicket} ) ->
  Sign = {sign, [{param, {name, "out1", false}, false},
                 {param, {name, "out2", false}, false}],
                [], []},
  E1 = [{str, 10, "bla"}],
  E2 = [{str, 11, "blub"}],
  Body = {natbody, #{"out1" => E1, "out2" => E2}},
  LamList = [{lam, 12, "f", Sign, Body}],
  F1 = [{app, 13, 1, LamList, #{}}],
  F2 = [{app, 14, 2, LamList, #{}}],
  [?_assertEqual( E1, apply( Eval, [F1, #{}, CreateTicket, #{}, #{}] ) ),
   ?_assertEqual( E2, apply( Eval, [F2, #{}, CreateTicket, #{}, #{}] ) )].

app_should_ignore_calling_context( {Eval, CreateTicket} ) ->
  Sign = {sign, [{param, {name, "out", false}, false}], [], []},
  Body = {natbody, #{"out" => [{var, 10, "x"}]}},
  LamList = [{lam, 11, "f", Sign, Body}],
  App = [{app, 12, 1, LamList, #{}}],
  CallCtx = #{"x" => [{str, 13, "blub"}]},
  ?_assertError( {sem, unbound_var, var, 10}, apply( Eval, [App, CallCtx, CreateTicket, #{}, #{}] ) ).

app_should_hand_down_global_context( {Eval, CreateTicket} ) ->
  Sign = {sign, [{param, {name, "out", false}, false}], [], []},
  Body = {natbody, #{"out" => [{var, 10, "x"}]}},
  LamList = [{lam, 11, "f", Sign, Body}],
  App = [{app, 12, 1, LamList, #{}}],
  X = [{lam, 13, "x", {sign, [{param, {name, "out", false}, false}], [], []}, {forbody, bash, "blub"}}],
  Global = #{"x" => X},
  ?_assertEqual( X, apply( Eval, [App, #{}, CreateTicket, Global, #{}] ) ).

binding_should_override_body( {Eval, CreateTicket} ) ->
  F = [{str, 10, "blub"}],
  Sign = {sign, [{param, {name, "out", false}, false}],
                [], [{param, {name, "x", false}, false}]},
  Body = {natbody, #{"x" => [{str, "bla"}], "out" => [{var, 11, "x"}]}},
  Lam = [{lam, 12, "f", Sign, Body}],
  App = [{app, 13, 1, Lam, #{"x" => F}}],
  ?_assertEqual( F, apply( Eval, [App, #{}, CreateTicket, #{}, #{}] ) ).

app_with_empty_task_list_should_fail( {Eval, CreateTicket} ) ->
  App = [{app, 10, 1, [], #{}}],
  ?_assertError( {sem, nil_tasklist, app, 10}, apply( Eval, [App, #{}, CreateTicket, #{}, #{}] ) ).

cross_product_should_be_derivable_from_signature( {Eval, CreateTicket} ) ->
  Sign = {sign, [{param, {name, "out1", false}, false},
                 {param, {name, "out2", false}, false}],
                [], [{param, {name, "p1", false}, false},
                     {param, {name, "p2", false}, false}]},
  E1 = [{str, 10, "A"}, {str, 11, "B"}],
  E2 = [{str, 12, "1"}, {str, 13, "2"}],
  Body = {natbody, #{"out1" => [{var, 14, "p1"}], "out2" => [{var, 15, "p2"}]}},
  Lam = [{lam, 16, "f", Sign, Body}],
  Binding = #{"p1" => E1, "p2" => E2},
  App1 = [{app, 17, 1, Lam, Binding}],
  App2 = [{app, 18, 2, Lam, Binding}],
  F1 = [{str, 10, "A"}, {str, 10, "A"}, {str, 11, "B"}, {str, 11, "B"}],
  F2 = [{str, 12, "1"}, {str, 13, "2"}, {str, 12, "1"}, {str, 13, "2"}],
  [?_assertEqual( F1, apply( Eval, [App1, #{}, CreateTicket, #{}, #{}] ) ),
   ?_assertEqual( F2, apply( Eval, [App2, #{}, CreateTicket, #{}, #{}] ) )].
  
dot_product_should_be_derivable_from_signature( {Eval, CreateTicket} ) ->
  Sign = {sign, [{param, {name, "out1", false}, false},
                 {param, {name, "out2", false}, false}],
                [], [{correl, [{name, "p1", false},
                               {name, "p2", false}]}]},
  E1 = [{str, 10, "A"}, {str, 11, "B"}],
  E2 = [{str, 12, "1"}, {str, 13, "2"}],
  Body = {natbody, #{"out1" => [{var, 14, "p1"}], "out2" => [{var, 15, "p2"}]}},
  Lam = [{lam, 16, "f", Sign, Body}],
  Binding = #{"p1" => E1, "p2" => E2},
  App1 = [{app, 17, 1, Lam, Binding}],
  App2 = [{app, 18, 2, Lam, Binding}],
  [?_assertEqual( E1, apply( Eval, [App1, #{}, CreateTicket, #{}, #{}] ) ),
   ?_assertEqual( E2, apply( Eval, [App2, #{}, CreateTicket, #{}, #{}] ) )].

aggregate_should_consume_list_as_whole( {Eval, CreateTicket} ) ->
  Sign = {sign, [{param, {name, "out", false}, true}],
                [], [{param, {name, "inp", false}, true}]},
  E1 = [{str, 10, "A"}],
  E2 = [{str, 11, "B"}, {str, 12, "C"}],
  Body = {natbody, #{"out" => E1++[{var, 13, "inp"}]}},
  Lam = [{lam, 14, "f", Sign, Body}],
  Binding = #{"inp" => E2},
  App = [{app, 15, 1, Lam, Binding}],
  ?_assertEqual( E1++E2, apply( Eval, [App, #{}, CreateTicket, #{}, #{}] ) ).

task_correlation_should_work( {Eval, CreateTicket} ) ->
  Sign = {sign, [{param, {name, "out", false}, false}],
                [{name, "c", false}],
                [{param, {name, "p", false}, false}]},
  Body1 = {natbody, #{"out" => [{var, 10, "c"}, {var, 11, "p"}]}},
  Body2 = {natbody, #{"out" => [{var, 12, "p"}, {var, 13, "c"}]}},
  Lam = [{lam, 14, "f", Sign, Body1}, {lam, 15, "g", Sign, Body2}],
  Binding = #{"c" => [{str, 16, "A"}, {str, 17, "B"}], "p" => [{str, 18, "1"}, {str, 19, "2"}]},
  App = [{app, 20, 1, Lam, Binding}],
  ?_assertEqual( [{str, 16, "A"}, {str, 18, "1"}, {str, 16, "A"}, {str, 19, "2"},
                  {str, 18, "1"}, {str, 17, "B"}, {str, 19, "2"}, {str, 17, "B"}],
                 apply( Eval, [App, #{}, CreateTicket, #{}, #{}] ) ).

cnd_false_should_eval_else_expr( {Eval, CreateTicket} ) ->
  E = [{cnd, 10, [], [{str, 11, "A"}], [{str, 12, "B"}]}],
  ?_assertEqual( [{str, 12, "B"}], apply( Eval, [E, #{}, CreateTicket, #{}, #{}] ) ).

cnd_evaluates_condition_before_decision1( {Eval, CreateTicket} ) ->
  Sign = {sign, [{param, {name, "out", false}, true}], [], []},
  Body = {natbody, #{"out" => []}},
  Lam = [{lam, 10, "f", Sign, Body}],
  App = [{app, 11, 1, Lam, #{}}],
  E = [{cnd, 12, App, [{str, 13, "A"}], [{str, 14, "B"}]}],
  ?_assertEqual( [{str, 14, "B"}], apply( Eval, [E, #{}, CreateTicket, #{}, #{}] ) ).

cnd_evaluates_condition_before_decision2( {Eval, CreateTicket} ) ->
  Sign = {sign, [{param, {name, "out", false}, true}], [], []},
  Body = {natbody, #{"out" => [{str, 10, "X"}]}},
  Lam = [{lam, 11, "f", Sign, Body}],
  App = [{app, 12, 1, Lam, #{}}],
  E = [{cnd, 13, App, [{str, 14, "A"}], [{str, 15, "B"}]}],
  ?_assertEqual( [{str, 14, "A"}], apply( Eval, [E, #{}, CreateTicket, #{}, #{}] ) ).

cnd_evaluates_only_on_final_condition( {Eval, CreateTicket} ) ->
  Sign = {sign, [{param, "out", false, true}], [], []},
  Body = {forbody, bash, ""},
  Lam = [{lam, 10, "f", Sign, Body}],
  App = [{app, 11, 1, Lam, #{}}],
  A = [{var, 12, "a"}],
  B = [{var, 13, "b"}],
  E = [{cnd, 14, App, A, B}],
  Rho = #{"a" => [{str, 15, "A"}], "b" => [{str, 16, "B"}]},
  X = apply( Eval, [E, Rho, CreateTicket, #{}, #{}] ),
  ?_assertMatch( [{cnd, 14, [{select, _, _, _}], A, B}], X ).
  
cnd_evaluates_then_expr( {Eval, CreateTicket} ) ->
  E = [{cnd, 10, [{str, 11, "Z"}], [{var, 12, "x"}], [{str, 13, "B"}]}],
  F = [{str, 14, "A"}],
  ?_assertEqual( F, apply( Eval, [E, #{"x" => F}, CreateTicket, #{}, #{}] ) ).

cnd_evaluates_else_expr( {Eval, CreateTicket} ) ->
  E = [{cnd, 10, [], [{str, 11, "B"}], [{var, 12, "x"}]}],
  F = [{str, 13, "A"}],
  ?_assertEqual( F, apply( Eval, [E, #{"x" => F}, CreateTicket, #{}, #{}] ) ).

foreign_app_with_cnd_param_is_left_untouched( {Eval, CreateTicket} ) ->
  Sign = {sign, [{param, {name, "out", false}, false}],
                [], [{param, {name, "p", false}, false}]},
  Body = {forbody, bash, ""},
  Lam = [{lam, 10, "f", Sign, Body}],
  App1 = [{app, 11, 1, Lam, #{"p" => [{str, 12, "A"}]}}],
  E = [{cnd, 13, App1, [], []}],
  App2 = [{app, 14, 1, Lam, #{"p" => E}}],
  X = apply( Eval, [App2, #{}, CreateTicket, #{}, #{}] ),
  ?_assertMatch( [{app, _, 1, Lam, _}], X ).

foreign_app_with_select_param_is_left_untouched( {Eval, CreateTicket} ) ->
  Sign = {sign, [{param, {name, "out", false}, false}],
                [], [{param, {name, "p", false}, false}]},
  Body = {forbody, bash, ""},
  Lam = [{lam, 10, "f", Sign, Body}],
  App1 = [{app, 11, 1, Lam, #{"p" => [{str, 12, "A"}]}}],
  App2 = [{app, 13, 1, Lam, #{"p" => App1}}],
  X = apply( Eval, [App2, #{}, CreateTicket, #{}, #{}] ),
  ?_assertMatch( [{app, _, 1, Lam, _}], X ).

comb_param_works( {Eval, CreateTicket} ) ->
  Sign = {sign, [{param, {name, "out1", false}, false}, {param, {name, "out2", false}, false}],
                [], [{comb, cnr, {name, "p", false}, ["p1", "p2"]}]},
  Body = {natbody, #{"out1" => [{var, 10, "p1"}], "out2" => [{var, 11, "p2"}]}},
  Binding = #{"p" => [{str, 12, "1"}, {str, 13, "2"}, {str, 14, "3"}]},
  Lam = [{lam, 15, "f", Sign, Body}],
  App1 = [{app, 16, 1, Lam, Binding}],
  App2 = [{app, 17, 2, Lam, Binding}],
  X = apply( Eval, [App1, #{}, CreateTicket, #{}, #{}] ),
  Y = apply( Eval, [App2, #{}, CreateTicket, #{}, #{}] ),
  [?_assertEqual( [{str, 12, "1"}, {str, 12, "1"}, {str, 13, "2"}], X ),
   ?_assertEqual( [{str, 13, "2"}, {str, 14, "3"}, {str, 14, "3"}], Y )].

cascading_app_does_not_break_enum( {Eval, CreateTicket} ) ->
  Sign1 = {sign, [{param, {name, "out", false}, true}], [], []},
  Body1 = {forbody, bash, "out=(1 2 3)"},
  Lam1 = [{lam, 10, "f", Sign1, Body1}],
  App1 = [{app, 11, 1, Lam1, #{}}],
  Sign2 = {sign, [{param, {name, "out", false}, false}], [],
                 [{param, {name, "p1", false}, false}, {param, {name, "p2", false}, false}]},
  Body2 = {natbody, #{"out" => [{var, 12, "p1"}, {var, 13, "p2"}]}},
  Lam2 = [{lam, 14, "g", Sign2, Body2}],
  App2 = [{app, 15, 1, Lam2, #{"p1" => [{str, 16, "A"}], "p2" => App1}}],
  X = apply( Eval, [App2, #{}, CreateTicket, #{}, #{}] ),
  [Ticket] = get_ticket_list( X ),
  Fin = #{{1, Ticket} => [{str, 15, "1"}, {str, 15, "2"}]},
  Y = apply( Eval, [X, #{}, CreateTicket, #{}, Fin] ),
  ?_assertEqual( [{str, 16, "A"}, {str, 15, "1"}, {str, 16, "A"}, {str, 15, "2"}], Y ).

app_task_param_is_evaluated( {Eval, CreateTicket} ) ->
  Sign = {sign, [{param, {name, "out", false}, false}], [], []},
  Body = {natbody, #{"out" => [{str, 10, "A"}]}},
  Lam = [{lam, 11, "f", Sign, Body}],
  App = [{app, 12, 1, [{var, 13, "f"}], #{}}],
  Rho = #{"f" => Lam},
  ?_assertEqual( [{str, 10, "A"}], apply( Eval, [App, Rho, CreateTicket, #{}, #{}] ) ).
  
app_non_final_result_preserves_app( {Eval, CreateTicket} ) ->
  Sign = {sign, [{param, {name, "out", false}, false}], [], []},
  Body = {natbody, #{"out" => [{select, 10, 1, {ticket, 10, Sign, {forbody, bash, "blub"}, #{}}}]}},
  Lam = [{lam, 11, "f", Sign, Body}],
  App = [{app, 12, 1, Lam, #{}}],
  X = apply( Eval, [App, #{}, CreateTicket, #{}, #{}] ),
  ?_assertMatch( App, X ).
  
app_non_final_result_preserves_app_with_new_lam( {Eval, CreateTicket} ) ->
  CSign = {sign, [{param, {name, "out", false}, true}], [], []},
  CBody = {forbody, bash, "blub"},
  CLam = [{lam, 10, "f", CSign, CBody}],
  CApp = [{app, 11, 1, CLam, #{}}],
  Sign = {sign, [{param, {name, "out", false}, false}], [], []},
  Body = {natbody, #{"out" => [{cnd, 12, CApp, [{var, 13, "x"}], [{var, 14, "x"}]}],
                     "x" => [{str, 15, "A"}]}},
  Lam = [{lam, 16, "g", Sign, Body}],
  App = [{app, 17, 1, Lam, #{}}],
  X = apply( Eval, [App, #{}, CreateTicket, #{}, #{}] ),
  [{app, _, 1, [{lam, _, _, Sign, {natbody, BodyMap1}}], #{}}] = X,
  Val = get( "out", BodyMap1 ),
  ?_assertMatch( [{cnd, 12, [{select, _, 1, _}], [{var, 13, "x"}], [{var, 14, "x"}]}],
                 Val ).
  
nested_app_undergoes_reduction( {Eval, CreateTicket} ) ->
  Sign = {sign, [{param, {name, "out", false}, false}], [], []},
  Body1 = {forbody, bash, ""},
  Lam1 = [{lam, 10, "f", Sign, Body1}],
  App1 = [{app, 11, 1, Lam1, #{}}],
  Body2 = {natbody, #{"out" => App1}},
  Lam2 = [{lam, 12, "g", Sign, Body2}],
  App2 = [{app, 13, 1, Lam2, #{}}],
  X = apply( Eval, [App2, #{}, CreateTicket, #{}, #{}] ),
  [Ticket] = get_ticket_list( X ),
  Fin = #{{1, Ticket} => [{str, 13, "A"}]},
  Y = apply( Eval, [X, #{}, CreateTicket, #{}, Fin] ),
  ?_assertEqual( [{str, 13, "A"}], Y ).

      
lam_can_be_result_of_cnd( {Eval, CreateTicket} ) ->
  Sign = {sign, [{param, {name, "out", false}, false}], [], []},
  Body1 = {natbody, #{"out" => [{str, 10, "A"}]}},
  Lam1 = [{lam, 11, "f", Sign, Body1}],
  Body2 = {natbody, #{"out" => [{str, 12, "B"}]}},
  Lam2 = [{lam, 13, "g", Sign, Body2}],
  Cnd = [{cnd, 14, [], Lam1, Lam2}],
  App = [{app, 15, 1, Cnd, #{}}],
  X = apply( Eval, [App, #{}, CreateTicket, #{}, #{}] ),
  ?_assertEqual( [{str, 12, "B"}], X ).
  
deferring_lam_does_not_defer_param_evaluation( {Eval, CreateTicket} ) ->
  Sign12 = {sign, [{param, {name, "out", false}, false}], [], [{param, {name, "p", false}, false}]},
  Body1 = {natbody, #{"out" => [{str, 10, "A"}]}},
  Lam1 = [{lam, 11, "f", Sign12, Body1}],
  Body2 = {natbody, #{"out" => [{str, 12, "B"}]}},
  Lam2 = [{lam, 13, "g", Sign12, Body2}],
  Sign0 = {sign, [{param, {name, "out", false}, false}], [], []},
  Lam0 = [{lam, 14, "h", Sign0, {forbody, bash, "blub"}}],
  App0 = [{app, 15, 1, Lam0, #{}}],
  Cnd = [{cnd, 16, App0, Lam1, Lam2}],
  App = [{app, 17, 1, Cnd, #{"p" => [{var, 18, "x"}]}}],
  X = apply( Eval, [App, #{"x" => [{str, 19, "C"}]}, CreateTicket, #{}, #{}] ),
  [{app, 17, 1, [{cnd, 16, [{select, 15, 1, {ticket, 15, _, _, _}}], _, _}], Binding}] = X,
  V = get( "p", Binding ),
  ?_assertEqual( [{str, 19, "C"}], V ).
  
    
recursion_terminates( {Eval, CreateTicket} ) ->
  DecSign = {sign, [{param, {name, "i1", false}, false},
                    {param, {name, "converge", false}, true}],
                   [],
                   [{param, {name, "i", false}, false}]},
  DecBody = {forbody, bash, "blub"},
  DecLam = [{lam, 10, "f", DecSign, DecBody}],
  RecSign = {sign, [{param, {name, "out", false}, true}],
                   [], [{param, {name, "i", false}, false}]},
  RecBody = {natbody,
             #{"i1"       => [{app, 11, 1, DecLam, #{"i" => [{var, 12, "i"}]}}],
               "converge" => [{app, 13, 2, DecLam, #{"i" => [{var, 14, "i"}]}}],
               "out"      => [{cnd, 15, [{var, 16, "converge"}],
                                    [{str, 17, "last"}],
                                    [{str, 18, "next to"},
                                     {app, 19, 1, [{var, 20, "rec"}],
                                           #{"i" => [{var, 21, "i1"}]}}]}]}},
  RecLam = [{lam, 22, "g", RecSign, RecBody}],
  App = [{app, 23, 1, [{var, 24, "rec"}], #{"i" => [{str, 25, "2"}]}}],
  Global = #{"rec" => RecLam},
  R = apply( Eval, [App, #{}, CreateTicket, Global, #{}] ),
  [Ticket0] = get_ticket_list( R ),
  Fin1 = #{{2, Ticket0} => []},
  S = apply( Eval, [R, #{}, CreateTicket, Global, Fin1] ),
  [Ticket1] = get_ticket_list( S ),
  Fin2 = Fin1#{{1, Ticket1} => [{str, 23, "1"}]},
  T = apply( Eval, [S, #{}, CreateTicket, Global, Fin2] ),
  [Ticket2] = get_ticket_list( T ),
  Fin3 = Fin2#{{2, Ticket2} => [{str, 23, "true"}]},
  U = apply( Eval, [T, #{}, CreateTicket, Global, Fin3] ),
  ?_assertEqual( [{str, 18, "next to"}, {str, 17, "last"}], U ).

  
% deftask f( out : )in bash *{blub}*
% deftask g( out : inp ) { out = inp; }
%
% g( inp: f() f() );
%
% -> g( inp: f() ) g( inp: f() );
app_select_param_is_enumerated( {Eval, CreateTicket} ) ->
  Sign1 = {sign, [{param, {name, "out", false}, false}], [], []},
  Body1 = {forbody, bash, "blub"},
  Lam1 = [{lam, 10, "f", Sign1, Body1}],
  Sign2 = {sign, [{param, {name, "out", false}, false}],
                 [],
                 [{param, {name, "inp", false}, false}]},
  Body2 = {natbody, #{"out" => [{var, 11, "inp"}]}},
  Lam2 = [{lam, 12, "g", Sign2, Body2}],
  A0 = {app, 13, 1, Lam1, #{}},
  A = [A0, A0],
  B = [{app, 14, 1, Lam2, #{"inp" => A}}],
  X = apply( Eval, [B, #{}, CreateTicket, #{}, #{}] ), 
  ?_assertMatch(
    [{app, 14,  1, Lam2, _}, {app, 14, 1, Lam2, _}],
    X ).
  
  
  
  
% HELPER FUNCTIONS

%% get_ticket_list/1
%
get_ticket_list( CompoundExpr ) when is_list( CompoundExpr ) ->
  usort( foldl(
    fun( Elem, AccIn ) -> get_ticket_list_single( Elem )++AccIn end,
    [], CompoundExpr ) ).

    
%% get_ticket_list_map/1
%
get_ticket_list_map( Map ) when is_map( Map ) ->
  usort( foldl(
    fun( Elem, AccIn ) -> get_ticket_list( Elem )++AccIn end,
    [], values( Map ) ) ).

    
%% get_ticket_list_single/1
%
get_ticket_list_single( {var, _, _} ) -> [];

get_ticket_list_single( {str, _, _} ) -> [];

get_ticket_list_single( {select, _, _, T={ticket, _, _, _, _}} ) -> [T];

get_ticket_list_single( {lam, _, _, _, {natbody, BodyMap}} )
when is_map( BodyMap ) ->
  get_ticket_list_map( BodyMap );

get_ticket_list_single( {lam, _, _, _, {forbody, _, _}} ) -> [];

get_ticket_list_single( {app, _, _, LamList, Binding} )
when is_list( LamList ),
     is_map( Binding ) ->
  usort( get_ticket_list( LamList )++get_ticket_list_map( Binding ) );

get_ticket_list_single( {cnd, _, C, _, _} ) -> get_ticket_list( C ).
  
  

-endif.
