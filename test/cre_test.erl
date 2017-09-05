-module( cre_test ).

-include_lib( "eunit/include/eunit.hrl" ).

-define( CRE_NAME, cre ).
-define( WRK_NAME, logic_worker ).
-define( CLIENT_NAME, logic_client ).


%%====================================================================
%% Test definition
%%====================================================================

cre_test_() ->
  {foreach,

   % setup function
   fun() ->
     cre_master:start_link( {local, ?CRE_NAME} ),
     logic_worker:start_link( {local, ?WRK_NAME}, ?CRE_NAME ),
     logic_client:start_link( {local, ?CLIENT_NAME}, ?CRE_NAME )
   end,

   fun( _ ) ->
     ok = cre_master:stop( ?CRE_NAME ),
     ok = logic_worker:stop( ?WRK_NAME ),
     ok = logic_client:stop( ?CLIENT_NAME )
   end,

   [
    {<<"false evaluates itself">>,         fun false_evaluates_itself/0},
    {<<"true evaluates itself">>,          fun true_evaluates_itself/0},
    {<<"not true evaluates false">>,       fun not_true_evaluates_false/0},
    {<<"not false evaluates true">>,       fun not_false_evaluates_true/0},
    {<<"true or true evaluates true">>,    fun true_or_true_evaluates_true/0},
    {<<"true or false evaluates true">>,   fun true_or_false_evaluates_true/0},
    {<<"false or true evaluates true">>,   fun false_or_true_evaluates_true/0},
    {<<"false or false evaluates false">>, fun false_or_false_evaluates_false/0}
   ]
  }.


%%====================================================================
%% Test implementation
%%====================================================================

false_evaluates_itself() ->
  ?assertNot( eval( false ) ).

true_evaluates_itself() ->
  ?assert( eval( true ) ).

not_true_evaluates_false() ->
  ?assertNot( eval( {'not', true} ) ).

not_false_evaluates_true() ->
  ?assert( eval( {'not', false} ) ).

true_or_true_evaluates_true() ->
  ?assert( eval( {'or', true, true} ) ).

true_or_false_evaluates_true() ->
  ?assert( eval( {'or', true, false} ) ).

false_or_true_evaluates_true() ->
  ?assert( eval( {'or', false, true} ) ).

false_or_false_evaluates_false() ->
  ?assertNot( eval( {'or', false, false} ) ).
%%====================================================================
%% Helper functions
%%====================================================================

eval( T ) ->
  logic_client:eval( ?CLIENT_NAME, T ).