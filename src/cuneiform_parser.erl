-module(cuneiform_parser).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("src/cuneiform_parser.yrl", 108).



-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).
-endif.






combine( {Target1, Rho1, Global1}, {Target2, Rho2, Global2} ) ->
  {Target1++Target2, maps:merge( Rho1, Rho2 ), maps:merge( Global1, Global2 )}.

mk_var( {id, Line, Name} ) -> {var, Line, Name}.

mk_str( {intlit, Line, N} ) -> {str, Line, N};
mk_str( {strlit, Line, S} ) -> {str, Line, S}.

get_line( {_, Line, _} ) -> Line.

get_name( {id, _Line, Name} ) -> Name.

mk_binding( {id, _, Name}, ExprList ) ->
  #{Name => ExprList}.
  
mk_assign( [], _ExprList, _Channel ) -> #{};

mk_assign( [{var, _Line, Name}|Rest], ExprList, Channel ) ->
  Rho = mk_assign( Rest, ExprList, Channel+1 ),
  Value = lists:flatmap( fun( E ) -> set_channel( E, Channel ) end, ExprList ),
  Rho#{Name => Value};
  
mk_assign( [E|_Rest], _ExprList, _Channel ) ->
  error( {parser, nonvar_expr_left_of_eq, element( 1, E ), element( 2, E )} ).


set_channel( {app, Line, _Channel, LamList, Binding}, N ) -> [{app, Line, N, LamList, Binding}];
set_channel( E, 1 ) -> [E];
set_channel( E, _ ) -> error( {parser, cannot_set_channel_on_nonapp_expr, element( 1, E ), element( 2, E )} ).
  
mk_natlam( {deftask, Line, _}, {id, _, Name}, Sign, Block ) ->
  #{Name => [{lam, Line, Name, Sign, {natbody, Block}}]}.
  
mk_forlam( {deftask, Line, _}, {id, _, Name}, Sign, Lang, {body, _, Code} ) ->
  #{Name => [{lam, Line, Name, Sign, {forbody, Lang, Code}}]}.
  
  
-ifdef( TEST ).

% TEST INDEX

parse_test_() -> [nil_should_be_recognized(),
                  var_should_be_recognized(),
                  multi_element_compoundexpr_should_be_recognized(),
                  multiple_targets_should_be_joined(),
                  strlit_should_be_recognized(),
                  intlit_should_be_recognized(),
                  cnd_should_be_recognized(),
                  apply_should_be_recognized(),
                  call_should_be_recognized(),
                  assign_should_be_recognized(),
                  native_deftask_should_be_recognized(),
                  foreign_deftask_should_be_recognized(),
                  sign_with_inparam_should_be_recognized(),
                  param_should_be_recognized(),
                  task_correl_should_be_recognized(),
                  correl_inparam_should_be_recognized(),
                  comb_noreplace_should_be_recognized()
                 ].


% ACTUAL TEST IMPLEMENTATION

nil_should_be_recognized() ->
  ?_assertEqual( {[], #{}, #{}}, parse_string( "nil;" ) ).
  
var_should_be_recognized() ->
  ?_assertEqual( {[{var, 1, "blub"}], #{}, #{}}, parse_string( "blub;" ) ).
  
multi_element_compoundexpr_should_be_recognized() ->
  ?_assertEqual( {[{var, 1, "bla"}, {var, 1, "blub"}], #{}, #{}},
                 parse_string( "bla blub;" ) ).
  
multiple_targets_should_be_joined() ->
  ?_assertEqual( {[{var, 1, "bla"}, {var, 1, "blub"}], #{}, #{}},
                 parse_string( "bla; blub;" ) ).
  
strlit_should_be_recognized() ->
  ?_assertEqual( {[{str, 1, "bla"}], #{}, #{}}, parse_string( "'bla';" ) ).
  
intlit_should_be_recognized() ->
  ?_assertEqual( {[{str, 1, "-5"}], #{}, #{}}, parse_string( "-5;" ) ).
  
cnd_should_be_recognized() ->
  ?_assertEqual( {[{cnd, 1, [], [{str, 1, "bla"}], [{str, 1, "blub"}]}],
                  #{}, #{}},
                 parse_string( "if nil then 'bla' else 'blub' end;" ) ).
  
apply_should_be_recognized() ->
  [?_assertEqual( {[{app, 1, 1, [{var, 1, "f"}], #{}}], #{}, #{}},
                  parse_string( "apply( task: f );" ) ),
   ?_assertEqual( {[{app, 1, 1, [{var, 1, "f"}], #{"x" => [{var, 1, "x"}]}}],
                   #{}, #{}},
                  parse_string( "apply( task: f, x: x );" ) ),
   ?_assertEqual( {[{app, 1, 1, [{var, 1, "f"}], #{"x" => [{var, 1, "x"}],
                                                   "y" => [{str, 1, "y"}]}}],
                   #{}, #{}},
                  parse_string( "apply( task: f, x: x, y: 'y' );" ) ),
   ?_assertEqual( {[{app, 1, 1, [{var, 1, "f"}, {var, 1, "g"}],
                          #{"x" => [{var, 1, "x"}]}}], #{}, #{}},
                  parse_string( "apply( task: f g, x: x );" ) )].
  
call_should_be_recognized() ->
  [?_assertEqual( {[{app, 1, 1, [{var, 1, "f"}], #{}}], #{}, #{}},
                  parse_string( "f();" ) ),
   ?_assertEqual( {[{app, 1, 1, [{var, 1, "f"}], #{"x" => [{var, 1, "x"}]}}],
                   #{}, #{}}, parse_string( "f( x: x );" ) ),
   ?_assertEqual( {[{app, 1, 1, [{var, 1, "f"}],
                     #{"x" => [{var, 1, "x"}],
                       "y" => [{str, 1, "y"}]}}], #{}, #{}},
                  parse_string( "f( x: x, y: 'y' );" ) )].


assign_should_be_recognized() ->
  [?_assertEqual( {[], #{"x" => [{str, 1, "x"}]}, #{}}, parse_string( "x = 'x';" ) ),              
   ?_assertEqual( {[], #{"x" => [{app, 1, 1, [{var, 1, "f"}], #{}}], "y" => [{app, 1, 2, [{var, 1, "f"}], #{}}]}, #{}}, parse_string( "x y = f();" ) ),
   ?_assertError( {parser, cannot_set_channel_on_nonapp_expr, str, 1}, parse_string( "x y = 'A';" ) ),
   ?_assertError( {parser, nonvar_expr_left_of_eq, str, 1}, parse_string( "'a' = 'A';" ) )].
   
native_deftask_should_be_recognized() ->
  ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                           {sign, [{param, {name, "out", false}, false}],
                                                  [], []},
                                           {natbody, #{"out" => [{str, 1, "A"}]}}}]}},
                 parse_string( "deftask f( out : ) { out = 'A'; }" ) ).
                 
foreign_deftask_should_be_recognized() ->
  [?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [], []},
                                            {forbody, bash, "out=A"}}]}},
                  parse_string( "deftask f( out : )in bash *{out=A}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [], []},
                                            {forbody, r, "out=\"A\""}}]}},
                  parse_string( "deftask f( out : )in R *{out=\"A\"}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [], []},
                                            {forbody, matlab, "out=\"A\""}}]}},
                  parse_string( "deftask f( out : )in matlab *{out=\"A\"}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [], []},
                                            {forbody, octave, "out=\"A\""}}]}},
                  parse_string( "deftask f( out : )in octave *{out=\"A\"}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [], []},
                                            {forbody, perl, ""}}]}},
                  parse_string( "deftask f( out : )in perl *{}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [], []},
                                            {forbody, python, ""}}]}},
                  parse_string( "deftask f( out : )in python *{}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [], []},
                                            {forbody, lisp, "(defparameter out \"A\")"}}]}},
                  parse_string( "deftask f( out : )in lisp *{(defparameter out \"A\")}*" ) )].
                  
sign_with_inparam_should_be_recognized() ->
  [?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [],
                                                   [{param, {name, "inp", false}, false}]},
                                            {forbody, lisp, "(defparameter out \"A\")"}}]}},
                  parse_string( "deftask f( out : inp )in lisp *{(defparameter out \"A\")}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [],
                                                   [{param, {name, "a", false}, false},
                                                    {param, {name, "b", false}, false}]},
                                            {forbody, lisp, "(defparameter out \"A\")"}}]}},
                  parse_string( "deftask f( out : a b )in lisp *{(defparameter out \"A\")}*" ) )].
                  
param_should_be_recognized() ->
  [?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [],
                                                   [{param, {name, "inp", false}, false}]},
                                            {forbody, bash, "blub"}}]}},
                  parse_string( "deftask f( out( String ) : inp( String ) )in bash *{blub}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", true}, false}],
                                                   [],
                                                   [{param, {name, "inp", true}, false}]},
                                            {forbody, bash, "blub"}}]}},
                  parse_string( "deftask f( out( File ) : inp( File ) )in bash *{blub}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, true}],
                                                   [],
                                                   [{param, {name, "inp", false}, true}]},
                                            {forbody, bash, "blub"}}]}},
                  parse_string( "deftask f( <out> : <inp> )in bash *{blub}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, true}],
                                                   [],
                                                   [{param, {name, "inp", false}, true}]},
                                            {forbody, bash, "blub"}}]}},
                  parse_string( "deftask f( <out( String )> : <inp( String )> )in bash *{blub}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", true}, true}],
                                                   [],
                                                   [{param, {name, "inp", true}, true}]},
                                            {forbody, bash, "blub"}}]}},
                  parse_string( "deftask f( <out( File )> : <inp( File )> )in bash *{blub}*" ) )].
  
task_correl_should_be_recognized() ->
  [?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [{name, "a", false}],
                                                   [{param, {name, "b", false}, false}]},
                                            {forbody, bash, "blub"}}]}},
                  parse_string( "deftask f( out : [task a] b )in bash *{blub}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [{name, "a", false},
                                                    {name, "b", false}],
                                                   []},
                                            {forbody, bash, "blub"}}]}},
                  parse_string( "deftask f( out : [task a b] )in bash *{blub}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [{name, "a", false}],
                                                   [{param, {name, "b", false}, false}]},
                                            {forbody, bash, "blub"}}]}},
                  parse_string( "deftask f( out : [task a( String )] b )in bash *{blub}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [{name, "a", false},
                                                    {name, "b", false}],
                                                   []},
                                            {forbody, bash, "blub"}}]}},
                  parse_string( "deftask f( out : [task a( String ) b( String )] )in bash *{blub}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [{name, "a", true}],
                                                   [{param, {name, "b", false}, false}]},
                                            {forbody, bash, "blub"}}]}},
                  parse_string( "deftask f( out : [task a( File )] b )in bash *{blub}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [{name, "a", true},
                                                    {name, "b", true}],
                                                   []},
                                            {forbody, bash, "blub"}}]}},
                  parse_string( "deftask f( out : [task a( File ) b( File )] )in bash *{blub}*" ) )].    
                  
correl_inparam_should_be_recognized() ->
  [?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [],
                                                   [{correl, [{name, "a", true},
                                                              {name, "b", true}]}]},
                                            {forbody, bash, "blub"}}]}},
                  parse_string( "deftask f( out : [a( File ) b( File )] )in bash *{blub}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [],
                                                   [{correl, [{name, "a", true},
                                                              {name, "b", true}]},
                                                    {param, {name, "c", false}, false}]},
                                            {forbody, bash, "blub"}}]}},
                  parse_string( "deftask f( out : [a( File ) b( File )] c )in bash *{blub}*" ) )].

comb_noreplace_should_be_recognized() ->
  ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                           {sign, [{param, {name, "out", false}, false}],
                                                  [],
                                                  [{comb, cnr, {name, "x", true}, ["a", "b", "c"]}]},
                                           {forbody, bash, "blub"}}]}},
                 parse_string( "deftask f( out : {comb noreplace x( File ): a b c} )in bash *{blub}*" ) ).







parse_string( S ) ->
  {ok, TokenList, _} = cuneiform_lexer:string( S ),
  {ok, ParseTree} = cuneiform_parser:parse( TokenList ),
  ParseTree.



-endif.



-file("/usr/local/lib/erlang/lib/parsetools-2.1/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2015. All Rights Reserved.
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
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_line}, 0, [], []).

-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_line}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_line}, 0, [], []).

-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(integer(), any()) -> no_return().
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs,_} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Line}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, Endline} ->
            yeccpars1(Tokens, {{F, A}, Endline}, State, States, Vstack);
        {eof, Endline} ->
            yeccpars1([], {no_func, Endline}, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_line}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, Endline}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(Endline), [],
              {no_func, Endline}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Line}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_line}) ->
    Line = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Line}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line}).

%% For internal use only.
yecc_end({Line,_Column}) ->
    {'$end', Line};
yecc_end(Line) ->
    {'$end', Line}.

yecctoken_end_location(Token) ->
    try erl_anno:end_location(element(2, Token)) of
        undefined -> yecctoken_location(Token);
        Loc -> Loc
    catch _:_ -> yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    try erl_scan:text(Token) of
        undefined -> yecctoken2string(Token);
        Txt -> Txt
    catch _:_ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    try erl_scan:location(Token)
    catch _:_ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string({_Cat, _, Val}) -> io_lib:format("~p",[Val]);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:write(Other);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("src/cuneiform_parser.erl", 483).

-dialyzer({nowarn_function, yeccpars2/7}).
yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, deftask, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, nil, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_1(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, beginif, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, deftask, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, id, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, intlit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, nil, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, strlit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_script(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_2/7}).
yeccpars2_2(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_2(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_3(S, eq, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_compoundexpr(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_4(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, beginif, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, id, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, intlit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, nil, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, strlit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_4_(Stack),
 yeccgoto_necompoundexpr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_5_(Stack),
 yeccgoto_stat(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_6/7}).
yeccpars2_6(S, semicolon, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_7_(Stack),
 yeccgoto_stat(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_8/7}).
yeccpars2_8(S, lparen, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_9(S, nil, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_10/7}).
yeccpars2_10(S, id, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_11(S, lparen, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_11_(Stack),
 yeccgoto_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_12_(Stack),
 yeccgoto_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_13_(Stack),
 yeccgoto_compoundexpr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_14_(Stack),
 yeccgoto_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_15/7}).
yeccpars2_15(S, id, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, rparen, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_16/7}).
yeccpars2_16(S, rparen, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_17/7}).
yeccpars2_17(S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_18_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_19: see yeccpars2_9

yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_compoundexpr(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_21(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_21_(Stack),
 yeccgoto_binding(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_22/7}).
yeccpars2_22(S, id, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_23_(Stack),
 yeccgoto_binding(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_24_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_25/7}).
yeccpars2_25(S, lparen, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_26/7}).
yeccpars2_26(S, in, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_26(S, lbrace, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_26(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_27/7}).
yeccpars2_27(S, id, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(S, ltag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_28/7}).
yeccpars2_28(S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_29(S, id, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(S, ltag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_29_(Stack),
 yeccgoto_paramlist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_30(S, lparen, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_30_(Stack),
 yeccgoto_param(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_31/7}).
yeccpars2_31(S, id, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_32/7}).
yeccpars2_32(S, lparen, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, rtag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_33/7}).
yeccpars2_33(S, file, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_34_(Stack),
 yeccgoto_param(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_35/7}).
yeccpars2_35(S, rparen, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_36/7}).
yeccpars2_36(S, rparen, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_37/7}).
yeccpars2_37(S, rtag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_38_(Stack),
 yeccgoto_param(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_39/7}).
yeccpars2_39(S, rtag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_40_(Stack),
 yeccgoto_param(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_41/7}).
yeccpars2_41(S, file, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_42/7}).
yeccpars2_42(S, rparen, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_43/7}).
yeccpars2_43(S, rparen, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_44_(Stack),
 yeccgoto_param(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_45_(Stack),
 yeccgoto_param(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_46_(Stack),
 yeccgoto_paramlist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_47(S, lbrace, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, lsquarebr, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, rparen, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_inparam(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_49/7}).
yeccpars2_49(S, rparen, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_50(S, id, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, lbrace, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, lsquarebr, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, ltag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_50_(Stack),
 yeccgoto_inparamlist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_51/7}).
yeccpars2_51(S, comb, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_52/7}).
yeccpars2_52(S, id, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(S, task, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_53_(Stack),
 yeccgoto_sign(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_54/7}).
yeccpars2_54(S, rsquarebr, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_55(S, id, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_55_(Stack),
 yeccgoto_namelist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_56(S, lparen, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_56_(Stack),
 yeccgoto_name(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_57/7}).
yeccpars2_57(S, id, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_58/7}).
yeccpars2_58(S, rsquarebr, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_59(S, lbrace, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(S, lsquarebr, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(S, rparen, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_60/7}).
yeccpars2_60(S, rparen, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_61: see yeccpars2_57

yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_62_(Stack),
 yeccgoto_sign(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_63_(Stack),
 yeccgoto_sign(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_64/7}).
yeccpars2_64(S, file, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_65/7}).
yeccpars2_65(S, rparen, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_66/7}).
yeccpars2_66(S, rparen, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_67_(Stack),
 yeccgoto_name(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_68_(Stack),
 yeccgoto_name(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_69_(Stack),
 yeccgoto_namelist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_70_(Stack),
 yeccgoto_inparam(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_71/7}).
yeccpars2_71(S, noreplace, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_72: see yeccpars2_57

-dialyzer({nowarn_function, yeccpars2_73/7}).
yeccpars2_73(S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_73(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_74/7}).
yeccpars2_74(S, id, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_75/7}).
yeccpars2_75(S, rbrace, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_76(S, id, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_76_(Stack),
 yeccgoto_idlist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_77_(Stack),
 yeccgoto_idlist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_78_(Stack),
 yeccgoto_inparam(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_79_(Stack),
 yeccgoto_inparamlist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_80_(Stack),
 yeccgoto_sign(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_81/7}).
yeccpars2_81(S, bash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, lisp, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, matlab, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, octave, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, perl, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, python, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, r, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_82/7}).
yeccpars2_82(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(S, beginif, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(S, id, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(S, intlit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(S, strlit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_83/7}).
yeccpars2_83(S, eq, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_84/7}).
yeccpars2_84(S, rbrace, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_85(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(S, beginif, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(S, id, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(S, intlit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(S, strlit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_block(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_86_(Stack),
 yeccgoto_block(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_87_(Stack),
 yeccgoto_defun(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_88: see yeccpars2_9

-dialyzer({nowarn_function, yeccpars2_89/7}).
yeccpars2_89(S, semicolon, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_90_(Stack),
 yeccgoto_assign(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_91/7}).
yeccpars2_91(S, body, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_92_(Stack),
 yeccgoto_lang(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_93_(Stack),
 yeccgoto_lang(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_(Stack),
 yeccgoto_lang(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_95_(Stack),
 yeccgoto_lang(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_96_(Stack),
 yeccgoto_lang(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_97_(Stack),
 yeccgoto_lang(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_(Stack),
 yeccgoto_lang(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_99_(Stack),
 yeccgoto_defun(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_100/7}).
yeccpars2_100(S, then, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_101: see yeccpars2_9

-dialyzer({nowarn_function, yeccpars2_102/7}).
yeccpars2_102(S, else, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_103: see yeccpars2_9

-dialyzer({nowarn_function, yeccpars2_104/7}).
yeccpars2_104(S, endif, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_105_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_106/7}).
yeccpars2_106(S, task, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_107/7}).
yeccpars2_107(S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_108: see yeccpars2_9

-dialyzer({nowarn_function, yeccpars2_109/7}).
yeccpars2_109(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, rparen, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_110: see yeccpars2_22

yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_111_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_112/7}).
yeccpars2_112(S, rparen, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_113_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_114_(Stack),
 yeccgoto_stat(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_115_(Stack),
 yeccgoto_necompoundexpr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_116_(Stack),
 yeccgoto_script(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccgoto_assign(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assign(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assign(82, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(85, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assign(85, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(85, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_binding(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(16, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binding(22=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binding(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_block(82, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(84, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_compoundexpr(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(6, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_compoundexpr(1, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(6, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_compoundexpr(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_compoundexpr(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(100, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_compoundexpr(19, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_compoundexpr(88, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(89, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_compoundexpr(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(102, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_compoundexpr(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(104, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_compoundexpr(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(109, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_defun(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_defun(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expr(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(1, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(19, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(82, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(85, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(88, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_idlist(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(75, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_idlist(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_inparam(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_inparam(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_inparam(59, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(50, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_inparamlist(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_inparamlist(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_inparamlist(59, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_lang(81, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(91, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_name(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(72, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_namelist(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_namelist(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_namelist(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(58, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_namelist(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(54, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_necompoundexpr(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_necompoundexpr(1, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_necompoundexpr(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_necompoundexpr(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_necompoundexpr(19=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_necompoundexpr(82, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(83, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_necompoundexpr(85, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(83, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_necompoundexpr(88=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_necompoundexpr(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_necompoundexpr(103=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_necompoundexpr(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_param(27, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_param(29, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_param(47=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_param(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_param(59=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_paramlist(27, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_paramlist(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_script(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_script(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_sign(25, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(26, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_stat(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stat(1, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_4_/1}).
-file("src/cuneiform_parser.yrl", 49).
yeccpars2_4_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_5_/1}).
-file("src/cuneiform_parser.yrl", 31).
yeccpars2_5_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { [ ] , # { } , __1 }
  end | __Stack].

-compile({inline,yeccpars2_7_/1}).
-file("src/cuneiform_parser.yrl", 30).
yeccpars2_7_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { [ ] , __1 , # { } }
  end | __Stack].

-compile({inline,yeccpars2_11_/1}).
-file("src/cuneiform_parser.yrl", 52).
yeccpars2_11_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   mk_var ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_12_/1}).
-file("src/cuneiform_parser.yrl", 53).
yeccpars2_12_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   mk_str ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_13_/1}).
-file("src/cuneiform_parser.yrl", 46).
yeccpars2_13_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ ]
  end | __Stack].

-compile({inline,yeccpars2_14_/1}).
-file("src/cuneiform_parser.yrl", 54).
yeccpars2_14_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   mk_str ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_18_/1}).
-file("src/cuneiform_parser.yrl", 60).
yeccpars2_18_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { app , get_line ( __1 ) , 1 , [ mk_var ( __1 ) ] , # { } }
  end | __Stack].

-compile({inline,yeccpars2_21_/1}).
-file("src/cuneiform_parser.yrl", 63).
yeccpars2_21_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   mk_binding ( __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_23_/1}).
-file("src/cuneiform_parser.yrl", 64).
yeccpars2_23_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   maps : merge ( mk_binding ( __1 , __3 ) , __5 )
  end | __Stack].

-compile({inline,yeccpars2_24_/1}).
-file("src/cuneiform_parser.yrl", 61).
yeccpars2_24_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { app , get_line ( __1 ) , 1 , [ mk_var ( __1 ) ] , __3 }
  end | __Stack].

-compile({inline,yeccpars2_29_/1}).
-file("src/cuneiform_parser.yrl", 76).
yeccpars2_29_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_30_/1}).
-file("src/cuneiform_parser.yrl", 79).
yeccpars2_30_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { param , { name , get_name ( __1 ) , false } , false }
  end | __Stack].

-compile({inline,yeccpars2_34_/1}).
-file("src/cuneiform_parser.yrl", 82).
yeccpars2_34_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { param , { name , get_name ( __2 ) , false } , true }
  end | __Stack].

-compile({inline,yeccpars2_38_/1}).
-file("src/cuneiform_parser.yrl", 83).
yeccpars2_38_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { param , { name , get_name ( __2 ) , false } , true }
  end | __Stack].

-compile({inline,yeccpars2_40_/1}).
-file("src/cuneiform_parser.yrl", 84).
yeccpars2_40_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { param , { name , get_name ( __2 ) , true } , true }
  end | __Stack].

-compile({inline,yeccpars2_44_/1}).
-file("src/cuneiform_parser.yrl", 80).
yeccpars2_44_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { param , { name , get_name ( __1 ) , false } , false }
  end | __Stack].

-compile({inline,yeccpars2_45_/1}).
-file("src/cuneiform_parser.yrl", 81).
yeccpars2_45_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { param , { name , get_name ( __1 ) , true } , false }
  end | __Stack].

-compile({inline,yeccpars2_46_/1}).
-file("src/cuneiform_parser.yrl", 77).
yeccpars2_46_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_50_/1}).
-file("src/cuneiform_parser.yrl", 86).
yeccpars2_50_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_53_/1}).
-file("src/cuneiform_parser.yrl", 69).
yeccpars2_53_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { sign , __2 , [ ] , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_55_/1}).
-file("src/cuneiform_parser.yrl", 93).
yeccpars2_55_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_56_/1}).
-file("src/cuneiform_parser.yrl", 96).
yeccpars2_56_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { name , get_name ( __1 ) , false }
  end | __Stack].

-compile({inline,yeccpars2_62_/1}).
-file("src/cuneiform_parser.yrl", 72).
yeccpars2_62_(__Stack0) ->
 [__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { sign , __2 , __6 , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_63_/1}).
-file("src/cuneiform_parser.yrl", 74).
yeccpars2_63_(__Stack0) ->
 [__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { sign , __2 , __6 , __8 }
  end | __Stack].

-compile({inline,yeccpars2_67_/1}).
-file("src/cuneiform_parser.yrl", 97).
yeccpars2_67_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { name , get_name ( __1 ) , false }
  end | __Stack].

-compile({inline,yeccpars2_68_/1}).
-file("src/cuneiform_parser.yrl", 98).
yeccpars2_68_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { name , get_name ( __1 ) , true }
  end | __Stack].

-compile({inline,yeccpars2_69_/1}).
-file("src/cuneiform_parser.yrl", 94).
yeccpars2_69_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_70_/1}).
-file("src/cuneiform_parser.yrl", 90).
yeccpars2_70_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { correl , __2 }
  end | __Stack].

-compile({inline,yeccpars2_76_/1}).
-file("src/cuneiform_parser.yrl", 100).
yeccpars2_76_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ get_name ( __1 ) ]
  end | __Stack].

-compile({inline,yeccpars2_77_/1}).
-file("src/cuneiform_parser.yrl", 101).
yeccpars2_77_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ get_name ( __1 ) | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_78_/1}).
-file("src/cuneiform_parser.yrl", 91).
yeccpars2_78_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { comb , cnr , __4 , __6 }
  end | __Stack].

-compile({inline,yeccpars2_79_/1}).
-file("src/cuneiform_parser.yrl", 87).
yeccpars2_79_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_80_/1}).
-file("src/cuneiform_parser.yrl", 70).
yeccpars2_80_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { sign , __2 , [ ] , __4 }
  end | __Stack].

-compile({inline,yeccpars2_86_/1}).
-file("src/cuneiform_parser.yrl", 67).
yeccpars2_86_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   maps : merge ( __1 , __2 )
  end | __Stack].

-compile({inline,yeccpars2_87_/1}).
-file("src/cuneiform_parser.yrl", 33).
yeccpars2_87_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   mk_natlam ( __1 , __2 , __3 , __5 )
  end | __Stack].

-compile({inline,yeccpars2_90_/1}).
-file("src/cuneiform_parser.yrl", 44).
yeccpars2_90_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   mk_assign ( __1 , __3 , 1 )
  end | __Stack].

-compile({inline,yeccpars2_92_/1}).
-file("src/cuneiform_parser.yrl", 36).
yeccpars2_92_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   bash
  end | __Stack].

-compile({inline,yeccpars2_93_/1}).
-file("src/cuneiform_parser.yrl", 37).
yeccpars2_93_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   lisp
  end | __Stack].

-compile({inline,yeccpars2_94_/1}).
-file("src/cuneiform_parser.yrl", 38).
yeccpars2_94_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   matlab
  end | __Stack].

-compile({inline,yeccpars2_95_/1}).
-file("src/cuneiform_parser.yrl", 39).
yeccpars2_95_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   octave
  end | __Stack].

-compile({inline,yeccpars2_96_/1}).
-file("src/cuneiform_parser.yrl", 40).
yeccpars2_96_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   perl
  end | __Stack].

-compile({inline,yeccpars2_97_/1}).
-file("src/cuneiform_parser.yrl", 41).
yeccpars2_97_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   python
  end | __Stack].

-compile({inline,yeccpars2_98_/1}).
-file("src/cuneiform_parser.yrl", 42).
yeccpars2_98_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   r
  end | __Stack].

-compile({inline,yeccpars2_99_/1}).
-file("src/cuneiform_parser.yrl", 34).
yeccpars2_99_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   mk_forlam ( __1 , __2 , __3 , __5 , __6 )
  end | __Stack].

-compile({inline,yeccpars2_105_/1}).
-file("src/cuneiform_parser.yrl", 56).
yeccpars2_105_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { cnd , get_line ( __1 ) , __2 , __4 , __6 }
  end | __Stack].

-compile({inline,yeccpars2_111_/1}).
-file("src/cuneiform_parser.yrl", 57).
yeccpars2_111_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { app , get_line ( __1 ) , 1 , __5 , # { } }
  end | __Stack].

-compile({inline,yeccpars2_113_/1}).
-file("src/cuneiform_parser.yrl", 59).
yeccpars2_113_(__Stack0) ->
 [__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { app , get_line ( __1 ) , 1 , __5 , __7 }
  end | __Stack].

-compile({inline,yeccpars2_114_/1}).
-file("src/cuneiform_parser.yrl", 29).
yeccpars2_114_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , # { } , # { } }
  end | __Stack].

-compile({inline,yeccpars2_115_/1}).
-file("src/cuneiform_parser.yrl", 50).
yeccpars2_115_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_116_/1}).
-file("src/cuneiform_parser.yrl", 27).
yeccpars2_116_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   combine ( __1 , __2 )
  end | __Stack].


-file("src/cuneiform_parser.yrl", 413).
