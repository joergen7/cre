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

-module( cre_query ).

-export( [code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2] ).
-export( [start_link/4] ).

-behaviour( gen_server ).

-include( "cre_query.hrl" ).

-import( cuneiform_csem, [eval/5, is_final_list/1] ).
-import( maps, [merge/2] ).
-import( file, [read_file/1] ).

% GEN_SERVER CALLBACK FUNCTIONS

code_change( _OldVsn, State, _Extra ) -> {ok, State}.

handle_call( stop, _From, State ) ->
  {stop, normal, ok, State}.

handle_cast( _Request, State ) -> {noreply, State}.

handle_info( {finished, FinMap}, {Sender, ExprList, Rho, CreateTicket, Global, Fin} ) ->

  Fin1 = merge( Fin, FinMap ),
  ExprList1 = eval( ExprList, Rho, CreateTicket, Global, Fin1 ),
  
  case is_final_list( ExprList1 ) of
  
    true ->
      Sender ! {finished, ExprList1},
      {stop, finished, {Sender, ExprList1, Rho, CreateTicket, Global, Fin1}};
      
    false ->
      {noreply, {Sender, ExprList1, Rho, CreateTicket, Global, Fin1}}
  end;
  
handle_info( Info={failed, _Ticket, _Interpreter, _Script, _Output},
             State={Sender, _ExprList, _Rho, _CreateTicket, _Global, _Fin} ) ->

  Sender ! Info,
  {stop, failed, State}.

init( {Sender, string, String, Datadir} ) ->
  init_string( Sender, String, Datadir );
  
init( {Sender, file, Filename, Datadir} ) ->
  {ok, Binary} = read_file( Filename ),
  init_string( Sender, binary_to_list( Binary ), Datadir ).

terminate( _Reason, _State ) -> ok.


% CONVENIENCE FUNCTIONS



start_link( Sender, Type, Filename, Datadir ) ->
  gen_server:start_link( ?MODULE, {Sender, Type, Filename, Datadir}, [] ).
  
% HELPER FUNCTIONS


init_string( Sender, String, Datadir ) ->

  {ok, TokenList, _} = cuneiform_lexer:string( String ),
  {ok, {ExprList, Rho, Global}} = cuneiform_parser:parse( TokenList ),
  
  CreateTicket = fun( Line, Sign, ForBody, Binding ) ->
                   create_ticket( cre_proxy, Line, Sign, ForBody, Binding,
                                  Datadir )
                 end,
  
  ExprList1 = eval( ExprList, Rho, CreateTicket, Global, #{} ),
               
  case is_final_list( ExprList1 ) of
  
    true ->
      Sender ! {finished, ExprList1},
      {stop, finished};
      
    false ->
      {ok, {Sender, ExprList1, Rho, CreateTicket, Global, #{}}}
  end.
  
  
create_ticket( ProxyPid, Line, Sign, ForBody, Binding, Datadir )
when is_pid( ProxyPid ) orelse is_atom( ProxyPid ),
     is_list( Datadir ),
     is_integer( Line ), Line > 0,
     is_tuple( Sign ),
     is_tuple( ForBody ),
     is_map( Binding ) ->
     
  Ticket = {ticket, Line, Sign, ForBody, Binding},
  ok = gen_server:call( ProxyPid, {add, Ticket, Datadir} ),
  Ticket.


%simplify( [] ) -> [];
%simplify( [{str, _, S}|R] ) -> [S|simplify( R )];
%simplify( [{lam, _, Name, _, _}|R] ) -> [{lam, Name}|simplify( R )]. 







  

