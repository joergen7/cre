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

-module( cre_proxy ).
-behaviour( gen_server ).

-include( "cre_proxy.hrl" ).



% export gen_server callback functions
-export( [code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
          terminate/2] ).

% export convenience functions          
-export( [stop/1, start_link/0] ).


-import( maps, [get/2, get/3, is_key/2, keys/1, remove/2] ).
-import( lists, [foldl/3, foreach/2, usort/1] ).


% SERVER CALLBACK FUNCTIONS

%% code_change/3
%
code_change( _OldVsn, State, _Extra ) -> {ok, State}.


%% handle_call/3
%
handle_call( stop, _From, State ) -> {stop, normal, ok, State};

handle_call( {add, Ticket, Datadir}, {QueryPid, _Tag}, State={ResultMap, RunMap} ) ->

  case is_key( Ticket, ResultMap ) of
  
    true ->
      QueryPid ! {finished, get( Ticket, ResultMap )},
      {reply, ok, State};
      
    false ->

      RunMap1 = RunMap#{Ticket => usort( [QueryPid|get( Ticket, RunMap, [] )] )},
      _Ref = monitor( process, QueryPid ),
      
      case is_key( Ticket, RunMap ) of
        true -> ok;
        false -> ok = gen_server:call( cre_stage, {add, Ticket, Datadir} )
      end,
      
      {reply, ok, {ResultMap, RunMap1}}
  end.
 

%% handle_cast/2
%
handle_cast( _Request, State ) -> {noreply, State}.


%% handle_info/2
%
handle_info( {finished, Ticket, Result}, {ResultMap, RunMap} ) ->

  QueryList = get( Ticket, RunMap ),
  FinalResult = finalize_result( Ticket, Result ),
  
  io:format( "PROXY returning result ~p~n", [FinalResult] ),
  
  foreach( fun( Query ) -> Query ! {finished, FinalResult} end, QueryList ),
  
  ResultMap1 = ResultMap#{Ticket => FinalResult},
  RunMap1 = remove( Ticket, RunMap ),
  
  {noreply, {ResultMap1, RunMap1}};

handle_info( Info={failed, Ticket, _Interpreter, _Script, _Output}, {ResultMap, RunMap} ) ->
  
  QueryList = get( Ticket, RunMap ),
  
  foreach( fun( Query ) -> Query ! Info end, QueryList ),

  RunMap1 = remove( Ticket, RunMap ),
  {noreply, {ResultMap, RunMap1}};
  
  
handle_info( {'DOWN', _Ref, process, QueryPid, _Info}, {ResultMap, RunMap} ) ->
  
  Fun = fun( Ticket, Acc ) ->
    QueryList = get( Ticket, RunMap )--[QueryPid],
    case QueryList of
      [] ->
        gen_server:call( cre_stage, {remove, Ticket} ),
        remove( Ticket, Acc );
      [_|_] ->
        Acc#{Ticket => QueryList}
    end
  end,
  
  RunMap1 = foldl( Fun, RunMap, keys( RunMap ) ),
  
  {noreply, {ResultMap, RunMap1}}.



%% init/1
%
init( [] ) ->
  {ok, {#{}, #{}}}.


%% terminate/2
%
terminate( _, _State ) -> ok.







% CONVENIENCE FUNCTIONS

%% start_link/0
%
start_link() ->
  gen_server:start_link( {local, cre_proxy}, ?MODULE, [], [] ).

%% stop/1
%
stop( Pid ) when is_pid( Pid ) ->
  gen_server:call( Pid, stop ).
  
  
finalize_result( Ticket={ticket, _Line, {sign, OutParam, [], _InParam}, _ForBody, _Binding}, Result ) ->
  finalize_result( OutParam, Ticket, Result, #{}, 1 ).
  
finalize_result( [], _Ticket, _Result, Acc, _Channel ) -> Acc;

finalize_result( [{param, {name, Name, _}, _}|R], Ticket, Result, Acc, Channel ) ->
  Acc1 = Acc#{{Channel, Ticket} => get( Name, Result )},
  finalize_result( R, Ticket, Result, Acc1, Channel+1 ).
