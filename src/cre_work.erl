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

-module( cre_work ).
-author( "Jörgen Brandt <brandjoe@hu-berlin.de>" ).

-behaviour( tract ).

%% ============================================================
%% Includes
%% ============================================================

-include( "cre_work.hrl" ).


%% ============================================================
%% API Exports
%% ============================================================

-export( [nslot/1, nslot/2, start_link/0] ).


%% ============================================================
%% Tract Function Exports
%% ============================================================

-export( [handle_call/3, handle_info/2, init/1, handle_recv/3, handle_abort/2, handle_commit/3] ).
          

%% ============================================================
%% Tract Functions
%% ============================================================



  
handle_call( nslot, _From, State={Nslot, _Queue, _RunMap} ) ->
  {reply, {nslot, Nslot}, State};


handle_call( {nslot, N}, _From, {_Nslot, Queue, RunMap} ) when N > 0 ->

  % try to fill up empty slots
  State1 = gobble_queue( {N, Queue, RunMap} ),

  {reply, ok, State1}.


handle_recv( TransId,
             {add, Lang, Script, Dir, OutList, ParamMap, TypeMap},
             {Nslot, Queue, RunMap} ) ->
    
  % add ticket to queue and attempt to start it
  State1 = gobble_queue( {Nslot, [{TransId, Lang, Script, Dir, OutList, ParamMap, TypeMap}|Queue], RunMap} ),
  
  {reply, ok, State1}.
      

handle_abort( TransId, {Nslot, Queue, RunMap} ) ->
  
  QueueFun = fun( Tuple={TransId1, _Lang, _Script, _Dir, _OutList, _ParamMap, _TypeMap}, AccIn ) ->

               case TransId1 of

                 TransId -> AccIn;
                 _       -> [Tuple|AccIn]

               end
             end,

  RunMapFun = fun( Pid, AccIn ) ->

                TransId1 = maps:get( Pid, RunMap ),

                case TransId1 of

                  TransId ->

                    Pid ! {'EXIT', self(), abort},
                    AccIn;

                  _ ->

                    AccIn#{ Pid => TransId1 }

                end
              end,

  Queue1 = lists:foldl( QueueFun, [], Queue ),
  RunMap1 = lists:foldl( RunMapFun, #{}, maps:keys( RunMap ) ),

  State1 = gobble_queue( {Nslot, Queue1, RunMap1} ),

  {noreply, State1}.
  

%% handle_commit/3
%
handle_commit( _TransId, _Reply, State ) ->
  {noreply, State}.


%% handle_info/2
%
handle_info( {finished, Pid, Result, Output}, {Nslot, Queue, RunMap} ) ->

  % retrieve transaction id
  TransId = maps:get( Pid, RunMap ),

  % commit transaction
  tract:commit( TransId, {finished, Result, Output} ),

  % remove pid from run map
  NewRunMap = maps:remove( Pid, RunMap ),
		       
  % update run map and attempt to start new ticket
  NewState = gobble_queue( {Nslot, Queue, NewRunMap} ),
  
  {noreply, NewState};

handle_info( {failed, Pid, Output}, {Nslot, Queue, RunMap} ) ->

  % retrieve transaction id
  TransId = maps:get( Pid, RunMap ),

  % commit transaction
  tract:commit( TransId, {failed, Output} ),

  % remove pid from run map
  NewRunMap = maps:remove( Pid, RunMap ),
    
  % update run map and attempt to start new ticket
  NewState = gobble_queue( {Nslot, Queue, NewRunMap} ),
  
  {noreply, NewState}.



%% init/1
%
init( Nslot ) when Nslot > 0 ->
  {ok, {Nslot, [], #{}}}.





%% ============================================================
%% API Functions
%% ============================================================

%% nslot/1
%
nslot( Pid ) -> gen_server:call( Pid, nslot ).

%% nslot/2
%
nslot( Pid, N ) -> gen_server:call( Pid, {nslot, N} ).

%% start_link/0
%
start_link() ->

  Ncore = erlang:system_info( logical_processors_available ),

  gen_server:start_link( {local, cre_work},
                         ?MODULE,
                         Ncore,
                         [] ).



%% ============================================================
%% Internal Functions
%% ============================================================



      
%% gobble_queue/1
%
gobble_queue( State={_Nslot, [], _RunMap} ) ->
  State;
  
gobble_queue( State={Nslot,
                     [{TransId, Lang, Script, Dir, OutList, ParamMap, TypeMap}|Rest],
                     RunMap} ) ->

  % check if the maximum number of processes is already reached
  case maps:size( RunMap ) >= Nslot of
  
    true ->
    
      % no more processes can be allocated
      
      State;
      
    false ->
    
      % start ticket
      Pid = effi:spawn_link_run( Lang, Script, Dir, OutList, ParamMap, TypeMap ),

      % store pid and transactioni id in runmap
      NewRunMap = RunMap#{Pid => TransId},

      gobble_queue( {Nslot, Rest, NewRunMap} )

  end.
  

%% ============================================================
%% Deprecated Functions
%% ============================================================

  
%% probe_precond/3
%
probe_precond( InParam, Dir, Binding ) ->

  % check whether directory exists
  case file:read_file_info( Dir ) of
  
    {error, enoent} ->
      {err, Dir};

    {ok, _} ->
    
      % directory exists
      probe_param( InParam, Dir, Binding )
  end.

%% probe_param/3
%
probe_param( [], _Dir, _Binding ) -> ok;

probe_param( [{param, {name, _Name, false}, _IsList}|R], Dir, Binding ) ->
  probe_param( R, Dir, Binding );

probe_param( [{param, {name, Name, true}, _IsList}|R], Dir, Binding ) ->
  V = maps:get( Name, Binding ),
  case probe_str( V, Dir ) of
  
    ok -> probe_param( R, Dir, Binding );
    {err, F} -> {err, F}
  end.

%% probe_str/2
%
probe_str( [], _Dir ) -> ok;

probe_str( [{str, _Line, S}|R], Dir ) ->

  F = string:join( [Dir, S], "/" ),
  
  case file:read_file_info( F ) of
  
    {ok, _} ->
      probe_str( R, Dir );
      
    {error, enoent} ->
      {err, F}
  end.
  


%% is_dirinuse/3
%%
%% @doc Checks if there is another request currently running (or queued)
%%      using the specified directory.
%%
is_dirinuse( Directory, Queue, RunMap ) ->

  Pred1 = fun( {_Pid, Dir, _Ticket} ) -> Dir =:= Directory end,
  
  Pred2 = fun( Port ) ->
            {execinfo, _, Dir, _, _, _, _, _} = maps:get( Port, RunMap ),
            Dir =:= Directory
          end,

  lists:any( Pred1, Queue ) orelse lists:any( Pred2, maps:keys( RunMap ) ).

  

