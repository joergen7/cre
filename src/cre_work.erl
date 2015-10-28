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

-export( [add_ticket/3, ls/1, nslot/1, nslot/2, remove_ticket/2, start_link/0,
          stop/1] ).


%% ============================================================
%% Tract Function Exports
%% ============================================================

-export( [code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
          terminate/2] ).
          

%% ============================================================
%% Tract Functions
%% ============================================================



  
handle_call( nslot, _From, State={Nslot, _Queue, _RunMap} ) ->
  {reply, {nslot, Nslot}, State};


handle_call( {nslot, N}, _From, {_Nslot, Queue, RunMap} ) when N > 0 ->

  % try to fill up empty slots
  State1 = gobble_queue( {N, Queue, RunMap} ),

  {reply, ok, State1}.


handle_recv( {add, Ticket, Dir}, {Pid, _Tag}, {Nslot, Queue, RunMap} ) ->


    
  % check whether directory is in use
  case is_dirinuse( Dir, Queue, RunMap ) of
  
    true ->
    
      error( {dir_is_in_use, Dir} );
      
    false ->

      % directory is not in use
        
      % add ticket to queue and attempt to start it
      State1 = gobble_queue( {Nslot, [{Pid, Dir, Ticket}|Queue], RunMap} ),
  
      {reply, ok, State1}
      
  end;
  
handle_call( {remove, Ticket}, _From, {Nslot, Queue, RunMap} ) ->

  case member( Ticket, Queue ) of
  
    true ->
      {reply, ok, {Nslot, Queue--[Ticket], RunMap}};
      
    false ->
      
      Fun = fun( Port, Acc ) ->
      
              Execinfo = get( Port, RunMap ),
              {execinfo, _, _, T, _, _, _, _} = Execinfo,
              
              case T =:= Ticket of
              
                false ->
                
                  Acc#{Port => Execinfo};
                  
                true ->
                
                  effi:destroy_port( Port ),
                  Acc
                
                  
              end
            end,
      
      % kill ticket instances and update run map
      RunMap1 = foldl( Fun, #{}, keys( RunMap ) ),
      
      % start new tickets if available
      State1 = gobble_queue( {Nslot, Queue, RunMap1} ),
      
      {reply, ok, State1}
  end.


%% handle_cast/2
%
handle_cast( _Request, State ) -> {noreply, State}.


%% handle_info/2
%
handle_info( {finished, Result, Out}, {Nslot, Queue, RunMap} ) ->
		   
  % retrieve exec info
  {execinfo, Pid,
             Dir,
             Ticket={ticket, _, {sign, OutParam, [], _}, _, _},
             Interpreter, Script, Output, Result} = get( Port, RunMap ),
  
  % check post-conditions
  case probe_param( OutParam, Dir, Result ) of

    {err, S} ->
    
      Suffix = list_to_binary( ["\nOutput contract: File '",
                                        S, "' does not exist.\n"] ),
      
      % notify client about failure
      Pid ! {failed, Dir,
                     Ticket,
                     Interpreter,
                     Script,
                     <<Output/binary, Suffix/binary>>},
      ok;
  
    ok ->
    
      io:format( "WORK returning result ~p~n", [Result] ),
  
      % notify client about success
      Pid ! {finished, Ticket, Output, Result},
      ok
  end,
    
  % update run map and attempt to start new ticket
  State1 = gobble_queue( {Nslot, Queue, remove( Port, RunMap )} ),
  
  {noreply, State1};

handle_info( {failed, Result, Output}, {Nslot, Queue, RunMap} ) ->

  % retrieve exec info
  {execinfo, Pid,
             Dir,
             Ticket={ticket, _, {sign, OutParam, [], _}, _, _},
             Interpreter, Script, Output, Result} = get( Port, RunMap ),

  % notify client about failure
  Pid ! {failed, Dir,
                 Ticket,
                 Interpreter,
                 Script,
                 Output},
    
  % update run map and attempt to start new ticket
  State1 = gobble_queue( {Nslot, Queue, remove( Port, RunMap )} ),
  
  {noreply, State1}.



%% init/1
%
init( Nslot ) when Nslot > 0 ->
  process_flag( trap_exit, true ),
  {ok, {Nslot, [], #{}}}.


%% terminate/2
%
terminate( _Reason, {_Nslot, _Queue, RunMap} ) ->

  % destroy all ports
  foreach( fun effi:destroy_port/1, keys( RunMap ) ),
  
  ok.



% CONVENIENCE FUNCTIONS

%% add_ticket/3
%
add_ticket( Pid, Ticket, Dir ) -> gen_server:call( Pid, {add, Ticket, Dir} ).

%% ls/1
%
ls( Pid ) -> gen_server:call( Pid, ls ).

%% nslot/1
%
nslot( Pid ) -> gen_server:call( Pid, nslot ).

%% nslot/2
%
nslot( Pid, N ) -> gen_server:call( Pid, {nslot, N} ).

%% remove_ticket/2
%
remove_ticket( Pid, Ticket ) -> gen_server:call( Pid, {remove, Ticket} ).

%% start_link/0
%
start_link() ->
  gen_server:start_link( {local, cre_work}, ?MODULE,
                         system_info( logical_processors_available ), [] ).

%% stop/1
%
stop( Pid ) -> gen_server:call( Pid, stop ).


% HELPER FUNCTIONS



      
%% gobble_queue/1
%
gobble_queue( State={_Nslot, [], _RunMap} ) ->
  State;
  
gobble_queue( State={Nslot,
                     [{Pid,
                       Dir,
                       Ticket={ticket, _, {sign, OutParam, [], InParam},
                                       {forbody, Lang, Script}, Binding}}|Rest],
                     RunMap} ) ->

  % check if the maximum number of processes is already reached
  case maps:size( RunMap ) >= Nslot of
  
    true ->
    
      % no more processes can be allocated
      
      State;
      
    false ->
    
      % check pre-conditions
      case probe_precond( InParam, Dir, Binding ) of
  
        {err, S} ->
    
          % notify caller that the ticket has failed
          Pid ! {failed, Dir,
                         Ticket,
                         Lang,
                         Script,
                         list_to_binary( io_lib:format( "Input contract: File or directory '~s' does not exist.\n", [S] ) )},
      
          gobble_queue( {Nslot, Rest, RunMap} );
      
        ok ->
    
          % start ticket
          ChildPid = effi:spawn_link_run( Lang, Script, Dir, OutList, ParamMap, TypeMap ),

          % create exec info entry in run map
          RunMap1 = RunMap#{ChildPid => {execinfo, Pid, Dir, Ticket},

          gobble_queue( {Nslot, Rest, RunMap1} )
      end 
  end.
  
    
%% probe_precond/3
%
probe_precond( InParam, Dir, Binding ) ->

  % check whether directory exists
  case read_file_info( Dir ) of
  
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
  V = get( Name, Binding ),
  case probe_str( V, Dir ) of
  
    ok -> probe_param( R, Dir, Binding );
    {err, F} -> {err, F}
  end.

%% probe_str/2
%
probe_str( [], _Dir ) -> ok;

probe_str( [{str, _Line, S}|R], Dir ) ->

  F = join( [Dir, S], "/" ),
  
  case read_file_info( F ) of
  
    {ok, _} ->
      probe_str( R, Dir );
      
    {error, enoent} ->
      {err, F}
  end.
  


%% is_dirinuse/3
%
is_dirinuse( Directory, Queue, RunMap ) ->

  Pred1 = fun( {_Pid, Dir, _Ticket} ) -> Dir =:= Directory end,
  
  Pred2 = fun( Port ) ->
            {execinfo, _, Dir, _, _, _, _, _} = get( Port, RunMap ),
            Dir =:= Directory
          end,

  any( Pred1, Queue ) orelse any( Pred2, keys( RunMap ) ).

  

