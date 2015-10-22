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

-behaviour( gen_server ).

-include( "cre_work.hrl" ).

-export( [code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
          terminate/2] ).
          
-export( [add_ticket/3, ls/1, nslot/1, nslot/2, remove_ticket/2, start_link/0,
          stop/1] ).



-define( CFMSG, "<CFMSG> " ).
-define( COLON, ":" ).
-define( COMMA, "," ).
-define( MAXPROC, 8 ).



-import( maps, [get/2, is_key/2, keys/1, remove/2] ).
-import( lists, [any/2, flatten/1, foldl/3, foreach/2, member/2, reverse/1] ).
-import( string, [join/2, substr/3, tokens/2] ).
-import( file, [read_file_info/1] ).
-import( erlang, [system_info/1] ).


% SERVER CALLBACK FUNCTIONS

%% code_change/3
%
code_change( _OldVsn, State, _Extra ) -> {ok, State}.


%% handle_call/3
%
handle_call( stop, _From, State ) ->
  {stop, normal, ok, State};
  
  
handle_call( ls, _From, State={_Nslot, Queue, RunMap} ) ->

  Fun = fun( Port, Acc ) ->
          {execinfo, _, _, T, _, _, _, _} = get( Port, RunMap ),
          [T|Acc]
        end,
        
  Ls = Queue++foldl( Fun, [], keys( RunMap ) ),
        
  {reply, {ls, Ls}, State};
  
handle_call( nslot, _From, State={Nslot, _Queue, _RunMap} ) ->
  {reply, {nslot, Nslot}, State};
  
handle_call( {nslot, N}, _From, {_Nslot, Queue, RunMap} ) when N > 0 ->

  % try to fill up empty slots
  State1 = gobble_queue( {N, Queue, RunMap} ),

  {reply, ok, State1};


handle_call( {add, Ticket, Dir}, {Pid, _Tag}, {Nslot, Queue, RunMap} ) ->


    
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
                
                  destroy_port( Port ),
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
handle_info( {Port, {data, {eol, ?CFMSG++Assoc}}}, {Nslot, Queue, RunMap} ) ->

  % parse the received association
  {Name, V} = parse_assoc( Assoc ),
  
  % retrieve exec info
  {execinfo, Pid, Dir, Ticket={ticket, Line, _, _, _}, Interpreter, Script, Output, Result} =
    get( Port, RunMap ),
  
  % update run map
  RunMap1 = RunMap#{ Port := {execinfo, Pid,
                                        Dir,
                                        Ticket,
                                        Interpreter,
                                        Script, Output, Result#{Name => [{str, Line, X} || X <- V]}}},
  
  {noreply, {Nslot, Queue, RunMap1}};
  
  
handle_info( {Port, {data, {noeol, ?CFMSG++Assoc}}}, {Nslot, Queue, RunMap} ) ->

  error( cfmsg_too_large );  
  
  
handle_info( {Port, {data, {Eol, Line}}}, {Nslot, Queue, RunMap} ) ->

  io:format( "Received: ~p", [Line] ),


  % retrieve exec info
  {execinfo, Pid, Dir, Ticket, Interpreter, Script, Output, Result} =
    get( Port, RunMap ),
    
  Suffix = list_to_binary( Line ),
  
  % update run map
  RunMap1 = RunMap#{ Port := {execinfo, Pid,
                                        Dir,
                                        Ticket,
                                        Interpreter,
                                        Script,
                                        adjoin( Output, Suffix, Eol ),
                                        Result}},
  
  {noreply, {Nslot, Queue, RunMap1}};
  
  
handle_info( {Port, {exit_status, 0}}, {Nslot, Queue, RunMap} ) ->

  io:format( "Port ~p terminated normally.", [Port] ),


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
  
  
handle_info( {Port, {exit_status, _Status}}, {Nslot, Queue, RunMap} ) ->

  io:format( "Port ~p terminated with error.", [Port] ),

  % retrieve exec info
  {execinfo, Pid, Dir, Ticket, Interpreter, Script, Output, _Result} =
    get( Port, RunMap ),
  
  % notify client
  Pid ! {failed, Dir, Ticket, Interpreter, Script, Output},

  % update run map and attempt to start new ticket
  State1 = gobble_queue( {Nslot, Queue, remove( Port, RunMap )} ),
  
  {noreply, State1};
  
  
handle_info( Msg={'EXIT', Port, normal}, State ) when is_port( Port ) ->
 io:format( "~p~n", [Msg] ),
 {noreply, State}.


%% init/1
%
init( Nslot ) when Nslot > 0 ->
  process_flag( trap_exit, true ),
  {ok, {Nslot, [], #{}}}.


%% terminate/2
%
terminate( _Reason, {_Nslot, _Queue, RunMap} ) ->

  % destroy all ports
  foreach( fun destroy_port/1, keys( RunMap ) ),
  
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


adjoin( Output, Suffix, eol ) ->
  <<Output/binary, Suffix/binary, "\n">>;
  
adjoin( Output, Suffix, noeol ) ->
  <<Output/binary, Suffix/binary>>.

%% prepare/3
%
prepare( {ticket, _, {sign, OutList, [], InList},
         {forbody, Lang, Body}, Binding} ) ->

  Fun1 = fun( {param, {name, Name, _IsFile}, IsList}, Acc ) ->
           Acc++get_assignment( Lang, Name, IsList, get( Name, Binding ) )
         end,
        
  Prefix = foldl( Fun1, "", InList ),
  
  
  Fun2 = fun( {param, {name, Name, _IsFile}, IsList}, Acc ) ->
           Acc++get_dismissal( Lang, Name, IsList )
         end,
  
  Suffix = foldl( Fun2, "", OutList ),

  {get_interpreter( Lang ),
   flatten(
     [get_prolog( Lang ), "\n",
      Prefix, "\n", Body, "\n\n", Suffix, "\n", get_epilog( Lang)] )}.

      
%% get_interpreter/1
%
get_interpreter( bash ) -> "bash";              % bash %%%%%%%%%%%%%%%%%%%%%%%%%
get_interpreter( r ) -> "Rscript --vanilla -";  % r %%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_interpreter( python ) -> "python";          % python %%%%%%%%%%%%%%%%%%%%%%%
get_interpreter( scala ) -> "scala";
get_interpreter( java ) -> "bsh".
      
%% get_prolog/1
%
get_prolog( bash ) -> "set -eu -o pipefail\n";  % bash %%%%%%%%%%%%%%%%%%%%%%%%%
get_prolog( r ) -> "";                          % r %%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_prolog( python ) -> "".                     % python %%%%%%%%%%%%%%%%%%%%%%%
      

%% get_epilog/1
%
get_epilog( bash ) -> "";                       % bash %%%%%%%%%%%%%%%%%%%%%%%%%
get_epilog( r ) -> "";                          % r %%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_epilog( python ) -> "".                     % python %%%%%%%%%%%%%%%%%%%%%%%


%% get_assignment/4
%
get_assignment( bash, Name, false, Value ) ->   % bash %%%%%%%%%%%%%%%%%%%%%%%%%
  flatten( [Name, $=, list_quoted( Value ), $\n] );
  
get_assignment( bash, Name, true, Value ) ->
  flatten( [Name, "=(", list_quoted( Value ), ")\n"] );
  

get_assignment( r, Name, false, Value ) ->      % r %%%%%%%%%%%%%%%%%%%%%%%%%%%%
  flatten( [Name, $=, list_quoted( Value ), $\n] );
    
get_assignment( r, Name, true, Value ) ->
  flatten( [Name, "=c(", list_quoted( Value ), ")\n"] );
  
  
get_assignment( python, Name, false, Value ) -> % python %%%%%%%%%%%%%%%%%%%%%%%
  flatten( [Name, $=, list_quoted( Value ), $\n] );
  
get_assignment( python, Name, true, Value ) ->
  flatten( [Name, "=[", list_quoted( Value ), "]\n"] ).

  
%% get_dismissal/3
%
get_dismissal( bash, Name, false ) ->           % bash %%%%%%%%%%%%%%%%%%%%%%%%%
  "echo \""++?CFMSG++Name++?COLON++"[\\\"$"++Name++"\\\"]\"\n";

get_dismissal( bash, Name, true ) ->
  "TMP=`printf \""++?COMMA++"\\\"%s\\\"\" ${"++Name++
  "[@]}`\nTMP=${TMP:1}\necho \""++?CFMSG++Name++?COLON++"[$TMP]\"\n";

  
get_dismissal( r, Name, false ) ->              % r %%%%%%%%%%%%%%%%%%%%%%%%%%%%
  "cat(\""++?CFMSG++Name++?COLON++"[\","++Name++",\"]\\n\")\n";
  
get_dismissal( r, Name, true ) ->
  "cat(\""++?CFMSG++Name++?COLON++"[\",paste(\"\\\"\","++Name++
  ",\"\\\"\",collapse=\",\",sep=\"\"),\"]\\n\",sep=\"\")\n";
  
  
get_dismissal( python, Name, false ) ->         % python %%%%%%%%%%%%%%%%%%%%%%%
  "print(\""++?CFMSG++Name++?COLON++"[\\\"\"+"++Name++"+\"\\\"]\\n\")\n";

get_dismissal( python, Name, true ) ->
  "print(\""++?CFMSG++Name++?COLON++"[\"+\",\".join("++Name++")+\"]\\n\")\n".
  

%% list_quoted/1
%
list_quoted( Value ) ->
  list_quoted( Value, [] ).
  
list_quoted( [], Acc ) ->
  flatten( reverse( Acc ) );
  
list_quoted( [{str, _, V}|R], "" ) ->
  list_quoted( R, [$",V,$"] );
  
list_quoted( [{str, _, V}|R], Acc ) ->
  list_quoted( R, [[$",V,$"], ",",Acc] ).
  

%% parse_assoc/1
%
parse_assoc( Assoc ) ->
  [Name, S1] = tokens( Assoc, ?COLON ),
  S2 = substr( S1, 2, length( S1 )-2 ),
  L1 = tokens( S2, ?COMMA ),
  L2 = [substr( S, 2, length( S )-2 ) || S <- L1],
  {Name, L2}.

  
%% run/1
%
run( Interpreter, Script, Dir ) ->

  % run ticket
  io:format( "Opening ~s port in ~s.~n", [Interpreter, Dir] ),
  Port = open_port( {spawn, Interpreter},
                    [exit_status,
                     stderr_to_stdout,
                     {cd, Dir},
                     {line, 1000000}] ),
  io:format( "Sending data:~n~s~n", [Script] ),
  Port ! {self(), {command, Script}},

  io:format( "Done creating port.~n" ),

  Port.

%% gobble_queue/1
%
gobble_queue( State={_Nslot, [], _RunMap} ) ->
  State;
  
gobble_queue( State={Nslot,
                     [{Pid,
                       Dir,
                       Ticket={ticket, _, {sign, _, [], InParam},
                                       _, Binding}}|Rest],
                     RunMap} ) ->

  % check if the maximum number of processes is already reached
  case maps:size( RunMap ) >= Nslot of
  
    true ->
    
      % no more processes can be allocated
      
      State;
      
    false ->
    
      % there is still room
      


      % prepare interpreter and script
      {Interpreter, Script} = prepare( Ticket ),

      % check pre-conditions
      case probe_precond( InParam, Dir, Binding ) of
  
        {err, S} ->
    
          % notify caller that the ticket has failed
          Pid ! {failed, Dir,
                         Ticket,
                         Interpreter,
                         Script,
                         list_to_binary( io_lib:format( "Input contract: File or directory '~s' does not exist.\n", [S] ) )},
      
          gobble_queue( {Nslot, Rest, RunMap} );
      
        ok ->
    
          % start ticket
          Port = run( Interpreter, Script, Dir ),

          % create exec info entry in run map
          RunMap1 = RunMap#{Port => {execinfo, Pid,
                                               Dir,
                                               Ticket,
                                               Interpreter, Script, <<>>, #{}}},

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
  
%% destroy_port/1
%
destroy_port( Port ) when is_port( Port ) ->

  % linux specific code %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  {os_pid, OsPid} = erlang:port_info( Port, os_pid ),
                  
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%
  Port ! {self(), close},    %
  receive                    %
    {Port, closed} -> ok     %
  end,                       %
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  _ = os:cmd( io_lib:format( "kill -9 ~p `pgrep -P ~p`", [OsPid, OsPid] ) ),
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ok.
          

