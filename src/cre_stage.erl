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

-module( cre_stage ).
-behaviour( gen_server ).

-export( [add_ticket/2, code_change/3, handle_call/3, handle_cast/2,
          handle_info/2, init/1, ls/1, start_link/0, stop/1, terminate/2] ).


-import( file, [del_dir/1, delete/1, list_dir/1, make_dir/1, make_link/2,
                read_file_info/1] ).
-import( filelib, [ensure_dir/1, is_dir/1] ).
-import( gen_server, [call/2] ).
-import( lists, [flatten/1, foldl/3, foreach/2, last/1] ).
-import( maps, [get/2, keys/1, remove/2, values/1] ).
-import( os, [cmd/1] ).
-import( string, [join/2, tokens/2] ).

-define( BASEDIR, "/tmp/base" ).
-define( REPODIR, "/tmp/repo" ).
-define( RNDNUM, random:uniform( 9223372036854775807 ) ).
          

% SERVER CALLBACK FUNCTIONS

%% code_change/3
%
code_change( _OldVsn, State, _Extra ) -> {ok, State}.


%% handle_call/3
%
handle_call( stop, _From, State ) ->

  {stop, normal, ok, State};
  
handle_call( ls, _From, State={_Basedir, _Repodir, _Nextid, RunMap} ) ->
  {reply, [Ticket || {execinfo, _, _, Ticket} <- values( RunMap )], State};
  
handle_call( {add, Ticket, Datadir}, {Pid, _Tag}, {Basedir, Repodir, Nextid, RunMap} ) ->

  io:format( "STAGE receiving ticket ~p~n", [Ticket] ),

  Dir = flatten( io_lib:format( "~s/~p", [Basedir, Nextid] ) ),
  
  % create links to input data
  Ticket1 = prepare_dir( Dir, Repodir, Datadir, Ticket ),

  % update run map
  RunMap1 = RunMap#{Ticket1 => {execinfo, Pid, Dir, Ticket}},

  % submit job
  ok = call( cre_work, {add, Ticket1, Dir} ),
  
  
  {reply, ok, {Basedir, Repodir, Nextid+1, RunMap1}};
  
handle_call( {remove, Ticket}, _From, {Basedir, Repodir, Nextid, RunMap} ) ->

  Fun = fun( Ticket1, Acc ) ->
          {execinfo, _, _, Original} = get( Ticket1, RunMap ),
          case Original =:= Ticket of
            true ->
              gen_server:call( cre_work, {remove, Ticket1} ),
              remove( Ticket1, Acc );
            false -> Acc
          end
        end,
        
  RunMap1 = foldl( Fun, RunMap, keys( RunMap ) ),
  
  {reply, ok, {Basedir, Repodir, Nextid, RunMap1}}.
  


%% handle_cast/2
%
handle_cast( _Request, State ) -> {noreply, State}.


%% handle_info/2
%
handle_info( {finished, Ticket1, _Output, Result},
             {Basedir, Repodir, Nextid, RunMap} ) ->

  % retrieve info
  {execinfo, Pid, Dir, Ticket} = get( Ticket1, RunMap ),
  {ticket, Line, {sign, OutParam, [], _InParam}, _ForBody, _Binding} = Ticket,
  
  % move result files to repo
  Result1 = postprocess_dir( Dir, Repodir, OutParam, Result ),
  
  io:format( "STAGE returning result ~p~n", [Result] ),
  
  % deliver finished message
  Pid ! {finished, Ticket, Result1},

  % remove ticket from run map
  RunMap1 = remove( Ticket1, RunMap ),  

  {noreply, {Basedir, Repodir, Nextid, RunMap1}};

handle_info( {failed, Dir, Ticket1, Interpreter, Script, Output},
             {Basedir, Repodir, Nextid, RunMap} ) ->

  % retrieve info
  {execinfo, Pid, Dir, Ticket} = get( Ticket1, RunMap ),
  
  % deliver failed message
  Pid ! {failed, Ticket, Interpreter, Script, Output},

  % remove ticket from run map
  RunMap1 = remove( Ticket1, RunMap ),  

  {noreply, {Basedir, Repodir, Nextid, RunMap1}}.


%% init/1
%
init( {Basedir, Repodir} ) ->

  % delete base directory if it exists
  ok = rm_if_exists( Basedir ),
  
  % create base directory
  ok = ensure_dir( Basedir ),
  ok = make_dir( Basedir ),
  
  % delete repo directory if it exists
  ok = rm_if_exists( Repodir ),
  
  % create repo directory
  ok = ensure_dir( Repodir ),
  ok = make_dir( Repodir ),
  
  {ok, {Basedir, Repodir, 1, #{}}}.


%% terminate/2
%
terminate( _, _State ) -> ok.







% CONVENIENCE FUNCTIONS

%% start_link/0
%
start_link() ->
  gen_server:start_link( {local, cre_stage}, ?MODULE, {?BASEDIR, ?REPODIR}, [] ).

%% add_ticket/2
%
add_ticket( Pid, Ticket ) -> gen_server:call( Pid, {add, Ticket} ).

%% ls/1
%
ls( Pid ) -> gen_server:call( Pid, ls ).

%% stop/1
%
stop( Pid ) -> gen_server:call( Pid, stop ).


% SERVER OPERATIONS

%% prepare_dir/3
%
prepare_dir( Dir, Repodir, Datadir, {ticket, Line, {sign, OutParam, [], InParam}, ForBody, Binding} ) ->

  ok = make_dir( Dir ),
        
  Binding1 = foldl( fun( P, Acc ) ->
                      process_param( P, Acc, Dir, [Repodir, Datadir], Binding )
                    end,
                    #{}, InParam ),
  
  {ticket, Line, {sign, OutParam, [], InParam}, ForBody, Binding1}.


postprocess_dir( Dir, Repodir, OutParam, Result ) ->

  % link into repository directory
  foldl( fun( P, Acc ) ->
                    process_param( P, Acc, Repodir, [Dir], Result )
                  end,
                  #{}, OutParam ).
  
  
  
%% process_param/6
%
process_param( {param, {name, Name, false}, _IsList}, Acc, _Dest, _SrcList, Binding ) ->
  Acc#{Name => get( Name, Binding )};
  
process_param( {param, {name, Name, true}, _IsList}, Acc, Dest, SrcList, Binding ) ->
  
  V = get( Name, Binding ),
  V1 = [rebase_expr( E, Dest, SrcList ) || E <- V],

  Acc#{Name => V1}.
  

%% rebase_expr/4
%
rebase_expr( {str, Line, S}, Dest, SrcList ) ->

  {ok, Filename} = locate( S, SrcList ),
  S1 = flatten( io_lib:format( "~p_~s", [?RNDNUM, basename( Filename )] ) ),
  Filename1 = join( [Dest, S1], "/" ),
  ok = make_link( Filename, Filename1 ),
  {str, Line, S1}.
  


% HELPER FUNCTIONS

%% rm_if_exists/1
%
rm_if_exists( Dir ) ->
  rm( [Dir] ).

%% rm/1
%
rm( [] ) ->
  ok;
  
rm( [H|T] ) when is_list( H ) ->

  case is_dir( H ) of
  
    true ->
    
      {ok, L} = list_dir( H ),
      case rm( [join( [H, F], "/" ) || F <- L] ) of
        ok ->
          case del_dir( H ) of
            ok -> rm( T );
            X -> X
          end;
        Y -> Y
      end;
      
    false ->
    
      case delete( H ) of 
        ok -> rm( T );
        {error, enoent} -> rm( T );
        Z -> Z
      end
  end.

  
%% locate/2
%
locate( _Suffix, [] ) ->
  {error, enoent};
  
locate( Suffix, [Prefix|Rest] ) ->

  Filename = join( [Prefix, Suffix], "/" ),
  
  case read_file_info( Filename ) of
    {ok, _} -> {ok, Filename};
    {error, enoent} -> locate( Suffix, Rest )
  end.

%% basename/1
%
basename( Filename ) ->
  last( tokens( Filename, "/" ) ).
  

  

