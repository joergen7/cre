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
-author( "Jörgen Brandt <brandjoe@hu-berlin.de>" ).

-behaviour( tract ).


%% ============================================================
%% Tract Function Exports
%% ============================================================

-export( [] ).


%% ============================================================
%% API Exports
%% ============================================================

-export( [] ).

          
%% ============================================================
%% Server Callback Functions
%% ============================================================


%% handle_call/3
%%
handle_call( _Request, _From, State ) ->
  {reply, ok, State}.

  
handle_recv( ToStageId,
             {add, {ticket, Line, Sign, {forbody, Lang, Script}, Binding}, Datadir},
             {BaseDir, RepoDir, NextId, SignMap} ) ->

  % derive working directory    
  Dir = string:join( [BaseDir, NextId], "/" ),
  Binding1 = prepare_dir( Dir, RepoDir, DataDir, Sign, Binding ),
  OutNameList = outname_list( Sign ),
  TypeMap = type_map( Sign ),
  ParamMap = param_map( Sign, Binding1 ),
  ToWorkId = send_reschedule( cre_work, {add, Lang, Script, Dir, OutNameList, ParamMap, TypeMap}, TransId )
  NewSignMap = SignMap#{ToStageId => {Line, Sign}},

  {noreply, {BaseDir, RepoDir, NextId+1, NewSignMap}}.


handle_abort( ToStageId, {BaseDir, RepoDir, NextId, SignMap ) ->

  NewSignMap = maps:remove( ToStageId, SignMap )

  {noreply, {BaseDir, RepoDir, NextId, NewSignMap}}.
  

handle_commit( ToWorkId, ToStageId, {finished, Result, Output}, {BaseDir, RepoDir, NextId, SignMap} ) ->

  {Line, Sign} = maps:get( ToStageId, SignMap ),
  {sign, OutParam, [], _InParam} = Sign,
  Binding = binding_map( Sign, Result, Line ),
  Binding1 = postprocess_dir( Dir, RepoDir, OutParam, Binding )  
  NewSignMap = maps:remove( ToStageId, SignMap ),
  
  tract:commit( ToStageId, {finished, Binding1, Output} ),

  {noreply, {BaseDir, RepoDir, NextId, NewSignMap}};


handle_commit( ToWorkId, ToStageId, {failed, Output}, {BaseDir, RepoDir, NextId, SignMap} ) ->

  NewSignMap = maps:remove( ToStageId, SignMap ),

  tract:commit( ToStageId, {failed, Output} ),

  {noreply, {BaseDir, RepoDir, NextId, NewSignMap}}.




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






%% ============================================================
%% API Functions
%% ============================================================

%% start_link/0
%%
start_link() ->
  gen_server:start_link( {local, cre_stage}, ?MODULE, {?BASEDIR, ?REPODIR}, [] ).


%% ============================================================
%% Internal Functions
%% ============================================================

%% prepare_dir/4
%
prepare_dir( Dir, Repodir, Datadir, {sign, _OutParam, [], InParam} Binding ) ->

  ok = make_dir( Dir ),
        
  foldl( fun( P, Acc ) ->
           process_param( P, Acc, Dir, [Repodir, Datadir], Binding )
         end,
         #{}, InParam ).


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
  S1 = string:join( [?RNDNUM, filename:basename( Filename )], "_" ),
  Filename1 = string:join( [Dest, S1], "/" ),
  ok = file:make_link( Filename, Filename1 ),
  {str, Line, S1}.
  

%% rm_if_exists/1
%
rm_if_exists( Ref ) ->

  case filelib:is_dir( Ref ) of

    true ->

      {ok, L} = file:list_dir( Ref ),
      ok = lists:foreach( fun rm_if_exists/1, L ),
      file:del_dir( Ref );

    false ->

      case file:delete( Ref ) of
	ok -> ok;
	enoent -> ok;
	Reason -> error( Reason )
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

  

  

%% outname_list/1
%%
outname_list( {sign, OutList, [], _InList} ) ->
  lists:foldl( fun acc_outname_list/2, [], OutList ).


%% acc_outname_list/2
%%
acc_outname_list( {param, {name, Name, _}, _}, NameList ) ->
  [Name|NameList].


%% param_map/2
%%
param_map( {sign, _OutList, [], InList}, Binding ) ->
  list:foldl( fun acc_param_map/2, #{}, InList ).


%% acc_param_map/2
%%
acc_param_map( {param, {name, Name, }, ParamMap ) ->

  StrList = maps:get( Name, Binding ),
  SimpleStrList = [StrValue || {str, _Line, StrValue} <- StrList],

  ParamMap#{Name => SimpleStrList}.
 

%% type_map/1
%%
type_map( {sign, OutList, [] InList} ) ->
  lists:foldl( fun acc_type_map/2, #{}, OutList++InList ).


%% acc_type_map/2
%%
acc_type_map( {param, {name, Name, _IsFile}, IsList}, TypeMap ) ->
  TypeMap#{Name => IsList}.
    

