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
%% Includes
%% ============================================================

-include( "cre_stage.hrl" ).


%% ============================================================
%% Tract Function Exports
%% ============================================================

-export( [handle_abort/2, handle_call/3, handle_commit/4, handle_info/2, handle_recv/3, init/1] ).


%% ============================================================
%% API Exports
%% ============================================================

-export( [start_link/0] ).

          
%% ============================================================
%% Server Callback Functions
%% ============================================================


%% handle_call/3
%%
handle_call( _Request, _From, State ) ->
  {reply, ok, State}.


%% handle_recv/3
%%
handle_recv( ToStageId,
             {add, {ticket, Line, Sign, {forbody, Lang, Script}, Binding}, DataDir},
             {BaseDir, RepoDir, NextId, RunMap} ) ->

  Dir = string:join( [BaseDir, NextId], "/" ),
  Binding1 = prepare_dir( Dir, RepoDir, DataDir, Sign, Binding ),
  OutNameList = outname_list( Sign ),
  TypeMap = type_map( Sign ),
  ParamMap = param_map( Sign, Binding1 ),
  NewRunMap = RunMap#{ToStageId => {Line, Sign, Dir}},

  % send request to worker
  _ToWorkId = tract:send_reschedule(
               cre_work,
               {add, Lang, Script, Dir, OutNameList, ParamMap, TypeMap},
               ToStageId ),

  {noreply, {BaseDir, RepoDir, NextId+1, NewRunMap}}.


%% handle_abort/2
%%
handle_abort( ToStageId, {BaseDir, RepoDir, NextId, RunMap} ) ->

  NewRunMap = maps:remove( ToStageId, RunMap ),

  {noreply, {BaseDir, RepoDir, NextId, NewRunMap}}.
  

%% handle_commit/4
%%
handle_commit( _ToWorkId, ToStageId, {finished, Result, Output}, {BaseDir, RepoDir, NextId, RunMap} ) ->

  {Line, Sign, Dir} = maps:get( ToStageId, RunMap ),
  {sign, OutParam, [], _InParam} = Sign,
  Binding = binding_map( Line, Sign, Result ),
  Binding1 = postprocess_dir( Dir, RepoDir, OutParam, Binding ),
  NewRunMap = maps:remove( ToStageId, RunMap ),
  
  % commit finished ticket
  ok = tract:commit( ToStageId, {finished, Binding1, Output} ),

  {noreply, {BaseDir, RepoDir, NextId, NewRunMap}};


handle_commit( _ToWorkId, ToStageId, {failed, Output}, {BaseDir, RepoDir, NextId, RunMap} ) ->

  NewRunMap = maps:remove( ToStageId, RunMap ),

  % commit failed ticket
  tract:commit( ToStageId, {failed, Output} ),

  {noreply, {BaseDir, RepoDir, NextId, NewRunMap}}.


%% handle_info/2
%%
handle_info( _Info, State ) ->
  {noreply, State}.


%% init/1
%%
init( {Basedir, Repodir} ) ->

  % delete base directory if it exists
  ok = rm_if_exists( Basedir ),
  
  % create base directory
  ok = filelib:ensure_dir( Basedir ),
  ok = file:make_dir( Basedir ),
  
  % delete repo directory if it exists
  ok = rm_if_exists( Repodir ),
  
  % create repo directory
  ok = filelib:ensure_dir( Repodir ),
  ok = file:make_dir( Repodir ),
  
  {ok, {Basedir, Repodir, 1, #{}}}.


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
prepare_dir( Dir, Repodir, Datadir, {sign, _OutParam, [], InParam}, Binding ) ->

  ok = file:make_dir( Dir ),

  lists:foldl( fun( P, Acc ) ->
                 process_param( P, Acc, Dir, [Repodir, Datadir], Binding )
               end,
               #{}, InParam ).


postprocess_dir( Dir, Repodir, OutParam, Result ) ->

  % link into repository directory
  lists:foldl( fun( P, Acc ) ->
                 process_param( P, Acc, Repodir, [Dir], Result )
               end,
               #{}, OutParam ).
  
  
  
%% process_param/6
%
process_param( {param, {name, Name, false}, _IsList}, Acc, _Dest, _SrcList, Binding ) ->
  Acc#{Name => maps:get( Name, Binding )};
  
process_param( {param, {name, Name, true}, _IsList}, Acc, Dest, SrcList, Binding ) ->
  
  V = maps:get( Name, Binding ),
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

  Filename = string:join( [Prefix, Suffix], "/" ),
  
  case file:read_file_info( Filename ) of
    {ok, _} -> {ok, Filename};
    {error, enoent} -> locate( Suffix, Rest )
  end.


%% outname_list/1
%%
outname_list( {sign, OutList, [], _InList} ) ->

  AccOutnameList = fun( {param, {name, Name, _}, _}, NameList ) ->
                     [Name|NameList]
                   end,

  lists:foldl( AccOutnameList, [], OutList ).


%% param_map/2
%%
param_map( {sign, _OutList, [], InList}, Binding ) ->

  AccParamMap = fun( {param, {name, Name, _IsFile}, _IsList}, ParamMap ) ->

                  StrList = maps:get( Name, Binding ),
                  SimpleStrList = [StrValue || {str, _Line, StrValue} <- StrList],

                  ParamMap#{Name => SimpleStrList}

                end,

  list:foldl( AccParamMap, #{}, InList ).


%% type_map/1
%%
type_map( {sign, OutList, [], InList} ) ->

  AccTypeMap = fun( {param, {name, Name, _IsFile}, IsList}, TypeMap ) ->
                 TypeMap#{Name => IsList}
               end,

  lists:foldl( AccTypeMap, #{}, OutList++InList ).


%% binding_map/3
%%
binding_map( Line, {sign, OutList, [], _InList}, Result ) ->

  AccBindingMap = fun( {param, {name, Name, _IsFile}, _IsList}, Binding ) ->

                    SimpleStrList = maps:get( Name, Result ),
                    StrList = [{str, Line, Value} || Value <- SimpleStrList],
                    
                    Binding#{Name => StrList}

                  end,

  lists:foldl( AccBindingMap, #{}, OutList ).
  
