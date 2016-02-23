%% -*- erlang -*-
%
% Cuneiform: A Functional Language for Large Scale Scientific Data Analysis
%
% Copyright 2016 Jörgen Brandt, Marc Bux, and Ulf Leser
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
% limitations under the License.-module( refactor ).

-module( refactor ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).

%% =============================================================================
%% API exports
%% =============================================================================

-export( [get_refactoring/5, apply_refactoring/1] ).


%% =============================================================================
%% Includes
%% =============================================================================

-include( "abstract_syntax.hrl" ).

%% =============================================================================
%% API Functions
%% =============================================================================

%% get_refactoring %%

-spec get_refactoring( ParamLst, Fa0, DestDir, SrcDirLst, R ) ->
  {RefactorLst, MissingLst, Fa1}
when ParamLst    :: [param()],
     Fa0         :: #{string() => [str()]},
     DestDir     :: string(),
     SrcDirLst   :: [string()],
     R           :: pos_integer(),
     RefactorLst :: [{string(), string()}],
     MissingLst  :: [string()],
     Fa1         :: #{string() => [str()]}.

get_refactoring( ParamLst, Fa0, DestDir, SrcDirLst, R ) ->
  lists:foldl( fun( Param, AccIn ) ->
                 acc_refactoring( Param, AccIn, Fa0, DestDir, SrcDirLst, R )
               end,
               {[], [], #{}}, ParamLst ).

%% apply_refactoring %%

-spec apply_refactoring( Refactor ) -> ok
when Refactor :: {string(), string()} | [{string(), string()}].

apply_refactoring( RefactorLst ) when is_list( RefactorLst ) ->
  lists:foreach( fun apply_refactoring/1, RefactorLst );

apply_refactoring( {Existing, New} ) ->

  case filelib:ensure_dir( New ) of
    {error, R1} -> error( {R1, ensure_dir, New} );
    ok          -> ok
  end,

  case file:make_symlink( Existing, New ) of
    {error, R2} -> error( {R2, make_symlink, [Existing, New]} );
    ok          -> ok
  end.




%% =============================================================================
%% Internal Functions
%% =============================================================================

%% acc_refactoring %%

-spec acc_refactoring( Param, {RefactorLst, MissingLst, Fa1}, Fa0, DestDir,
                       SrcDirLst, R ) ->
  {RefactorLst1, MissingLst1, Fa2}
when Param        :: param(),
     RefactorLst  :: [{string(), string()}],
     MissingLst   :: [string()],
     Fa1          :: #{string() => [str()]},
     Fa0          :: #{string() => [str()]},
     DestDir      :: string(),
     SrcDirLst    :: [string()],
     R            :: pos_integer(),
     RefactorLst1 :: [{string(), string()}],
     MissingLst1  :: [string()],
     Fa2          :: #{string() => [str()]}.

acc_refactoring( {param, {name, N, false}, _Pl}, {RefactorLst, MissingLst, Fa1},
                 Fa0, _DestDir, _SrcDirList, _R ) ->
  {RefactorLst, MissingLst, Fa1#{N => maps:get( N, Fa0 )}};

acc_refactoring( {param, {name, N, true}, _Pl}, {RefactorLst, MissingLst, Fa1},
                 Fa0, DestDir, SrcDirLst, R ) ->
  FileLst = [S || {str, _, S} <- maps:get( N, Fa0 )],
  {RefactorLst1, MissingLst1, FileLst1} =
    lists:foldl( fun( File, AccIn ) ->
                   acc_file( File, AccIn, DestDir, SrcDirLst, R )
                 end,
                 {RefactorLst, MissingLst, []}, FileLst ),
  FileLst2 = [{str, _, S} || S <- lists:reverse( FileLst1 )],
  {RefactorLst1, MissingLst1, Fa1#{N => FileLst2}}.

%% acc_file %%

-spec acc_file( File, {RefactorLst, MissingLst, FileLst}, DestDir, SrcDirLst,
                R ) ->
  {RefactorLst1, MissingLst1, FileLst1}
when File         :: string(),
     RefactorLst  :: [{string(), string()}],
     MissingLst   :: [string()],
     FileLst      :: [string()],
     DestDir      :: string(),
     SrcDirLst    :: [string()],
     R            :: pos_integer(),
     RefactorLst1 :: [{string(), string()}],
     MissingLst1  :: [string()],
     FileLst1     :: [string()].

acc_file( File, {RefactorLst, MissingLst, FileLst}, _DestDir, [], _R ) ->
  Basename = filename:basename( File ),
  {RefactorLst, [File|MissingLst], [Basename|FileLst]};

acc_file( File, AccIn={RefactorLst, MissingLst, FileLst}, DestDir, [H|T], R ) ->
  AbsSrc = string:join( [H, File], "/" ),
  io:format( "Looging for ~s~n", [AbsSrc] ),
  case filelib:is_regular( AbsSrc ) of
    false -> acc_file( File, AccIn, DestDir, T, R );
    true  ->
      Basename = filename:basename( File ),
      DestName = string:join( [integer_to_list( R ), Basename], "_" ),
      AbsDest = string:join( [DestDir, DestName], "/" ),
      {[{AbsSrc, AbsDest}|RefactorLst], MissingLst, [Basename|FileLst]}
  end.


