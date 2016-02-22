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

-export( [get_refactoring/5] ).


%% =============================================================================
%% Includes
%% =============================================================================

-include( "abstract_syntax.hrl" ).

-spec get_refactoring( ParamLst, Fa0, DestDir, SrcDirLst, R ) -> {RefactorLst, MissingLst, Fa1}
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

acc_refactoring( {param, {name, _N, false}, _Pl}, AccIn, _Fa0, _DestDir,
                 _SrcDirList, _R ) ->
  AccIn;

acc_refactoring( {param, {name, N, true}, _Pl}, {RefactorLst, MissingLst, Fa1},
                 Fa0, DestDir, SrcDirLst, R ) ->
  FileLst = [S || {str, S} <- maps:get( N, Fa0 )],
  {RefactorLst1, MissingLst1, FileLst1} =
    lists:foldl( fun( File, AccIn ) ->
                   acc_file( File, AccIn, DestDir, SrcDirLst, R )
                 end,
                 {RefactorLst, MissingLst, []}, FileLst ),
  {RefactorLst1, MissingLst1, Fa1#{N => lists:reverse( FileLst1 )}}.

acc_file( File, {RefactorLst, MissingLst, FileLst}, _DestDir, [], _R ) ->
  {RefactorLst, [File|MissingLst], FileLst};

acc_file( File, AccIn={RefactorLst, MissingLst, FileLst}, DestDir, [H|T], R ) ->
  Basename = filename:basename( File ),
  AbsSrc = string:join( [H, Basename], "/" ),
  case filelib:is_regular( AbsSrc ) of
    false -> acc_file( File, AccIn, DestDir, T, R );
    true  ->
      DestName = string:join( [integer_to_list( R ), Basename], "_" ),
      AbsDest = string:join( [DestDir, DestName], "/" ),
      {[{AbsSrc, AbsDest}|RefactorLst], MissingLst, [Basename|FileLst]}
  end.


  