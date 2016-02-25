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
% limitations under the License.

-module( cre_local ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).

-behaviour( cre ).
-export( [init/0, stage/4] ).

-define( BASEDIR, "/tmp/cf" ).
-define( WORK, "work" ).
-define( REPO, "repo" ).

init() ->
  _Output = os:cmd( string:join( ["rm", "-rf", ?BASEDIR], " " ) ),
  ok.


stage( Lam={lam, _LamLine, _LamName, {sign, Lo, Li}, {forbody, _Lang, Script}},
       Fa, DataDir, R ) ->

  Dir = string:join( [?BASEDIR, ?WORK, integer_to_list( R )], "/" ),
  RepoDir = string:join( [?BASEDIR, ?REPO], "/" ),

  % create working directory
  case filelib:ensure_dir( [Dir, "/"] ) of
    {error, R1} -> error( {R1, ensure_dir, [Dir, "/"]} );
    ok          -> ok
  end,

  % resolve input files
  Triple1 = refactor:get_refactoring( Li, Fa, Dir, [DataDir, RepoDir], R ),
  {RefactorLst1, MissingLst1, Fa1} = Triple1,

  case MissingLst1 of
    [_|_] -> {failed, not_found, MissingLst1};
    []    ->

      % link in input files
      refactor:apply_refactoring( RefactorLst1 ),

      % create option list for effi
      OptList = cre:get_optlist( Lam, Fa1, Dir, R ),

      % start effi
      case effi:check_run( OptList, Script ) of
        {failed, R2, Data} -> {failed, R2, Data};
        {finished, Sum}    ->

          Ret1 = maps:get( ret, Sum ),

          % resolve output files
          Triple2 = refactor:get_refactoring( Lo, Ret1, RepoDir, [Dir], R ),
          {RefactorLst2, [], Ret2} = Triple2,

          % link out output files
          refactor:apply_refactoring( RefactorLst2 ),

          % update result map
          {finished, Sum#{ret => Ret2}}
      end
  end.