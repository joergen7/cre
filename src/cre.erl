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

-module( cre ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).

%% =============================================================================
%% Function Exports
%% =============================================================================

-export( [start_link/0, submit/1, format_optlist/1, get_optlist/4,
          stage_reply/6] ).

-behaviour( gen_server ).
-export( [code_change/3, handle_cast/2, handle_info/2, init/1, terminate/2,
          handle_call/3] ).

%% =============================================================================
%% Includes
%% =============================================================================

-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).
-endif.

-include( "abstract_syntax.hrl" ).

%% =============================================================================
%% Callback Function Declarations
%% =============================================================================

-callback init() -> ok.
-callback stage( Lam, Fa, DataDir, R ) -> tuple()
when Lam     :: lam(),
     Fa      :: #{string() => [str()]},
     DataDir :: string(),
     R       :: pos_integer().


%% =============================================================================
%% Generic Server Functions
%% =============================================================================

code_change( _OldVsn, State, _Extra ) -> {ok, State}.
handle_cast( _Request, State )        -> {noreply, State}.
terminate( _Reason, _State )          -> ok.

%% Initialization %%

init( [] ) ->
  Mod = cre_local, % CRE callback module implementing the stage function
  R = 1,           % Next id
  {ok, {Mod, R}}.

%% Call Handler %%

handle_call( {submit, {app, _, _, Lam={lam, _, Name, {sign, Lo, _}, _}, Fa}},
             _From, {Mod, R} ) ->

  _Pid = spawn_link( ?MODULE, stage_reply, [self(), Lam, Fa, Mod, "/home/jorgen/data", R] ),

  {reply, {fut, Name, R, Lo}, {R+1}};

handle_call( Request, _From, State ) ->
  {reply, {error, invalid_request, Request}, State}.

%% Info Handler %%

handle_info( {}, State ) ->
  {noreply, State}.

%% =============================================================================
%% API Functions
%% =============================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, Error}
when Error :: {already_started, pid()} | term().

start_link() ->
  gen_server:start_link( {local, ?MODULE}, ?MODULE, [], [] ).


-spec submit( App :: app() ) -> fut().

submit( App ) ->
  gen_server:call( ?MODULE, {submit, App} ).


-spec format_optlist( OptList::[{atom(), _}] ) -> iolist().

format_optlist( OptList ) ->
  string:join( [format_optpair( OptPair ) || OptPair <- OptList], " " ).

-spec get_optlist( Lam, Fa, Dir, R ) -> [{atom(), _}]
when Lam :: lam(),
     Fa  :: #{string() => [str()]},
     Dir :: string(),
     R   :: pos_integer().

get_optlist( {lam, _, Name, {sign, Lo, Li}, {forbody, Lang, _}}, Fa, Dir, R ) ->
  GeneralOpt = [{dir, Dir}, {prefix, R}, {lang, Lang}, {taskname, Name}],
  OutputOpt  = [acc_out( N, Pl ) || {param, {name, N, _}, Pl} <- Lo],
  InputOpt   = [acc_in( N, Pl, Fa ) || {param, {name, N, _}, Pl} <- Li],
  FileOpt    = lists:foldl( fun acc_file/2, [], Lo++Li ),
  GeneralOpt++OutputOpt++InputOpt++FileOpt.

-spec stage_reply( From, Lam, Fa, Mod, DataDir, R ) -> tuple()
when From    :: pid(),
     Lam     :: lam(),
     Fa      :: #{string() => str()},
     Mod     :: atom(),
     DataDir :: string(),
     R       :: pos_integer().

stage_reply( From, Lam, Fa, Mod, DataDir, R ) ->
  From ! apply( Mod, stage, [Lam, Fa, DataDir, R] ).


%% =============================================================================
%% Internal Functions
%% =============================================================================

-spec format_optpair( OptPair::{atom(), _} ) -> iolist().

format_optpair( {lang, L} ) -> io_lib:format( "-l ~w", [L] ).

-spec acc_out( N::string(), Pl::boolean() ) -> {atom(), string()}.

acc_out( N, Pl ) ->
  K = case Pl of
        true -> listout;
        false -> singout
      end,
  {K, N}.

-spec acc_file( A::app(), Acc0::[{atom(), string()}] ) -> [{atom(), string()}].

acc_file( {param, {name, N, Pf}, _}, Acc0 ) when Pf -> [{file, N}|Acc0];
acc_file( _,                         Acc0 )         -> Acc0.

-spec acc_in( N, Pl, Fa ) -> {atom(), string()}
when N  :: string(),
     Pl :: boolean(),
     Fa :: #{string() => [str()]}.

acc_in( N, Pl, Fa ) ->
  StrList = maps:get( N, Fa ),
  CommaSeparated = string:join( [S || {str, S} <- StrList], "," ),
  V = lists:flatten( io_lib:format( "~s:~s", [N, CommaSeparated] ) ),
  K = case Pl of
    true -> listin;
    false -> singin
  end,
  {K, V}.

-ifdef( TEST ).

format_optpair_formats_bash_test() ->
  S = format_optpair( {lang, bash} ),
  ?assertEqual( "-l bash", lists:flatten( S ) ).

format_optpair_formats_python_test() ->
  S = format_optpair( {lang, python} ),
  ?assertEqual( "-l python", lists:flatten( S ) ).

format_optpair_formats_r_test() ->
  S = format_optpair( {lang, r} ),
  ?assertEqual( "-l r", lists:flatten( S ) ).

acc_in_should_compose_single_binding_test() ->
  N = "a",
  Pl = false,
  Fa = #{"a" => [{str, "blub"}]},
  ?assertEqual( {singin, "a:blub"}, acc_in( N, Pl, Fa ) ).

-endif.