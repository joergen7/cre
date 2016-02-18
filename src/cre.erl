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
-author( "Jörgen Brandt <brandjoe@hu-berlin.de>" ).

-behavior( gen_server ).

%% =============================================================================
%% Function Exports
%% =============================================================================

-export( [code_change/3, handle_cast/2, handle_info/2, init/1, terminate/2,
          handle_call/3] ).

-export( [start/0, submit/1] ).

%% =============================================================================
%% Abstract Syntax
%% =============================================================================

-type str()     :: {str, S::string()}.
-type fut()     :: {fut, Name::string(), R::pos_integer(), Lo::[param()]}.
-type app()     :: {app, Line::pos_integer(), C::pos_integer(),
                         Lambda::lam(), Fa::#{string() => [str()]}}.
-type lam()     :: {lam, Line::pos_integer(), Name::string(),
                         S::sign(), B::forbody()}.
-type sign()    :: {sign, Lo::[param()], Li::[param()]}.
-type param()   :: {param, N::string(), Pl::boolean()}.
-type forbody() :: {forbody, L::lang(), S::string()}.
-type lang()    :: bash | python | r.

%% =============================================================================
%% Generic Server Functions
%% =============================================================================

code_change( _OldVsn, State, _Extra ) -> {ok, State}.
handle_cast( _Request, State )        -> {noreply, State}.
handle_info( _Info, State )           -> {noreply, State}.
init( [] )                            -> {ok, {1}}.
terminate( _Reason, _State )          -> ok.

%% Call Handler %%

handle_call( {submit, A={app, _, _, {lam, _, Name, {sign, Lo, _}, _}, _}},
             _From, {R} ) ->

  _Pid = spawn_link( ?MODULE, stage, [A, R] ),

  {reply, {fut, Name, R, Lo}, {R+1}}.

%% =============================================================================
%% API Functions
%% =============================================================================

start() ->
  gen_server:start_link( {local, ?MODULE}, ?MODULE, [], [] ).


-spec submit( App :: app() ) -> fut().

submit( App ) when is_tuple( App ) ->
  gen_server:call( ?MODULE, {submit, App} ).

%% =============================================================================
%% Internal Functions
%% =============================================================================

-spec run( A::app(), R::pos_integer() ) -> ok.

stage( {app, _, _, {lam, _, Name, {sign, Lo, _}, {forbody, Lang, Script}}, Fa}, R ) ->

  % create prefix string
  Prefix = lists:flatten( io_lib:format( "~B", [R] ) ),

  % general info
  GeneralOpt = [{lang, Lang}, {prefix, Prefix}, {taskname, Name}].

  % output names
  OutputOpt = [{case Pl of true -> listout; false -> singout end, N} || {param, {name, N, _}}, Pl} <- Lo]

  % output file declarations
  OutputFileOpt = lists:foldl( fun acc_file/2, [], Lo ),

  % input parameters
  InputOpt = 


acc_file( {param, {name, N, Pf}, _}, Acc0 ) when Pf -> [{file, N}|Acc0];
acc_file( {param, {name, _, _}, _}, Acc0 )          -> Acc0.

