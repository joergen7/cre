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
%% Callback Function Declarations
%% =============================================================================

-callback stage( A::app(), R::pos_integer() ) -> fut().


%% =============================================================================
%% Function Exports
%% =============================================================================

-export( [start_link/0, submit/1, format_optlist/1, get_optlist/1] ).

-behavior( gen_server ).
-export( [code_change/3, handle_cast/2, handle_info/2, init/1, terminate/2,
          handle_call/3] ).

%% =============================================================================
%% Includes
%% =============================================================================

-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).
-endif.

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
-type param()   :: {param, M::name(), Pl::boolean()}.
-type name()    :: {name, N::string(), Pf::boolean()}.
-type forbody() :: {forbody, L::lang(), S::string()}.
-type lang()    :: bash | python | r.

%% =============================================================================
%% Generic Server Functions
%% =============================================================================

code_change( _OldVsn, State, _Extra ) -> {ok, State}.
handle_cast( _Request, State )        -> {noreply, State}.
handle_info( _Info, State )           -> {noreply, State}.
init( [] )                            -> {ok, {cre_local, 1}}.
terminate( _Reason, _State )          -> ok.

%% Call Handler %%

handle_call( {submit, A={app, _, _, {lam, _, Name, {sign, Lo, _}, _}, _}},
             _From, {Mod, R} ) ->

  _Pid = spawn_link( Mod, stage, [A, R] ),

  {reply, {fut, Name, R, Lo}, {R+1}}.

%% =============================================================================
%% API Functions
%% =============================================================================

start_link() ->
  gen_server:start_link( {local, ?MODULE}, ?MODULE, [], [] ).


-spec submit( App :: app() ) -> fut().

submit( App ) when is_tuple( App ) ->
  gen_server:call( ?MODULE, {submit, App} ).


-spec format_optlist( OptList::[{atom(), _}] ) -> iolist().

format_optlist( OptList ) ->
  string:join( [format_optpair( OptPair ) || OptPair <- OptList], " " ).

-spec get_optlist( A::app() ) -> [{atom(), _}].

get_optlist( {app, _, _, {lam, _, Name, {sign, Lo, Li}, {forbody, Lang, _}}, Fa} ) ->
  GeneralOpt = [{lang, Lang}, {taskname, Name}],                         % general info
  OutputOpt  = [acc_out( N, Pl ) || {param, {name, N, _}, Pl} <- Lo],    % output names
  InputOpt   = [acc_in( N, Pl, Fa ) || {param, {name, N, _}, Pl} <- Li], % input parameters
  FileOpt    = lists:foldl( fun acc_file/2, [], Lo++Li ),                % input and output file declarations
  GeneralOpt++OutputOpt++InputOpt++FileOpt.

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
  CommaSeparated = string:join( [S|| {str, S} <- StrList], "," ),
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

-endif.