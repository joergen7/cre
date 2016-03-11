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

-export( [start_link/0, submit/2, stage_reply/6] ).

-behaviour( gen_server ).
-export( [code_change/3, handle_cast/2, handle_info/2, init/1, terminate/2,
          handle_call/3] ).

%% =============================================================================
%% Includes
%% =============================================================================

-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).
-endif.

%% =============================================================================
%% Callback Function Declarations
%% =============================================================================

-callback init() -> ok.
-callback stage( Lam, Fa, DataDir, R ) -> tuple()
when Lam     :: effi:lam(),
     Fa      :: #{string() => [effi:str()]},
     DataDir :: string(),
     R       :: pos_integer().


%% =============================================================================
%% Abstract Syntax
%% =============================================================================

-type fut()     :: {fut, Name::string(), R::pos_integer(), Lo::[effi:param()]}.
-type app()     :: {app, AppLine::pos_integer(), C::pos_integer(),
                         Lambda::effi:lam(), Fa::#{string() => [effi:str()]}}.

%% =============================================================================
%% Generic Server Functions
%% =============================================================================

code_change( _OldVsn, State, _Extra ) -> {ok, State}.
handle_cast( Request, _State ) -> error( {bad_request, Request} ).
terminate( _Reason, _State ) -> ok.

%% Initialization %%

init( [] ) ->
  Mod       = cre_local,  % CRE callback module implementing the stage function
  SubscrSet = sets:new(), % list of subscribers
  Cache     = #{},        % cache
  R         = 1,          % next id

  % initialize CRE
  apply( Mod, init, [] ),

  {ok, {Mod, SubscrSet, Cache, R}}.

%% Call Handler %%

handle_call( {submit, App, DataDir}, {Pid, _Tag}, {Mod, SubscrSet, Cache, R} ) ->

  {app, _, _, Lam, Fa} = App,
  {lam, _, Name, {sign, Lo, _}, _} = Lam,

  % construct cache key
  Ckey = {Lam, Fa, DataDir},

  % add pid to set of subscribers
  SubscrSet1 = sets:add_element( Pid, SubscrSet ),

  case maps:is_key( Ckey, Cache ) of

    false ->

      % create new future
      Fut = {fut, Name, R, Lo},

      % start process
      _Pid = spawn_link( ?MODULE, stage_reply, [self(), Lam, Fa, Mod, DataDir, R] ),

      {reply, Fut, {Mod, SubscrSet1, Cache#{Ckey => Fut}, R+1}};

    true ->

      % retrieve future from cache
      Fut = maps:get( Ckey, Cache ),

      {reply, Fut, {Mod, SubscrSet1, Cache, R}}
  end;

handle_call( Request, _From, _State ) ->
  error( {bad_request, Request} ).

%% Info Handler %%

handle_info( {failed, Reason, Data}, {Mod, SubscrSet, Cache, R} ) ->
  lists:foreach( fun( Subscr ) ->
                   Subscr ! {failed, Reason, Data}
                 end,
                 sets:to_list( SubscrSet ) ),
  {noreply, {Mod, sets:new(), Cache, R}};

handle_info( {finished, Sum}, {Mod, SubscrSet, Cache, R} ) ->

  lists:foreach( fun( Subscr ) ->
                   Subscr ! {finished, Sum}
                 end,
                 sets:to_list( SubscrSet ) ),
  {noreply, {Mod, SubscrSet, Cache, R}};

handle_info( Info, _State ) ->
  error( {bad_msg, Info} ).

%% =============================================================================
%% API Functions
%% =============================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, Error}
when Error :: {already_started, pid()} | term().

start_link() ->
  gen_server:start_link( {local, ?MODULE}, ?MODULE, [], [] ).


-spec submit( App::app(), DataDir::string() ) -> fut().

submit( App, DataDir ) ->
  gen_server:call( ?MODULE, {submit, App, DataDir} ).

-spec stage_reply( From, Lam, Fa, Mod, DataDir, R ) -> tuple()
when From    :: pid(),
     Lam     :: effi:lam(),
     Fa      :: #{string() => effi:str()},
     Mod     :: atom(),
     DataDir :: string(),
     R       :: pos_integer().

stage_reply( From, Lam, Fa, Mod, DataDir, R ) ->
  Result = apply( Mod, stage, [Lam, Fa, DataDir, R] ),
  From ! Result.


%% =============================================================================
%% Internal Functions
%% =============================================================================


-ifdef( TEST ).


-endif.