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
-callback stage( Lam, Fa, R, DataDir ) -> tuple()
when Lam     :: effi:lam(),
     Fa      :: #{string() => [effi:str()]},
     R       :: pos_integer(),
     DataDir :: string().


%% =============================================================================
%% Type Definitions
%% =============================================================================

-type fut()       :: {fut, LamName::string(), R::pos_integer(),
                           Lo::[effi:param()]}.

-type app()       :: {app, AppLine::pos_integer(), C::pos_integer(),
                           Lambda::effi:lam(), Fa::#{string() => [effi:str()]}}.

-type ckey()      :: {effi:lam(), #{string() => [effi:str()]}, string()}.

-type response()  :: {failed, pos_integer(), atom(), term()}
                   | {finished, #{atom() => term()}}.

-type cre_state() :: {atom(), #{pos_integer() => sets:set( pid() )},
                      #{pos_integer() => response()}, #{ckey() => fut()},
                      pos_integer()}.

-type submit()    :: {submit, app(), string()}.

%% =============================================================================
%% Generic Server Functions
%% =============================================================================

code_change( _OldVsn, State, _Extra ) -> {ok, State}.
handle_cast( Request, _State ) -> error( {bad_request, Request} ).
terminate( _Reason, _State ) -> ok.

%% Initialization %%

-spec init( [] ) -> {ok, cre_state()}.

init( [] ) ->
  Mod       = native,     % CRE callback module implementing the stage function
  SubscrMap = #{},        % mapping of a future to a set of subscriber pids
  ReplyMap  = #{},        % mapping of a future to a response
  Cache     = #{},        % cache mapping a cache key to a future
  R         = 1,          % next id

  % initialize CRE
  apply( Mod, init, [] ),

  {ok, {Mod, SubscrMap, ReplyMap, Cache, R}}.

%% Call Handler %%

-spec handle_call( Request, From, State ) -> {reply, fut(), cre_state()}
when Request :: submit(),
     From    :: {pid(), term()},
     State   :: cre_state().

handle_call( {submit, App, DataDir}, {Pid, _Tag}, {Mod, SubscrMap, ReplyMap, Cache, R} ) ->

  {app, _AppLine, _Channel, Lam, Fa} = App,
  {lam, _LamLine, LamName, {sign, Lo, _Li}, _Body} = Lam,

  % construct cache key
  Ckey = {Lam, Fa, DataDir},

  case maps:is_key( Ckey, Cache ) of

    false ->

      % create new future
      Fut = {fut, LamName, R, Lo},

      % start process
      _Pid = spawn_link( ?MODULE, stage_reply, [self(), Lam, Fa, Mod, R, DataDir] ),

      SubscrMap1 = SubscrMap#{R => sets:from_list( [Pid] )},
      Cache1 = Cache#{Ckey => Fut},
      R1 = R+1,

      {reply, Fut, {Mod, SubscrMap1, ReplyMap, Cache1, R1}};

    true ->

      % retrieve future from cache
      Fut = maps:get( Ckey, Cache ),

      {fut, _, S, _} = Fut,
      #{S := SubscrSet} = SubscrMap,

      SubscrMap1 = SubscrMap#{S => sets:add_element( Pid, SubscrSet )},

      case maps:is_key( S, ReplyMap ) of
        false -> ok;
        true  -> Pid ! maps:get( S, ReplyMap )
      end,

      {reply, Fut, {Mod, SubscrMap1, ReplyMap, Cache, R}}
  end;

handle_call( Request, _From, _State ) ->
  error( {bad_request, Request} ).

%% Info Handler %%

-spec handle_info( Info, State ) -> {noreply, cre_state()}
when Info  :: response(),
     State :: cre_state().

handle_info( Info={failed, R, Reason, Data}, {Mod, SubscrMap, ReplyMap, Cache, R} ) ->

  % retrieve subscriber set
  #{R := SubscrSet} = SubscrMap,

  % notify subscribers
  lists:foreach( fun( Subscr ) ->
                   Subscr ! {failed, Reason, Data}
                 end,
                 sets:to_list( SubscrSet ) ),

  ReplyMap1 = ReplyMap#{R => Info},

  {noreply, {Mod, SubscrMap, ReplyMap1, Cache, R}};

handle_info( Info={finished, Sum}, {Mod, SubscrMap, ReplyMap, Cache, R} ) ->

  % retrieve subscriber set
  #{id := S} = Sum,
  #{S := SubscrSet} = SubscrMap,

  % notify subscribers
  lists:foreach( fun( Subscr ) ->
                   Subscr ! {finished, Sum}
                 end,
                 sets:to_list( SubscrSet ) ),


  ReplyMap1 = ReplyMap#{S => Info},

  {noreply, {Mod, SubscrMap, ReplyMap1, Cache, R}};

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