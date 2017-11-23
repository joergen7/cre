%% -*- erlang -*-
%%
%% A common runtime environment (CRE) for distributed workflow languages.
%%
%% Copyright 2015-2017 Jörgen Brandt
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @author Jörgen Brandt <joergen.brandt@onlinehome.de>
%% @version 0.1.1
%% @copyright 2015-2017 Jörgen Brandt
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( cre_client ).
-behaviour( gen_pnet ).

%%====================================================================
%% Exports
%%====================================================================

-export( [code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
          terminate/2, trigger/3] ).

-export( [place_lst/0, trsn_lst/0, init_marking/2, preset/1, is_enabled/3,
          fire/3] ).

-export( [start_link/3, start_link/4, eval/2, cre_reply/4, stop/1] ).

%%====================================================================
%% Includes
%%====================================================================

-include_lib( "gen_pnet/include/gen_pnet.hrl" ).

%%====================================================================
%% Callback definitions
%%====================================================================

-callback init( InitArg :: _ ) -> UsrInfo :: _.

-callback is_value( E :: _, UsrInfo :: _ ) -> boolean().

-callback step( E :: _, UsrInfo :: _ ) ->
            {ok, _} | {ok_send, _, _} | norule.

-callback recv( E :: _, A :: _, Delta :: _, UsrInfo :: _ ) -> _.

%%====================================================================
%% Record definitions
%%====================================================================

-record( client_state, {cre_name, client_mod, usr_info} ).


%%====================================================================
%% API functions
%%====================================================================

start_link( CreName, ClientMod, ClientArg ) ->
  gen_pnet:start_link( ?MODULE, {CreName, ClientMod, ClientArg}, [] ).

start_link( ClientName, CreName, ClientMod, ClientArg ) ->
  gen_pnet:start_link( ClientName,
                       ?MODULE,
                       {CreName, ClientMod, ClientArg},
                       [] ).

eval( ClientName, T ) ->
  gen_pnet:call( ClientName, {eval, T}, infinity ).

cre_reply( ClientName, I, A, Delta ) ->
  gen_pnet:cast( ClientName, {cre_reply, I, A, Delta} ).

stop( ClientName ) ->
  gen_pnet:stop( ClientName ).


%%====================================================================
%% Interface callback functions
%%====================================================================

-spec code_change( OldVsn :: _, NetState :: #net_state{}, Extra :: _ ) ->
        {ok, #net_state{}} | {error, _}.

code_change( _OldVsn, NetState, _Extra ) -> {ok, NetState}.


-spec handle_call( Request :: _, From :: {pid(), _},
                   NetState :: #net_state{} ) ->
              {reply, _}
            | {reply, _, #{ atom() => [_] }, #{ atom() => [_] }}
            | noreply
            | {noreply, #{ atom() => [_] }, #{ atom() => [_] }}
            | {stop, _, _}.

handle_call( {eval, T}, From, _NetState ) ->
  {noreply, #{}, #{ 'ClientRequest' => [{From, T}] }};

handle_call( _Request, _From, _NetState ) ->
  {reply, {error, bad_msg}}.


-spec handle_cast( Request :: _, NetState :: #net_state{} ) ->
              noreply
            | {noreply, #{ atom() => [_] }, #{ atom() => [_] }}
            | {stop, _}.

handle_cast( {cre_reply, I, A, Delta}, _NetState ) ->
  {noreply, #{}, #{ 'CreReply' => [{I, A, Delta}] }};

handle_cast( _Request, _NetState ) -> noreply.



-spec handle_info( Info :: _, NetState :: #net_state{} ) ->
              noreply
            | {noreply, #{ atom() => [_] }, #{ atom() => [_] }}
            | {stop, _}.

handle_info( _Request, _NetState ) -> noreply.


-spec init( {CreName, ClientMod, ClientArg} ) -> _
when CreName   :: gen_pnet:name(),
     ClientMod :: atom(),
     ClientArg :: _.

init( {CreName, ClientMod, ClientArg} )
when is_atom( ClientMod ) ->

  UsrInfo = ClientMod:init( ClientArg ),

  ClientState = #client_state{ cre_name   = CreName,
                               client_mod = ClientMod,
                               usr_info   = UsrInfo },

  ClientState.


-spec terminate( Reason :: _, NetState :: #net_state{} ) -> ok.

terminate( _Reason, _NetState ) -> ok.


-spec trigger( Place :: atom(), Token :: _, NetState :: #net_state{} ) -> pass | drop.

trigger( 'ClientReply', {I, T}, _NetState ) ->
  _ = gen_pnet:reply( I, T ),
  drop;

trigger( 'CreRequest', {I, A}, NetState ) ->
  ClientState = gen_pnet:get_usr_info( NetState ),
  #client_state{ cre_name = CreName } = ClientState,
  cre_master:cre_request( CreName, self(), I, A ),
  drop;

trigger( _Place, _Token, _NetState ) -> pass.


%%====================================================================
%% Petri net callback functions
%%====================================================================

-spec place_lst() -> [atom()].

place_lst() ->
  ['ClientRequest', 'ClientReply', 'CreRequest', 'CreReply', 'Program'].


-spec trsn_lst() -> [atom()].

trsn_lst() -> [start, terminate, step, send, recv].


-spec init_marking( Place :: atom(), ClientState :: #client_state{} ) -> [_].

init_marking( _Place, _ClientState)  -> [].


-spec preset( Trsn :: atom() ) -> [atom()].

preset( start )     -> ['ClientRequest'];
preset( terminate ) -> ['Program'];
preset( step )      -> ['Program'];
preset( send )      -> ['Program'];
preset( recv )      -> ['Program', 'CreReply'].


-spec is_enabled( Trsn, Mode, ClientState ) -> boolean()
when Trsn        :: atom(),
     Mode        :: #{ atom() => [_]},
     ClientState :: #client_state{}.

is_enabled( start, _Mode, _ClientState ) -> true;

is_enabled( terminate, #{ 'Program' := [{_I, {[], [], E}}] },
                       #client_state{ client_mod = ClientMod,
                                      usr_info   = UsrInfo } ) ->
  ClientMod:is_value( E, UsrInfo );

is_enabled( step, #{ 'Program' := [{_I, {_Q, _C, E}}] }, 
                  #client_state{ client_mod = ClientMod,
                                 usr_info   = UsrInfo } ) ->
  not ClientMod:is_value( E, UsrInfo );

is_enabled( send, #{ 'Program' := [{_I, {[_|_], _C, _E}}] },
                  _ClientState ) ->
  true;

is_enabled( recv, #{ 'Program'  := [{I, {_Q, _C, _E}}],
                     'CreReply' := [{I, _A, _Delta}]},
                  _ClientState ) ->
  true;

is_enabled( _Trsn, _Mode, _ClientState ) -> false.


-spec fire( Trsn, Mode, ClientState ) -> abort | {produce, #{ atom() => [_] }}
when Trsn        :: atom(),
     Mode        :: #{ atom() => [_] },
     ClientState :: #client_state{}.

fire( start, #{ 'ClientRequest' := [{I, E}] }, _ClientState ) ->
  {produce, #{ 'Program' => [{I, {[], [], E}}] }};

fire( terminate, #{ 'Program' := [{I, {[], [], E}}] }, _ClientState ) ->
  {produce, #{ 'ClientReply' => [{I, E}] }};

fire( step, #{ 'Program' := [{I, {Q, [{A, Delta}|T], E}}] },
            #client_state{ client_mod = ClientMod, usr_info = UsrInfo } ) ->
  E1 = ClientMod:recv( E, A, Delta, UsrInfo ),
  {produce, #{ 'Program' => [{I, {Q, T, E1}}] }};

fire( step, #{ 'Program' := [{I, {Q, [], E}}] },
            #client_state{ client_mod = ClientMod, usr_info = UsrInfo } ) ->
  case  ClientMod:step( E, UsrInfo ) of
    {ok, E1}         -> {produce, #{ 'Program' => [{I, {Q, [], E1}}] }};
    {ok_send, E1, A} -> {produce, #{ 'Program' => [{I, {[A|Q], [], E1}}] }};
    norule           -> abort
  end;

fire( send, #{ 'Program' := [{I, {[A|Q1], C, E}}] }, _ClientState ) ->
  {produce, #{ 'Program'    => [{I, {Q1, C, E}}],
               'CreRequest' => [{I, A}] }};

fire( recv, #{ 'Program'  := [{I, {Q, C, E}}],
               'CreReply' := [{I, A, Delta}] }, _ClientState ) ->
  C1 = [{A, Delta}|C],
  {produce, #{ 'Program' => [{I, {Q, C1, E}}] }}.

