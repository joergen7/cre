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
%% @version 0.1.0
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

-export( [start_link/3, eval/2, demand/1, cre_reply/4] ).

%%====================================================================
%% Includes
%%====================================================================

%%====================================================================
%% Callback definitions
%%====================================================================

-callback init( Arg :: _ ) -> UsrInfo :: _.

-callback is_value( T :: _, UsrInfo :: _ ) -> boolean().

-callback step( {Q :: [_], C :: #{ _ => _ }, T :: _}, UsrInfo :: _ ) ->
            {ok, {Q1 :: [_], C1 :: #{ _ => _ }, T1 :: _}} | norule.


%%====================================================================
%% Record definitions
%%====================================================================

-record( client_state, {cre_name, client_mod, usr_info} ).


%%====================================================================
%% API functions
%%====================================================================

start_link( CreName, ClientMod, ClientArg ) ->
  gen_pnet:start_link( ?MODULE, {CreName, ClientMod, ClientArg}, [] ).

eval( ClientName, T ) ->
  gen_pnet:call( ClientName, {eval, T} ).

demand( ClientName ) ->
  gen_pnet:cast( ClientName, demand ).

cre_reply( ClientName, I, A, Delta ) ->
  gen_pnet:cast( ClientName, {cre_reply, I, A, Delta} ).


%%====================================================================
%% Interface callback functions
%%====================================================================

-spec code_change( OldVsn :: _, NetState :: _, Extra :: _ ) ->
        {ok, _} | {error, _}.

code_change( _OldVsn, NetState, _Extra ) -> {ok, NetState}.


-spec handle_call( Request :: _, From :: {pid(), _},
                   NetState :: _ ) ->
              {reply, _}
            | {reply, _, #{ atom() => [_] }, #{ atom() => [_] }}
            | noreply
            | {noreply, #{ atom() => [_] }, #{ atom() => [_] }}
            | {stop, _, _}.

handle_call( {eval, T}, From, _NetState ) ->
  {noreply, #{}, #{ 'ClientRequest' => [{From, T}] }}.


-spec handle_cast( Request :: _, NetState :: _ ) ->
              noreply
            | {noreply, #{ atom() => [_] }, #{ atom() => [_] }}
            | {stop, _}.

handle_cast( demand, _NetState ) ->
  {noreply, #{}, #{ 'Demand' => [unit] }}.


-spec handle_info( Info :: _, NetState :: _ ) ->
              noreply
            | {noreply, #{ atom() => [_] }, #{ atom() => [_] }}
            | {stop, _}.

handle_info( _Request, _NetState ) -> noreply.


-spec init( {CreName, ClientMod, ClientArg} ) -> {ok, _}
when CreName   :: gen_pnet:name(),
     ClientMod :: atom(),
     ClientArg :: _.

init( {CreName, ClientMod, ClientArg} )
when is_atom( ClientMod ) ->

  UsrInfo = ClientMod:init( ClientArg ),

  ClientState = #client_state{ cre_name   = CreName,
                               client_mod = ClientMod,
                               usr_info   = UsrInfo },

  cre_master:add_client( CreName, self() ),

  {ok, gen_pnet:new( ?MODULE, ClientState )}.


-spec terminate( Reason :: _, NetState :: _ ) -> ok.

terminate( _Reason, _NetState ) -> ok.


-spec trigger( Place :: atom(), Token :: _, NetState :: _ ) -> pass | drop.

trigger( 'ClientReply', {I, T}, _NetState ) ->
  gen_server:reply( I, T );

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
  ['ClientRequest', 'ClientReply',
   'Demand', 'CreRequest', 'CreReply',
   'Program'].


-spec trsn_lst() -> [atom()].

trsn_lst() -> [start, terminate, step, send, recv].


-spec init_marking( Place :: atom(), ClientState :: #client_state{} ) -> [_].

init_marking( _Place, _ClientState)  -> [].


-spec preset( Trsn :: atom() ) -> [atom()].

preset( start )     -> ['ClientRequest'];
preset( terminate ) -> ['Program'];
preset( step )      -> ['Program'];
preset( send )      -> ['Program', 'Demand'];
preset( recv )      -> ['Program', 'CreReply'].


-spec is_enabled( Trsn, Mode, ClientState ) -> boolean()
when Trsn        :: atom(),
     Mode        :: #{ atom() => [_]},
     ClientState :: #client_state{}.

is_enabled( start, _Mode, _ClientState ) -> true;

is_enabled( terminate, #{ 'Program' := [{_I, {[], _C, T}}] },
                       #client_state{ client_mod = ClientMod,
                                      usr_info   = UsrInfo } ) ->
  ClientMod:is_value( T, UsrInfo );

is_enabled( step, _Mode, _ClientState ) -> true;

is_enabled( send, #{ 'Program' := [{_I, {[_|_], _C, _T}}],
                     'Demand'  := [unit] },
                  _ClientState ) ->
  true;

is_enabled( recv, #{ 'Program'  := [{I, {_Q, _C, _T}}],
                     'CreReply' := [{I, _A, _Delta}]},
                  _ClientState ) ->
  true;

is_enabled( _Trsn, _Mode, _ClientState ) -> false.


-spec fire( Trsn, Mode, ClientState ) -> abort | {produce, #{ atom() => [_] }}
when Trsn        :: atom(),
     Mode        :: #{ atom() => [_] },
     ClientState :: #client_state{}.

fire( start, #{ 'ClientRequest' := [{I, T}] }, _ClientState ) ->
  {produce, #{ 'Program' => [{I, {[], #{}, T}}] }};

fire( terminate, #{ 'Program' := [{I, {_Q, _C, T}}] }, _ClientState ) ->
  {produce, #{ 'ClientReply' => [{I, T}] }};

fire( step, #{ 'Program' := {I, {Q, C, T}} },
            #client_state{ client_mod = ClientMod } ) ->
  case  ClientMod:step( {Q, C, T} ) of
    {ok, {Q1, C, T1}} -> {produce, #{ 'Program' => [{I, {Q1, C, T1}}] }};
    norule            -> abort
  end;

fire( send, #{ 'Program' := [{I, {[A|Q1], C, T}}],
	             'Demand'  := [unit] }, _ClientState ) ->
  {produce, #{ 'Program'    => [{I, {Q1, C, T}}],
               'CreRequest' => [{I, A}] }};

fire( recv, #{ 'Program'  := [{I, {Q, C, T}}],
               'CreReply' := [{I, A, Delta}] }, _ClientState ) ->
  C1 = C#{ A => Delta },
  {produce, #{ 'Program' => [{I, {Q, C1, T}}] }}.

