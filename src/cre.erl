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
%% Generic Server Functions
%% =============================================================================

code_change( _OldVsn, State, _Extra ) -> {ok, State}.
handle_cast( _Request, State )        -> {noreply, State}.
handle_info( _Info, State )           -> {noreply, State}.
init( [] )                            -> {ok, []}.
terminate( _Reason, _State )          -> ok.

%% Call Handler %%
handle_call( _Request, _From, State ) -> {reply, ok, State}.

%% =============================================================================
%% API Functions
%% =============================================================================

start() ->
  gen_server:start_link( {local, ?MODULE}, ?MODULE, [], [] ).

submit( App ) ->
  gen_server:call( ?MODULE, {submit, App} ).

%% =============================================================================
%% Internal Functions
%% =============================================================================

