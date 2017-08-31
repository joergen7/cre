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

-export( [start_link/0, start_link/1] ).

%%====================================================================
%% Includes
%%====================================================================

-include_lib( "gen_pnet/include/gen_pnet.hrl" ).


%%====================================================================
%% API functions
%%====================================================================

start_link() -> gen_pnet:start_link( ?MODULE, [], [] ).

start_link( ServerName ) -> gen_pnet:start_link( ServerName, ?MODULE, [], [] ).


%%====================================================================
%% Interface callback functions
%%====================================================================

code_change( _OldVsn, NetState, _Extra ) -> {ok, NetState}.

handle_call( _Request, _From, _NetState ) -> {reply, {error, bad_msg}}.

handle_cast( _Request, _NetState ) -> noreply.

handle_info( _Request, _NetState ) -> noreply.

init( _Args ) -> {ok, gen_pnet:new( ?MODULE, [] )}.

terminate( _Reason, _NetState ) -> ok.

trigger( _Place, _Token, _NetState ) -> pass.


%%====================================================================
%% Petri net callback functions
%%====================================================================

place_lst() ->
  ['ClientRequest', 'ClientReply',
   'Demand', 'CreRequest', 'CreReply',
   'Program', 'Guard'].

trsn_lst() -> [].

init_marking( _Place, _UsrInfo ) -> [].

preset( _Trsn ) -> [].

is_enabled( _Trsn, _Mode, _UsrInfo ) -> false.

fire( _Trsn, _Mode, _UsrInfo ) -> abort.


