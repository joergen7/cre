%% -*- erlang -*-
%%
%% A common runtime environment (CRE) for distributed workflow languages.
%%
%% Copyright 2015-2018 Jörgen Brandt
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
%% @version 0.1.2
%% @copyright 2015-2018 Jörgen Brandt
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( cre_sup ).
-behaviour( supervisor ).


%%====================================================================
%% Exports
%%====================================================================

-export( [start_link/0] ).
-export( [init/1] ).


%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link( {local, cre_master_sup}, ?MODULE, [] ).

%%====================================================================
%% Application callback functions
%%====================================================================

init( _Args ) ->

    SupFlags = #{
                  strategy  => one_for_one,
                  intensity => 1,
                  period    => 5
                },

    ChildSpec = #{
                   id       => cre_master,
                   start    => {cre_master, start_link, [{local, cre_master}]},
                   restart  => temporary,
                   shutdown => 5000,
                   type     => worker,
                   modules  => [cre_master]
                 },

    {ok, {SupFlags, [ChildSpec]}}.