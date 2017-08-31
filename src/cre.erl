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

-module( cre ).
-behaviour( application ).


%%====================================================================
%% Exports
%%====================================================================

-export( [start/2, stop/1] ).
-export( [start/0, pid/0] ).

%%====================================================================
%% Application callback functions
%%====================================================================

start( _Type, _Args ) ->
  cre_sup:start_link().
  
stop( _State ) ->
  ok.

%%====================================================================
%% API functions
%%====================================================================

start() ->
  application:start( cre ).


-spec pid() -> {ok, pid()} | {error, undefined}.

pid() ->
  case global:whereis_name( cre_sup ) of
  	undefined -> {error, undefined};
  	SupPid    ->
      Children = supervisor:which_children( SupPid ),
      [{undefined, CrePid, worker, [cre_master]}] = Children,
      {ok, CrePid}
  end.
