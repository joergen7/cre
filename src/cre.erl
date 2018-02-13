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

-module( cre ).
-behaviour( application ).


%%====================================================================
%% Exports
%%====================================================================

-export( [start/0, pid/1] ).
-export( [start/2, stop/1] ).
-export( [main/1] ).

%%====================================================================
%% API functions
%%====================================================================

start() ->
  application:start( cre ).


-spec pid( CreNode :: atom() ) -> {ok, pid()} | {error, undefined}.

pid( CreNode ) when is_atom( CreNode ) ->

  % query cre process pid
  case rpc:call( CreNode, erlang, whereis, [cre_master] ) of
    undefined          -> {error, cre_process_not_registered};
    {badrpc, nodedown} -> {error, cre_node_down};
    CrePid             -> {ok, CrePid}
  end.


%%====================================================================
%% Application callback functions
%%====================================================================

start( _Type, _Args ) ->

  error_logger:info_report( [{info,        "starting common runtime environment"},
                             {application, cre},
                             {node,        node()}] ),

  cre_sup:start_link().
  
stop( _State ) ->
  ok.


%%====================================================================
%% Escript main function
%%====================================================================

main( _Args ) ->

  % start the application
  ok = start(),
  true = link( whereis( cre_master_sup ) ),

  % wait indefinitely
  receive
  	_ ->
      ok = timer:sleep( 1000 ),
      ok
  end.