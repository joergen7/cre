%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2019 Jörgen Brandt <joergen@cuneiform-lang.org>
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
%% @author Jörgen Brandt <joergen@cuneiform-lang.org>
%% @version 0.1.9
%% @copyright 2015-2019
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
%% Includes
%%====================================================================

-include( "cre.hrl" ).


%%====================================================================
%% Definitions
%%====================================================================

-define( PORT, 4142 ).

%%====================================================================
%% API functions
%%====================================================================

-spec start() -> ok | {error, _}.

start() ->
  {ok, _} = application:ensure_all_started( cre ),
  ok.


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

-spec start( Type :: _, Args :: _ ) -> {ok, pid()} | {error, _}.

start( _Type, _Args ) ->

  {ok, Port} = start_cre_webservice( ?PORT ),

  error_logger:info_report( [{info,        "starting cre"},
                             {application, cre},
                             {vsn,         ?VSN},
                             {node,        node()},
                             {port,        Port}] ),

  cre_sup:start_link().


-spec stop( State :: _ ) -> ok.

stop( _State ) ->
  ok.


%%====================================================================
%% Escript main function
%%====================================================================


-spec main( Args :: _ ) -> ok.

main( _Args ) ->



  % start the cre application
  ok = start(),


  % create monitor
  _ = monitor( process, cre_sup ),

  % wait indefinitely
  receive
  	{'DOWN', _Ref, process, _Object, _Info} ->
      timer:sleep( 1000 )
  end.


%%====================================================================
%% Internal functions
%%====================================================================

% start_cre_webservice/1
% @doc Attempts to start the CRE web service under the given port.
%      If the port is in use we try new port numbers until we find a
%      free port.

-spec start_cre_webservice( Port :: inet:port_number() ) ->
  {ok, inet:port_number()} | {error, _}.

start_cre_webservice( Port )
when is_integer( Port ),
     Port >= 0,
     Port < 65536 ->

  Dispatch =
    cowboy_router:compile(
      [{'_', [
              {"/[status.json]", cre_status_handler, []},
              {"/history.json", cre_history_handler, []}
             ]}] ),

  Reply = cowboy:start_clear( cre_status_listener,
                          [{port, Port}],
                          #{ env => #{ dispatch => Dispatch } } ),

  case Reply of
    {ok, _ListenerPid}  -> {ok, Port};
    {error, eaddrinuse} -> start_cre_webservice( Port+1 );
    {error, Reason}     -> {error, Reason}
  end.
