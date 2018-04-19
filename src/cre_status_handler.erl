%% -*- erlang -*-
%%
%% Common runtime environment for distributed programming languages
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
%% @version 0.1.7
%% @copyright 2015-2018 Jörgen Brandt
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------
-module( cre_status_handler ).
-behavior( cowboy_handler ).


%%====================================================================
%% Exports
%%====================================================================

-export( [init/2] ).


%%====================================================================
%% Includes
%%====================================================================

-include( "cre.hrl" ).


%%====================================================================
%% Cowboy handler callback functions
%%====================================================================

init( Req0, State ) ->

  StatusMap = cre_master:get_status( cre_master ),
  Doc = jsone:encode( StatusMap ),
  
  Reply =
    cowboy_req:reply(
      200,
      #{ <<"content-type">> => <<"application/json">>},
      Doc,
      Req0 ),

  {ok, Reply, State}.


