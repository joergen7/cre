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
%% @version 0.1.5
%% @copyright 2015-2018 Jörgen Brandt
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------
-module( cre_index_handler ).
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

  % cre info

  #{ total_load := TotalLoad,
     n_wrk      := NWrk } = cre_master:cre_info( cre_master ),

  CreInfo =
    cre_html:table( [
      cre_html:table_row( [<<"Version:">>, <<?VSN>>] ),
      cre_html:table_row( [<<"CRE node:">>, cre_html:monospace( atom_to_binary( node(), utf8 ) )] ),
      cre_html:table_row( [<<"Total worker slots:">>, integer_to_binary( NWrk )] )] ),

  % cre load

  CreLoad = cre_html:p( cre_html:progress_span( TotalLoad, 64 ) ),

  % node table

  NodeInfoLst = cre_master:node_info( cre_master ),

  F =
    fun( NodeInfo ) ->
      #{ node := Node, n_wrk := NWrk, node_load := NodeLoad } = NodeInfo,
      B1 = cre_html:monospace( atom_to_binary( Node, utf8 ) ),
      B2 = integer_to_binary( NWrk ),
      B3 = cre_html:progress_span( NodeLoad, 16 ),
      cre_html:table_row( [B1, B2, B3] )
    end,

  NodeRowLst = [F( WrkInfo ) || WrkInfo <- NodeInfoLst],
  NodeHeader = cre_html:table_header( ["Worker node", "Worker slots", "Load"] ),

  NodeTable = cre_html:table( NodeHeader, NodeRowLst ),

  % queue table

  #{ queued_lst   := QueuedLst,
     active_lst   := ActiveLst,
     complete_lst := CompleteLst } = cre_master:app_info( cre_master ),


  G =
    fun( T ) ->
      #{ app_id := AppId, lambda_name := LambdaName } = T,
      Skip = byte_size( AppId )-7,
      <<_:Skip/binary, B/binary>> = AppId,
      cre_html:table_row( [cre_html:monospace( B ), cre_html:monospace( LambdaName )] )
    end,

  QueuedRowLst = [G( T ) || T <- QueuedLst],
  QueuedHeader = cre_html:table_header( ["App id", "Lambda name"] ),

  QueuedTable = cre_html:table( QueuedHeader, QueuedRowLst ),
  QueuedSubsection = cre_html:subsection( "Queued", QueuedTable ),

  % active table

  H =
    fun( T ) ->
      #{ app_id := AppId, lambda_name := LambdaName, node := Node } = T,
      Skip = byte_size( AppId )-7,
      <<_:Skip/binary, B/binary>> = AppId,
      cre_html:table_row( [cre_html:monospace( B ), cre_html:monospace( LambdaName ), cre_html:monospace( Node )] )
    end,

  ActiveRowLst = [H( T ) || T <- ActiveLst],
  ActiveHeader = cre_html:table_header( ["App id", "Lambda name", "Worker node"] ),

  ActiveTable = cre_html:table( ActiveHeader, ActiveRowLst ),
  ActiveSubsection = cre_html:subsection( "Active", ActiveTable ),


  % complete table

  I =
    fun( T ) ->
      #{ app_id := AppId, lambda_name := LambdaName, node := Node, status := Status } = T,
      Skip = byte_size( AppId )-7,
      <<_:Skip/binary, B/binary>> = AppId,
      cre_html:table_row( [cre_html:monospace( B ),
                           cre_html:monospace( LambdaName ),
                           cre_html:monospace( Node ),
                           cre_html:monospace( Status )] )
    end,

  CompleteHeader = cre_html:table_header( ["App id", "Lambda name", "Worker node", "Status"] ),
  CompleteRowLst = [I( T ) || T <- CompleteLst],
  CompleteTable = cre_html:table( CompleteHeader, CompleteRowLst ),
  CompleteSubsection = cre_html:subsection( "Complete", CompleteTable ),

  Title = cre_html:title( "CRE status" ),
  NodeSection = cre_html:section( "Worker nodes", NodeTable ),
  TaskSection = cre_html:section( "Applications", <<QueuedSubsection/binary, "\n",
                                    ActiveSubsection/binary, "\n",
                                    CompleteSubsection/binary, "\n">> ),

  Head = cre_html:head( "CRE status" ),

  Body =
    cre_html:body(
      [Title,
       CreInfo,
       CreLoad,
       NodeSection,
       TaskSection] ),
  
  Page = cre_html:html( Head, Body ),

  Reply =
    cowboy_req:reply(
      200,
      #{ <<"content-type">> => <<"text/html">>},
      Page,
      Req0 ),

  {ok, Reply, State}.


