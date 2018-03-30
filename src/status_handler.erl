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
-module( status_handler ).
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

  CreInfo = table( [
                    table_row( [<<"Version:">>, <<?VSN>>] ),
                    table_row( [<<"CRE node:">>, monospace( atom_to_binary( node(), utf8 ) )] ),
                    table_row( [<<"Total worker slots:">>, integer_to_binary( NWrk )] )
                   ]),

  % cre load

  CreLoad = p( progress_span( TotalLoad, 64 ) ),

  % node table

  NodeInfoLst = cre_master:node_info( cre_master ),

  F =
    fun( NodeInfo ) ->
      #{ node := Node, n_wrk := NWrk, node_load := NodeLoad } = NodeInfo,
      B1 = monospace( atom_to_binary( Node, utf8 ) ),
      B2 = integer_to_binary( NWrk ),
      B3 = progress_span( NodeLoad, 16 ),
      table_row( [B1, B2, B3] )
    end,

  NodeRowLst = [F( WrkInfo ) || WrkInfo <- NodeInfoLst],
  NodeHeader = table_header( ["Worker node", "Worker slots", "Load"] ),

  NodeTable = table( NodeHeader, NodeRowLst ),

  % queue table

  #{ queued_lst   := QueuedLst,
     active_lst   := ActiveLst,
     complete_lst := CompleteLst } = cre_master:task_info( cre_master ),


  G =
    fun( T ) ->
      #{ app_id := AppId, lambda_name := LambdaName } = T,
      Skip = byte_size( AppId )-7,
      <<_:Skip/binary, B/binary>> = AppId,
      table_row( [monospace( B ), monospace( LambdaName )] )
    end,

  QueuedRowLst = [G( T ) || T <- QueuedLst],
  QueuedHeader = table_header( ["App id", "Lambda"] ),

  QueuedTable = table( QueuedHeader, QueuedRowLst ),

  % active table

  H =
    fun( T ) ->
      #{ app_id := AppId, lambda_name := LambdaName, node := Node } = T,
      Skip = byte_size( AppId )-7,
      <<_:Skip/binary, B/binary>> = AppId,
      table_row( [monospace( B ), monospace( LambdaName ), monospace( atom_to_binary( Node, utf8 ) )] )
    end,

  ActiveRowLst = [H( T ) || T <- ActiveLst],
  ActiveHeader = table_header( ["App id", "Lambda", "Worker node"] ),

  ActiveTable = table( ActiveHeader, ActiveRowLst ),


  % complete table

  CompleteRowLst = [G( T ) || T <- CompleteLst],
  CompleteTable = table( QueuedHeader, CompleteRowLst ),


  TaskHeader = table_header( ["Queued", "Active", "Complete"] ),
  TaskTable = table( TaskHeader, [table_row( [QueuedTable, ActiveTable, CompleteTable] )] ),







  Title = title( "CRE status" ),
  NodeSection = section( "Worker nodes", NodeTable ),
  TaskSection = section( "Tasks", TaskTable ),

  Head = head( "CRE status" ),

  Body =
    body(
      [Title,
       CreInfo,
       CreLoad,<<"<hr />">>,
       NodeSection,
       TaskSection] ),
  
  Page = html( Head, Body ),

  Req =
    cowboy_req:reply(
      200,
      #{ <<"content-type">> => <<"text/html">>},
      Page,
      Req0 ),

  {ok, Req, State}.


%%====================================================================
%% Internal functions
%%====================================================================

-spec html( Head :: binary(), Body :: binary() ) -> binary().

html( Head, Body )
when is_binary( Head ),
     is_binary( Body ) ->
  <<"<html>\n", Head/binary, "\n", Body/binary, "</html>">>.


-spec head( Title :: string() ) -> binary().

head( Title ) when is_list( Title ) ->
  B = list_to_binary( Title ),
  <<"<head>\n<title>", B/binary, "</title>\n</head>\n">>.


-spec body( BodyLst :: [binary()] ) -> binary().

body( BodyLst ) when is_list( BodyLst ) ->

  F =
    fun( Element, Acc ) ->
      <<Acc/binary, "\n", Element/binary>>
    end,


  B = lists:foldl( F, <<>>, BodyLst ),

  <<"<body>\n", B/binary, "</body>\n">>.


-spec section( Title :: string(), Body :: binary() ) -> binary().

section( Title, Body )
when is_list( Title ),
     is_binary( Body ) ->
  B = list_to_binary( Title ),
  <<"<div>\n<h2>", B/binary, "</h2>\n", Body/binary, "</div>\n">>.


-spec p( Content :: binary() ) -> binary().

p( Content ) when is_binary( Content ) ->
  <<"<p>\n", Content/binary, "</p>\n">>.


-spec progress_span( Ratio :: float(), Width :: pos_integer() ) -> binary().

progress_span( Ratio, Width )
when is_float( Ratio ), Ratio >= 0, Ratio =< 1,
     is_integer( Width ), Width > 0 ->

  NBar = trunc( Ratio*Width ),
  NSpace = Width-NBar,

  B1 = list_to_binary( lists:duplicate( NBar, $| ) ),
  B2 = list_to_binary( lists:duplicate( NSpace, "&nbsp;" ) ),

  <<"<span style=\"font-family : monospace\">[",
    "<span style=\"color : darkgreen\">",
    B1/binary,
    "</span>",
    B2/binary,
    "]</span>">>.


-spec title( Title :: string() ) -> binary().

title( Title ) when is_list( Title ) ->
  B = list_to_binary( Title ),
  <<"<h1>", B/binary, "</h1>\n">>.


-spec table_header( HeaderLst :: [string()] ) -> binary().

table_header( HeaderLst )
when is_list( HeaderLst ) ->

  F =
    fun( Header, Acc ) ->
      B = list_to_binary( Header ),
      <<Acc/binary, "<th>", B/binary, "</th>">>
    end,

  Tr = lists:foldl( F, <<>>, HeaderLst ),

  <<"<tr style=\"text-align : left\">", Tr/binary, "</tr>\n">>.


-spec table_row( DataLst :: [binary()] ) -> binary().

table_row( DataLst ) when is_list( DataLst ) ->

  F =
    fun( Data, Acc ) ->
      <<Acc/binary, "<td>", Data/binary, "</td>">>
    end,

  Tr = lists:foldl( F, <<>>, DataLst ),

  <<"<tr style=\"vertical-align : top\">", Tr/binary, "</tr>\n">>.


-spec table( RowLst :: [binary()] ) -> binary().

table( RowLst ) when is_list( RowLst ) ->

  B = lists:foldl( fun( E, A ) -> <<A/binary, E/binary>> end, <<>>, RowLst ),

  <<"<table>\n", B/binary, "</table>">>.


-spec table( Header :: binary(), RowLst :: [binary()] ) -> binary().

table( Header, RowLst )
when is_binary( Header ),
     is_list( RowLst ) ->

  B = lists:foldl( fun( E, A ) -> <<A/binary, E/binary>> end, <<>>, RowLst ),

  <<"<table>\n", Header/binary, B/binary, "</table>">>.

monospace( Content ) ->
  <<"<span style=\"font-family : monospace\">",
    Content/binary,
    "</span>\n">>.
