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
-module( cre_html ).

-export( [html/2, head/1, body/1, section/2, subsection/2, p/1, a/2,
          monospace/1, table/2, table/1, title/1, table_header/1, table_row/1,
          progress_span/2] ).

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


a( Href, Body ) when is_binary( Href ), is_binary( Body ) ->
  <<"<a href=\"", Href/binary, "\">", Body/binary, "</a>">>.

subsection( Title, Body )
when is_list( Title ),
     is_binary( Body ) ->
  B = list_to_binary( Title ),
  <<"<h3>", B/binary, "</h3>\n", Body/binary>>.