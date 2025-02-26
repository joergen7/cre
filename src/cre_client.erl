%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jörgen Brandt <joergen@cuneiform-lang.org>
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
%% @copyright 2015
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module(cre_client).
-behavior(gen_server).

%%====================================================================
%% Exports
%%====================================================================

-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         init/1,
         terminate/2,
         handle_info/2]).

-export([start_link/3, start_link/4, eval/2, cre_reply/4, stop/1]).

%%====================================================================
%% Constant definitions
%%====================================================================

-define(INTERVAL, 250).

%%====================================================================
%% Callback definitions
%%====================================================================


-callback init(InitArg :: _) -> UsrInfo :: _.

-callback is_value(E :: _, UsrInfo :: _) -> boolean().

-callback step(E :: _, UsrInfo :: _) -> {ok, _, [_]}.

-callback recv(E :: _, ReplyLst :: [{_, _}], UsrInfo :: _) -> _.

-callback load(_, UserInfo :: _) -> _.

-callback unload(_, UserInfo :: _) -> _.

%%====================================================================
%% Record definitions
%%====================================================================

-record(client_state, {
          cre_name,
          client_mod,
          usr_info,
          request_map = #{},
          reply_map = #{},
          state_map = #{}
         }).

%%====================================================================
%% API functions
%%====================================================================


start_link(CreName, ClientMod, ClientArg) ->
    gen_server:start_link(?MODULE, {CreName, ClientMod, ClientArg}, []).


start_link(ClientName, CreName, ClientMod, ClientArg) ->
    gen_server:start_link(ClientName,
                          ?MODULE,
                          {CreName, ClientMod, ClientArg},
                          []).


eval(ClientName, E) ->
    gen_server:call(ClientName, {eval, E}, infinity).


cre_reply(ClientName, I, A, Delta) ->
    gen_server:cast(ClientName, {cre_reply, I, A, Delta}).


stop(ClientName) ->
    gen_server:stop(ClientName).


%%====================================================================
%% gen_server callback functions
%%====================================================================


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, _ClientState) ->
    ok.


init({CreName, ClientMod, ClientArg}) ->

    UsrInfo = ClientMod:init(ClientArg),

    ClientState = #client_state{
                    cre_name = CreName,
                    client_mod = ClientMod,
                    usr_info = UsrInfo
                   },

    _MonitorRef = monitor(process, CreName),

    {ok, ClientState}.


handle_call({eval, E}, From, ClientState) ->

    #client_state{
      client_mod = ClientMod,
      usr_info = UserInfo,
      request_map = RequestMap,
      reply_map = ReplyMap,
      state_map = StateMap
     } = ClientState,

    Self = self(),

    P = ClientMod:load(E, UserInfo),

    ClientState1 =
        ClientState#client_state{
          request_map = RequestMap#{From => P},
          reply_map = ReplyMap#{From => []},
          state_map = StateMap#{From => idle}
         },

    gen_server:cast(Self, {continue, From}),

    {noreply, ClientState1};

handle_call(_Request, _From, ClientState) ->
    {reply, {error, bad_msg}, ClientState}.


handle_cast({cre_reply, From, A, Delta}, ClientState) ->

    #client_state{
      reply_map = ReplyMap,
      state_map = StateMap
     } = ClientState,

    ClientState1 =
        case StateMap of
            #{From := State} ->
                case State of
                    idle -> start_timer(From);
                    _ -> ok
                end,

                #{From := ReplyLst} = ReplyMap,

                ClientState#client_state{
                  reply_map = ReplyMap#{From => [{A, Delta} | ReplyLst]},
                  state_map = #{From => primed}
                 };

            _ -> ClientState
        end,

    {noreply, ClientState1};

handle_cast({continue, From}, ClientState) ->

    #client_state{
      cre_name = CreName,
      client_mod = ClientMod,
      usr_info = UserInfo,
      request_map = RequestMap,
      reply_map = ReplyMap,
      state_map = StateMap
     } = ClientState,

    #{From := P} = RequestMap,
    #{From := ReplyLst} = ReplyMap,

    Self = self(),

    Send =
        fun(A) ->
                cre_master:cre_request(CreName, Self, From, A)
        end,

    % receive new result pairs
    P1 =
        case ReplyLst of
            [] -> P;
            [_ | _] -> ClientMod:recv(P, ReplyLst, UserInfo)
        end,

    % evaluate
    {ok, P2, ALst} = ClientMod:step(P1, UserInfo),

    % send new tasks
    lists:foreach(Send, ALst),

    % check if a value resulted
    ClientState1 =
        case ClientMod:is_value(P2, UserInfo) of

            true ->
                gen_server:reply(From, ClientMod:unload(P2, UserInfo)),
                ClientState#client_state{
                  request_map = maps:remove(From, RequestMap),
                  reply_map = maps:remove(From, ReplyMap),
                  state_map = maps:remove(From, StateMap)
                 };

            false ->
                ClientState#client_state{
                  request_map = RequestMap#{From => P2},
                  reply_map = ReplyMap#{From => []},
                  state_map = StateMap#{From => idle}
                 }
        end,

    {noreply, ClientState1};

handle_cast(_Request, ClientState) ->
    {noreply, ClientState}.


handle_info({'DOWN', _MonitorRef, process, _Object, _Info}, ClientState) ->
    {stop, cre_down, ClientState};

handle_info(_Info, ClientState) ->
    {noreply, ClientState}.


-spec start_timer(From :: _) -> ok.

start_timer(From) ->

    Self = self(),

    F =
        fun() ->
                timer:sleep(?INTERVAL),
                gen_server:cast(Self, {continue, From})
        end,

    _Pid = spawn_link(F),

    ok.
