% The Cuneiform Runtime Environment is an interpreter of the functional
% programming language Cuneiform.
%
% Copyright 2013-2015 Jörgen Brandt, Marc Bux, and Ulf Leser
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

-include( "_abstract_syntax.hrl" ).

% TYPE DECLARATIONS

-type execinfo() :: {execinfo, Pid::pid(), Dir::string(),
                               Ticket::ticket(),
                               Interpreter::string(), Script::string(),
                               Output::binary(),
                               Result::#{string()=>[expr()]}}.

-type stateinfo() :: {Nslot::pos_integer(), Queue::[ticket()],
                      RunMap::#{port()=>execinfo()}}.

                      
% FUNCTION PROTOTYPES
                      
% gen_server callback functions

-spec code_change( OldVsn::term(), State::term(), Extra::term() ) ->
        {ok, stateinfo()}.
              
-spec handle_call( Request, From, State) -> Ret
when Request :: ls
              | nslot
              | stop
              | {nslot, pos_integer()}
              | {remove, ticket()}
              | {add, ticket(), string()},
     From    :: {pid(), term()},
     State   :: stateinfo(),
     Ret     :: {reply, ok | {ls, [ticket()]} | {nslot, pos_integer()}, stateinfo()}
              | {stop, normal, ok, stateinfo()}.

-spec handle_cast( _Request, State::stateinfo()) -> {noreply, stateinfo()}.


-spec handle_info( Info, State ) -> {noreply, stateinfo()}
when Info  :: {port(), {data, term()} | {exit_status, integer()}},
     State :: stateinfo().

-spec init( Nslot::pos_integer() ) -> {ok, stateinfo()}.
-spec terminate( normal | shutdown, State::stateinfo()) -> ok.


% convenience functions

-spec add_ticket( Pid::atom() | pid(), Ticket::ticket(), Dir::string() ) -> ok.
-spec ls( Pid:: atom() | pid() ) -> [ticket()].
-spec nslot( Pid::atom() | pid() ) -> pos_integer().
-spec nslot( Pid::atom() | pid(), Nslot::pos_integer() ) -> ok.
-spec remove_ticket( Pid::atom() | pid(), Ticket::ticket() ) -> ok.
-spec start_link() -> {'ok', pid()}.
-spec stop( Pid::atom() | pid() ) -> ok.


%% ============================================================
%% Constant Definitions
%% ============================================================

-define( MAXPROC, 8 ).



% helper functions

%-spec prepare({'ticket',_,{'sign',[any()],[],[any()]},{'forbody','bash',_},_}) -> {[97 | 98 | 104 | 115,...],[any()]}.
%-spec get_assignment('bash',_,boolean(),[any()]) -> [any()].
%-spec get_dismissal('bash',[any()],boolean()) -> [any(),...].
%-spec parse_assoc(string()) -> {nonempty_string(),[[any()]]}.

%-spec gobble_queue({_,maybe_improper_list(),_}) -> {_,maybe_improper_list(),_}.
%-spec probe_precond(_,atom() | binary() | [atom() | [any()] | char()],_) -> 'ok' | {'err',atom() | binary() | [atom() | [any()] | char()]}.
%-spec probe_param(maybe_improper_list(),_,_) -> 'ok' | {'err',string()}.
-spec probe_str( [str()], string() ) -> 'ok' | {'err', string()}.
%-spec is_dirinuse(_,[any()],_) -> boolean().


