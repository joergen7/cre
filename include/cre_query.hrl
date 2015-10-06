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

-type parsetree() :: {[expr()], #{string()=>[expr()]}, #{string()=>[lam()]}}.

-type stateinfo() :: {Sender::pid(),
                      ExprList::[expr()],
                      Rho::#{string()=>[expr()]},
                      CreateTicket::fun( ( pos_integer(), sign(), forbody(),
                                           #{string()=>[expr()]} ) -> ticket() ),
                      Global::#{string()=>[lam()]},
                      Fin::#{{pos_integer(), ticket()} => [str()]}}.
                      
                      
% FUNCTION SPECS

-spec init( {Sender, Type, Content, Datadir} ) -> Result
when Sender  :: pid(),
     Type    :: string | file,
     Content :: string(),
     Datadir :: string(),
     Result  :: {stop, finished} | {ok, stateinfo()}.
     
-spec handle_info( Info, State ) -> Result
when Info   :: {finished, #{{pos_integer(), ticket()} => [str()]}}
             | {failed, ticket(), string(), string(), binary()},
     State  :: stateinfo(),
     Result :: {stop, failed | finished, stateinfo()}
             | {noreply, stateinfo()}.
             
             
-spec init_string( Sender, String, Datadir ) -> Result
when Sender  :: pid(),
     String  :: string(),
     Datadir :: string(),
     Result  :: {stop, finished}
              | {ok, stateinfo()}.
              
              
-spec create_ticket( ProxyPid, Line, Sign, ForBody, Binding, Datadir ) -> Ticket
when ProxyPid :: atom() | pid(),
     Line     :: pos_integer(),
     Sign     :: simplesign(),
     ForBody  :: forbody(),
     Binding  :: #{string()=>[expr()]},
     Datadir  :: string(),
     Ticket   :: ticket().