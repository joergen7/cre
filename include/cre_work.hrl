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
                      


% convenience functions

-spec nslot( Pid::atom() | pid() ) -> pos_integer().
-spec nslot( Pid::atom() | pid(), Nslot::pos_integer() ) -> ok.
-spec start_link() -> {'ok', pid()}.


