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

% ABSTRACT SYNTAX DEFINITION

-type str()        :: {str, pos_integer(), string()}.
-type var()        :: {var, pos_integer(), string()}.

-type ticket()     :: {ticket, Line::pos_integer(),
                               simplesign(),
                               forbody(), Binding::#{string()=>[str()]}}.
                       
-type select()     :: {select, Line::pos_integer(), Channel::pos_integer(), ticket()}.

-type sign()       :: {sign, OutList::[param()], CorrelList::[name()],
                             InList::[inparam()]}.
                             
-type simplesign() :: {sign, OutList::[param()], [], InList::[param()]}.
                             
-type name()       :: {name, ParamName::string(), IsFile::boolean()}.

-type inparam()    :: param() | correl() | comb().

-type param()      :: {param, name(), IsList::boolean()}.
-type correl()     :: {correl, CorrelList::[name()]}.
-type comb()       :: {comb, Type::combtype(), name(), AliasList::[string()]}.

-type combtype()   :: cnr.

-type lang()       :: bash | lisp | matlab | octave | perl | python | r.

-type body()       :: natbody() | forbody().
-type natbody()    :: {natbody, #{string() => [expr()]}}.
-type forbody()    :: {forbody, lang(), string()}.

-type lam()        :: {lam, pos_integer(), string(), sign(), body()}.

-type app()        :: {app, pos_integer(), Channel::pos_integer(), LamList::[expr()],
                            BindingBlock::#{string() => [expr()]}}.

-type cnd()        :: {cnd, pos_integer(), [expr()], [expr()], [expr()]}.

-type expr()       :: str() | var() | select() | lam() | app() | cnd().
