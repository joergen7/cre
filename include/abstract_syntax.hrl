%% =============================================================================
%% Abstract Syntax
%% =============================================================================

-type str()     :: {str, Line::pos_integer(), S::string()}.
-type fut()     :: {fut, Name::string(), R::pos_integer(), Lo::[param()]}.
-type app()     :: {app, AppLine::pos_integer(), C::pos_integer(),
                         Lambda::lam(), Fa::#{string() => [str()]}}.
-type lam()     :: {lam, LamLine::pos_integer(), Name::string(),
                         S::sign(), B::forbody()}.
-type sign()    :: {sign, Lo::[param()], Li::[param()]}.
-type param()   :: {param, M::name(), Pl::boolean()}.
-type name()    :: {name, N::string(), Pf::boolean()}.
-type forbody() :: {forbody, L::lang(), S::string()}.
-type lang()    :: bash | python | r.
