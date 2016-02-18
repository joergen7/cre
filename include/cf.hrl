%% =============================================================================
%% Abstract Syntax
%% =============================================================================

%% Expression %% ===============================================================

-type expr()    :: str() | var() | select() | cnd() | app().                    % (1)
-type str()     :: {str, S::string()}.                                          % (2)
-type var()     :: {var, Line::pos_integer(), N::string()}.                     % (3)
-type select()  :: {select, Line::pos_integer(), C::pos_integer(), U::fut()}.   % (4)
-type fut()     :: {fut, Name::string(), R::pos_integer(), Lp::[boolean()]}.    % (5)
-type cnd()     :: {cnd, Line::pos_integer(),                                   % (6)
                         Xc::[expr()], Xt::[expr()], Xe::[expr()]}.
-type app()     :: {app, Line::pos_integer(), C::pos_integer(),                 % (7)
                         Lambda::lam() | var(), Fa::#{string() => [expr()]}}.

%% Lambda %% ===================================================================

-type lam()     :: {lam, Line::pos_integer(), Name::string(),                   % (8)
                         S::sign(), B::body()}.

% Task Signature
-type sign()    :: {sign, Lo::[param()], Li::[inparam()]}.                      % (9)
-type param()   :: {param, N::string(), Pl::boolean()}.                         % (10)
-type inparam() :: param() | correl().                                          % (11)
-type correl()  :: {correl, Lc::[string()]}.                                    % (12)

% Body
-type body()    :: natbody() | forbody().                                       % (13)
-type natbody() :: {natbody, Fb::#{string() => [expr()]}}.                      % (14)
-type forbody() :: {forbody, L::lang(), S::string()}.                           % (15)
-type lang()    :: bash | python | r.                                           % (16)

%% Argument Pair %% ============================================================

-type argpair() :: {L0::[inparam()], F::#{string() => [expr()]}}.               % (17)

%% Evaluation Context %% =======================================================

-type ctx()     :: {Rho   :: #{string() => [expr()]},                           % (18)
                    Mu    :: fun( ( app() ) -> fut() ),
                    Gamma :: #{string() => lam()},
                    Omega :: #{{pos_integer(), pos_integer()} => [expr()]}}.
