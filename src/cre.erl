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

% @doc Root supervisor and start/stop callbacks for Cuneiform application.

-module( cre ).

-export( [file/2, init/1, start/0, start/2, start_link/0, stop/1] ).

-include( "cre.hrl" ).

-import( supervisor, [start_child/2, which_children/1] ).
-import( gen_server, [call/2] ).

-behavior( application ).
-behavior( supervisor ).

%% supervisor:init/1
%
% @spec init( Args::[] ) -> ignore
%
% @doc  Provides initialization info for the supervisor behavior.
%
% @todo Define a generic server to be started
%       instead of returning ignore.
%
init( [] ) ->

  RestartStrategy = one_for_all,
  MaxRestart = 5,
  MaxTime = 60,
  
  % work server
  Work = #{id       => work,
           start    => {cre_work, start_link, []},
           restart  => permanent,
           shutdown => 5000,
           type     => worker,
           modules  => [cre_work]},

  % start stage server
  Stage = #{id       => stage,
            start    => {cre_stage, start_link, []},
            restart  => permanent,
            shutdown => 5000,
            type     => worker,
            modules  => [cre_stage]},
                
  % start proxy server
  Proxy = #{id       => proxy,
            start    => {cre_proxy, start_link, []},
            restart  => permanent,
            shutdown => 5000,
            type     => worker,
            modules  => [cre_proxy]},

                
                
 
  % ignore.
  {ok, {{RestartStrategy, MaxRestart, MaxTime}, [Work, Stage, Proxy]}}.                  
  



%% application:start/2
%
% @doc Starts the application.
%
% @spec start( StartType::normal, StartArgs::[] ) -> {ok, pid()}
%
start( normal, [] ) -> start_link().


start() ->
  application:start( cre ).
  
%% application:stop/2
%
% @doc Stops the application.
%
% @spec stop( State ) -> ok
%
stop( _State ) -> ok.

  
start_link() -> supervisor:start_link( {local, ?MODULE}, ?MODULE, [] ).
  

file( Filename, Datadir ) ->

  QuerySpec = #{id       => make_ref(),
                start    => {cre_query, start_link, [self(), file, Filename, Datadir]},
                restart  => temporary,
                shutdown => 5000,
                type     => worker,
                modules  => [cre_query]},

  supervisor:start_child( cre, QuerySpec ).


