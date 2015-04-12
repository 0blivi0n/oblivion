%% Copyright 2014 Joaquim Rocha <jrocha@gmailbox.org>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

-module(oblivion_boot).

-define(APP_LIST, [
		crypto,
		ranch,
		cowlib,
		cowboy,
		columbo,
		cclock,
		gibreel,
		jsx,
		syntax_tools,
		compiler,
		erlydtl,
		kill_bill,
		oblivion
		]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([stop/0, start/0]).

start() -> start(?APP_LIST).

stop() -> stop(lists:reverse(?APP_LIST)).

%% ====================================================================
%% Internal functions
%% ====================================================================

start([]) ->  ok;
start([App|T]) -> 
	ok = application:start(App),
	start(T).

stop([]) ->  ok;
stop([App|T]) -> 
	ok = application:stop(App),
	stop(T).
