%%
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

-module(oblivion_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	ok = start_webserver(),
	
	oblivion_sup:start_link().

stop(_State) ->
	ok.

start_webserver() ->
	{ok, RestPort} = application:get_env(oblivion, oblivion_rest_port),
	
	ServerConfig = {server_config, oblivion_server, [
				{port, RestPort}
				]},
	{ok, ServerID} = kill_bill:config_server(ServerConfig),
	
	RestConfig = {webapp_config, oblivion_rest, [
				{context, "/hpserver"},
				{action, [
						{"/", oblivion_rest}		
						]}
				]},
	ok = kill_bill:deploy(ServerID, RestConfig),
	
	ok = kill_bill:start_server(ServerID),
	ok.