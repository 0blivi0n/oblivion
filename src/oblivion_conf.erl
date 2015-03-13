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

-module(oblivion_conf).

-define(CACHE_CONFIG(Name, Options), {Name, Options}).

-define(NODES_CONFIG_PARAM, node_list).
-define(CACHE_CONFIG_PARAM, cache_list).
-define(CONFIG_DATA(NodeConfig, CacheConfig), [
		{?NODES_CONFIG_PARAM, NodeConfig}, 
		{?CACHE_CONFIG_PARAM, CacheConfig}
		]).

-define(EMPTY, ?CONFIG_DATA([], [])).

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/0]).
-export([read/0, write/1, write/2, delete/0]).
-export([nodes/1, nodes/2, caches/1]).
-export([add_node/2, delete_node/2, update_nodes/2]).
-export([add_cache/3, delete_cache/2]).
-export([empty/1]).

new() -> ?EMPTY.

read() ->
	{ok, FileName} = application:get_env(oblivion, oblivion_persistence_file),
	case file_exists(FileName) of
		true ->
			case read_config(FileName) of
				{ok, Config} -> {ok, Config};
				{error, Reason} -> {error, Reason}
			end;
		false -> {ok, new()}
	end.	

write(Config) ->
	{ok, FileName} = application:get_env(oblivion, oblivion_persistence_file),
	store_config(FileName, Config).

write(Nodes, Caches) ->
	Config = ?CONFIG_DATA(Nodes, Caches),
	case write(Config) of
		ok -> {ok, Config};
		{error, Reason} -> {error, Reason}
	end.

delete() -> write(new()).

nodes(?CONFIG_DATA(Nodes, _)) -> Nodes.

nodes(?CONFIG_DATA(Nodes, _), Operation) -> nodes(Nodes, Operation);
nodes(Nodes, import) -> lists:delete(node(), Nodes);
nodes(Nodes, export) -> Nodes.

caches(?CONFIG_DATA(_, Caches)) -> Caches.

add_node(Node, ?CONFIG_DATA(Nodes, Caches)) ->
	Config = ?CONFIG_DATA([Node|Nodes], Caches),
	case write(Config) of
		ok -> {ok, Config};
		{error, Reason} -> {error, Reason}
	end.

delete_node(Node, ?CONFIG_DATA(Nodes, Caches)) ->
	Config = ?CONFIG_DATA(lists:delete(Node, Nodes), Caches),
	case write(Config) of
		ok -> {ok, Config};
		{error, Reason} -> {error, Reason}
	end.

update_nodes(Nodes, ?CONFIG_DATA(_, Caches)) ->
	Config = ?CONFIG_DATA(nodes(Nodes, import), Caches),
	case write(Config) of
		ok -> {ok, Config};
		{error, Reason} -> {error, Reason}
	end.	

add_cache(Cache, Options, ?CONFIG_DATA(Nodes, Caches)) ->
	Caches1 = lists:keystore(Cache, 1, Caches, ?CACHE_CONFIG(Cache, Options)),
	Config = ?CONFIG_DATA(Nodes, Caches1),
	case write(Config) of
		ok -> {ok, Config};
		{error, Reason} -> {error, Reason}
	end.

delete_cache(Cache, ?CONFIG_DATA(Nodes, Caches)) ->
	Caches1 = lists:keydelete(Cache, 1, Caches),
	Config = ?CONFIG_DATA(Nodes, Caches1),
	case write(Config) of
		ok -> {ok, Config};
		{error, Reason} -> {error, Reason}
	end.

empty(?EMPTY) -> true;
empty(_Config) -> false.

%% ====================================================================
%% Internal functions
%% ====================================================================

file_exists(FileName) ->
	case file:read_file_info(FileName) of
		{ok, _} -> true;
		{error, enoent} -> false;
		{error, Reason} -> {error, Reason}
	end.

read_config(FileName) ->
	file:script(FileName).

store_config(FileName, Config) ->
	case file:open(FileName, write) of
		{ok, H} ->
			io:format(H, "~p.", [Config]),
			file:close(H);
		{error, Reason} -> {error, Reason}
	end.
