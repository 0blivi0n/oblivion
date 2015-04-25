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

-module(oblivion_api).

-include("oblivion_api.hrl").
-include_lib("gibreel/include/gibreel.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([system/0]).
-export([get/2, put/4, version/2, delete/3]).
-export([keys/2, flush/1]).
-export([caches/2]).
-export([create/2, config/1, drop/1]).
-export([nodes/1, node_list/0, online_node_list/0]).
-export([add_node/1, delete_node/1]).

system() ->
	{ok, Version} = application:get_key(oblivion, vsn),
	[{?KEY_NODE, node()}, {?KEY_VERSION, list_to_binary(Version)}].	

get(CacheName, Key) ->
	Cache = cache_name(CacheName),
	g_cache:get(Cache, Key).

put(CacheName, Key, Value, Options) ->
	Cache = cache_name(CacheName),
	g_cache:store(Cache, Key, Value, Options).

version(CacheName, Key) ->
	Cache = cache_name(CacheName),
	case g_cache:get(Cache, Key) of
		{ok, _Value, Version} -> {ok, Version};
		Other -> Other
	end.

delete(CacheName, Key, Options) ->
	Cache = cache_name(CacheName),
	g_cache:remove(Cache, Key, Options).

keys(CacheName, Sort) ->
	Cache = cache_name(CacheName),
	case g_cache:get_all_keys(Cache) of
		no_cache -> no_cache;
		List -> sort(List, Sort)
	end.

flush(CacheName) ->
	Cache = cache_name(CacheName),
	g_cache:flush(Cache).

caches(Include, Sort) ->
	CacheList = gibreel:list_caches(),
	SortedCacheList = sort(CacheList, Sort),
	FunConfig = fun(Cache, true) ->
			case gibreel:cache_config(Cache) of
				no_cache -> ignore;
				Config -> {ok, convert_from_gibreel(Config)}
			end;
		(_Cache, false) -> name
	end,
	FunName = fun(Cache) ->
			Cache2 = atom_to_binary(Cache, utf8),
			<<Prefix:4/binary, CacheName/binary>> = Cache2,
			case Prefix of
				?CACHENAME_PREFIX -> {ok, CacheName};
				_ -> ignore
			end					  
	end,
	lists:filtermap(fun(Cache) -> 			 
				case FunName(Cache) of
					{ok, Name} ->
						case FunConfig(Cache, Include) of
							{ok, Config} ->
								Reply = [
										{?KEY_CACHE, Name},
										{?KEY_CONFIG, Config}
										],
								{true, Reply};
							ignore -> false;
							name -> {true, Name}
						end;
					ignore -> false
				end
		end, SortedCacheList).	

create(CacheName, Config) ->
	Cache = cache_name(CacheName),
	Options = convert_to_gibreel(Config),
	oblivion_server:create_cache(Cache, Options).

config(CacheName) ->
	Cache = cache_name(CacheName),
	case gibreel:cache_config(Cache) of
		no_cache -> no_cache;
		Options -> convert_from_gibreel(Options)
	end.

drop(CacheName) ->
	Cache = cache_name(CacheName),
	oblivion_server:delete_cache(Cache).

nodes(Sort) ->
	NodeList = node_list(),
	OnlineNodes = online_node_list(),
	SortedNodeList = sort(NodeList, Sort),
	lists:map(fun(Node) ->
				Online = lists:member(Node, OnlineNodes),
				ServerData = case Online of
					true ->
						{Server, Port, Broadcast} = oblivion_server:get_node_port(Node),
						[
							{?KEY_SERVER, list_to_binary(Server)},
							{?KEY_PORT, Port},
							{?KEY_BROADCAST, Broadcast}
							];
					_ -> []
				end,
				[
					{?KEY_NODE, Node}, 
					{?KEY_ONLINE, Online}
					] ++ ServerData
		end, SortedNodeList).

node_list() -> [node()|columbo:known_nodes()].

online_node_list() -> [node()|columbo:online_nodes()].

add_node(NodeName) when is_binary(NodeName) -> 
	Node = binary_to_atom(NodeName, utf8),
	add_node(Node);
add_node(Node) -> oblivion_server:add_node(Node).

delete_node(NodeName) when is_binary(NodeName) -> 
	Node = binary_to_atom(NodeName, utf8),
	delete_node(Node);
delete_node(Node) -> oblivion_server:delete_node(Node).

%% ====================================================================
%% Internal functions
%% ====================================================================

cache_name(CacheName) -> 
	Cache = <<?CACHENAME_PREFIX/binary, CacheName/binary>>,
	binary_to_atom(Cache, utf8).

sort(List, false) -> List;
sort(List, true) -> lists:sort(List).

convert_to_gibreel(Options) -> convert_to_gibreel(Options, []).

convert_to_gibreel([{?KEY_MAX_AGE, Value}|T], Output) -> convert_to_gibreel(T, [{max_age, Value}|Output]);
convert_to_gibreel([{?KEY_MAX_SIZE, Value}|T], Output) -> convert_to_gibreel(T, [{max_size, Value}|Output]);
convert_to_gibreel([_|T], Output) -> convert_to_gibreel(T, Output);
convert_to_gibreel([], Output) -> Output.

convert_from_gibreel(Options) -> 
	lists:filtermap(fun({max_age, ?NO_MAX_AGE}) -> false;
			({max_age, Value}) -> {true, {?KEY_MAX_AGE, Value}};
			({max_size, ?NO_MAX_SIZE}) -> false;
			({max_size, Value}) -> {true, {?KEY_MAX_SIZE, Value}};
			(_) -> false
		end, Options).