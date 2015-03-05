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

-module(oblivion_rest).

-include("oblivion_rest.hrl").
-include_lib("gibreel/include/gibreel.hrl").

-behaviour(kb_action_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/3]).

%% Data management

%GET /caches/{cache}/keys/{key} - Return the value
handle(<<"GET">>, [<<"caches">>, CacheName, <<"keys">>, Key], Req) ->
	Cache = binary_to_atom(CacheName, utf8),
	case g_cache:get(Cache, Key) of
		no_cache -> cache_not_found(Req);
		not_found -> key_not_found(Req);
		error -> unexpected_error(Req);
		{ok, Value, Version} -> success(200, Value, ?ETAG_HEADER_LIST(Version), Req)
	end;

%PUT /caches/{cache}/keys/{key}[?version={versionID}] - Add ou change value
handle(<<"PUT">>, [<<"caches">>, CacheName, <<"keys">>, Key], Req) ->
	try kb_action_helper:get_json(Req) of
		{Value, Req1} ->
			Cache = binary_to_atom(CacheName, utf8),
			{Args, Req2} = kb_action_helper:get_args(Req1),
			Options = get_options(Args),
			case g_cache:store(Cache, Key, Value, Options) of
				no_cache -> cache_not_found(Req2);
				invalid_version -> invalid_version(Req2);
				{ok, Version} -> success(201, ?OK, ?ETAG_HEADER_LIST(Version), Req2)
			end
	catch _:_ -> invalid_json(Req)
	end; 

%HEAD /caches/{cache}/keys/{key} - Get key version
handle(<<"HEAD">>, [<<"caches">>, CacheName, <<"keys">>, Key], Req) ->
	Cache = binary_to_atom(CacheName, utf8),
	case g_cache:get(Cache, Key) of
		no_cache -> cache_not_found(Req);
		not_found -> key_not_found(Req);
		error -> unexpected_error(Req);
		{ok, _Value, Version} -> success_head(200, ?ETAG_HEADER_LIST(Version), Req)
	end;

%DELETE /caches/{cache}/keys/{key}[?version={versionID}] - Delete key
handle(<<"DELETE">>, [<<"caches">>, CacheName, <<"keys">>, Key], Req) ->
	Cache = binary_to_atom(CacheName, utf8),
	{Args, Req1} = kb_action_helper:get_args(Req),
	Options = get_options(Args),
	case g_cache:remove(Cache, Key, Options) of
		no_cache -> cache_not_found(Req1);
		invalid_version -> invalid_version(Req1);
		ok -> success(200, ?OK, ?BASIC_HEADER_LIST, Req1)
	end;

%GET /caches/{cache}/keys - get cache key list
handle(<<"GET">>, [<<"caches">>, CacheName, <<"keys">>], Req) ->
	Cache = binary_to_atom(CacheName, utf8),
	case g_cache:get_all_keys(Cache) of
		no_cache -> cache_not_found(Req);
		KeyList ->
			Reply = [{<<"keys">>, KeyList}],
			success(200, Reply, ?BASIC_HEADER_LIST, Req)
	end;

%DELETE /caches/{cache}/keys - Delete all key from cache 
handle(<<"DELETE">>, [<<"caches">>, CacheName, <<"keys">>], Req) ->
	Cache = binary_to_atom(CacheName, utf8),
	case g_cache:flush(Cache) of
		no_cache -> cache_not_found(Req);
		ok -> success(202, ?OK, ?BASIC_HEADER_LIST, Req)
	end;

%% Cache management

%GET /caches - Return cache list 
handle(<<"GET">>, [<<"caches">>], Req) ->
	CacheList = gibreel:list_caches(),
	Reply = [{<<"caches">>, CacheList}],
	success(200, Reply, ?BASIC_HEADER_LIST, Req);

%PUT /caches/{cache} - Create a new cache
handle(<<"PUT">>, [<<"caches">>, CacheName], Req) ->
	try kb_action_helper:get_json(Req) of
		{Config, Req1} ->
			Cache = binary_to_atom(CacheName, utf8),
			Options = convert_to_gibreel(Config),
			case oblivion:create_cache(Cache, Options) of
				{error, duplicated} -> duplicated_cache(Req1);
				{error, Reason} -> validation_error(Reason, Req1);
				ok -> success(201, ?OK, ?BASIC_HEADER_LIST, Req1)
			end
	catch _:_ -> invalid_json(Req)
	end; 

%GET /caches/{cache} - Return cache configuration
handle(<<"GET">>, [<<"caches">>, CacheName], Req) ->
	Cache = binary_to_atom(CacheName, utf8),
	case oblivion:get_cache_config(Cache) of
		no_cache -> cache_not_found(Req);
		Options -> success(200, convert_from_gibreel(Options), ?BASIC_HEADER_LIST, Req)
	end;

%DELETE /caches/{cache} - Delete cache
handle(<<"DELETE">>, [<<"caches">>, CacheName], Req) ->
	Cache = binary_to_atom(CacheName, utf8),
	case oblivion:delete_cache(Cache) of
		no_cache -> cache_not_found(Req);
		ok -> success(202, ?OK, ?BASIC_HEADER_LIST, Req)
	end;

%% System management

%GET /nodes - Return cluster node list
handle(<<"GET">>, [<<"nodes">>], Req) ->
	NodeList = oblivion:get_node_list(),
	OnlineNodes = oblivion:get_online_node_list(),
	RetList = lists:map(fun(Node) ->
					Online = lists:member(Node, OnlineNodes),
					[{<<"node">>, Node}, {<<"online">>, Online}]					
			end, NodeList),
	Reply = [{<<"nodes">>, RetList}],
	success(200, Reply, ?BASIC_HEADER_LIST, Req);

%PUT /nodes/{node} - Add node to cluster
handle(<<"PUT">>, [<<"nodes">>, Node], Req) ->
	NewNode = binary_to_atom(Node, utf8),
	case oblivion:add_node(NewNode) of
		{error, Reason} -> validation_error(Reason, Req);
		ok -> success(202, ?OK, ?BASIC_HEADER_LIST, Req)
	end;

%DELETE /nodes/{node} - Delete node from cluster
handle(<<"DELETE">>, [<<"nodes">>, Node], Req) ->
	NewNode = binary_to_atom(Node, utf8),
	oblivion:delete_node(NewNode),
	success(202, ?OK, ?BASIC_HEADER_LIST, Req);

%% Fail
handle(_Method, _Path, Req) -> ?rest_error(?OPERATION_NOT_SUPPORTED_ERROR, Req).

%% ====================================================================
%% Internal functions
%% ====================================================================

get_options([]) -> []; 
get_options(Args) -> 
	lists:filtermap(fun({?VERSION_TAG, Version}) -> {true, {?OPTION_VERSION, Version}};
			(_) -> false
		end, Args).


convert_to_gibreel(Options) -> convert_to_gibreel(Options, []).

convert_to_gibreel([{<<"max-age">>, Value}|T], Output) -> convert_to_gibreel(T, [{max_age, Value}|Output]);
convert_to_gibreel([{<<"max-size">>, Value}|T], Output) -> convert_to_gibreel(T, [{max_size, Value}|Output]);
convert_to_gibreel([{<<"synchronize-on-startup">>, true}|T], Output) -> convert_to_gibreel(T, [{sync_mode, ?FULL_SYNC_MODE}|Output]);
convert_to_gibreel([{<<"synchronize-on-startup">>, false}|T], Output) -> convert_to_gibreel(T, [{sync_mode, ?LAZY_SYNC_MODE}|Output]);
convert_to_gibreel([_|T], Output) -> convert_to_gibreel(T, Output);
convert_to_gibreel([], Output) -> Output.

convert_from_gibreel(Options) -> 
	lists:filtermap(fun({max_age, ?NO_MAX_AGE}) -> false;
			({max_age, Value}) -> {true, {<<"max-age">>, Value}};
			({max_size, ?NO_MAX_SIZE}) -> false;
			({max_size, Value}) -> {true, {<<"max-size">>, Value}};
			({sync_mode, ?LAZY_SYNC_MODE}) -> {true, {<<"synchronize-on-startup">>, false}};
			({sync_mode, ?FULL_SYNC_MODE}) -> {true, {<<"synchronize-on-startup">>, true}};
			(_) -> false
		end, Options).

unexpected_error(Req) -> ?rest_error(?UNEXPECTED_ERROR, Req).

cache_not_found(Req) ->	?rest_error(?CACHE_NOT_EXISTS_ERROR, Req).
key_not_found(Req) -> ?rest_error(?KEY_NOT_EXISTS_ERROR, Req).
invalid_version(Req) -> ?rest_error(?INVALID_VERSION_ERROR, Req).
invalid_json(Req) -> ?rest_error(?INVALID_JSON_ERROR, Req).
duplicated_cache(Req) -> ?rest_error(?DUPLICATED_CACHE_ERROR, Req).

validation_error(Reason, Req) when is_list(Reason) -> 
	validation_error(list_to_binary(Reason), Req);
validation_error(Reason, Req) ->
	Error = ?ERROR(?STATUS_406, Reason),
	?rest_error(Error, Req).

success(Status, Reply, Headers, Req) -> {json, Status, Headers, Reply, Req}.
success_head(Status, Headers, Req) -> {raw, Status, Headers, <<"">>, Req}.