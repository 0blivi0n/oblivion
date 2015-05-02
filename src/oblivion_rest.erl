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

%% System

%GET /system - Return the server verion
handle(<<"GET">>, [<<"system">>], Req) ->
	success(200, oblivion_api:system(), ?BASIC_HEADER_LIST, Req);

%% Data management

%GET /caches/{cache}/keys/{key} - Return the value
handle(<<"GET">>, [<<"caches">>, CacheName, <<"keys">>, Key], Req) ->
	case oblivion_api:get(CacheName, Key) of
		no_cache -> cache_not_found(Req);
		not_found -> key_not_found(Req);
		error -> unexpected_error(Req);
		{ok, Value, Version} -> success(200, Value, ?ETAG_HEADER_LIST(Version), Req)
	end;

%PUT /caches/{cache}/keys/{key}[?version={versionID}] - Add ou change value
handle(<<"PUT">>, [<<"caches">>, CacheName, <<"keys">>, Key], Req) ->
	try kb_action_helper:get_json(Req) of
		{Value, Req1} ->
			{Args, Req2} = kb_action_helper:get_args(Req1),
			Options = options(Args, [?OPTION_VERSION]),
			case oblivion_api:put(CacheName, Key, Value, Options) of
				no_cache -> cache_not_found(Req2);
				invalid_version -> invalid_version(Req2);
				{ok, Version} -> success(201, ?OK, ?ETAG_HEADER_LIST(Version), Req2)
			end
	catch _:_ -> invalid_json(Req)
	end; 

%HEAD /caches/{cache}/keys/{key} - Get key version
handle(<<"HEAD">>, [<<"caches">>, CacheName, <<"keys">>, Key], Req) ->
	case oblivion_api:version(CacheName, Key) of
		no_cache -> cache_not_found(Req);
		not_found -> key_not_found(Req);
		error -> unexpected_error(Req);
		{ok, Version} -> success_head(200, ?ETAG_HEADER_LIST(Version), Req)
	end;

%DELETE /caches/{cache}/keys/{key}[?version={versionID}] - Delete key
handle(<<"DELETE">>, [<<"caches">>, CacheName, <<"keys">>, Key], Req) ->
	{Args, Req1} = kb_action_helper:get_args(Req),
	Options = options(Args, [?OPTION_VERSION]),
	case oblivion_api:delete(CacheName, Key, Options) of
		no_cache -> cache_not_found(Req1);
		invalid_version -> invalid_version(Req1);
		ok -> success(200, ?OK, ?BASIC_HEADER_LIST, Req1)
	end;

%GET /caches/{cache}/keys[?list=<true|false>[&sort=<true|false>]] - get size or cache key list
handle(<<"GET">>, [<<"caches">>, CacheName, <<"keys">>], Req) ->
	{Args, Req1} = kb_action_helper:get_args(Req),
	Options = options(Args, [?LIST_TAG, ?SORT_TAG]),
	List = option(?LIST_TAG, Options, false),
	Reply = case List of
		true ->
			Sort = option(?SORT_TAG, Options, false),
			case oblivion_api:keys(CacheName, Sort) of
				no_cache -> no_cache;
				KeyList -> [{<<"keys">>, KeyList}]
			end;
		false -> oblivion_api:size(CacheName)
	end,
	case Reply of
		no_cache -> cache_not_found(Req1);
		_ -> success(200, Reply, ?BASIC_HEADER_LIST, Req1)
	end;

%DELETE /caches/{cache}/keys - Delete all key from cache 
handle(<<"DELETE">>, [<<"caches">>, CacheName, <<"keys">>], Req) ->
	case oblivion_api:flush(CacheName) of
		no_cache -> cache_not_found(Req);
		ok -> success(202, ?OK, ?BASIC_HEADER_LIST, Req)
	end;

%% Cache management

%GET /caches[?sort=<true|false>[&include_config=<true|false>[&include_size=<true|false>]]] - Return cache list 
handle(<<"GET">>, [<<"caches">>], Req) ->
	{Args, Req1} = kb_action_helper:get_args(Req),
	Options = options(Args, [?SORT_TAG, ?INCLUDE_CONFIG_TAG, ?INCLUDE_SIZE_TAG]),
	Sort = option(?SORT_TAG, Options, false),
	IncludeConfig = option(?INCLUDE_CONFIG_TAG, Options, false),
	IncludeSize = option(?INCLUDE_SIZE_TAG, Options, false),
	Caches = oblivion_api:caches(IncludeConfig, IncludeSize, Sort),
	Reply = [{<<"caches">>, Caches}],
	success(200, Reply, ?BASIC_HEADER_LIST, Req1);

%PUT /caches/{cache} - Create a new cache
handle(<<"PUT">>, [<<"caches">>, CacheName], Req) ->
	try kb_action_helper:get_json(Req) of
		{Config, Req1} ->
			case oblivion_api:create(CacheName, Config) of
				{error, duplicated} -> duplicated_cache(Req1);
				{error, Reason} -> validation_error(Reason, Req1);
				ok -> success(201, ?OK, ?BASIC_HEADER_LIST, Req1)
			end
	catch _:_ -> invalid_json(Req)
	end; 

%GET /caches/{cache} - Return cache configuration
handle(<<"GET">>, [<<"caches">>, CacheName], Req) ->
	case oblivion_api:config(CacheName) of
		no_cache -> cache_not_found(Req);
		Config -> success(200, Config, ?BASIC_HEADER_LIST, Req)
	end;

%DELETE /caches/{cache} - Delete cache
handle(<<"DELETE">>, [<<"caches">>, CacheName], Req) ->
	case oblivion_api:drop(CacheName) of
		no_cache -> cache_not_found(Req);
		ok -> success(202, ?OK, ?BASIC_HEADER_LIST, Req)
	end;

%% System management

%GET /nodes[?sort=<true|false>] - Return cluster node list
handle(<<"GET">>, [<<"nodes">>], Req) ->
	{Args, Req1} = kb_action_helper:get_args(Req),
	Options = options(Args, [?SORT_TAG]),
	Sort = option(?SORT_TAG, Options, false),
	Nodes = oblivion_api:nodes(Sort),
	Reply = [{<<"nodes">>, Nodes}],
	success(200, Reply, ?BASIC_HEADER_LIST, Req1);

%PUT /nodes/{node} - Add node to cluster
handle(<<"PUT">>, [<<"nodes">>, Node], Req) ->
	case oblivion_api:add_node(Node) of
		{error, Reason} -> validation_error(Reason, Req);
		ok -> success(202, ?OK, ?BASIC_HEADER_LIST, Req);
		duplicated -> duplicated_node(Req)
	end;

%DELETE /nodes/{node} - Delete node from cluster
handle(<<"DELETE">>, [<<"nodes">>, Node], Req) ->
	case oblivion_api:delete_node(Node) of
		{error, _Reason} -> unexpected_error(Req);
		ok -> success(202, ?OK, ?BASIC_HEADER_LIST, Req)
	end;

%% Fail
handle(_Method, _Path, Req) -> ?rest_error(?OPERATION_NOT_SUPPORTED_ERROR, Req).

%% ====================================================================
%% Internal functions
%% ====================================================================

options([], _Select) -> []; 
options(Args, Select) -> 
	Options = lists:filtermap(fun({?VERSION_TAG, Version}) -> {true, {?OPTION_VERSION, Version}};
				({?SORT_TAG, <<"true">>}) -> {true, {?SORT_TAG, true}};
				({?SORT_TAG, <<"false">>}) -> {true, {?SORT_TAG, false}};
				({?INCLUDE_CONFIG_TAG, <<"true">>}) -> {true, {?INCLUDE_CONFIG_TAG, true}};
				({?INCLUDE_CONFIG_TAG, <<"false">>}) -> {true, {?INCLUDE_CONFIG_TAG, false}};				
				({?INCLUDE_SIZE_TAG, <<"true">>}) -> {true, {?INCLUDE_SIZE_TAG, true}};
				({?INCLUDE_SIZE_TAG, <<"false">>}) -> {true, {?INCLUDE_SIZE_TAG, false}};	
				({?LIST_TAG, <<"true">>}) -> {true, {?LIST_TAG, true}};
				({?LIST_TAG, <<"false">>}) -> {true, {?LIST_TAG, false}};									 								 	   
				(_) -> false
			end, Args),
	lists:filter(fun({Key, _}) -> lists:member(Key, Select) end, Options).

option(_Tag, [], Default) -> Default;
option(Tag, Options, Default) ->
	case lists:keyfind(Tag, 1, Options) of
		false -> Default;
		{_, Value} -> Value
	end.

unexpected_error(Req) -> ?rest_error(?UNEXPECTED_ERROR, Req).

cache_not_found(Req) ->	?rest_error(?CACHE_NOT_EXISTS_ERROR, Req).
key_not_found(Req) -> ?rest_error(?KEY_NOT_EXISTS_ERROR, Req).
invalid_version(Req) -> ?rest_error(?INVALID_VERSION_ERROR, Req).
invalid_json(Req) -> ?rest_error(?INVALID_JSON_ERROR, Req).
duplicated_cache(Req) -> ?rest_error(?DUPLICATED_CACHE_ERROR, Req).
duplicated_node(Req) -> ?rest_error(?DUPLICATED_NODE_ERROR, Req).

validation_error(Reason, Req) when is_list(Reason) -> 
	validation_error(list_to_binary(Reason), Req);
validation_error(Reason, Req) ->
	Error = ?ERROR(?STATUS_406, Reason),
	?rest_error(Error, Req).

success(Status, Reply, Headers, Req) -> {json, Status, Headers, Reply, Req}.
success_head(Status, Headers, Req) -> {raw, Status, Headers, <<"">>, Req}.