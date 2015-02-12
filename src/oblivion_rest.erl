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
		{ok, Value, Version} -> success(Value, ?ETAG_HEADER_LIST(Version), Req)
	end;

%PUT /caches/{cache}/keys/{key}[?version={versionID}] - Add ou change value
handle(<<"PUT">>, [<<"caches">>, CacheName, <<"keys">>, Key], Req) ->
	Cache = binary_to_atom(CacheName, utf8),
	{Value, Req1} = kb_action_helper:get_json(Req),
	{Args, Req2} = kb_action_helper:get_args(Req1),
	Options = get_options(Args),
	case g_cache:store(Cache, Key, Value, Options) of
		no_cache -> cache_not_found(Req2);
		invalid_version -> invalid_version(Req2);
		{ok, Version} -> success(?OK, ?ETAG_HEADER_LIST(Version), Req2)
	end;

%HEAD /caches/{cache}/keys/{key} - Get key version
handle(<<"HEAD">>, [<<"caches">>, CacheName, <<"keys">>, Key], Req) ->
	Cache = binary_to_atom(CacheName, utf8),
	case g_cache:get(Cache, Key) of
		no_cache -> cache_not_found(Req);
		not_found -> key_not_found(Req);
		error -> unexpected_error(Req);
		{ok, _Value, Version} -> success_head(?ETAG_HEADER_LIST(Version), Req)
	end;

%DELETE /caches/{cache}/keys/{key}[?version={versionID}] - Delete key
handle(<<"DELETE">>, [<<"caches">>, CacheName, <<"keys">>, Key], Req) ->
	Cache = binary_to_atom(CacheName, utf8),
	{Args, Req1} = kb_action_helper:get_args(Req),
	Options = get_options(Args),
	case g_cache:remove(Cache, Key, Options) of
		no_cache -> cache_not_found(Req1);
		invalid_version -> invalid_version(Req1);
		ok -> success(?OK, ?BASIC_HEADER_LIST, Req1)
	end;

%GET /caches/{cache}/keys - get cache key list
handle(<<"GET">>, [<<"caches">>, CacheName, <<"keys">>], Req) ->
	Cache = binary_to_atom(CacheName, utf8),
	case g_cache:get_all_keys(Cache) of
		no_cache -> cache_not_found(Req);
		KeyList ->
			Reply = [{<<"keys">>, KeyList}],
			success(Reply, ?BASIC_HEADER_LIST, Req)
	end;

%DELETE /caches/{cache}/keys - Delete all key from cache 
handle(<<"DELETE">>, [<<"caches">>, CacheName, <<"keys">>], Req) ->
	Cache = binary_to_atom(CacheName, utf8),
	case g_cache:flush(Cache) of
		no_cache -> cache_not_found(Req);
		ok -> success(?OK, ?BASIC_HEADER_LIST, Req)
	end;

%% Cache management

%GET /caches - Return cache list 
%POST /caches - Create a new cache
%GET /caches/{cache} - Return cache configuration
%DELETE /caches/{cache} - Delete cache

%% System management

%GET /nodes - Return cluster node list
%PUT /nodes/{node} - Add node to cluster
%DELETE /nodes/{node} - Delete node from cluster

%% Fail
handle(_Method, _Path, Req) -> ?rest_error(?OPERATION_NOT_SUPPORTED_ERROR, Req).

%% ====================================================================
%% Internal functions
%% ====================================================================

get_options(Args) -> get_options(Args, []).

get_options([{?VERSION_TAG, Version}|T], Options) -> 
	get_options(T, lists:keystore(?OPTION_VERSION, 1, {?OPTION_VERSION, Version}, Options));
get_options([], Options) -> Options.

unexpected_error(Req) -> ?rest_error(?UNEXPECTED_ERROR, Req).

cache_not_found(Req) ->	?rest_error(?CACHE_NOT_EXISTS_ERROR, Req).
key_not_found(Req) -> ?rest_error(?KEY_NOT_EXISTS_ERROR, Req).
invalid_version(Req) -> ?rest_error(?INVALID_VERSION_ERROR, Req).

success(Reply, Headers, Req) -> {json, 200, Headers, Reply, Req}.
success_head(Headers, Req) -> {raw, 200, Headers, <<"">>, Req}.