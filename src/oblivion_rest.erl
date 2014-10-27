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

-include("oblivion_error.hrl").
-include("oblivion_rest.hrl").

-define(FIELD(Key, Value), {Key, Value}).
-define(FIELDS(Key, Value), [?FIELD(Key, Value)]).

-define(ERROR(ErrorCode), {error, ErrorCode}).
-define(SUCCESS(Fields), {ok, Fields}).
-define(SUCCESS(Key, Value), ?SUCCESS(?FIELDS(Key, Value))).

-behaviour(kb_action_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/3]).

%% Data management
% GET /{cache}/{key}
handle(<<"GET">>, [Cache, Key], Req) ->
	CacheName = cache_name(Cache),
	Response = g_cache:get(CacheName, Key),
	Reply = cache_to_reply(Response),
	{json, Reply, Req};

% PUT /{cache}/{key} + BODY({value})
handle(<<"PUT">>, [Cache, Key], Req) ->
	{JSon, Req1} = kb_action_helper:get_json(Req),
	Reply = case jsondoc:get_value(?DATA_TAG, JSon) of
		undefined -> reply(?ERROR(?ERROR_INVALID_REQUEST));
		Value ->
			CacheName = cache_name(Cache),
			Response = g_cache:store(CacheName, Key, Value),
			cache_to_reply(Response)	
	end,
	{json, Reply, Req1};

% DELETE /{cache}/{key}
handle(<<"DELETE">>, [Cache, Key], Req) ->
	CacheName = cache_name(Cache),
	Response = g_cache:delete(CacheName, Key),
	Reply = cache_to_reply(Response),
	{json, Reply, Req};

%% Cache management
% GET / 
handle(<<"GET">>, [], Req) ->
	CacheList = gibreel:list_caches(),
	Reply = reply(?SUCCESS(?CACHE_LIST_TAG, CacheList)),
	{json, Reply, Req};

% POST /{cache} + BODY({config})
handle(<<"POST">>, [Cache], Req) ->
	{JSon, Req1} = kb_action_helper:get_json(Req),
	CacheName = cache_name(Cache),
	Options = lists:foldl(fun({?CACHE_OPTION_MAX_AGE, MaxAge}, Acc) -> [{max_age, MaxAge}, 
						{purge_interval, MaxAge}|Acc];
				({?CACHE_OPTION_MAX_SIZE, MaxSize}, Acc) -> [{max_size, MaxSize}|Acc]
			end, [], jsondoc:to_proplist(JSon)),
	Reply = case oblivion:create_cache(CacheName, Options) of
		ok -> reply(ok);
		{error, duplicated} -> reply(?ERROR(?ERROR_CACHE_ALREDY_EXISTS));
		{error, _Reason} -> reply(?ERROR(?ERROR_INVALID_REQUEST))
	end,
	{json, Reply, Req1};

% GET /{cache}
handle(<<"GET">>, [_Cache], Req) ->
	Reply = reply(?ERROR(?ERROR_OPERATION_NOT_IMPLEMENTED)),
	{json, Reply, Req};

% PUT /{cache} + BODY({config})
handle(<<"PUT">>, [_Cache], Req) ->
	Reply = reply(?ERROR(?ERROR_OPERATION_NOT_IMPLEMENTED)),
	{json, Reply, Req};

% DELETE /{cache}
handle(<<"DELETE">>, [_Cache], Req) ->
	Reply = reply(?ERROR(?ERROR_OPERATION_NOT_IMPLEMENTED)),
	{json, Reply, Req};

%% System management

handle(_Method, _Path, Req) ->
	Reply = reply(?ERROR(?ERROR_OPERATION_NOT_SUPPORTED)),
	{json, Reply, Req}.

%% ====================================================================
%% Internal functions
%% ====================================================================

reply(ok) -> success_msg([]);
reply(?SUCCESS(Fields)) -> success_msg(Fields);
reply(?ERROR(Error)) -> error_msg(Error).

error_msg(Error) ->
	Reply = [
			{?SUCCESS_TAG, false},
			{?ERROR_TAG, Error}
			],
	jsondoc:from_proplist(Reply).

success_msg(Fields) ->
	Reply = [{?SUCCESS_TAG, true}] ++ Fields,
	jsondoc:from_proplist(Reply).

cache_name(Cache) ->
	binary_to_atom(Cache, utf8).

cache_to_reply({ok, Value}) -> reply(?SUCCESS(?DATA_TAG, Value));
cache_to_reply(not_found) -> reply(?ERROR(?ERROR_KEY_NOT_FOUND));
cache_to_reply(ok) -> reply(ok);
cache_to_reply(no_cache) -> reply(?ERROR(?ERROR_CACHE_NOT_FOUND));
cache_to_reply(error) -> reply(?ERROR(?ERROR_SERVER_ERROR)).