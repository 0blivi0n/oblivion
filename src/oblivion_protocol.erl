%%
%% Copyright 2014-15 Joaquim Rocha <jrocha@gmailbox.org>
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

-module(oblivion_protocol).

-include("oblivion_protocol.hrl").
-include_lib("gibreel/include/gibreel.hrl").

-behaviour(mercury_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/4]).

%% handle(Operation :: binary(), Resource :: list(), Params :: list(), Payload :: empty | any()) 
%%	-> {reply, Status :: integer(), Params :: list(), Payload :: any()} 
%%	| {reply, Status :: integer(), Params :: list()}
%%	| {reply, Status :: integer()}.

%% System

% GET system
handle(<<"GET">>, [<<"system">>], [], empty) ->
	Payload = oblivion_api:system(),
	success_reply(200, [], Payload);

% GET /nodes[?sort=<true|false>]
handle(<<"GET">>, [<<"nodes">>], Args, empty) ->
	Options = options(Args, [?SORT_TAG]),
	Sort = option(?SORT_TAG, Options, false),
	Nodes = oblivion_api:nodes(Sort),
	Reply = [{<<"nodes">>, Nodes}],
	success_reply(200, [], Reply);

%% Data management

% GET /caches/{cache}/keys/{key}
handle(<<"GET">>, [<<"caches">>, CacheName, <<"keys">>, Key], [], empty) ->
	case oblivion_api:get(CacheName, Key) of
		no_cache -> error_reply(?CACHE_NOT_EXISTS_ERROR);
		not_found -> error_reply(?KEY_NOT_EXISTS_ERROR);
		error -> error_reply(?UNEXPECTED_ERROR);
		{ok, Value, Version} -> 
			success_reply(200, [{?VERSION_TAG, Version}], Value)
	end;	

% PUT /caches/{cache}/keys/{key}[?version={versionID}]
handle(<<"PUT">>, [<<"caches">>, CacheName, <<"keys">>, Key], Args, Value) ->
	case is_ejson(Value) of
		true ->
			Options = options(Args, [?OPTION_VERSION]),
			case oblivion_api:put(CacheName, Key, Value, Options) of
				no_cache -> error_reply(?CACHE_NOT_EXISTS_ERROR);
				invalid_version -> error_reply(?INVALID_VERSION_ERROR);
				{ok, Version} -> success_reply(201, [{?VERSION_TAG, Version}])
			end;
		_ -> error_reply(?INVALID_JSON_ERROR)
	end; 

% VERSION /caches/{cache}/keys/{key} 
handle(<<"VERSION">>, [<<"caches">>, CacheName, <<"keys">>, Key], [], empty) ->
	case oblivion_api:version(CacheName, Key) of
		no_cache -> error_reply(?CACHE_NOT_EXISTS_ERROR);
		not_found -> error_reply(?KEY_NOT_EXISTS_ERROR);
		error -> error_reply(?UNEXPECTED_ERROR);
		{ok, Version} -> 
			success_reply(200, [{?VERSION_TAG, Version}])			
	end;

% DELETE /caches/{cache}/keys/{key}[?version={versionID}] 
handle(<<"DELETE">>, [<<"caches">>, CacheName, <<"keys">>, Key], Args, empty) ->
	Options = options(Args, [?OPTION_VERSION]),
	case oblivion_api:delete(CacheName, Key, Options) of
		no_cache -> error_reply(?CACHE_NOT_EXISTS_ERROR);
		invalid_version -> error_reply(?INVALID_VERSION_ERROR);
		ok -> success_reply(200)
	end;

% GET /caches/{cache}/keys[?list=<true|false>[&sort=<true|false>]] 
handle(<<"GET">>, [<<"caches">>, CacheName, <<"keys">>], Args, empty) ->
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
		no_cache -> error_reply(?CACHE_NOT_EXISTS_ERROR);
		_ -> success_reply(200, [], Reply)
	end;

% DELETE /caches/{cache}/keys 
handle(<<"DELETE">>, [<<"caches">>, CacheName, <<"keys">>], [], empty) ->
	case oblivion_api:flush(CacheName) of
		no_cache -> error_reply(?CACHE_NOT_EXISTS_ERROR);
		ok -> success_reply(202)
	end;

%% Cache management

% GET /caches[?sort=<true|false>[&include_config=<true|false>[&include_size=<true|false>]]]
handle(<<"GET">>, [<<"caches">>], Args, empty) ->
	Options = options(Args, [?SORT_TAG, ?INCLUDE_CONFIG_TAG, ?INCLUDE_SIZE_TAG]),
	Sort = option(?SORT_TAG, Options, false),
	IncludeConfig = option(?INCLUDE_CONFIG_TAG, Options, false),
	IncludeSize = option(?INCLUDE_SIZE_TAG, Options, false),
	Caches = oblivion_api:caches(IncludeConfig, IncludeSize, Sort),
	Reply = [{<<"caches">>, Caches}],
	success_reply(200, [], Reply);

handle(_Operation, _Resource, _Params, _Payload) -> 
	error_reply(?OPERATION_NOT_SUPPORTED_ERROR).

%% ====================================================================
%% Internal functions
%% ====================================================================

options([], _Select) -> []; 
options(Args, Select) when is_list(Args) -> 
	Options = lists:filtermap(fun({?VERSION_TAG, Version}) -> {true, {?OPTION_VERSION, Version}};
				({?SORT_TAG, _}) -> true;
				({?INCLUDE_CONFIG_TAG, _}) -> true;			
				({?INCLUDE_SIZE_TAG, _}) -> true;
				({?LIST_TAG, _}) -> true;								 								 	   
				(_) -> false
			end, Args),
	lists:filter(fun({Key, _}) -> lists:member(Key, Select) end, Options);
options(_Args, _Select) -> [].

option(_Tag, [], Default) -> Default;
option(Tag, Options, Default) ->
	case lists:keyfind(Tag, 1, Options) of
		false -> Default;
		{_, Value} -> Value
	end.

success_reply(Status) ->
	{reply, Status}.

success_reply(Status, Params) ->
	{reply, Status, Params}.

success_reply(Status, Params, Payload) ->
	Reply = json(Payload),
	{reply, Status, Params, Reply}.

json(Data) ->
	jsondoc:ensure(Data).

error_reply(Error) ->
	?ERROR(Status, Reason) = Error,			
	Reply = {[{?ERROR_REASON_TAG, Reason}]},
	{reply, Status, Reply}.

is_ejson(Term = {L}) when is_list(L) -> jsondoc:is_jsondoc(Term);
is_ejson(Term) when is_list(Term) -> 
	case lists:filter(fun(X) -> is_ejson(X) =:= false end, Term) of
		[] -> true;
		_ -> false
	end;
is_ejson(Term) when is_binary(Term) -> true;
is_ejson(true) -> true;
is_ejson(false) -> true;
is_ejson(null) -> true;
is_ejson(Term) when is_integer(Term) -> true;
is_ejson(Term) when is_float(Term) -> true;
is_ejson(_Term) -> false.
