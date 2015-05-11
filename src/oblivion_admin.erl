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

-module(oblivion_admin).

-include("oblivion_api.hrl").

-define(MENU_CACHE, 1).
-define(MENU_NODE, 2).
-define(MENU_ABOUT, 3).

-define(NO_ALERTS, []).
-define(NO_ARGS, []).

-define(ARGS(Menu, Alerts, Args), [
		{menu, Menu},
		{alerts, Alerts},
		{system, oblivion_api:system()}
		] ++ Args).

-define(ALERT(Type, Msg), [
		{type, Type},
		{message, Msg}
		]).

-define(ALERT_SUCCESS(Msg), ?ALERT(<<"success">>, Msg)).
-define(ALERT_INFO(Msg), ?ALERT(<<"info">>, Msg)).
-define(ALERT_WARNING(Msg), ?ALERT(<<"warning">>, Msg)).
-define(ALERT_DANGER(Msg), ?ALERT(<<"danger">>, Msg)).

-behaviour(kb_action_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/3]).

%% Index
handle(<<"GET">>, [], Req) -> 
	cache_list(Req, ?NO_ALERTS);

%% Cache
handle(<<"GET">>, [<<"cache">>, <<"list">>], Req) -> 
	cache_list(Req, ?NO_ALERTS);

handle(<<"GET">>, [<<"cache">>, <<"create">>], Req) -> 
	Args = [
			{<<"noMaxSize">>, true},
			{<<"noMaxAge">>, true}
			],
	cache_create(Req, ?NO_ALERTS, Args);

handle(<<"POST">>, [<<"cache">>, <<"create">>], Req) ->
	{Args, Req1} = kb_action_helper:get_args(Req),
	case create_cache_parameters(Args) of
		{ok, CacheName, Options} ->
			case oblivion_api:create(CacheName, Options) of
				ok ->
					Alert = ?ALERT_SUCCESS(<<"Cache '", CacheName/binary, "' was created.">>),
					cache_list(Req, [Alert]);
				{error, ErrorType} ->
					ErrorMessage = case ErrorType of
						duplicated -> <<"Duplicated cache name.">>;
						Other -> Other
					end,
					cache_create(Req1, [?ALERT_DANGER(ErrorMessage)], Args)
			end;
		{error, Errors} ->
			Alerts = [?ALERT_WARNING(Error) || Error <- Errors],
			cache_create(Req1, Alerts, Args)
	end;

handle(<<"GET">>, [<<"cache">>, CacheName, <<"delete">>], Req) ->
	Args = [{cache, CacheName}],
	cache_delete(Req, ?NO_ALERTS, Args);

handle(<<"POST">>, [<<"cache">>, <<"delete">>], Req) ->
	{Args, Req1} = kb_action_helper:get_args(Req),
	Alert = case lists:keyfind(<<"cache">>, 1, Args) of
		{_, CacheName} ->
			case oblivion_api:drop(CacheName) of
				ok -> 
					?ALERT_SUCCESS(<<"Cache '", CacheName/binary, "' was deleted.">>);
				_ -> 
					?ALERT_DANGER(<<"A technical problem prevented the execution of the operation with success.">>)
			end;
		false -> 
			?ALERT_WARNING(<<"Invalid request! Please choose the cache to delete from the cache list.">>)
	end,
	cache_list(Req1, [Alert]);

handle(<<"GET">>, [<<"cache">>, CacheName, <<"flush">>], Req) ->
	Args = [{cache, CacheName}],
	cache_flush(Req, ?NO_ALERTS, Args);

handle(<<"POST">>, [<<"cache">>, <<"flush">>], Req) ->
	{Args, Req1} = kb_action_helper:get_args(Req),
	Alert = case lists:keyfind(<<"cache">>, 1, Args) of
		{_, CacheName} ->
			case oblivion_api:flush(CacheName) of
				ok -> 
					?ALERT_INFO(<<"The eviction of all elements from cache '", CacheName/binary, "' was started!">>);
				no_cache -> 
					?ALERT_WARNING(<<"The cache '", CacheName/binary, "' was deleted!">>)
			end;
		false -> 
			?ALERT_WARNING(<<"Invalid request! Please choose the cache to flush from the cache list.">>)
	end,
	cache_list(Req1, [Alert]);

%% Cluster
handle(<<"GET">>, [<<"node">>, <<"list">>], Req) -> 
	node_list(Req, ?NO_ALERTS);

handle(<<"GET">>, [<<"node">>, <<"add">>], Req) -> 
	node_add(Req, ?NO_ALERTS, ?NO_ARGS);

handle(<<"POST">>, [<<"node">>, <<"add">>], Req) ->
	{Args, Req1} = kb_action_helper:get_args(Req),
	case lists:keyfind(<<"name">>, 1, Args) of
		{_, <<>>} ->
			Alert = ?ALERT_WARNING(<<"Invalid request! Please provide a valid node name (format: name@server).">>),
			node_add(Req1, [Alert], Args);
		{_, NodeName} ->
			case oblivion_api:add_node(NodeName) of
				ok -> 
					Alert = ?ALERT_SUCCESS(<<"Node '", NodeName/binary, "' added.">>),
					node_list(Req1, [Alert]);
				duplicated -> 
					Alert = ?ALERT_WARNING(<<"Node '", NodeName/binary, "' was already part of the cluster.">>),
					node_add(Req1, [Alert], Args);
				{error, Reason} -> 
					Alert = ?ALERT_DANGER(Reason),
					node_add(Req1, [Alert], Args)
			end;
		false ->
			Alert = ?ALERT_WARNING(<<"Invalid request! Please provide a valid node name (format: name@server).">>),
			node_add(Req1, [Alert], Args)
	end;

handle(<<"GET">>, [<<"node">>, NodeName, <<"remove">>], Req) ->
	Args = [{node, NodeName}],
	node_remove(Req, ?NO_ALERTS, Args);

handle(<<"POST">>, [<<"node">>, <<"remove">>], Req) ->
	{Args, Req1} = kb_action_helper:get_args(Req),
	Alert = case lists:keyfind(<<"node">>, 1, Args) of
		{_, NodeName} ->
			case oblivion_api:delete_node(NodeName) of
				ok -> 
					?ALERT_SUCCESS(<<"Node '", NodeName/binary, "' removed.">>);
				_ -> 
					?ALERT_DANGER(<<"A technical problem prevented the execution of the operation with success.">>)
			end;
		false -> 
			?ALERT_WARNING(<<"Invalid request! Please choose the node to remove from the node list.">>)
	end,
	node_list(Req1, [Alert]);

%% About
handle(<<"GET">>, [<<"about">>], Req) ->
	{dtl, obv_about_dtl, ?ARGS(?MENU_ABOUT, ?NO_ALERTS, ?NO_ARGS), Req};

%% Fail
handle(_Method, _Path, Req) -> 
	{raw, 404, [], "Not found!", Req}.

%% ====================================================================
%% Internal functions
%% ====================================================================

cache_list(Req, Alerts) ->
	CacheList = oblivion_api:caches(true, true, true),
	Args = [{caches, CacheList}],
	{dtl, obv_cache_list_dtl, ?ARGS(?MENU_CACHE, Alerts, Args), Req}.

cache_create(Req, Alerts, Args) ->
	{dtl, obv_cache_create_dtl, ?ARGS(?MENU_CACHE, Alerts, Args), Req}.

cache_delete(Req, Alerts, Args) ->
	{dtl, obv_cache_delete_dtl, ?ARGS(?MENU_CACHE, Alerts, Args), Req}.

cache_flush(Req, Alerts, Args) ->
	{dtl, obv_cache_flush_dtl, ?ARGS(?MENU_CACHE, Alerts, Args), Req}.

node_list(Req, Alerts) ->
	NodeList = oblivion_api:nodes(true),
	Args = [{nodes, NodeList}],
	{dtl, obv_node_list_dtl, ?ARGS(?MENU_NODE, Alerts, Args), Req}.

node_add(Req, Alerts, Args) ->
	{dtl, obv_node_add_dtl, ?ARGS(?MENU_NODE, Alerts, Args), Req}.

node_remove(Req, Alerts, Args) ->
	{dtl, obv_node_delete_dtl, ?ARGS(?MENU_NODE, Alerts, Args), Req}.

create_cache_parameters(Args) ->
	ParamFun = [
			fun cache_name/1,
			fun max_size/1,
			fun max_age/1
			],
	Results = lists:map(fun(Fun) -> Fun(Args) end, ParamFun),
	{OkList, ErrorList} = lists:partition(fun({error, _}) -> false;
				(_) -> true 
			end, Results),
	case ErrorList of
		[] -> 
			{_, CacheName} = lists:keyfind(<<"name">>, 1, OkList),
			Options = lists:filtermap(fun({_, no_value}) -> false;
						({<<"maxSize">>, MaxSize}) -> {true, {?KEY_MAX_SIZE, MaxSize}};
						({<<"maxAge">>, MaxAge}) -> {true, {?KEY_MAX_AGE, MaxAge}};
						(_) -> false 
					end, OkList),
			{ok, CacheName, Options};
		_ ->
			Errors = lists:map(fun({error, Error}) -> Error end, ErrorList),
			{error, Errors}
	end.

cache_name(Args) ->
	case lists:keyfind(<<"name">>, 1, Args) of
		false -> {error, <<"Invalid request! Please provide a name.">>};
		{_, <<>>} -> {error, <<"Invalid request! Please provide a name.">>};
		{_, CacheName} -> {<<"name">>, CacheName}
	end.

max_size(Args) ->
	case lists:keyfind(<<"noMaxSize">>, 1, Args) of
		false ->
			case lists:keyfind(<<"maxSize">>, 1, Args) of
				false -> {error, <<"Invalid request! Please provide a value for 'Max size'.">>};
				{_, <<>>} -> {error, <<"Invalid request! Please provide a value for 'Max size'.">>};
				{_, MaxSize} ->
					case integer(MaxSize) of
						error -> {error, <<"Invalid request! The field 'Max size' must be a valid integer.">>};
						Value -> {<<"maxSize">>, Value}
					end
			end;
		_ -> {ok, no_value}
	end.

max_age(Args) ->
	case lists:keyfind(<<"noMaxAge">>, 1, Args) of
		false ->
			case lists:keyfind(<<"maxAge">>, 1, Args) of
				false -> {error, <<"Invalid request! Please provide a value for 'Max age'.">>};
				{_, <<>>} -> {error, <<"Invalid request! Please provide a value for 'Max age'.">>};
				{_, MaxAge} ->
					case integer(MaxAge) of
						error -> {error, <<"Invalid request! The field 'Max age' must be a valid integer.">>};
						Value -> {<<"maxAge">>, Value}
					end
			end;
		_ -> {ok, no_value}
	end.

integer(Value) ->
	List = binary_to_list(Value),
	case string:to_integer(List) of
		{Int, []} -> Int;
		_ -> error
	end.