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
handle(<<"GET">>, [], Req) -> cache_list(Req, ?NO_ALERTS);

%% Cache
handle(<<"GET">>, [<<"cache">>, <<"list">>], Req) -> cache_list(Req, ?NO_ALERTS);

handle(<<"GET">>, [<<"cache">>, <<"create">>], Req) -> 
	{dtl, obv_cache_create_dtl, ?ARGS(?MENU_CACHE, ?NO_ALERTS, []), Req};

handle(<<"POST">>, [<<"cache">>, <<"create">>], Req) -> 
	{dtl, obv_cache_create_dtl, ?ARGS(?MENU_CACHE, [?ALERT_DANGER(<<"Invalid value for max-age">>)], []), Req};

handle(<<"GET">>, [<<"cache">>, CacheName, <<"delete">>], Req) -> 
	Args = [{cache, CacheName}],
	{dtl, obv_cache_delete_dtl, ?ARGS(?MENU_CACHE, ?NO_ALERTS, Args), Req};

handle(<<"POST">>, [<<"cache">>, <<"delete">>], Req) -> 
	cache_list(Req, [?ALERT_SUCCESS(<<"Cache was deleted">>)]);

handle(<<"GET">>, [<<"cache">>, CacheName, <<"flush">>], Req) -> 
	Args = [{cache, CacheName}],
	{dtl, obv_cache_flush_dtl, ?ARGS(?MENU_CACHE, ?NO_ALERTS, Args), Req};

handle(<<"POST">>, [<<"cache">>, <<"flush">>], Req) -> 
	cache_list(Req, [?ALERT_INFO(<<"Cache was flushed">>)]);

%% Cluster
handle(<<"GET">>, [<<"node">>, <<"list">>], Req) -> node_list(Req, ?NO_ALERTS);

handle(<<"GET">>, [<<"node">>, <<"add">>], Req) -> 
	{dtl, obv_node_add_dtl, ?ARGS(?MENU_NODE, ?NO_ALERTS, []), Req};

handle(<<"POST">>, [<<"node">>, <<"add">>], Req) -> 
	node_list(Req, ?NO_ALERTS);

handle(<<"GET">>, [<<"node">>, NodeName, <<"remove">>], Req) -> 
	Args = [{node, NodeName}],
	{dtl, obv_node_delete_dtl, ?ARGS(?MENU_NODE, ?NO_ALERTS, Args), Req};

handle(<<"POST">>, [<<"node">>, <<"remove">>], Req) -> 
	node_list(Req, ?NO_ALERTS);

%% About
handle(<<"GET">>, [<<"about">>], Req) -> 
	{dtl, obv_about_dtl, ?ARGS(?MENU_ABOUT, ?NO_ALERTS, []), Req};

%% Fail
handle(_Method, _Path, Req) -> {raw, 404, [], "Not found!", Req}.

%% ====================================================================
%% Internal functions
%% ====================================================================

cache_list(Req, Alerts) ->
	CacheList = oblivion_api:caches(true, true),
	Args = [{caches, CacheList}],
	{dtl, obv_cache_list_dtl, ?ARGS(?MENU_CACHE, Alerts, Args), Req}.

node_list(Req, Alerts) ->
	NodeList = oblivion_api:nodes(true),
	Args = [{nodes, NodeList}],
	{dtl, obv_node_list_dtl, ?ARGS(?MENU_NODE, Alerts, Args), Req}.