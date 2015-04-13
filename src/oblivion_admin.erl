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

-define(ARGS(Menu, Alerts, Args), [
		{menu, Menu},
		{alerts, Alerts},
		{system, oblivion_api:system()}
		] ++ Args).

-behaviour(kb_action_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/3]).

%% Index
handle(<<"GET">>, [], Req) -> cache_list(Req, []);

%% Cache
handle(<<"GET">>, [<<"cache">>, <<"list">>], Req) -> cache_list(Req, []);

handle(<<"GET">>, [<<"cache">>, <<"create">>], Req) -> 
	{dtl, obv_cache_create_dtl, [], Req};

handle(<<"POST">>, [<<"cache">>, <<"create">>], Req) -> 
	cache_list(Req, []);

handle(<<"POST">>, [<<"cache">>, <<"delete">>], Req) -> 
	cache_list(Req, []);

handle(<<"GET">>, [<<"cache">>, <<"flush">>], Req) -> 
	cache_list(Req, []);

%% Cluster
handle(<<"GET">>, [<<"node">>, <<"list">>], Req) -> node_list(Req, []);

handle(<<"GET">>, [<<"node">>, <<"add">>], Req) -> 
	{dtl, obv_node_add_dtl, [], Req};

handle(<<"POST">>, [<<"node">>, <<"add">>], Req) -> 
	node_list(Req, []);

handle(<<"POST">>, [<<"node">>, <<"remove">>], Req) -> 
	node_list(Req, []);

%% About
handle(<<"GET">>, [<<"about">>], Req) -> 
	{dtl, obv_about_dtl, ?ARGS(3, [], []), Req};

%% Fail
handle(_Method, _Path, Req) -> {raw, 404, [], "Not found!", Req}.

%% ====================================================================
%% Internal functions
%% ====================================================================

cache_list(Req, Alerts) ->
	CacheList = oblivion_api:caches(true, true),
	Args = [{caches, CacheList}],
	{dtl, obv_cache_list_dtl, ?ARGS(1, Alerts, Args), Req}.

node_list(Req, Alerts) ->
	NodeList = oblivion_api:nodes(true),
	Args = [{nodes, NodeList}],
	{dtl, obv_node_list_dtl, ?ARGS(2, Alerts, Args), Req}.