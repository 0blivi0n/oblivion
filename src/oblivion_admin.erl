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
handle(<<"GET">>, [], Req) -> cache_list(Req, ?NO_ALERTS);

%% Cache
handle(<<"GET">>, [<<"cache">>, <<"list">>], Req) -> cache_list(Req, ?NO_ALERTS);

handle(<<"GET">>, [<<"cache">>, <<"create">>], Req) -> cache_create(Req, ?NO_ALERTS, ?NO_ARGS);

handle(<<"POST">>, [<<"cache">>, <<"create">>], Req) ->
	{Args, Req1} = kb_action_helper:get_args(Req),
	case get_create_cache_parameters(Args) of
		{ok, {CacheName, Options}} ->
			case oblivion_api:create(CacheName, Options) of
				ok -> cache_list(Req, [?ALERT_SUCCESS(<<"Cache ", CacheName/binary, " created.">>)]);
				{error, ErrorType} ->
					ErrorMessage = case ErrorType of
							duplicated -> <<"Cache exists.">>;
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
	case get_mandatory_value(Args, <<"cache">>) of
		{ok, CacheName} ->
			case oblivion_api:drop(CacheName) of
				{error, _Reason} -> cache_delete(Req1, [?ALERT_DANGER(<<"Cache was not deleted.">>)], Args);
				_ -> cache_list(Req1, [?ALERT_SUCCESS(<<"Cache ", CacheName/binary, " was deleted.">>)])
			end;
		{error, _Reason} -> cache_delete(Req1, [?ALERT_WARNING(<<"Missing cache parameter.">>)], Args)
	end;

handle(<<"GET">>, [<<"cache">>, CacheName, <<"flush">>], Req) ->
	Args = [{cache, CacheName}],
	cache_flush(Req, ?NO_ALERTS, Args);

handle(<<"POST">>, [<<"cache">>, <<"flush">>], Req) ->
	{Args, Req1} = kb_action_helper:get_args(Req),
	case get_mandatory_value(Args, <<"cache">>) of
		{ok, CacheName} ->
			case oblivion_api:flush(CacheName) of
				ok -> cache_list(Req, [?ALERT_SUCCESS(<<"Cache ", CacheName/binary, " was flushed.">>)]);
				no_cache -> cache_flush(Req1, [?ALERT_DANGER(<<"Cache does not exist.">>)], Args)
			end;
		{error, _Reason} -> cache_flush(Req1, [?ALERT_WARNING(<<"Missing cache parameter.">>)], Args)
	end;

%% Cluster
handle(<<"GET">>, [<<"node">>, <<"list">>], Req) -> node_list(Req, ?NO_ALERTS);

handle(<<"GET">>, [<<"node">>, <<"add">>], Req) -> node_add(Req, ?NO_ALERTS, ?NO_ARGS);

handle(<<"POST">>, [<<"node">>, <<"add">>], Req) ->
	{Args, Req1} = kb_action_helper:get_args(Req),
	case get_mandatory_value(Args, <<"name">>) of
		{ok, NodeName} ->
			case oblivion_api:add_node(NodeName) of
				ok -> node_list(Req1, [?ALERT_SUCCESS(<<"Node ", NodeName/binary, " added.">>)]);
				{error, Reason} -> node_add(Req1, [?ALERT_DANGER(Reason)], Args)
			end;
		{error, _Reason} -> node_add(Req1, [?ALERT_WARNING(<<"Node name can not be empty.">>)], Args)
	end;

handle(<<"GET">>, [<<"node">>, NodeName, <<"remove">>], Req) ->
	Args = [{node, NodeName}],
	node_remove(Req, ?NO_ALERTS, Args);

handle(<<"POST">>, [<<"node">>, <<"remove">>], Req) ->
	{Args, Req1} = kb_action_helper:get_args(Req),
	case get_mandatory_value(Args, <<"node">>) of
		{ok, NodeName} ->
			case oblivion_api:delete_node(NodeName) of
				ok -> node_list(Req1, [?ALERT_SUCCESS(<<"Node ", NodeName/binary, " deleted.">>)]);
				{error, Reason} -> node_remove(Req1, [?ALERT_DANGER(Reason)], Args)
			end;
		{error, _Reason} -> node_remove(Req1, [?ALERT_WARNING(<<"Node name can not be empty.">>)], Args)
	end;

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

get_create_cache_parameters(Args) -> get_create_cache_parameters(Args, [], {undefined, []}).
get_create_cache_parameters([], [], Parameters) -> {ok, Parameters};
get_create_cache_parameters([], Errors, _Parameters) -> {error, lists:reverse(Errors)};
get_create_cache_parameters([{<<"name">>, <<>>}|Tail], Errors, Parameters) ->
	get_create_cache_parameters(Tail, [<<"Cache name can not be empty.">>|Errors], Parameters);
get_create_cache_parameters([{<<"name">>, NewCacheName}|Tail], Errors, Parameters={_OldCacheName, Options}) ->
	try
		_NameAsAtom = binary_to_atom(NewCacheName, utf8),
		get_create_cache_parameters(Tail, Errors, {NewCacheName, Options})
	catch
		error: badarg -> get_create_cache_parameters(Tail, [<<"Cache name can not be empty.">>|Errors], Parameters)
	end;
get_create_cache_parameters([{_Key, <<>>}|Tail], Errors, Parameters) ->
	get_create_cache_parameters(Tail, Errors, Parameters);
get_create_cache_parameters([{<<"maxSize">>, MaxSize}|Tail], Errors, Parameters) ->
	get_create_cache_integer_parameter(?KEY_MAX_SIZE, MaxSize, <<"Max Size">>, Tail, Errors, Parameters);
get_create_cache_parameters([{<<"maxAge">>, MaxAge}|Tail], Errors, Parameters) ->
	get_create_cache_integer_parameter(?KEY_MAX_AGE, MaxAge, <<"Max Age">>, Tail, Errors, Parameters).

get_create_cache_integer_parameter(KeyName, BinValue, LabelName, RemainingArgs, Errors, Parameters={CacheName, Options}) ->
	try
		IntegerValue = binary_to_integer(BinValue),
		get_create_cache_parameters(RemainingArgs, Errors, {CacheName, [{KeyName, IntegerValue}|Options]})
	catch
		error: badarg -> get_create_cache_parameters(RemainingArgs, [<<LabelName/binary, " has to be an integer.">>|Errors], Parameters)
	end.

get_mandatory_value(Args, Key) ->
	case proplists:get_value(Key, Args) of
		undefined -> {error, missing};
		<<>> -> {error, empty};
		Value -> {ok, Value}
	end.
