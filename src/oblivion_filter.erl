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

-module(oblivion_filter).

-include("oblivion_rest.hrl").

-behaviour(kb_filter_handler).

-define(VAL_CONTENT, {fun validate_content/1, ?INVALID_CONTENT_HEADER_ERROR}).
-define(VAL_ACCEPT, {fun validate_accept/1, ?INVALID_ACCEPT_HEADER_ERROR}).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/3]).

handle(<<"PUT">>, _Path, Req) -> validate([?VAL_CONTENT, ?VAL_ACCEPT], Req);
handle(<<"POST">>, _Path, Req) -> validate([?VAL_CONTENT, ?VAL_ACCEPT], Req);
handle(_Method, _Path, Req) -> validate([?VAL_ACCEPT], Req).

%% ====================================================================
%% Internal functions
%% ====================================================================

validate([], Req) -> {next, [], Req};
validate([{Fun, Error}|T], Req) ->
	case Fun(Req) of
		{ok, Req1} -> validate(T, Req1);
		{error, Req1} -> ?rest_error(Error, Req1)
	end.

validate_content(Req) ->
	{ContentType, Req1} = kb_action_helper:get_content_type(Req),
	case header_value(ContentType) of
		?HEADER_VALUE_CONTENT_TYPE_JSON -> {ok, Req1};
		_ -> {error, Req1}
	end.

header_value(Header) ->
	Parts = binary:split(Header, <<";">>),
	[Value | _] = Parts,
	Value.

validate_accept(Req) ->
	{AcceptList, Req1} = kb_action_helper:get_accept_header(Req),
	case is_valid_accept(AcceptList) of
		true -> {ok, Req1};
		false -> {error, Req1}
	end.

is_valid_accept([]) -> false;
is_valid_accept([<<"application/json">>|_]) -> true;
is_valid_accept([<<"application/*">>|_]) -> true;
is_valid_accept([<<"*/*">>|_]) -> true;
is_valid_accept([<<"*">>|_]) -> true;
is_valid_accept([_|T]) -> is_valid_accept(T).
