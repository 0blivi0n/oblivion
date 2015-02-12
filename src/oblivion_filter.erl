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

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/3]).

handle(<<"PUT">>, _Path, Req) -> validate_content_accept(Req);
handle(<<"POST">>, _Path, Req) -> validate_content_accept(Req);
handle(_Method, _Path, Req) -> validate_accept(Req).

%% ====================================================================
%% Internal functions
%% ====================================================================

validate_content_accept(Req) ->
	{ContentType, Req1} = kb_action_helper:get_content_type(Req),
	case header_value(ContentType) of
		?HEADER_VALUE_CONTENT_TYPE_JSON -> validate_accept(Req1);
		_ -> ?rest_error(?INVALID_CONTENT_HEADER_ERROR, Req1)
	end.

validate_accept(Req) ->
	{Headers, Req1} = kb_action_helper:get_headers(Req),
	case lists:keyfind(?HEADER_ACCEPT, 1, Headers) of
		false -> {next, [], Req1};
		{_, Content} ->
			case is_valid_accept(header_value(Content)) of
				true -> {next, [], Req1};
				false -> ?rest_error(?INVALID_ACCEPT_HEADER_ERROR, Req1)
			end
	end.

is_valid_accept(?HEADER_VALUE_CONTENT_TYPE_JSON) -> true;
is_valid_accept(<<"application/*">>) -> true;
is_valid_accept(<<"*/*">>) -> true;
is_valid_accept(<<"*">>) -> true;
is_valid_accept(_) -> false.

header_value(Header) ->
	Parts = binary:split(Header, <<";">>),
	[Value | _] = Parts,
	Value.