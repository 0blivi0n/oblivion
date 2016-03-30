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

%% ====================================================================
%% HTTP
%% ====================================================================

%% STATUS CODES %%
-define(HTTP_STATUS(Code, Msg), {Code, Msg}).

%% 2xx Success
-define(STATUS_200, ?HTTP_STATUS(200, <<"OK">>)).
-define(STATUS_201, ?HTTP_STATUS(201, <<"Created">>)).
-define(STATUS_202, ?HTTP_STATUS(202, <<"Accepted">>)).
-define(STATUS_204, ?HTTP_STATUS(204, <<"No Content">>)).

%% 3xx Redirection
-define(STATUS_304, ?HTTP_STATUS(304, <<"Not Modified">>)).

%% 4xx Client Error
-define(STATUS_400, ?HTTP_STATUS(400, <<"Bad Request">>)).
-define(STATUS_401, ?HTTP_STATUS(401, <<"Unauthorized">>)).
-define(STATUS_403, ?HTTP_STATUS(403, <<"Forbidden">>)).
-define(STATUS_404, ?HTTP_STATUS(404, <<"Not Found">>)).
-define(STATUS_405, ?HTTP_STATUS(405, <<"Method Not Allowed">>)).
-define(STATUS_406, ?HTTP_STATUS(406, <<"Not Acceptable">>)).
-define(STATUS_409, ?HTTP_STATUS(409, <<"Conflict">>)).
-define(STATUS_412, ?HTTP_STATUS(412, <<"Precondition Failed">>)).
-define(STATUS_415, ?HTTP_STATUS(415, <<"Unsupported Media Type">>)).

%% 5xx Server Error
-define(STATUS_500, ?HTTP_STATUS(500, <<"Internal Server Error">>)).
-define(STATUS_501, ?HTTP_STATUS(501, <<"Not Implemented">>)).
-define(STATUS_502, ?HTTP_STATUS(502, <<"Bad Gateway">>)).
-define(STATUS_503, ?HTTP_STATUS(503, <<"Service Unavailable">>)).
-define(STATUS_504, ?HTTP_STATUS(504, <<"Gateway Timeout">>)).

%% HEADERS %%

%% Header names
-define(HEADER_CONTENT_TYPE, <<"content-type">>).
-define(HEADER_ACCEPT, <<"accept">>).
-define(HEADER_CACHE_CONTROL, <<"Cache-Control">>).
-define(HEADER_ETAG, <<"ETag">>).
-define(HEADER_LOCATION, <<"Location">>).
-define(HEADER_RETRY_AFTER, <<"Retry-After">>).

%% Header Values
-define(HEADER_VALUE_CONTENT_TYPE_JSON, <<"application/json">>).
-define(HEADER_VALUE_CONTENT_TYPE_TEXT, <<"text/plain">>).
-define(HEADER_VALUE_CONTENT_TYPE_TEXT_UTF8, <<"text/plain;charset=utf-8">>).

-define(HEADER_VALUE_ACCEPT_ALL, <<"*/*">>).

-define(HEADER_VALUE_CACHE_CONTROL_REVALIDATE, <<"must-revalidate">>).

%% ====================================================================
%% ERRORS
%% ====================================================================

-define(ERROR(Status, Reason), {Status, Reason}).

-define(UNEXPECTED_ERROR, ?ERROR(?STATUS_500, <<"The server was unable to process the request due to an internal error">>)).

-define(INVALID_ACCEPT_HEADER_ERROR, ?ERROR(?STATUS_406, <<"Must accept 'application/json'">>)).
-define(INVALID_CONTENT_HEADER_ERROR, ?ERROR(?STATUS_400, <<"Content type must be 'application/json'">>)).
-define(OPERATION_NOT_SUPPORTED_ERROR, ?ERROR(?STATUS_501, <<"Operation not supported">>)).
-define(CACHE_NOT_EXISTS_ERROR, ?ERROR(?STATUS_404, <<"Cache doesn't exists">>)).
-define(KEY_NOT_EXISTS_ERROR, ?ERROR(?STATUS_404, <<"Key doesn't exists">>)).
-define(INVALID_VERSION_ERROR, ?ERROR(?STATUS_409, <<"Specified version is not the latest for target key">>)).
-define(INVALID_JSON_ERROR, ?ERROR(?STATUS_400, <<"The body is not a valid json">>)).
-define(DUPLICATED_CACHE_ERROR, ?ERROR(?STATUS_412, <<"Cache already exists">>)).
-define(DUPLICATED_NODE_ERROR, ?ERROR(?STATUS_412, <<"Node was already part of the cluster">>)).

%% ====================================================================
%% TAGS
%% ====================================================================

-define(ERROR_TAG, <<"error">>).
-define(ERROR_REASON_TAG, <<"reason">>).

-define(VERSION_TAG, <<"version">>).
-define(SORT_TAG, <<"sort">>).
-define(INCLUDE_CONFIG_TAG, <<"include_config">>).
-define(INCLUDE_SIZE_TAG, <<"include_size">>).
-define(LIST_TAG, <<"list">>).

%% ====================================================================
%% OUTPUT MACROS
%% ====================================================================

-define(BASIC_HEADER_LIST, [{?HEADER_CONTENT_TYPE, ?HEADER_VALUE_CONTENT_TYPE_JSON}, {?HEADER_CACHE_CONTROL, ?HEADER_VALUE_CACHE_CONTROL_REVALIDATE}]).
-define(ERROR_HEADER_LIST, [{?HEADER_CONTENT_TYPE, ?HEADER_VALUE_CONTENT_TYPE_JSON}, {?HEADER_CACHE_CONTROL, ?HEADER_VALUE_CACHE_CONTROL_REVALIDATE}]).
-define(ETAG_HEADER_LIST(ETag), [{?HEADER_CONTENT_TYPE, ?HEADER_VALUE_CONTENT_TYPE_JSON}, {?HEADER_CACHE_CONTROL, ?HEADER_VALUE_CACHE_CONTROL_REVALIDATE}, {?HEADER_ETAG, integer_to_binary(ETag)}]).

-define(ERROR_REPLY(Msg, Reason), [{?ERROR_TAG, Msg}, {?ERROR_REASON_TAG, Reason}]).

-define(OK, [{<<"ok">>, true}]).

-define(rest_error(Error, Req), (fun() -> 
				?ERROR(Status, Reason) = Error,						  
				?HTTP_STATUS(StatusCode, StatusError) = Status,
				{json, StatusCode, ?ERROR_HEADER_LIST, ?ERROR_REPLY(StatusError, Reason), Req}
		end)()).
