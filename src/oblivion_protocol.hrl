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

%% ====================================================================
%% ERRORS
%% ====================================================================

-define(ERROR(Status, Reason), {Status, Reason}).

-define(UNEXPECTED_ERROR, ?ERROR(500, <<"The server was unable to process the request due to an internal error">>)).

-define(OPERATION_NOT_SUPPORTED_ERROR, ?ERROR(501, <<"Operation not supported">>)).
-define(CACHE_NOT_EXISTS_ERROR, ?ERROR(404, <<"Cache doesn't exists">>)).
-define(KEY_NOT_EXISTS_ERROR, ?ERROR(404, <<"Key doesn't exists">>)).
-define(INVALID_VERSION_ERROR, ?ERROR(409, <<"Specified version is not the latest for target key">>)).
-define(INVALID_JSON_ERROR, ?ERROR(400, <<"The payload is not a valid json">>)).

%% ====================================================================
%% TAGS
%% ====================================================================

-define(ERROR_REASON_TAG, <<"reason">>).

-define(VERSION_TAG, <<"version">>).
-define(SORT_TAG, <<"sort">>).
-define(INCLUDE_CONFIG_TAG, <<"include_config">>).
-define(INCLUDE_SIZE_TAG, <<"include_size">>).
-define(LIST_TAG, <<"list">>).
