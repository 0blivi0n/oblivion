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

-behaviour(kb_action_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/3]).

handle(_Method, _Path, Req) ->
	Reply = reply(?ERROR_OPERATION_NOT_SUPPORTED),
	{json, Reply, Req}.

%% ====================================================================
%% Internal functions
%% ====================================================================

reply(ok) -> success_msg([]);
reply({ok, Fields}) -> success_msg(Fields);
reply({error, Error}) -> error_msg(Error).

error_msg(Error) ->
	Reply = [
			{?SUCCESS_TAG, false},
			{?ERROR_TAG, Error}
			],
	jsondoc:from_proplist(Reply).

success_msg(Fields) ->
	Reply = [{?SUCCESS_TAG, true}] ++ Fields,
	jsondoc:from_proplist(Reply).

