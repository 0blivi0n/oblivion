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

-behaviour(kb_action_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/3]).

%% Index
handle(<<"GET">>, [], Req) -> cache_list(Req);

%% Cache
handle(<<"GET">>, [<<"cache">>, <<"list">>], Req) -> cache_list(Req);

%% Cluster

%% About

%% Fail
handle(_Method, _Path, Req) -> {raw, 404, [], "Not found!", Req}.

%% ====================================================================
%% Internal functions
%% ====================================================================

cache_list(Req) ->
	{dtl, obv_cache_list_dtl, [], Req}.