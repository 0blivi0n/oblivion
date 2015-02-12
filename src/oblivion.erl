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

-module(oblivion).

-behaviour(gen_server).

-define(SERVER, {local, ?MODULE}).

%% ====================================================================
%% API functions
%% ====================================================================
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).
-export([start/0, start_link/0]).
-export([create_cache/2]).

start() ->
	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowlib),
	ok = application:start(cowboy),
	ok = application:start(columbo),
	ok = application:start(cclock),
	ok = application:start(gibreel),
	ok = application:start(kill_bill),
	ok = application:start(oblivion).

start_link() ->
	gen_server:start_link(?SERVER, ?MODULE, [], []).

create_cache(CacheName, Options) ->
	gen_server:call(?MODULE, {create_cache, CacheName, Options}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

-record(state, {}).

%% init
init([]) ->
	process_flag(trap_exit, true),
	error_logger:info_msg("~p starting on [~p]...\n", [?MODULE, self()]),
	{ok, #state{}}.

%% handle_call
handle_call({create_cache, CacheName, Options}, _From, State) ->
	ServerOptions = Options ++ [{cluster_nodes, all}, {sync_mode, lazy}],
	Reply = gibreel:create_cache(CacheName, ServerOptions),
	{reply, Reply, State}.

%% handle_cast
handle_cast(_Msg, State) -> {noreply, State}.

%% handle_info
handle_info(_Info, State) -> {noreply, State}.

%% terminate
terminate(_Reason, _State) -> ok.

%% code_change
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

