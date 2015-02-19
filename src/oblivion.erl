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

-include_lib("gibreel/include/gibreel.hrl").

-behaviour(gen_server).

-define(SERVER, {local, ?MODULE}).

-define(SYNC_TIMEOUT, 5000).

-define(CACHE_CONFIG(Name, Options), {Name, Options}).

-define(NODES_CONFIG_PARAM, node_list).
-define(CACHE_CONFIG_PARAM, cache_list).
-define(CONFIG_DATA(NodeConfig, CacheConfig), [
		{?NODES_CONFIG_PARAM, NodeConfig}, 
		{?CACHE_CONFIG_PARAM, CacheConfig}
		]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).
-export([start/0, start_link/0]).
-export([create_cache/2, get_cache_config/1, delete_cache/1]).
-export([get_node_list/0, add_node/1, delete_node/1, get_online_node_list/0]).
-export([get_cluster_name/0]).

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

get_cluster_name() ->
	Node = atom_to_binary(node(), utf8),
	[ClusterName|_] = binary:split(Node, <<"@">>),
	ClusterName.

get_node_list() ->
	gen_server:call(?MODULE, {get_node_list}).

get_online_node_list() ->
	AllNodes = get_node_list(),
	Online = columbo:online_nodes(),
	lists:filter(fun(Node) -> 
				lists:member(Node, Online) 
		end, AllNodes).

add_node(Server) ->
	Node = node_name(Server),
	case net_adm:ping(Node) of
		pong -> gen_server:call(?MODULE, {add_node, Node});
		pang -> {error, <<"Node not responding">>}
	end.

delete_node(Server) ->
	Node = node_name(Server),
	gen_server:call(?MODULE, {delete_node, Node}).

create_cache(CacheName, Options) ->
	gen_server:call(?MODULE, {create_cache, CacheName, Options}).

get_cache_config(CacheName) ->
	gibreel:cache_config(CacheName).

delete_cache(CacheName) ->
	gen_server:call(?MODULE, {delete_cache, CacheName}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

-record(state, {nodes, caches, persistence_file}).

%% init
init([]) ->
	process_flag(trap_exit, true),
	case load_persistence() of
		{error, Reason} -> 
			error_logger:error_msg("~p fail to start because [~p]\n", [?MODULE, Reason]),
			{stop,Reason};
		{ok, PersistenceFile, Nodes, Caches} -> 
			error_logger:info_msg("~p starting on [~p]...\n", [?MODULE, self()]),
			{ok, #state{nodes=Nodes, caches=Caches, persistence_file=PersistenceFile}}
	end.

%% handle_call
handle_call({create_cache, CacheName, Options}, _From, State=#state{nodes=Nodes, caches=Caches, persistence_file=FileName}) ->
	case create_cache(CacheName, Options, Nodes) of
		ok -> 
			notify({create_cache, CacheName, Options}, Nodes),
			Caches1 = lists:keystore(CacheName, 1, Caches, {CacheName, Options}),
			store_config(FileName, ?CONFIG_DATA(Nodes, Caches1)),
			{reply, ok, State#state{caches=Caches1}};
		Other -> {reply, Other, State}
	end;

handle_call({delete_cache, CacheName}, _From, State=#state{nodes=Nodes, caches=Caches, persistence_file=FileName}) ->
	gibreel:delete_cache(CacheName),
	notify({delete_cache, CacheName}, Nodes),
	Caches1 = lists:keydelete(CacheName, 1, Caches),
	store_config(FileName, ?CONFIG_DATA(Nodes, Caches1)),
	{reply, ok, State#state{caches=Caches1}};

handle_call({get_node_list}, _From, State=#state{nodes=Nodes}) ->
	{reply, Nodes, State};

handle_call({add_node, Node}, _From, State=#state{nodes=Nodes, caches=Caches, persistence_file=FileName}) ->
	NewNodes = case lists:member(Node, Nodes) of
		true -> Nodes;
		false -> 
			Nodes1 = [Node|Nodes],
			columbo:add_node(Node),
			columbo:refresh(),
			update_caches(Nodes1),
			notify({nodes, [node()|Nodes1]}, Nodes1),
			store_config(FileName, ?CONFIG_DATA(Nodes1, Caches)),
			Nodes1
	end, 
	{reply, ok, State#state{nodes=NewNodes}};

handle_call({delete_node, Node}, _From, State=#state{nodes=Nodes, caches=Caches, persistence_file=FileName}) ->
	NewNodes = case lists:member(Node, Nodes) of
		true ->
			Nodes1 = lists:delete(Node, Nodes),
			update_caches(Nodes1),
			notify({nodes, [node()|Nodes1]}, Nodes),
			store_config(FileName, ?CONFIG_DATA(Nodes1, Caches)),
			Nodes1;
		false -> Nodes
	end, 
	{reply, ok, State#state{nodes=NewNodes}}.

%% handle_cast
handle_cast(_Msg, State) -> {noreply, State}.

%% handle_info
handle_info({create_cache, CacheName, Options}, State=#state{nodes=Nodes, caches=Caches, persistence_file=FileName}) -> 
	create_cache(CacheName, Options, Nodes),
	Caches1 = lists:keystore(CacheName, 1, Caches, {CacheName, Options}),
	store_config(FileName, ?CONFIG_DATA(Nodes, Caches1)),
	{noreply, State#state{caches=Caches1}};

handle_info({delete_cache, CacheName}, State=#state{nodes=Nodes, caches=Caches, persistence_file=FileName}) -> 
	gibreel:delete_cache(CacheName),
	Caches1 = lists:keydelete(CacheName, 1, Caches),
	store_config(FileName, ?CONFIG_DATA(Nodes, Caches1)),
	{noreply, State#state{caches=Caches1}};

handle_info({nodes, NewNodes}, State=#state{caches=Caches, persistence_file=FileName}) -> 
	Nodes = lists:delete(node(), NewNodes),
	update_caches(Nodes),
	store_config(FileName, ?CONFIG_DATA(Nodes, Caches)),
	{noreply, State#state{nodes=Nodes}};

handle_info({config_request, Cluster, From}, State=#state{nodes=Nodes, caches=Caches}) -> 
	case get_cluster_name() of
		Cluster -> From ! {config_response, ?CONFIG_DATA(Nodes, Caches)};
		_ -> ok
	end,
	{noreply, State};

handle_info(_Info, State) -> 
	{noreply, State}.

%% terminate
terminate(_Reason, _State) -> ok.

%% code_change
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

node_name(Server) when is_atom(Server) ->
	node_name(atom_to_binary(Server, utf8));
node_name(Server) ->
	ClusterName = get_cluster_name(),
	NodeName = <<ClusterName, $@, Server>>,
	binary_to_atom(NodeName, utf8).

update_caches(Nodes) ->
	lists:foreach(fun(C) -> 
				gibreel:change_cluster_nodes(C, Nodes) 
		end, gibreel:list_caches()).

create_cache(CacheName, Options, []) ->
	create_cache(CacheName, Options, local);
create_cache(CacheName, Options, Nodes) ->
	Config = lists:keystore(cluster_nodes, 1, Options, {cluster_nodes, Nodes}),
	Config1 = case lists:keyfind(max_age, 1, Config) of
		false -> Config;
		{_, ?NO_MAX_AGE} -> Config;
		_ -> [{purge_interval, 60}|Config]
	end,
	gibreel:create_cache(CacheName, Config1).

notify(_Msg, []) -> ok;
notify(Msg, Nodes) ->
	spawn(fun() -> columbo:send_to_nodes(?MODULE, Nodes, Msg) end).

load_persistence() ->
	{ok, FileName} = application:get_env(oblivion, oblivion_persistence_file),
	case file_exists(FileName) of
		true ->
			case read_config(FileName) of
				{ok, SavedConfig} ->
					?CONFIG_DATA(SavedNodes, _) = SavedConfig,
					columbo:add_nodes(SavedNodes),
					columbo:refresh(),
					notify({config_request, get_cluster_name(), self()}, SavedNodes),
					CurrentConfig = receive
						{config_response, RemoteConfig} ->
							store_config(FileName, RemoteConfig),
							RemoteConfig
					after ?SYNC_TIMEOUT -> SavedConfig
					end,
					?CONFIG_DATA(NewNodes, Caches) = CurrentConfig,
					Nodes = lists:delete(node(), NewNodes),
					lists:foreach(fun({CacheName, Options}) ->
								create_cache(CacheName, Options, Nodes)
						end, Caches),
					{ok, FileName, Nodes, Caches};
				{error, Reason} -> {error, Reason}
			end;
		false -> {ok, FileName, [], []}
	end.

file_exists(FileName) ->
	case file:read_file_info(FileName) of
		{ok, _} -> true;
		{error, enoent} -> false;
		{error, Reason} -> {error, Reason}
	end.

read_config(FileName) ->
	file:script(FileName).

store_config(FileName, Config) ->
	{ok, H} = file:open(FileName, write),
	io:format(H, "~p.", [Config]),
	file:close(H).