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

-record(state, {config}).

%% init
init([]) ->
	process_flag(trap_exit, true),
	case load_persistence() of
		{error, Reason} -> 
			error_logger:error_msg("~p fail to start because [~p]\n", [?MODULE, Reason]),
			{stop,Reason};
		{ok, Config} -> 
			error_logger:info_msg("~p starting on [~p]...\n", [?MODULE, self()]),
			{ok, #state{config=Config}}
	end.

%% handle_call
handle_call({create_cache, CacheName, Options}, _From, State=#state{config=Config}) ->
	Nodes = oblivion_conf:nodes(Config),
	case cache_setup(CacheName, Options) of
		ok -> 
			case oblivion_conf:add_cache(CacheName, Options, Config) of
				{ok, Config1} ->
					notify({create_cache, CacheName, Options}, Nodes),
					{reply, ok, State#state{config=Config1}};
				Other ->
					gibreel:delete_cache(CacheName),
					{reply, Other, State}
			end;
		Other -> {reply, Other, State}
	end;

handle_call({delete_cache, CacheName}, _From, State=#state{config=Config}) ->
	case oblivion_conf:delete_cache(CacheName, Config) of
		{ok, Config1} ->
			gibreel:delete_cache(CacheName),
			Nodes = oblivion_conf:nodes(Config1),
			notify({delete_cache, CacheName}, Nodes),
			{reply, ok, State#state{config=Config1}};
		Other -> {reply, Other, State}
	end;

handle_call({get_node_list}, _From, State=#state{config=Config}) ->
	Nodes = oblivion_conf:nodes(Config),
	{reply, Nodes, State};

handle_call({add_node, Node}, _From, State=#state{config=Config}) ->
	Nodes = oblivion_conf:nodes(Config),
	case lists:member(Node, Nodes) of
		true -> {reply, ok, State};
		false -> 
			case oblivion_conf:add_node(Node, Config) of
				{ok, Config1} ->
					Nodes1 = oblivion_conf:nodes(Config1),
					update_caches(Nodes1),
					notify({nodes, oblivion_conf:nodes(Nodes1, export)}, Nodes),
					notify({startup, Config1}, Node),
					{reply, ok, State#state{config=Config1}};
				Other -> {reply, Other, State}
			end
	end;

handle_call({delete_node, Node}, _From, State=#state{config=Config}) ->
	Nodes = oblivion_conf:nodes(Config),
	case lists:member(Node, Nodes) of
		true -> 
			case oblivion_conf:delete_node(Node, Config) of
				{ok, Config1} ->
					Nodes1 = oblivion_conf:nodes(Config1),
					update_caches(Nodes1),
					notify({nodes, oblivion_conf:nodes(Nodes1, export)}, Nodes1),
					notify({shutdown}, Node),
					{reply, ok, State#state{config=Config1}};
				Other -> {reply, Other, State}
			end;			
		false -> {reply, ok, State}
	end.

%% handle_cast
handle_cast(_Msg, State) -> {noreply, State}.

%% handle_info
handle_info({create_cache, CacheName, Options}, State=#state{config=Config}) -> 
	Nodes = oblivion_conf:nodes(Config),
	case cache_setup(CacheName, Options) of
		ok -> 
			case oblivion_conf:add_cache(CacheName, Options, Config) of
				{ok, Config1} -> {noreply, State#state{config=Config1}};
				{error, Reason} ->
					error_logger:error_msg("~p: Error saving cache configuration ~p: ~p\n", [?MODULE, CacheName, Reason]),
					{noreply, State}
			end;
		{error, Reason} -> 
			error_logger:error_msg("~p: Error creating cache ~p: ~p\n", [?MODULE, CacheName, Reason]),
			{noreply, State}
	end;

handle_info({delete_cache, CacheName}, State=#state{config=Config}) -> 
	case oblivion_conf:delete_cache(CacheName, Config) of
		{ok, Config1} ->
			gibreel:delete_cache(CacheName),
			{noreply, State#state{config=Config1}};
		{error, Reason} ->
			error_logger:error_msg("~p: Error removing cache configuration ~p: ~p\n", [?MODULE, CacheName, Reason]),
			{noreply, State}
	end;

handle_info({nodes, NewNodes}, State=#state{config=Config}) -> 
	case oblivion_conf:update_nodes(NewNodes, Config) of
		{ok, Config1} ->
			Nodes = oblivion_conf:nodes(Config1),
			update_caches(Nodes),
			{noreply, State#state{config=Config1}};
		{error, Reason} ->
			error_logger:error_msg("~p: Error updating node configuration ~p: ~p\n", [?MODULE, NewNodes, Reason]),
			{noreply, State}
	end;

handle_info({config_request, Cluster, From}, State=#state{config=Config}) -> 
	case get_cluster_name() of
		Cluster -> From ! {config_response, Config};
		_ -> ok
	end,
	{noreply, State};

handle_info({startup, RemoteConfig}, State=#state{config=OldConfig}) -> 
	case oblivion_conf:empty(OldConfig) of
		true ->
			Nodes = oblivion_conf:nodes(RemoteConfig, import),
			Caches = oblivion_conf:caches(RemoteConfig),
			lists:foreach(fun({CacheName, Options}) ->
						cache_setup(CacheName, Options)
				end, Caches),
			case oblivion_conf:write(Nodes, Caches) of
				{ok, Config} -> {noreply, State=#state{config=Config}};
				{error, Reason} ->
					error_logger:error_msg("~p: Error saving configuration ~p: ~p\n", [?MODULE, RemoteConfig, Reason]),
					{noreply, State}
			end;
		false -> {noreply, State}
	end;

handle_info({shutdown}, State=#state{config=Config}) -> 
	Caches = oblivion_conf:caches(Config),
	lists:foreach(fun({CacheName, _}) ->
				gibreel:delete_cache(CacheName)
		end, Caches),
	case oblivion_conf:delete() of
		{ok, Empty} -> {noreply, State#state{config=Empty}};
		{error, Reason} ->
			error_logger:error_msg("~p: Error deleting configuration: ~p\n", [?MODULE, Reason]),
			Empty = oblivion_conf:new(),
			{noreply, State#state{config=Empty}}
	end;

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
	lists:foreach(fun(Cache) -> 
				gibreel:change_cluster_nodes(Cache, Nodes) 
		end, gibreel:list_caches()).

cache_setup(CacheName, Options) ->
	Config = lists:keystore(cluster_nodes, 1, Options, {cluster_nodes, ?CLUSTER_NODES_ALL}),
	Config1 = case lists:keyfind(max_age, 1, Config) of
		false -> Config;
		{_, ?NO_MAX_AGE} -> Config;
		_ -> lists:keystore(purge_interval, 1, Options, {purge_interval, 60})
	end,
	gibreel:create_cache(CacheName, Config1).

notify(_Msg, []) -> ok;
notify(Msg, Nodes) when is_list(Nodes) ->
	spawn(fun() -> columbo:send_to_nodes(?MODULE, Nodes, Msg) end);
notify(Msg, Node) -> {?MODULE, Node} ! Msg.

load_persistence() ->
	case oblivion_conf:read() of
		{ok, SavedConfig} ->
			SavedNodes = oblivion_conf:nodes(SavedConfig),
			RequestCount = request_config(SavedNodes),
			RemoteConfig = receive_config(RequestCount, SavedConfig),
			Nodes = oblivion_conf:nodes(RemoteConfig, import),
			Caches = oblivion_conf:caches(RemoteConfig),
			lists:foreach(fun({CacheName, Options}) ->
						cache_setup(CacheName, Options)
				end, Caches),
			oblivion_conf:write(Nodes, Caches);
		{error, Reason} -> {error, Reason}
	end.

request_config([]) -> 0;
request_config(Nodes) ->
	lists:foldl(fun(Node, Acc) -> 
				{?MODULE, Node} ! {config_request, get_cluster_name(), self()}, 
				Acc + 1 
		end, 0, Nodes).

receive_config(0, DefaultConfig) -> DefaultConfig;
receive_config(_Count, DefaultConfig) ->
	receive
		{config_response, RemoteConfig} -> RemoteConfig
	after ?SYNC_TIMEOUT -> DefaultConfig
	end.