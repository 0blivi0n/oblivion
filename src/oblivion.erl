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
-export([create_cache/2, delete_cache/1]).
-export([add_node/1, delete_node/1, get_node_port/1]).

start() ->
	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowlib),
	ok = application:start(cowboy),
	ok = application:start(columbo),
	ok = application:start(cclock),
	ok = application:start(gibreel),
	ok = application:start(jsx),
	ok = application:start(syntax_tools),
	ok = application:start(compiler),
	ok = application:start(erlydtl),
	ok = application:start(kill_bill),
	ok = application:start(oblivion_admin),
	ok = application:start(oblivion).

start_link() ->
	gen_server:start_link(?SERVER, ?MODULE, [], []).

add_node(Node) ->
	case validate_node(Node) of
		{error, _} = Error -> Error;
		ok ->
			case net_adm:ping(Node) of
				pong -> gen_server:call(?MODULE, {add_node, Node});
				pang -> {error, <<"Node not responding">>}
			end
	end.

delete_node(Node) ->
	gen_server:call(?MODULE, {delete_node, Node}).

get_node_port(Node) when Node /= node() ->
	gen_server:call(?MODULE, {node_port, Node});
get_node_port(_Node) ->
	get_node_port().

create_cache(CacheName, Options) ->
	gen_server:call(?MODULE, {create_cache, CacheName, Options}).

delete_cache(CacheName) ->
	gen_server:call(?MODULE, {delete_cache, CacheName}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

-record(state, {config, servers}).

%% init
init([]) ->
	process_flag(trap_exit, true),
	case load_persistence() of
		{error, Reason} -> 
			error_logger:error_msg("~p fail to start because [~p]\n", [?MODULE, Reason]),
			{stop,Reason};
		{ok, Config} -> 
			error_logger:info_msg("~p starting on [~p]...\n", [?MODULE, self()]),
			{ok, #state{config=Config, servers=dict:new()}}
	end.

%% handle_call
handle_call({node_port, Node}, _From, State=#state{servers=Servers}) ->
	case dict:find(Node, Servers) of
		{ok, Value} -> {reply, Value, State};
		error -> 
			Value = gen_server:call({?MODULE, Node}, {node_port}),
			Servers1 = dict:store(Node, Value, Servers),
			{reply, Value, State#state{servers=Servers1}}
	end;

handle_call({node_port}, _From, State) ->
	{reply, get_node_port(), State};

handle_call({create_cache, CacheName, Options}, _From, State=#state{config=Config}) ->
	case cache_setup(CacheName, Options) of
		ok -> 
			case oblivion_conf:add_cache(CacheName, Options, Config) of
				{ok, Config1} ->
					notify({create_cache, CacheName, Options}),
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
			notify({delete_cache, CacheName}),
			{reply, ok, State#state{config=Config1}};
		Other -> {reply, Other, State}
	end;

handle_call({add_node, Node}, _From, State=#state{config=Config}) ->
	Nodes = oblivion_conf:nodes(Config),
	case lists:member(Node, [node(), Nodes]) of
		true -> {reply, ok, State};
		false -> 
			case oblivion_conf:add_node(Node, Config) of
				{ok, Config1} ->
					columbo:add_node(Node),
					Nodes1 = oblivion_conf:nodes(Config1),
					notify({nodes, oblivion_conf:nodes(Nodes1, export)}),
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
					columbo:delete_node(Node),
					Nodes1 = oblivion_conf:nodes(Config1),
					notify({nodes, oblivion_conf:nodes(Nodes1, export)}),
					{reply, ok, State#state{config=Config1}};
				Other -> {reply, Other, State}
			end;			
		false -> {reply, ok, State}
	end.

%% handle_cast
handle_cast(_Msg, State) -> {noreply, State}.

%% handle_info
handle_info({create_cache, CacheName, Options}, State=#state{config=Config}) -> 
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
			columbo:add_nodes(NewNodes),
			KnownNodes = columbo:known_nodes(),
			lists:foreach(fun(Node) ->
						case lists:member(Node, NewNodes) of
							true -> ok;
							false -> columbo:delete_node(Node)
						end
				end, KnownNodes),
			{noreply, State#state{config=Config1}};
		{error, Reason} ->
			error_logger:error_msg("~p: Error updating node configuration ~p: ~p\n", [?MODULE, NewNodes, Reason]),
			{noreply, State}
	end;

handle_info({config_request, From}, State=#state{config=Config}) -> 
	From ! {config_response, Config},
	{noreply, State};

handle_info({startup, RemoteConfig}, State=#state{config=OldConfig}) -> 
	case oblivion_conf:empty(OldConfig) of
		true ->
			Nodes = oblivion_conf:nodes(RemoteConfig, import),
			columbo:add_nodes(Nodes),
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

handle_info(_Info, State) -> 
	{noreply, State}.

%% terminate
terminate(_Reason, _State) -> ok.

%% code_change
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

validate_node(Node) ->
	BinNode = atom_to_binary(Node, utf8),
	case binary:split(BinNode, <<"@">>) of
		[_Name, _Server] -> ok;
		_ -> {error, <<"Not valid node name">>}
	end.

cache_setup(CacheName, Options) ->
	Config = lists:keystore(cluster_nodes, 1, Options, {cluster_nodes, ?CLUSTER_NODES_ALL}),
	Config1 = case lists:keyfind(max_age, 1, Config) of
		false -> Config;
		{_, ?NO_MAX_AGE} -> Config;
		_ -> lists:keystore(purge_interval, 1, Options, {purge_interval, 60})
	end,
	gibreel:create_cache(CacheName, Config1).

notify(Msg, Node) -> {?MODULE, Node} ! Msg.

notify(Msg) -> columbo:send_to_all(?MODULE, Msg).

load_persistence() ->
	case oblivion_conf:read() of
		{ok, SavedConfig} ->
			SavedNodes = oblivion_conf:nodes(SavedConfig),
			RequestCount = request_config(SavedNodes),
			RemoteConfig = receive_config(RequestCount, SavedConfig),
			Nodes = oblivion_conf:nodes(RemoteConfig, import),
			columbo:add_nodes(Nodes),
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
				{?MODULE, Node} ! {config_request, self()}, 
				Acc + 1 
		end, 0, Nodes).

receive_config(0, DefaultConfig) -> DefaultConfig;
receive_config(_Count, DefaultConfig) ->
	receive
		{config_response, RemoteConfig} -> RemoteConfig
	after ?SYNC_TIMEOUT -> DefaultConfig
	end.

get_node_port() ->
	Server = net_adm:localhost(),
	{ok, Port} = application:get_env(oblivion, oblivion_http_port),
	{ok, Broadcast} = application:get_env(columbo, udp_port),
	{Server, Port, Broadcast}.