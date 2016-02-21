%% Copyright 2014-16 Joaquim Rocha <jrocha@gmailbox.org>
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

-module(oblivion_server).

-include("oblivion.hrl").
-include_lib("gibreel/include/gibreel.hrl").
-include_lib("columbo/include/columbo_events.hrl").
-include_lib("event_broker/include/event_broker.hrl").

-behaviour(gen_server).

-define(SERVER, {local, ?SERVER_NAME}).

-define(SYNC_TIMEOUT, 2000).

-define(BASIC_CACHE_PROPS, [
		{cluster_nodes, ?CLUSTER_NODES_ALL},
		{sync_mode, ?FULL_SYNC_MODE}
		]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).
-export([start_link/0]).
-export([create_cache/2, delete_cache/1]).
-export([add_node/1, delete_node/1, get_node_port/1]).

start_link() ->
	gen_server:start_link(?SERVER, ?MODULE, [], []).

add_node(Node) ->
	case validate_node(Node) of
		{error, _} = Error -> Error;
		ok ->
			case net_adm:ping(Node) of
				pong -> gen_server:call(?SERVER_NAME, {add_node, Node});
				pang -> {error, <<"Node isn't responding">>}
			end
	end.

delete_node(Node) ->
	gen_server:call(?SERVER_NAME, {delete_node, Node}).

get_node_port(Node) when Node /= node() ->
	gen_server:call(?SERVER_NAME, {node_port, Node});
get_node_port(_Node) ->
	get_node_port().

create_cache(CacheName, Options) ->
	gen_server:call(?SERVER_NAME, {create_cache, CacheName, Options}).

delete_cache(CacheName) ->
	gen_server:call(?SERVER_NAME, {delete_cache, CacheName}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

-record(state, {config, servers}).

%% init
init([]) ->
	process_flag(trap_exit, true),
	case load_persistence() of
		{error, Reason} -> 
			error_logger:error_msg("~p fail to start because [~p]\n", [?SERVER_NAME, Reason]),
			{stop,Reason};
		{ok, Config} -> 
			error_logger:info_msg("~p starting on [~p]...\n", [?SERVER_NAME, self()]),
			ok = eb_filter_by_ref:start_filter(?OBL_FEED_NODES, ?SERVER_NAME),
			{ok, #state{config=Config, servers=dict:new()}}
	end.

%% handle_call
handle_call({node_port, Node}, _From, State=#state{servers=Servers}) ->
	case dict:find(Node, Servers) of
		{ok, Value} -> {reply, Value, State};
		error -> 
			Value = gen_server:call({?SERVER_NAME, Node}, {node_port}),
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
		true -> {reply, duplicated, State};
		false -> 
			columbo:add_node(Node),
			case oblivion_conf:update(Config) of
				{ok, Config1} ->
					ExportNodes = [node(), columbo:known_nodes()],
					notify({nodes, ExportNodes}, Nodes),
					notify({startup, ExportNodes, oblivion_conf:caches(Config1)}, Node),
					{reply, ok, State#state{config=Config1}};
				Other -> {reply, Other, State}
			end
	end;

handle_call({delete_node, Node}, _From, State=#state{config=Config}) ->
	Nodes = oblivion_conf:nodes(Config),
	case lists:member(Node, Nodes) of
		true -> 
			columbo:delete_node(Node),
			case oblivion_conf:update(Config) of
				{ok, Config1} ->
					ExportNodes = [node(), columbo:known_nodes()],
					notify({nodes, ExportNodes}),
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
				{ok, Config1} -> 
					{noreply, State#state{config=Config1}};
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
	columbo:add_nodes(NewNodes),
	case oblivion_conf:update(Config) of
		{ok, Config1} ->
			{noreply, State#state{config=Config1}};
		{error, Reason} ->
			error_logger:error_msg("~p: Error updating node configuration ~p: ~p\n", [?MODULE, NewNodes, Reason]),
			{noreply, State}
	end;

handle_info(?NOTIFICATION(Event), State=#state{config=Config}) -> 
	Node = eb_event:get_property(?COLUMBO_EVENT_PROP_NODE, Event),
	Nodes = oblivion_conf:nodes(Config),
	case lists:member(Node, Nodes) of
		true -> {noreply, State};
		false ->
			case oblivion_conf:update(Config) of
				{ok, Config1} ->
					{noreply, State#state{config=Config1}};
				{error, Reason} ->
					error_logger:error_msg("~p: Error updating node configuration: ~p\n", [?MODULE, Reason]),
					{noreply, State}
			end
	end;

handle_info({config_request, Node, From}, State=#state{config=Config}) -> 
	From ! {config_response, Config},
	columbo:add_node(Node),
	case oblivion_conf:update(Config) of
		{ok, Config1} ->
			{noreply, State#state{config=Config1}};
		_ -> {noreply, State}
	end;

handle_info({startup, RemoteNodes, RemoteCaches}, State=#state{config=OldConfig}) -> 
	columbo:add_nodes(RemoteNodes),
	case oblivion_conf:empty(OldConfig) of
		true ->
			lists:foreach(fun({CacheName, Options}) ->
						cache_setup(CacheName, Options)
				end, RemoteCaches),
			case oblivion_conf:write(columbo:known_nodes(), RemoteCaches) of
				{ok, Config} -> 
					{noreply, State#state{config=Config}};
				{error, Reason} ->
					error_logger:error_msg("~p: Error saving configuration ~p: ~p\n", [?MODULE, RemoteCaches, Reason]),
					{noreply, State}
			end;
		false -> 
			case oblivion_conf:update(OldConfig) of
				{ok, Config} -> {noreply, State#state{config=Config}};
				_ -> {noreply, State}
			end
	end;

handle_info(_Info, State) -> 
	{noreply, State}.

%% terminate
terminate(_Reason, _State) -> 
	eb_filter_by_ref:stop_filter(?OBL_FEED_NODES),
	ok.

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
	Config = set(Options, ?BASIC_CACHE_PROPS),
	Config1 = case lists:keyfind(max_age, 1, Config) of
		false -> Config;
		{_, ?NO_MAX_AGE} -> Config;
		_ -> lists:keystore(purge_interval, 1, Config, {purge_interval, 60})
	end,
	gibreel:create_cache(CacheName, Config1).

set(PropList, []) -> PropList;
set(PropList, [H={Key,_}|T]) ->
	PropList1 = lists:keystore(Key, 1, PropList, H),
	set(PropList1, T).

notify(Msg, Nodes) when is_list(Nodes)-> 
	columbo:send_to_nodes(?SERVER_NAME, Nodes, Msg);
notify(Msg, Node) -> 
	{?SERVER_NAME, Node} ! Msg.

notify(Msg) -> columbo:send_to_all(?SERVER_NAME, Msg).

load_persistence() ->
	case oblivion_conf:read() of
		{ok, SavedConfig} ->
			SavedNodes = oblivion_conf:nodes(SavedConfig),
			ConnectionNodes = unique(SavedNodes, nodes()),
			RequestCount = request_config(ConnectionNodes),
			RemoteConfig = receive_config(RequestCount, SavedConfig),
			Nodes = oblivion_conf:nodes(RemoteConfig),
			columbo:add_nodes(Nodes),
			Caches = oblivion_conf:caches(RemoteConfig),
			lists:foreach(fun({CacheName, Options}) ->
						cache_setup(CacheName, Options)
				end, Caches),
			oblivion_conf:write(columbo:known_nodes(), Caches);
		{error, Reason} -> {error, Reason}
	end.

unique([], Nodes) -> Nodes;
unique([H|T], Nodes) ->
	case lists:member(H, T) of
		true -> unique(T, Nodes);
		false -> unique(T, [H|Nodes])
	end.

request_config([]) -> 0;
request_config(Nodes) ->
	lists:foldl(fun(Node, Acc) -> 
				{?SERVER_NAME, Node} ! {config_request, node(), self()}, 
				Acc + 1 
		end, 0, Nodes).

receive_config(0, DefaultConfig) -> DefaultConfig;
receive_config(_Count, DefaultConfig) ->
	receive
		{config_response, RemoteConfig} -> RemoteConfig
	after ?SYNC_TIMEOUT -> DefaultConfig
	end.

get_node_port() ->
	Server = get_host_ip(),
	{ok, HttpPort} = application:get_env(oblivion, oblivion_http_port),
	{ok, Port} = application:get_env(oblivion, oblivion_protocol_port),
	{ok, Broadcast} = application:get_env(columbo, udp_port),
	{Server, Port, HttpPort, Broadcast}.

get_host_ip() ->
	Localhost = net_adm:localhost(),
	case application:get_env(oblivion, public_ip) of
		undefined -> 
			case inet:getifaddrs() of
				{ok, NetConfig} -> 
					case find_address(NetConfig) of
						{Ip1, Ip2, Ip3, Ip4} -> 
							io_lib:format("~b.~b.~b.~b", [Ip1, Ip2, Ip3, Ip4]);
						{Ip1, Ip2, Ip3, Ip4, Ip5, Ip6, Ip7, Ip8} -> 
							io_lib:format("~.16b:~.16b:~.16b:~.16b:~.16b:~.16b:~.16b:~.16b", [Ip1, Ip2, Ip3, Ip4, Ip5, Ip6, Ip7, Ip8]);
						undefined -> Localhost
					end;
				_ -> Localhost
			end;		
		{ok, PublicIP} -> PublicIP
	end.

find_address([]) -> undefined;
find_address([H|T]) ->
	{_, Props} = H,
	Flags = proplists:get_value(flags, Props, []),
	Up = lists:member(up, Flags),
	LoopBack =  lists:member(loopback, Flags),
	if Up =:= true, LoopBack =:= false ->
			case addr(Props, 4) of
				undefined ->
					case addr(Props, 8) of
						undefined -> find_address(T);
						Ip6 -> Ip6
					end;
				Ip4 -> Ip4
			end;
		true -> find_address(T)
	end.

addr(Props, Size) ->
	List = lists:filter(fun({addr, Ip}) when is_tuple(Ip) andalso tuple_size(Ip) =:= Size -> true;
				(_) -> false end, Props),
	case List of 
		[] -> undefined;
		[{_, Ip}] -> Ip
	end.
