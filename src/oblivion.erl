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
	ConvertedOptions = convert_to_gibreel(Options),
	gen_server:call(?MODULE, {create_cache, CacheName, ConvertedOptions}).

get_cache_config(CacheName) ->
	case gibreel:cache_config(CacheName) of
		no_cache -> no_cache;
		ConvertedOptions -> convert_from_gibreel(ConvertedOptions)
	end.

delete_cache(CacheName) ->
	gen_server:call(?MODULE, {delete_cache, CacheName}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

-record(state, {nodes=[]}).

%% init
init([]) ->
	process_flag(trap_exit, true),
	error_logger:info_msg("~p starting on [~p]...\n", [?MODULE, self()]),
	{ok, #state{}}.

%% handle_call
handle_call({create_cache, CacheName, Options}, _From, State=#state{nodes=Nodes}) ->
	Reply = create_cache(CacheName, Options, Nodes),
	notify({create_cache, CacheName, Options}, Nodes),
	{reply, Reply, State};

handle_call({delete_cache, CacheName}, _From, State=#state{nodes=Nodes}) ->
	gibreel:delete_cache(CacheName),
	notify({delete_cache, CacheName}, Nodes),
	{reply, ok, State};

handle_call({get_node_list}, _From, State=#state{nodes=Nodes}) ->
	{reply, Nodes, State};

handle_call({add_node, Node}, _From, State=#state{nodes=Nodes}) ->
	NewNodes = case lists:member(Node, Nodes) of
		true -> Nodes;
		false -> 
			Nodes1 = [Node|Nodes],
			columbo:add_node(Node),
			columbo:refresh(),
			update_caches(Nodes1),
			notify({nodes, [node()|Nodes1]}, Nodes1),
			Nodes1
	end, 
	{reply, ok, State#state{nodes=NewNodes}};

handle_call({delete_node, Node}, _From, State=#state{nodes=Nodes}) ->
	NewNodes = case lists:member(Node, Nodes) of
		true ->
			Nodes1 = lists:delete(Node, Nodes),
			update_caches(Nodes1),
			notify({nodes, [node()|Nodes1]}, Nodes),
			Nodes1;
		false -> Nodes
	end, 
	{reply, ok, State#state{nodes=NewNodes}}.

%% handle_cast
handle_cast(_Msg, State) -> {noreply, State}.

%% handle_info
handle_info({create_cache, CacheName, Options}, State=#state{nodes=Nodes}) -> 
	create_cache(CacheName, Options, Nodes),
	{noreply, State};

handle_info({delete_cache, CacheName}, State) -> 
	gibreel:delete_cache(CacheName),
	{noreply, State};

handle_info({nodes, NewNodes}, State) -> 
	Nodes = lists:delete(node(), NewNodes),
	update_caches(Nodes),
	{noreply, State#state{nodes=Nodes}}.

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

convert_to_gibreel(Options) -> convert_to_gibreel(Options, []).

convert_to_gibreel([{<<"max-age">>, Value}|T], Output) -> 
	convert_to_gibreel(T, [{max_age, Value}, {purge_interval, 60}|Output]);
convert_to_gibreel([{<<"max-size">>, Value}|T], Output) -> convert_to_gibreel(T, [{max_size, Value}|Output]);
convert_to_gibreel([{<<"synchronize-on-startup">>, true}|T], Output) -> convert_to_gibreel(T, [{sync_mode, ?FULL_SYNC_MODE}|Output]);
convert_to_gibreel([{<<"synchronize-on-startup">>, false}|T], Output) -> convert_to_gibreel(T, [{sync_mode, ?LAZY_SYNC_MODE}|Output]);
convert_to_gibreel([_|T], Output) -> convert_to_gibreel(T, Output);
convert_to_gibreel([], Output) -> Output.

convert_from_gibreel(Options) -> 
	lists:filtermap(fun({max_age, ?NO_MAX_AGE}) -> false;
			({max_age, Value}) -> {true, {<<"max-age">>, Value}};
			({max_size, ?NO_MAX_SIZE}) -> false;
			({max_size, Value}) -> {true, {<<"max-size">>, Value}};
			({sync_mode, ?LAZY_SYNC_MODE}) -> {true, {<<"synchronize-on-startup">>, false}};
			({sync_mode, ?FULL_SYNC_MODE}) -> {true, {<<"synchronize-on-startup">>, true}};
			(_) -> false
		end, Options).

update_caches(Nodes) ->
	lists:foreach(fun(C) -> 
				gibreel:change_cluster_nodes(C, Nodes) 
		end, gibreel:list_caches()).

create_cache(CacheName, Options, []) ->
	create_cache(CacheName, Options, local);
create_cache(CacheName, Options, Nodes) ->
	Config = lists:keystore(cluster_nodes, 1, Options, {cluster_nodes, Nodes}),
	gibreel:create_cache(CacheName, Config).

notify(_Msg, []) -> ok;
notify(Msg, Nodes) ->
	spawn(fun() -> columbo:send_to_nodes(?MODULE, Nodes, Msg) end).