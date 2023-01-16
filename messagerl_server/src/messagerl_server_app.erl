%%%-------------------------------------------------------------------
%%% messagerl_server public API
%%%-------------------------------------------------------------------

-module(messagerl_server_app).

-behaviour(application).

-include("messagerl_server.hrl").

-export([start/2, stop/1]).
-export([install/0]).


start(normal, []) ->
	mnesia:wait_for_tables([messagerl_contacts,
							messagerl_usernames,
							messagerl_profiles,
							messagerl_node], 5000),
	messagerl_server_sup:start_link().


stop(_State) ->
	application:stop(mnesia),
	ok.


%% creates the mnesia tables that we need for later use
install() ->
	ok = mnesia:create_schema([node()]),
	application:start(mnesia),
	mnesia:create_table(messagerl_contacts,
						[{attributes, record_info(fields, messagerl_contacts)},
						 {index, {#messagerl_contacts.token}},
						 {disc_copies, [node()]}]),
	mnesia:create_table(messagerl_usernames,
						[{attributes, record_info(fields, messagerl_usernames)},
						 {index, {#messagerl_usernames.username_hash}},
						 {disc_copies, [node()]}]),
	mnesia:create_table(messagerl_profiles,
						[{attributes, record_info(fields, messagerl_profiles)},
						 {index, {#messagerl_profiles.token}},
						 {disc_copies, [node()]}]),
	mnesia:create_table(messagerl_node,
						[{attributes, record_info(fields, messagerl_node)},
						 {index, {#messagerl_node.token}},
						 {ram_copies, [node()]}]),
	application:stop(mnesia).

