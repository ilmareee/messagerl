%%%-------------------------------------------------------------------
%%% messagerl_client public API
%%%-------------------------------------------------------------------

-module(messagerl_client_app).

-behaviour(application).

-include("messagerl_client.hrl").

-export([start/2, stop/1]).
-export([install/0]).


start(normal, []) ->
	mnesia:wait_for_tables([messagerl_cli_contact,
							messagerl_cli_groups,
							messagerl_cli_msg], 5000),
	messagerl_client_sup:start_link().


stop(_State) ->
	application:stop(mnesia),
	ok.


%% creates the mnesia tables that we need for later use
install() ->
	ok = mnesia:create_schema([node()]),
	application:start(mnesia),
	mnesia:create_table(messagerl_cli_contact,
						[{attributes, record_info(fields, messagerl_cli_contact)},
						 {index, {#messagerl_cli_contact.name}},
						 {disc_copies, [node()]}]),
	mnesia:create_table(messagerl_cli_groups,
						[{attributes, record_info(fields, messagerl_cli_groups)},
						 {index, {#messagerl_cli_groups.name}},
						 {disc_copies, [node()]}]),
	mnesia:create_table(messagerl_cli_msg,
						[{attributes, record_info(fields, messagerl_cli_msg)},
						 {index, {#messagerl_cli_msg.time}},
						 {disc_copies, [node()]}]),
	application:stop(mnesia).

