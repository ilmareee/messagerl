-module(messagerl_cli_start).
-export([start_cli/0]).

start_cli() -> application:start(crypto), application:start(mnesia), application:start(messagerl_client).
