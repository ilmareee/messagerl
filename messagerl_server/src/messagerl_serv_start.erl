-module(messagerl_serv_start).
-export([start_serv/0]).

start_serv() -> application:start(crypto), application:start(mnesia), application:start(messagerl_server).
