-module(messagerl_client_api).

-export([register_user/0, login/0, logoff/0]).
-export([add_contact/1, del_contact/1, request_token/1]).
-export([send_message/2]).
-export([shutdown_serv/1, stop/0]).
-export([showmsg_new/0, showmsg_from/1]).


register_user() ->
	Name = io:get_line("Enter your username :"),
	Psk = io:get_line("Enter your password :"),
	HPsk = crypto:hash(sha512, Psk),
	{ok, Server} = application:get_env(messagerl_client, server),
	messagerl_client_serv ! {register_user, Name, HPsk, Server}.
login() ->
	Name = io:get_line("Enter your username :"),
	Psk = io:get_line("Enter your password :"),
	HPsk = crypto:hash(sha512, Psk),
	{ok, Server} = application:get_env(messagerl_client, server),
	messagerl_client_serv ! {login, Name, HPsk, Server}.
logoff() ->
	messagerl_client_serv ! logoff.


add_contact(Name) ->
	messagerl_client_serv ! {add_contact, Name}.
del_contact(Name) ->
	messagerl_client_serv ! {del_contact, Name}.
request_token(Name) ->
	messagerl_client_serv ! {request_token, Name}.

send_message(Name, Msg) ->
	messagerl_client_serv ! {send, Name, Msg}.

shutdown_serv(Psk) ->
	messagerl_client_serv ! {shutdown_serv, Psk}.

stop() ->
	application:stop(messagerl_client).

showmsg_new() ->
	Msgs = messagerl_client_mem:get_msg_new(),
	display_msgs(Msgs).

showmsg_from(Name) ->
	Msgs = messagerl_client_mem:get_msg_name(Name),
	display_msgs(Msgs).

display_msgs([]) ->
	ok;
display_msgs([Msg|Msgs]) ->
	{Date, From, _Group, _Read, Content} = Msg,
	io:format("at ~p, from ~p : ~p~n", [Date, From, Content]),
	display_msgs(Msgs).
