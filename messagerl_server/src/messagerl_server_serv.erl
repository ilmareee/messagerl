-module(messagerl_server_serv).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).


start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
	% to track if clients are going down
	process_flag(trap_exit, true),
	{ok, []}. %TODO

%% registers a new user to the server/tables
handle_call({register_user, Name, Psk}, _From, State) ->
	NameHash = crypto:hash(sha512,Name),
	case messagerl_server_mem:register_user(NameHash, Psk) of
		{ok, registered} -> {reply, {ok, registered}, State};
		{error, name_used} -> {reply, {error, name_used}, State}
	end;
%% logs in a user (he must already be registered)
handle_call({login, Name, Psk}, From, State) ->
	NameHash = crypto:hash(sha512,Name),
	{ok, Token} = messagerl_server_mem:get_token_name(NameHash),
	case messagerl_server_mem:get_psk(Token) of
		{ok, Psk} ->
			{ok, logged_in} = messagerl_server_mem:regis_con(Token, From),
			erlang:monitor(process, From),
			{reply, {ok, logged_in}, State};
		{ok, _ } -> {reply, wrong_psk, State};
		{error, notreg} -> {reply, {error, notreg}, State}
	end;
handle_call(logoff, From, State) ->
	messagerl_server_mem:unreg_con(From),
	{noreply, State};
%% transfers the contact request to the wanted user
handle_call({newcontact, Name, MyName}, _From, State) ->
	NameHash = crypto:hash(sha512,Name),
	{ok, Token} = messagerl_server_mem:get_token_name(NameHash),
	case messagerl_server_mem:get_node(Token) of
		{error, notcon} -> {reply, {error, notcon}, State};
		{ok, Node} ->
			gen_server:call(Node, {contact_request, MyName}),
			{reply, {ok, sent}, State}
	end;
%% respectively deletes the contacts when one asks it
handle_call({del_contact, Name}, From, State) ->
	NameHash = crypto:hash(sha512,Name),
	{ok, Token1} = messagerl_server_mem:get_token_name(NameHash),
	{ok, Token2} = messagerl_server_mem:get_token_node(From),
	messagerl_server_mem:del_contacts(Token1, Token2),
	{reply, {ok, done}, State};
%% transfers a message to the destination
handle_call({send, Msg, DestToken}, From, State) ->
	{ok, Sender} = messagerl_server_mem:get_token_node(From),
	case messagerl_server_mem:get_node(DestToken) of
		{error, notcon} -> {reply, {error, notcon}, State};
		{ok, Dest} -> gen_server:cast(Dest, {msg, Msg, Sender}),
					{reply, {ok, sent}, State}
	end;
%% broadcasts a message to the whole group
handle_call({sendgrp, Msg, Grp, DestTokens}, From, State) ->
	{ok, Sender} = messagerl_server_mem:get_token_node(From),
	NodeList = lists:map(fun(T) -> case messagerl_server_mem:get_node(T) of
									   {ok, Node} -> Node;
									   {error, notcon} -> {error, notcon}
									end
								end, DestTokens),
	NodeConOnly = [N || N <- NodeList, N =/= {error, notcon}],
	messagerl_server_mem:broadcast_msg({grpmsg, Msg, Grp, Sender}, NodeConOnly),
	{reply, {ok, sent_to, length(NodeConOnly)}, State};
%% sends a token to the user who requested it if it is one of their contacts
handle_call({request_token, Name}, From, State) ->
	Tok1 = messagerl_server_mem:get_token_node(From),
	Tok2 = messagerl_server_mem:get_token_name(Name),
	case messagerl_server_mem:are_contacts(Tok1, Tok2) of
		true -> {reply, {ok, Tok2}, State};
		false -> {reply, {error, notcontact}, State}
	end;
%% shutdowns the server if the user is admin
handle_call({shutdown_serv, Psk}, From, State) ->
	{ok, Token} = messagerl_server_mem:get_token_node(From),
	case messagerl_server_mem:get_psk(Token) of
		{ok, Psk} ->
			case messagerl_server_mem:get_perms(Token) of
				admin -> {stop, normal, ok, State};
				user -> {reply, {error, notadm}, State}
			end;
		{ok, _ } -> {reply, wrong_psk, State};
		{error, notreg} -> {reply, {error, notreg}, State}
	end;

handle_call(_Msg, _From, State) ->
	{noreply, State}.


%% if the second user accept the contact request, make it effective
handle_cast({accept_contact, Name, MyName}, State) ->
	NameHash = crypto:hash(sha512,Name),
	{ok, Token} = messagerl_server_mem:get_token_name(NameHash),
	MyHash = crypto:hash(MyName),
	{ok, MyToken} = messagerl_server_mem:get_token_name(MyHash),
	ok = messagerl_server_mem:add_contacts(Token, MyToken),
	case messagerl_server_mem:get_node(Token) of
		{error, notcon} -> {noreply, State};
		{ok, _Node} -> {noreply, State} %TODO
	end;
handle_cast(_Msg, State) ->
	{noreply, State}.

% tracks when a server is killed wich means the user was (violently)
% disconnected, therefore we must handle this
handle_info({'DOWN', _MonitorRef, _Type, Object, _Info}, State) ->
	messagerl_server_mem:unreg_con(Object),
	{ok, State};
handle_info(_Msg, State) ->
	{ok, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	messagerl_server_mem:broadcast_msg_all(shutdown),
	ok.



%% internal functions


