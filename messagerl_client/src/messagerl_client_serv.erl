-module(messagerl_client_serv).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).


start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).


init([]) ->
	{ok, []}.


%% handles when another user sent you a contact request
handle_call({contact_request, Name}, _From, [MyName]) ->
	io:format("~p sent you a contact request, do you accept ? [y/n]~n", Name),
	case io:get_chars("", 1) of
		"y" -> {reply, {accept_contact, Name, MyName}, [MyName]};
		_ -> {noreply, [MyName]}
	end;

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.




handle_cast({msg, Msg, From}, State) ->
	Time = erlang:monotonic_time(),
	messagerl_client_mem:save_msg(Time, From, Msg),
	UnreadNum = messagerl_client_mem:count_unread(),
	io:format("You have ~p unread message(s)~n", UnreadNum),
	io:format("You can display them with the read_new(). command~n"),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.





%% tries to register a new user to the server
handle_info({register_user, Name, HPsk, Server}, []) ->
	net_kernel:connect_node(Server),
	% remote server will be globally registered 
	% with the name messagerl_server_serv
	gen_server:call(messagerl_server_serv, {register_user, Name, HPsk}),
	receive
		{ok, registered} -> io:format("registered~n"),
							net_kernel:disconnect_node(Server),
						   {ok, []};
		{error, name_used} -> io:format("name already used, please take another one~n"),
							  net_kernel:disconnect_node(Server),
							  {ok, []}
	after 5000 ->
			  io:format("server is not responding~n"),
			  net_kernel:disconnect_node(Server),
			  {ok, []}
	end;
handle_info({register_user, _NewName, _HPsk, _Server}, [Name]) ->
	io:format("you are already logged in as ~p~n", Name),
	io:format("please log out before registering with the logoff(). command~n"),
	{ok, [Name]};


%% logs an user in, except if the node is already logged as another user
handle_info({login, Name, HPsk, Server}, []) ->
	net_kernel:connect_node(Server),
	% remote server will be globally registered 
	% with the name messagerl_server_serv
	gen_server:call(messagerl_server_serv, {login, Name, HPsk}),
	receive
		{ok, logged_in} -> io:format("logged in~n"),
						   {ok, [Name]};
		{error, wrong_psk} -> io:format("wrong password, try again~n"),
							  {ok, []};
		{error, notreg} -> io:format("you must register before you log in~n")
	after 5000 ->
			  io:format("server is not responding~n"),
			  {ok, []}
	end;
handle_info({login, _NewName, _HPsk, _Server}, [Name]) ->
	io:format("you are already logged in as ~p~n", Name),
	io:format("please log out before that with the logoff(). command~n"),
	{ok, [Name]};

%% logs off an user
handle_info(logoff, _State) ->
	gen_server:call(messagerl_server_serv, logoff),
	{ok, []};


%% tries to send a contact request to Name,
%% refuse to do so if you are not logged in
handle_info({addcontact, Name}, [MyName]) ->
	gen_server:call(messagerl_server_serv, {newcontact, Name, MyName}),
	receive
		{ok, sent} -> io:format("Request sent~n"),
					  {ok, [MyName]};
		{error, notcon} -> io:format("~p is not connected~n", Name),
						   {ok, [MyName]}
	after 5000 ->
			  io:format("server is not responding~n"),
			  {ok, [MyName]}
	end;
handle_info({addcontact, _Name}, []) ->
	io:format("please log in before trying to do anything~n"),
	{ok, []};


%% requests the token of said contact, if Name is not your contact
%% you will receive error notcontact
handle_info({request_token, Name}, [MyName]) ->
	HName = crypto:hash(sha512, Name),
	gen_server:call(messagerl_server_serv, {request_token, HName}),
	receive
		{ok, Token} -> io:format("token received sent~n"),
					   messagerl_client_mem:add_contact(Name, Token),
					  {ok, [MyName]};
		{error, notcontact} -> io:format("~p is not in your contacts~n", Name),
						   {ok, [MyName]}
	after 5000 ->
			  io:format("server is not responding~n"),
			  {ok, [MyName]}
	end;
handle_info({request_token, _Name}, []) ->
	io:format("please log in before trying to do anything~n"),
	{ok, []};


%% send a contact deletion request
handle_info({del_contact, Name}, [MyName]) ->
	messagerl_client_mem:del_contact(Name),
	gen_server:call(messagerl_server_serv, {del_contact, Name}),
	receive
		{ok, done} -> io:format("~p has been removed from your contacts~n", Name),
					  {ok, [MyName]}
	after 5000 ->
			  io:format("server is not responding~n"),
			  {ok, [MyName]}
	end;
handle_info({del_contact, _Name}, []) ->
	io:format("please log in before trying to do anything~n"),
	{ok, []};


%% sends a message to one user
handle_info({send, Name, Msg}, [MyName]) ->
	case messagerl_client_mem:get_token_name(Name) of
		{ok, Token} ->
			gen_server:call(messagerl_server_serv, {send, Msg, Token}),
			receive
				{ok, done} -> io:format("your message was sent~n", Name),
							  {ok, [MyName]};
				{error, notcon} -> io:format("dest user was not found~n")
			after 5000 ->
					  io:format("server is not responding~n"),
					  {ok, [MyName]}
			end;
		{error, notfound} ->
			io:format("please request this token with request_token(Name) before sending messages~n"),
			{ok, [MyName]}
	end;
handle_info({send, _Name, _Msg}, []) ->
	io:format("please log in before trying to do anything~n"),
	{ok, []};

%% shutdowns the server, the logged in user must be set as admin directly
%% on the server
handle_info({shutdown_serv, Psk}, State) ->
	HPsk = crypto:hash(sha512, Psk),
	gen_server:call(messagerl_server_serv, {shutdown_serv, HPsk}),
	receive
		{error, wrong_psk} -> io:format("wrong password, try again~n"),
							  {ok, State};
		{error, notreg} -> io:format("who tf are you ?~n"),
						   {ok, State};
		{error, notadm} -> io:format("you are not an admin~n"),
						   {ok, State};
		ok -> {ok, State}
	after 5000 ->
			  io:format("server is not responding ~n"),
			  {ok, State}
	end;

%% message recieved when the server shuts down
handle_info(shutdown, State) ->
	io:format("The server is going down, bye !!~n"),
	{ok, State};

handle_info(_Msg, State) ->
	{ok, State}.




code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% logs off an user on termination even if they did not did so
terminate(_Reason, _State) ->
	gen_server:call(messagerl_server_serv, logoff),
	ok.


%%


