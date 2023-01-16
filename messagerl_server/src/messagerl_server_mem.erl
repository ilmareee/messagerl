%% This is a wrapper for mnesia calls
-module(messagerl_server_mem).

-include_lib("stdlib/include/ms_transform.hrl").

-include("messagerl_server.hrl").

-export([register_user/2]).
-export([set_admin/1]).
-export([add_contacts/2, del_contacts/2, are_contacts/2]).
-export([regis_con/2, unreg_con/1]).
-export([get_token_name/1, get_token_node/1]).
-export([get_node/1, get_contacts/1, get_psk/1, get_perms/1, get_name/1]).

-export([broadcast_msg_all/1]).

%% registers a new user and creates its token
register_user(Name, Psk) ->
	Token = erlang:unique_integer(),
	F = fun() ->
		mnesia:write(#messagerl_contacts{token = Token, contacts = []}),
		mnesia:write(#messagerl_profiles{token = Token, psk = Psk}),
		mnesia:write(#messagerl_usernames{token = Token, username_hash = Name})
	end,
	mnesia:activity(transaction, F).

%% sets the given user as admin
set_admin(Name) ->
	HName = crypto:hash(sha512, Name),
	{ok, Token} = get_token_name(HName),
	{ok, Psk} = get_psk(Token),
	F = fun() ->
		mnesia:write(#messagerl_profiles{token = Token, psk = Psk, perms = admin}) end,
	mnesia:activity(F).

%% adds users to their respective contact list
add_contacts(Token1, Token2) ->
	F = fun() ->
		[{Token1, Contacts1}] = mnesia:wread({messagerl_contacts, Token1}),
		[{Token2, Contacts2}] = mnesia:wread({messagerl_contacts, Token2}),
		mnesia:write(#messagerl_contacts{token = Token1,
										 contacts = [Token2 | Contacts1]}),
		mnesia:write(#messagerl_contacts{token = Token2,
										 contacts = [Token1 | Contacts2]})
	end,
	mnesia:activity(transaction, F).
%% delete users from their respective contact list
%% (the policy is for contacts always going in pairs, even though contacts
%% lists on client side may not be updated they will not be able to contact
%% each other anymore (unless they re-add themselves to their contacts))
del_contacts(Tok1, Tok2) ->
	F = fun() ->
		[{Tok1, Conts1}] = mnesia:read({messagerl_contacts, Tok1}),
		[{Tok2, Conts2}] = mnesia:read({messagerl_contacts, Tok2}),
		mnesia:write(#messagerl_contacts{token = Tok1,
										 contacts = lists:delete(Tok2, Conts1)}),
		mnesia:write(#messagerl_contacts{token = Tok2,
										 contacts = lists:delete(Tok1, Conts2)})
	end,
	mnesia:activity(transaction, F).
%% Reply if two tokens are contacts
are_contacts(Tok1, Tok2) ->
	F = fun() ->
		mnesia:read({messagerl_contacts, Tok1})
	end,
	[{Tok1, Contacts}] = mnesia:activity(transaction, F),
	lists:any(fun(T) -> Tok2 == T end, Contacts).

%% stores the ip and pid of some user when they log on
%% and returns the ip of their contacts
regis_con(Token, Node) ->
	F = fun() ->
		mnesia:write(#messagerl_node{token=Token,
									 node = Node})
	end,
	ok = mnesia:activity(transaction, F),
	{ok, Contacts} = get_contacts(Token),
	NodeList = lists:map(fun(T) ->
							case get_node(T) of
								{ok, Node} -> Node;
								{error, notcon} -> {error, notcon}
							end
						end, Contacts),
	NodeConOnly = [P || P <- NodeList, P =/= {error, notcon}],
	broadcast_msg({connects, Token}, NodeConOnly),
	{ok, logged_in}.
%% deletes these informations when they log off
unreg_con(Node) ->
	{ok, Token} = get_token_node(Node),
	F = fun() -> mnesia:delete({messagerl_node, Token}) end,
	mnesia:activity(transaction, F),
	{ok, Contacts} = get_contacts(Token),
	NodeList = lists:map(fun(T) ->
							case get_node(T) of
								{ok, Node} -> Node;
								{error, notcon} -> {error, notcon}
							end
						end, Contacts),
	NodeConOnly = [P || P <- NodeList, P =/= {error, notcon}],
	broadcast_msg({disconnects, Token}, NodeConOnly),
	{ok, logged_off}.

%% returns the token of the given user
get_token_name(NameHash) ->
	F = fun() -> mnesia:match_object({messagerl_usernames, '_', NameHash}) end,
	Res = mnesia:activity(transaction, F),
	case Res of
		{Token, NameHash} -> {ok, Token};
		[] -> {error, unknown_name}
	end.
%% returns the token of the given pid
get_token_node(Node) ->
	F = fun() -> mnesia:match_object({messagerl_node, '_', Node}) end,
	[Res] = mnesia:activity(transaction, F),
	{Token, Node} = Res,
	{ok, Token}.


%% returns the data for the given token
get_node(Token) ->
	F = fun() -> mnesia:read(messagerl_node, Token) end,
	Res = mnesia:activity(transaction, F),
	case Res of
		[{Token, Node}] -> {ok, Node};
		[] -> {error, notcon}
	end.
get_contacts(Token) ->
	F = fun() -> mnesia:read(messagerl_contacts, Token) end,
	[Res] = mnesia:activity(transaction, F),
	{Token, Contacts} = Res,
	{ok, Contacts}.
get_psk(Token) ->
	F = fun() -> mnesia:read(messagerl_profiles, Token) end,
	[Res] = mnesia:activity(transaction, F),
	{Token, Psk, _} = Res,
	{ok, Psk}.
get_perms(Token) ->
	F = fun() -> mnesia:read(messagerl_profiles, Token) end,
	[Res] = mnesia:activity(transaction, F),
	{Token, _, Perms} = Res,
	{ok, Perms}.
get_name(Token) ->
	F = fun() -> mnesia:read(messagerl_usernames, Token) end,
	[Res] = mnesia:activity(transaction, F),
	{Token, Name} = Res,
	{ok, Name}.

%% broadcasts a message to all connected nodes
broadcast_msg_all(Msg) ->
	F = fun() -> mnesia:all_keys(messagerl_node) end,
	Tokens = mnesia:activity(transaction, F),
	NodeList = lists:map(fun(T) ->
							case get_node(T) of
								{ok, Node} -> Node;
								{error, notcon} -> {error, notcon}
							end
						end, Tokens),
	NodeConOnly = [P || P <- NodeList, P =/= {error, notcon}],
	broadcast_msg(Msg, NodeConOnly).

%% broadcasts a message to all given nodes
broadcast_msg(_Msg, []) ->
	ok;
broadcast_msg(Msg, [Node | Nodes]) ->
	Node ! Msg,
	broadcast_msg(Msg, Nodes).

