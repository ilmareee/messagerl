-module(messagerl_client_mem).

-include_lib("stdlib/include/ms_transform.hrl").

-include("messagerl_client.hrl").

-export([add_contact/2, del_contact/1, modify_name/2]).
-export([get_token_name/1, get_name_token/1]).
-export([add_group/2]).
-export([save_msg/3, save_msg_grp/4]).
-export([get_msg_new/0, get_msg_name/1, get_msg_grp/1]).
-export([count_unread/0]).

%% adds a new contact to your mnesia tables
add_contact(Name, Token) ->
	F = fun() ->
		mnesia:write(#messagerl_cli_contact{name = Name,
											token = Token})
	end,
	mnesia:activity(transaction, F).
%% removes a (formem) contact from the mnesia tables
del_contact(Name) ->
	F = fun() ->
		mnesia:delete({messagerl_cli_contact, Name})
	end,
	mnesia:activity(transaction, F).
%% modify the name for the user with the token Token
modify_name(NewName, Token) ->
	F = fun() ->
		case mnesia:match_object({messagerl_cli_contact, '_', Token}) of
			[] -> mnesia:write(#messagerl_cli_contact{name = NewName,
													  token = Token});
			[{Name, _}] -> mnesia:delete({messagerl_cli_contact, Name}),
						   mnesia:write(#messagerl_cli_contact{name = NewName,
															   token = Token})
		end
	end,
	mnesia:activity(transaction, F).

%% get the token of Name
get_token_name(Name) ->
	F = fun() ->
		mnesia:read(messagerl_cli_contact, Name)
	end,
	case mnesia:activity(transaction, F) of
		[] -> {error, notfound};
		[{Name, Token}] -> {ok, Token}
	end.
%% gets the name of the user with the token Token
get_name_token(Token) ->
	F = fun() ->
		mnesia:match_object({messagerl_cli_contact, '_', Token})
	end,
	case mnesia:activity(transaction, F) of
		[] -> {error, notfound};
		[{Name, Token}] -> {ok, Name}
	end.

%% creates a new group (Name) with the members Members
add_group(Name, Members) ->
	F = fun() ->
		mnesia:write(#messagerl_cli_groups{name = Name,
											 members = Members})
	end,
	mnesia:activity(transaction, F).

%% saves a message directly comming from a user to the mnesia table
save_msg(Time, From, Content) ->
	F = fun() ->
		mnesia:write(#messagerl_cli_msg{time = Time,
										  from = From,
										  content = Content})
	end,
	mnesia:activity(transaction, F).
%% saves a message comming from a group to the mnesia table
save_msg_grp(Time, From, Group, Content) ->
	F = fun() ->
		mnesia:write(#messagerl_cli_msg{time = Time,
										  from = From,
										  group = Group,
										  content = Content})
	end,
	mnesia:activity(transaction, F).

%% gets every message that was not yet read
get_msg_new() ->
	F = fun() ->
		mnesia:match_object(#messagerl_cli_msg{_ = '_', read = none})
	end,
	Res = mnesia:activity(transaction, F),
	lists:map(mark_read, Res),
	Res.
%% gets the messages sent directly by an user (not in a group)
get_msg_name(Name) ->
	F = fun() ->
		mnesia:match_object(#messagerl_cli_msg{_ = '_', from = Name, group = none})
	end,
	Res = mnesia:activity(transaction, F),
	lists:map(mark_read, Res),
	Res.
%% gets messages sent to the group GrpName
get_msg_grp(GrpName) ->
	F = fun() ->
		mnesia:match_object(#messagerl_cli_msg{_ = '_', group = GrpName})
	end,
	Res = mnesia:activity(transaction, F),
	lists:map(mark_read, Res),
	Res.

%% marks a message as read
mark_read(Msg) ->
	F = fun() ->
		mnesia:write(Msg#messagerl_cli_msg{read = read})
	end,
	mnesia:activity(transaction, F).
%% count the number of messages that are not marked as read
count_unread() ->
	F = fun() ->
		mnesia:match_object(#messagerl_cli_msg{_ = '_', read = none})
	end,
	Res = mnesia:activity(transaction, F),
	length(Res).

