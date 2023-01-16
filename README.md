# messagerl

This project is about creating a messenger application with Erlang/OTP

It tries to ensure persistance for messages and accounts on a server.
The policy is that only users who added each other to their respective
contacts can send each other messages, with the server verifying it and
giving the necessary token to send messages only to contacts.

It is not finished yet, it lacks a serious debug and revision,
but I radically lacked time for now

there are two part, a client (messagerl_client) and a server 
(messagerl_server), each of them containing it's own set of instructions
for installation and usage

The project tries to take advantage from the OTP abstractions for
message reception and process handling (and respawning), and from
the mnesia functionalities for storing and efficiently searching through
tables for user accounts and messages


