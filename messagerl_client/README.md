messagerl_client
=====

This is the client for the messagerl protocol
We have user persistance in the server, and a system of contacts
to send each other messages : you must log in with your username and
password, and you only can send messages to your contacts through the
token you can request once they have accepted : more explicitely :
1 register yourself
2 login
3 send contact request
4 once it is accepted, request your contact's token
5 now you can send them messages

You will be prompted if someone sends you a contact request, and be
notified in the shell when new messages arrives, with the number
of yet unread messages.
To read your messages you can either read every message an user sent you,
or read every unread message
Group messages are not fully implemented yet, so the api for it is not
accessible


to compile the client run in this directory :
    erl -make


then to install the client run the install.sh file and then in the prompt enter :
    messagerl_client_app:install().
and in the second prompt :
    q().


To modify the node of the server, modify it at the 12th line of ebin/messagerl_client.app




To use the application :
    run the start.sh file and then in the prompt :
     messagerl_cli_start:start_cli().


    - to register a new user (for the server), use messagerl_client_api:register_user().

    - to log in once you're registered use messagerl_client_api:login().
    - to log off use messagerl_client_api:logoff().

    - to send a contact request use messagerl_client_api:add_contact(Name).
        with Name being your contact's username
    - to delete a contact you made use messagerl_client_api:del_contact(Name).
        the same way
    - to request your contact's token use messagerl_client_api:request_token(Name).

    - to send message use messagerl_client_api:send_message(Name, Msg).
        with Msg being your message between double quotes ("<content>")

    - if you're an admin (see messagerl_server's doc to set an user as admin),
        you can shutdown the server with messagerl_client_api:shutdown_serv(Psk).
        with Psk being your password

    - you can stop the application with messagerl_client_api:stop().
