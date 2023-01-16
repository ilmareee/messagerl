messagerl_server
=====

This is the server for the messagerl protocol
We have user persistance in the server, and a system of contacts
to send each other messages : you must log in with your username and
password, and you only can send messages to your contacts through the
token you can request once they have accepted : more explicitely :
1 register yourself
2 login
3 send contact request
4 once it is accepted, request your contact's token
5 now you can send them messages


The server handles 4 mnesia tables in order to keep information about
the users for the persistance
Group messages are not fully implemented yet, so the api for it is not
accessible


To compile the server run in this directory :
    erl -make


To install the server run the install.sh file and then in the prompt enter :
    messagerl_server_app:install().
and in the second prompt :
    q().


To start the server :
    run the start.sh file and then in the prompt :
     messagerl_serv_start:start_serv().


    - to set the permissions of an user to admin use :
        messagerl_server_mem:set_admin(Name)
    with Name being the name of the user
