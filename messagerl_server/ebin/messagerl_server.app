{application, messagerl_server,
 [{description, "A server for the messagerl protocol"},
  {vsn, "0.1.0"},
  {registered, []},
  {mod, {messagerl_server_app, []}},
  {applications,
   [kernel,
    stdlib,
    crypto,
    mnesia
   ]},
  {env,[]},
  {modules, [messagerl_server_app, messagerl_server_mem, messagerl_server_serv, messagerl_server_sup]},

  {licenses, ["Apache-2.0"]},
  {links, []}
 ]}.
