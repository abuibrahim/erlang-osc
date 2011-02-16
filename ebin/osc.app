%% -*-Erlang-*-
{application, osc,
 [{description, "Open Sound Control Application"},
  {vsn, "1.0.0"},
  {modules, [osc_app, osc_sup, osc_lib, osc_server, osc_methods]},
  {registered, [osc_sup, osc_server]},
  {applications, [kernel, stdlib]},
  {mod, {osc_app, []}},
  {env, [{port, 7123}, {recbuf, 8192}]}
 ]}.
%% vim: set filetype=erlang :
