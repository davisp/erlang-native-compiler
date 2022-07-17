%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 ft=erlang et

{application, enc,
 [{description, "enc: Erlang Native Compiler"},
  {vsn, "2.6.4"},
  {modules, [ enc,
              rebar,
              rebar_config,
              rebar_port_compiler,
              rebar_utils]},
  {registered, []},
  {applications,
   [
    kernel,
    stdlib,
    sasl,
    compiler,
    crypto,
    syntax_tools,
    tools,
    eunit,
    reltool,
    dialyzer,
    asn1,
    snmp,
    edoc
   ]},
  {env, [{log_level, warn}]}
]}.
