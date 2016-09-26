# Erlang Native Compiler

Since rebar3 refuses to add support for NIFs I've created this self contained escript to compile native code. This is based on rebar2 by removing everything unrelated to the port compiler.

## Usage

1. Clone this repository
1. Run `make` in this directory
1. Copy `enc` to your project and commit it
1. Add these (or similar) hooks to your rebar.config:

```erlang
{pre_hooks, [{"", compile, "./enc compile"}]}.
{post_hooks, [{"", clean, "./enc clean"}]}.
```

After that enc should read your old rebar.config `port\_specs` and `port\_env` settings as expected (it is rebar2's port compiler after all...).

## Compatibility Note

Remember that you'll want to make sure and build `enc` with the oldest Erlang VM you want to support.
