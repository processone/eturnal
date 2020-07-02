To-Do List
==========

eturnal application
-------------------

- Write `doc/overview.edoc`.
- Document the configuration options.
- Document the port ranges that need to be accessible.
- Document the supported RFCs/extensions.
- Create PID file?
- Support systemd watchdog.
- Ship init script example.
- Ship `certbot --post-hook` script.
- Add `Makefile` that generates (temporary) `vars.config` from environment.
- Once rebar3 3.14 (or newer) is stable, replace the download URL in `README.md`
  with <https://rebar3.s3.amazonaws.com/rebar3>.
- Add built-in REST API (using the Inets HTTP server with `mod_esi`).
- Support persistent credentials (using Mnesia).

stun application
----------------

- Shutdown workers cleanly (no `brutal_kill`).
- Enforce `max_allocations` limit.
