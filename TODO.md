To-Do List
==========

eturnal application
-------------------

* Support systemd watchdog.
* Ship `certbot --post-hook` script.
* Add `Makefile` that generates (temporary) `vars.config` from environment.
* Once rebar3 3.14 (or newer) is stable, replace the download URL in `README.md`
  with <https://rebar3.s3.amazonaws.com/rebar3>.
* Add built-in REST API (using the Inets HTTP server with `mod_esi`).
* Support persistent credentials (using Mnesia).

stun application
----------------

* Shutdown workers cleanly (no `brutal_kill`).
