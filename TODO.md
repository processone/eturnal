To-Do List
==========

eturnal application
-------------------

- Write `doc/overview.edoc`.
- Document the configuration options.
- Document the port ranges that need to be accessible.
- Document the supported RFCs/extensions.
- Add test cases and set up CI.
- Create PID file?
- Support systemd watchdog.
- Ship init script example.
- Ship `certbot --post-hook` script.
- Add `Makefile` that generates (temporary) `vars.config` from environment.
- Add `scripts/make-source` script to produce release source tarball.
- Add sane workflow for cutting a release (currently, the release version is
  hard-coded in `rebar.config`, `scripts/make-binary`, `CHANGELOG.md`, and
  `README.md`; the application version in `src/eturnal.app.src`).
- Add built-in REST API (using the Inets HTTP server with `mod_esi`).
- Support persistent credentials (using Mnesia).

stun application
----------------

- Produce proper logging.
- Shutdown workers cleanly (no `brutal_kill`).
- Enforce `max_allocations` limit.
