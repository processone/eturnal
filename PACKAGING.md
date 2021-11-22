Packaging eturnal
=================

> The following notes might be interesting to distribution packagers.

The eturnal server is built as a so-called [Erlang/OTP release][1] (using the
[Rebar3][2] tool). An Erlang/OTP release is a stand-alone directory that
contains eturnal and (usually) all Erlang dependencies including the required
parts of the Erlang VM itself. Erlang/OTP releases are freely relocatable within
the file system and offer deployment features such as in-place upgrades of the
software. However, the release concept was not designed with distribution
packaging in mind, esp. regarding dependency management and the release
directory structure.

Dependency Management
---------------------

Besides the dependencies [mentioned][3] in the [INSTALL.md][4] file, eturnal
depends on the Erlang libraries referenced as `deps` in the [rebar.config][5]
file, _except_ for `recon` (which is just included for debugging purposes). The
complete list of (direct and transitive) Erlang dependencies is:

- [stun](https://github.com/processone/stun)
- [conf](https://github.com/processone/conf)
- [yval](https://github.com/processone/yval)
- [fast\_yaml](https://github.com/processone/fast_yaml)
- [fast\_tls](https://github.com/processone/fast_tls)
- [p1\_utils](https://github.com/processone/p1_utils)

The `mod_stats_influx` module adds the following list of optional dependencies
(if the module is enabled without those being available, eturnal will refuse to
start up and log a proper error message):

- [influx\_udp](https://github.com/weiss/influx_udp)
- [poolboy](https://github.com/devinus/poolboy)
- [ulitos](https://github.com/palkan/ulitos)

Assuming the required dependencies are installed into the distribution's
Erlang/OTP tree, [Rebar3][2]'s built-in dependency management can be skipped by
building eturnal (from the [official source tarball][6]) as follows:

    $ rm -f rebar.lock
    $ SKIP_DEPS=true ./rebar3 as distro release

This will yield a `_build/distro/rel/eturnal` directory containing the eturnal
files to be installed.

Directory Structure
-------------------

The following three directories must be installed to a common location (for
example, `/usr/lib/eturnal`), except that the contents of the `lib` folder
_may_ be installed into the global Erlang/OTP library tree instead:

- `_build/distro/rel/eturnal/bin`
- `_build/distro/rel/eturnal/lib`
- `_build/distro/rel/eturnal/releases`

The `eturnal_bin_prefix` setting in the [build.config][7] file must be set to
that location before building eturnal. The user for running eturnal and the
default configuration file path can be adjusted in that file as well.

The `bin` directory contains, among other things, the [eturnalctl][8] script,
which can be moved (or symlinked) elsewhere (e.g., into `/usr/sbin`). If it's
moved, systemd/init must be pointed to the new path.

The `_build/distro/rel/eturnal/etc` directory holds a [sample configuration
file][9], which can be installed into `/etc`. It also contains a [systemd
unit][10] and an [init script][11] example. The [LICENSE.txt][12] file can be
found in the `_build/distro/rel/eturnal/doc` directory. Other directories
created below the `_build` folder can be ignored.

 [1]: https://erlang.org/doc/design_principles/release_structure.html
 [2]: https://rebar3.org
 [3]: https://github.com/processone/eturnal/blob/master/INSTALL.md#requirements
 [4]: https://github.com/processone/eturnal/blob/master/INSTALL.md
 [5]: https://github.com/processone/eturnal/blob/master/rebar.config
 [6]: https://eturnal.net/download/
 [7]: https://github.com/processone/eturnal/blob/master/build.config
 [8]: https://github.com/processone/eturnal/blob/master/scripts/eturnalctl
 [9]: https://github.com/processone/eturnal/blob/master/config/eturnal.yml
[10]: https://github.com/processone/eturnal/blob/master/config/eturnal.service
[11]: https://github.com/processone/eturnal/blob/master/scripts/eturnal.init
[12]: https://github.com/processone/eturnal/blob/master/LICENSE
