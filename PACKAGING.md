Packaging eturnal
=================

> The following notes might be interesting to distribution packagers.

The eturnal server is built as a so-called [Erlang/OTP release][1] (using the
[rebar3][2] and [relx][3] tools). An Erlang/OTP release is a stand-alone
directory that contains eturnal and (usually) all Erlang dependencies including
the required parts of the Erlang VM itself. Erlang/OTP releases are freely
relocatable within the file system and offer deployment features such as
in-place upgrades of the software. However, the release concept was not designed
with distribution packaging in mind, esp. regarding dependency management and
the release directory structure.

Dependency Management
---------------------

Besides the dependencies [mentioned][4] in the [README.md][5] file, eturnal
depends on the Erlang libraries referenced as `deps` in the [rebar.config][6]
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
Erlang/OTP tree, [rebar3][2]'s built-in dependency management can be skipped by
building eturnal (from the [official source tarball][7]) as follows:

    $ rm -f rebar.lock
    $ SKIP_DEPS=true ./rebar3 as distro release

This will yield a `_build/distro/rel/eturnal` directory containing the eturnal
files to be installed.

Directory Structure
-------------------

The eturnal server is started (and controlled) by calling the `eturnalctl`
script, which is a small wrapper around the `eturnal` command. The latter
expects a few files and directories to be available in the following locations:

- The `bin` directory the `eturnal` command itself is installed to.
- The `releases` directory, which is assumed to be in the same folder as the
  `bin` directory.

Therefore, distributions might want to install the directories
`_build/distro/rel/eturnal/bin` and `_build/distro/rel/eturnal/releases` into
some (for example) `/usr/lib/eturnal` folder and adjust the path to the
`eturnal` command within the `eturnalctl` script accordingly. The `eturnalctl`
script itself can be installed elsewhere (e.g., into `/usr/sbin`). The
`_build/distro/rel/eturnal/lib/eturnal-$version` folder should be installed into
the main Erlang library directory as returned by:

    $ erl -noinput -eval 'io:put_chars(code:lib_dir()), io:nl(), halt()'

The `_build/stripped/rel/eturnal/etc` directory holds a [sample configuration
file][9], a [systemd unit][10], and an [init script][11]. The [LICENSE.txt][12]
file can be found in the `_build/stripped/rel/eturnal/doc` directory. Other
directories created below the `_build` folder can be ignored.

The following list of files could be patched before calling `./rebar3` in order
to specify a different system user for running eturnal, to adjust the paths to
the `eturnalctl` and `eturnal` scripts, and to change the default configuration
file path:

- [scripts/eturnalctl][8]
- [scripts/eturnal.init][11]
- [config/eturnal.service][10]
- [config/sys.config][13]

 [1]: https://erlang.org/doc/design_principles/release_structure.html
 [2]: https://www.rebar3.org
 [3]: https://erlware.github.io/relx/
 [4]: https://github.com/processone/eturnal/blob/master/README.md#requirements
 [5]: https://github.com/processone/eturnal/blob/master/README.md
 [6]: https://github.com/processone/eturnal/blob/master/rebar.config
 [7]: https://eturnal.net/download/
 [8]: https://github.com/processone/eturnal/blob/master/scripts/eturnalctl
 [9]: https://github.com/processone/eturnal/blob/master/config/eturnal.yml
[10]: https://github.com/processone/eturnal/blob/master/config/eturnal.service
[11]: https://github.com/processone/eturnal/blob/master/scripts/eturnal.init
[12]: https://github.com/processone/eturnal/blob/master/LICENSE
[13]: https://github.com/processone/eturnal/blob/master/config/sys.config
