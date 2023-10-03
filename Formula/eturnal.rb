class Eturnal < Formula
  desc "STUN/TURN server"
  homepage "https://eturnal.net"
  url "https://eturnal.net/download/eturnal-1.12.0.tar.gz"
  sha256 "5a36cbd93abc92d166b8d6d4cf6ffd2fb4fd3c4fdcd2ca3513890c93cbeef67c"
  license "Apache-2.0"
  head "https://github.com/processone/eturnal.git", branch: "master"

  depends_on "erlang" => :build
  depends_on "rebar3" => :build
  depends_on "libyaml"
  depends_on "openssl@3"

  on_linux do
    depends_on "ncurses"
    depends_on "zlib"
  end

  conflicts_with "ejabberd", because: "both install e.g. `p1_utils-x.x.x` lib"

  def install
    # build release
    ENV["ETURNAL_PREFIX"] = opt_prefix.to_s
    ENV["ETURNAL_ETC_DIR"] = etc.to_s
    system "rebar3", "as", "prod", "release"
    # conduct rebar3 test suites
    system "rebar3", "xref"
    system "rebar3", "eunit"
    system "rebar3", "ct"

    # install libraries & configuration files
    prefix.install "_build/prod/rel/#{name}/bin"
    prefix.install "_build/prod/rel/#{name}/lib"
    prefix.install "_build/prod/rel/#{name}/releases"
    prefix.install Dir["_build/prod/rel/#{name}/erts-*"]

    cd "_build/prod/rel/#{name}/etc" do
      etc.install "#{name}.yml"
      (prefix/"etc").install "systemd"
    end

    # move doc pages
    (share/"doc/#{name}").install "_build/prod/rel/#{name}/doc/LICENSE.txt"
    (share/"doc/#{name}").install "_build/prod/rel/#{name}/doc/README.md"
    (share/"doc/#{name}").install "_build/prod/rel/#{name}/doc/CHANGELOG.md"
  end

  def post_install
    (var/"log/#{name}").mkpath
    (var/"log/#{name}").install_symlink opt_prefix/"log"
    (var/"run/#{name}").mkpath
    (var/"run/#{name}").install_symlink opt_prefix/"run"
  end

  def caveats
    <<~EOS
      With macOS > 12.3 `#{name}ctl` can be invoked directly without specifying
      the path `$(brew --prefix)/opt/#{name}/bin/#{name}ctl`.

      #{name}'s configuration file `$(brew --prefix)/etc/#{name}.yml` uses the
      (indentation-sensitive!) YAML format. A documentation can be found on
      https://#{name}.net or on https://github.com/processone/#{name}.
    EOS
  end

  service do
    run [opt_bin/"eturnalctl", "foreground"]
  end

  test do
    ENV["LOGS_DIRECTORY"] = var/"log/eturnal"
    ENV["RUNTIME_DIRECTORY"] = var/"run/eturnal"
    system opt_prefix/"bin/#{name}ctl", "daemon"
    system opt_prefix/"bin/#{name}ctl", "ping"
    system opt_prefix/"bin/#{name}ctl", "info"
    system opt_prefix/"bin/#{name}ctl", "stop"
  end
end
