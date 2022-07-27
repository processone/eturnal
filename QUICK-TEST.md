# Quick Test of eturnal

On Linux/x64 systems, the following commands provide STUN/TURN services on
UDP/TCP port 3478 (on Linux/arm64 systems, replace `x64` with `arm64`):

```
curl https://eturnal.net/eturnal-1.10.0-linux-x64.tar.gz | tar -xzf -
eturnal/bin/eturnalctl foreground
```

The service can be stopped with `Ctrl`+`c` and deleted by removing the `eturnal`
directory.

## TURN Setup

- If the system's public IP address cannot be autodetected, it must be specified
  as `relay_ipv4_addr` in `eturnal/etc/eturnal.yml`.
- The UDP port range [49152][1]-[65535][2] must be accessible in addition to
  port [3478][3].
- Use `eturnal/bin/eturnalctl credentials` (in another shell) to retrieve a
  temporary username/password.
- Those credentials are invalidated on eturnal restart unless a `secret` was
  specified in `eturnal/etc/eturnal.yml` before generating them.

[1]: https://eturnal.net/documentation/#relay_min_port
[2]: https://eturnal.net/documentation/#relay_max_port
[3]: https://eturnal.net/documentation/#listen
