# Quick Test of eturnal with Docker

On Linux docker hosts, the following commands provide STUN/TURN services on
UDP/TCP port 3478:

```
docker run -d --rm --name eturnal \
  -p 3478:3478 -p 3478:3478/udp \
  -p 50000-50100:50000-50100/udp \
  -e ETURNAL_RELAY_MIN_PORT=50000 \
  -e ETURNAL_RELAY_MAX_PORT=50100 \
  ghcr.io/processone/eturnal:latest
```

The service can be stopped with `docker stop eturnal`.

The image can be removed with `docker rmi ghcr.io/processone/eturnal:latest`.

## TURN Setup

- If the system's public IP address cannot be autodetected, it must be specified
  as an environment variable `ETURNAL_RELAY_IPV4_ADDR` within the `docker run` command.
- The example UDP port range [50000][1]-[50100][2] must be accessible in addition to
  port [3478][3].
- Use `docker exec eturnal eturnalctl credentials` to retrieve a
  temporary username/password.
- Those credentials are invalidated on eturnal restart unless a `secret` was
  specified with the environment variable `ETURNAL_SECRET` within the `docker run` command before generating them.
- Use e.g. [https://icetest.info][4] for a quick connection test.

## Example workflow
```shell
# docker run -d --rm --name eturnal -p 3478:3478 -p 3478:3478/udp -p 50000-50100:50000-50100/udp -e ETURNAL_RELAY_MIN_PORT=50000 -e ETURNAL_RELAY_MAX_PORT=50100 ghcr.io/processone/eturnal:latest
Unable to find image 'ghcr.io/processone/eturnal:latest' locally
latest: Pulling from processone/eturnal
2408cc74d12b: Already exists 
de841c42ca3c: Pull complete 
4f4fb700ef54: Pull complete 
Digest: sha256:9ace89295692502120a065ef92546eddee952c46128dd9d06a2ab3e7f513aa97
Status: Downloaded newer image for ghcr.io/processone/eturnal:latest
9359f3f5e047d3801b1cc1d8bb1d1a7da0406ad0b5c5111f0ac8b711fa0aa057
##
# docker logs eturnal | egrep 'Start|IPv4|STUN|TURN'
2022-07-23 07:06:28.645727+00:00 [notice] Starting eturnal 1.9.1 on Erlang/OTP 24 (ERTS 12.3.1)
2022-07-23 07:06:28.645951+00:00 [info] Relay IPv4 address: 80.68.24.166 (port range: 50000-50100)
2022-07-23 07:06:28.646162+00:00 [info] Started mod_log_stun
2022-07-23 07:06:28.646349+00:00 [info] Listening on [::]:3478 (udp) (STUN/TURN)
2022-07-23 07:06:28.646523+00:00 [info] Listening on [::]:3478 (tcp) (STUN/TURN)
##
# docker exec eturnal eturnalctl info
eturnal 1.9.1 on Erlang/OTP 24 (ERTS 12.3.1)
Uptime: 0 days, 0 hours, 0 minutes, 20 seconds
Active TURN sessions: 0
Processes: 73
Total length of run queues: 1
Total CPU usage (reductions): 1087991
Allocated memory (MiB): 36
##
# docker exec eturnal eturnalctl credentials
Username: 1658645797
Password: VnIk+7J3E7smzET6DytxX00Q+XU=
```

Workflow commands during/after connecting to the server, e.g. with [https://icetest.info][4]

```shell
# docker logs eturnal | egrep 'request|authentication|allocation'
2022-07-23 07:10:35.632957+00:00 [info] Responding to STUN request [UDP, session fez426jli6f3, anonymous, client 98.112.234.97:11388]
2022-07-23 07:10:35.673616+00:00 [info] Responding to STUN request [UDP, session o3y9sx0fb4g4, anonymous, client 98.112.234.97:1836]
2022-07-23 07:10:35.683408+00:00 [info] Accepting long-term STUN/TURN authentication [UDP, session mszi8d37hm67, user 1658648497, client 98.112.234.97:11388]
2022-07-23 07:10:35.683830+00:00 [notice] Creating TURN allocation (lifetime: 600 seconds) [UDP, session mszi8d37hm67, user 1658648497, client 98.112.234.97:11388, relay 80.68.24.166:50029]
2022-07-23 07:10:35.724713+00:00 [info] Accepting long-term STUN/TURN authentication [UDP, session 2jnlheam2zrh, user 1658648497, client 98.112.234.97:1836]
2022-07-23 07:10:35.725302+00:00 [notice] Creating TURN allocation (lifetime: 599 seconds) [UDP, session 2jnlheam2zrh, user 1658648497, client 98.112.234.97:1836, relay 80.68.24.166:50030]
##
# docker exec eturnal eturnalctl sessions
2 active TURN sessions:

-- TURN session of 1658648497 --
          Client: 98.112.234.97:11388 (UDP)
           Relay: 80.68.24.166:50029 (UDP)
   Permission(s): none
         Peer(s): none
            Sent: 0 KiB (0 packets)
        Received: 0 KiB (0 packets)
     Running for: 5 seconds

-- TURN session of 1658648497 --
          Client: 98.112.234.97:1836 (UDP)
           Relay: 80.68.24.166:50030 (UDP)
   Permission(s): none
         Peer(s): none
            Sent: 0 KiB (0 packets)
        Received: 0 KiB (0 packets)
     Running for: 5 seconds
```

[1]: https://eturnal.net/documentation/#relay_min_port
[2]: https://eturnal.net/documentation/#relay_max_port
[3]: https://eturnal.net/documentation/#listen
[4]: https://icetest.info/
