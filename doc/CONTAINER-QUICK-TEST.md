# Quick Test of eturnal with Docker

On Linux docker hosts, the following command provides STUN/TURN services on
UDP/TCP port 3478:

```
docker run -d --rm --name eturnal \
    -p 3478:3478/udp -p 3478:3478 -p 50000-50050:50000-50050/udp \
    -e ETURNAL_RELAY_MIN_PORT=50000 -e ETURNAL_RELAY_MAX_PORT=50050 \
  ghcr.io/processone/eturnal:latest
```

The service can be stopped with `docker stop eturnal`.

The image can be removed with `docker rmi ghcr.io/processone/eturnal:latest`.

## TURN Setup

- If the system's public IP address cannot be autodetected, it must be defined
  with the environment variable `ETURNAL_RELAY_IPV4_ADDR` within the `docker run` command.
- The example UDP port range [50000][1]-[50050][2] must be accessible in addition to
  port [3478][3].
- Use `docker exec eturnal eturnalctl credentials` to retrieve a
  temporary username/password.
- Those credentials are invalidated on eturnal restart unless a `secret` was
  defined with the environment variable `ETURNAL_SECRET` within the `docker run` command before generating them.
- Use e.g. [icetest.info][4] or [trickle ice test][5] for a quick connection test.

## Example workflow

Create the STUN/TURN service, check basic logging and create credentials for TURN authentication.

```shell
# docker run -d --rm --name eturnal -p 3478:3478 -p 3478:3478/udp -p 50000-50050:50000-50050/udp -e ETURNAL_RELAY_MIN_PORT=50000 -e ETURNAL_RELAY_MAX_PORT=50050 ghcr.io/processone/eturnal:latest
Unable to find image 'ghcr.io/processone/eturnal:latest' locally
latest: Pulling from processone/eturnal
2408cc74d12b: Already exists 
de841c42ca3c: Pull complete 
4f4fb700ef54: Pull complete 
Digest: sha256:9ace89295692502120a065ef92546eddee952c46128dd9d06a2ab3e7f513aa97
Status: Downloaded newer image for ghcr.io/processone/eturnal:latest
9359f3f5e047d3801b1cc1d8bb1d1a7da0406ad0b5c5111f0ac8b711fa0aa057

# docker logs eturnal | egrep 'Start|IPv4|STUN|TURN'
2022-07-23 07:06:28.645727+00:00 [notice] Starting eturnal 1.9.1 on Erlang/OTP 24 (ERTS 12.3.1)
2022-07-23 07:06:28.645951+00:00 [info] Relay IPv4 address: 203.0.113.4 (port range: 50000-50050)
2022-07-23 07:06:28.646162+00:00 [info] Started mod_log_stun
2022-07-23 07:06:28.646349+00:00 [info] Listening on [::]:3478 (udp) (STUN/TURN)
2022-07-23 07:06:28.646523+00:00 [info] Listening on [::]:3478 (tcp) (STUN/TURN)

# docker exec eturnal eturnalctl info
eturnal 1.9.1 on Erlang/OTP 24 (ERTS 12.3.1)
Uptime: 0 days, 0 hours, 0 minutes, 20 seconds
Active TURN sessions: 0
Processes: 73
Total length of run queues: 1
Total CPU usage (reductions): 1087991
Allocated memory (MiB): 36

# docker exec eturnal eturnalctl credentials
Username: 1658645797
Password: VnIk+7J3E7smzET6DytxX00Q+XU=
```

Workflow commands during/after connecting to the service, e.g. with [icetest.info][4] or [trickle ice test][5].

```shell
# docker logs eturnal | egrep 'request|authentication|allocation'
2022-07-23 07:10:35.632957+00:00 [info] Responding to STUN request [UDP, session fez426jli6f3, anonymous, client 192.0.2.44:11388]
2022-07-23 07:10:35.673616+00:00 [info] Responding to STUN request [UDP, session o3y9sx0fb4g4, anonymous, client 192.0.2.44:1836]
2022-07-23 07:10:35.683408+00:00 [info] Accepting long-term STUN/TURN authentication [UDP, session mszi8d37hm67, user 1658648497, client 192.0.2.44:11388]
2022-07-23 07:10:35.683830+00:00 [notice] Creating TURN allocation (lifetime: 600 seconds) [UDP, session mszi8d37hm67, user 1658648497, client 192.0.2.44:11388, relay 203.0.113.4:50029]
2022-07-23 07:10:35.724713+00:00 [info] Accepting long-term STUN/TURN authentication [UDP, session 2jnlheam2zrh, user 1658648497, client 192.0.2.44:1836]
2022-07-23 07:10:35.725302+00:00 [notice] Creating TURN allocation (lifetime: 599 seconds) [UDP, session 2jnlheam2zrh, user 1658648497, client 192.0.2.44:1836, relay 203.0.113.4:50030]

# docker exec eturnal eturnalctl sessions
2 active TURN sessions:

-- TURN session of 1658648497 --
          Client: 192.0.2.44:11388 (UDP)
           Relay: 203.0.113.4:50029 (UDP)
   Permission(s): none
         Peer(s): none
            Sent: 0 KiB (0 packets)
        Received: 0 KiB (0 packets)
     Running for: 5 seconds

-- TURN session of 1658648497 --
          Client: 192.0.2.44:1836 (UDP)
           Relay: 203.0.113.4:50030 (UDP)
   Permission(s): none
         Peer(s): none
            Sent: 0 KiB (0 packets)
        Received: 0 KiB (0 packets)
     Running for: 5 seconds
```

Stop the STUN/TURN service and remove the docker image.

```shell
# docker stop eturnal
eturnal

# docker rmi ghcr.io/processone/eturnal:latest
Untagged: ghcr.io/processone/eturnal:latest
Untagged: ghcr.io/processone/eturnal@sha256:9ace89295692502120a065ef92546eddee952c46128dd9d06a2ab3e7f513aa97
Deleted: sha256:5859fbc1ff0afa881f64abef2bb2ec63749fc6a5974fdba2e6c23c8b1596a3e8
Deleted: sha256:734fe7cad7d71f2862e4b3ffd84456e1247970aa76fbdca16e9f3cc0fc9d3b4b
Deleted: sha256:7963eafc331350c84d8631682cbe79da6f1108c5c067b60a4191582ecc132ba5
```

[1]: https://eturnal.net/doc/#relay_min_port
[2]: https://eturnal.net/doc/#relay_max_port
[3]: https://eturnal.net/doc/#listen
[4]: https://icetest.info/
[5]: https://webrtc.github.io/samples/src/content/peerconnection/trickle-ice/
