# [eturnal](https://eturnal.net) container image with `acme.sh`

This variant includes the `acme.sh` [script](https://github.com/acmesh-official/acme.sh).
It works like the [standalone eturnal container image](https://github.com/processone/eturnal/tree/master/doc/CONTAINER.md),
but contains a cron job for creating/renewing TLS certificates.

These images have a tag suffix `-acme`.

## Who could profit from this variant?

This image is suited for those, who would like to have a convenient way to
manage TLS certificates and infusing them to eturnal without e.g. an external
cron job on the host machine.

## Configuration

The `acme.sh` script within this image can be customized with the below listed
environment variables.

eturnal may be configured with the environment variables mentioned below as well
or by simply mounting an `eturnal.yml` configuration file (recommended).

All [usage and configuration aspects](https://github.com/processone/eturnal/tree/master/doc/CONTAINER.md)
from the standalone variant apply to this version.

### ACME options

| Name  | Description  |  Default value | Additional notes  |
| ------------ | ------------ | ------------ | ------------ |
| `ACME_EMAIL`  | a valid email address  | `admin@example.com` |   |
| `ACME_DOMAIN`  | domain(s) for the issued certificates  | `turn.example.com` | for multiple domains, separate them with a comma: `ACME_DOMAIN="turn.example.com,turn2.example.com,turn3.example.com"`  |
| `ACME_KEY_SIZE`  | [key lengths](https://github.com/acmesh-official/acme.sh#10-issue-ecc-certificates)  | `4096` |   |
| `ACME_SH_UPGRADE`  | defines, whether the cron job also upgrades `acme.sh`  | `true` |  |
| `ACME_CA`  | defines the [CA](https://github.com/acmesh-official/acme.sh/wiki/CA)  | `zerossl` |   |
| `ACME_CRON_PERIOD`  | defines renewal interval  | `60d` |   |
| `ACME_CHALLENGE`  | either `http` (default), `https` or `dns`. | `http` | When using `http` or `https` it must not interfere with the `LISTEN_TCP_TLS_PORT` (default: `3478`) |
| `DNS_PROVIDER`  | only needed if `ACME_CHALLENGE=dns`, specifies the [DNS service](https://github.com/acmesh-official/acme.sh/wiki/dnsapi) to be used, e.g. `DNS_PROVIDER=dns_cf`  |  | the respective API keys, token, etc. must be defined as environment variables in the `docker run` cmd, e.g. `-e CF_Token="sdfsdfsdfljlbjkljlkjsdfoiwje" -e CF_Account_ID="xxxxxxxxxxxxx"`  |

### eturnal related configurations

#### Using a custom `eturnal.yml` configuration file

Just mount your `eturnal.yml` configuration file into the running container at
the following path:

    -v /path/to/eturnal.yml:/etc/eturnal.yml

Values specified in the `eturnal.yml` file prevail `ETURNAL_*` environment
variables (see variables below).

**Hint:** If you use a custom `eturnal.yml` configuration file, [tls_crt_file](https://eturnal.net/documentation/#tls_crt_file)
and [tls_key_file](https://eturnal.net/documentation/#tls_key_file) must be set
to:

```yaml
  tls_crt_file: /opt/eturnal/tls/fullchain.pem
  tls_key_file: /opt/eturnal/tls/key.pem
```

The following variables can be omitted, if a custom `eturnal.yml` configuration
file is mounted into the container.

#### Listener options

| Name  | Description  |  Default value | Additional notes  |
| ------------ | ------------ | ------------ | ------------ |
| `LISTEN_UDP_PORT`  | Defines the UDP listener [here](https://eturnal.net/documentation/#listen)  | `3478` |  |
| `LISTEN_TCP_TLS_PORT`  | Defines the multiplex TCP/TLS listener [here](https://eturnal.net/documentation/#listen)  | `3478` | This may be used for port `443` (https) |
| `ETURNAL_RELAY_IPV4_ADDR`  | More infos [here](https://eturnal.net/documentation/#relay_ipv4_addr)  |  | no default, auto-detected if possible |
| `ETURNAL_RELAY_IPV6_ADDR`  | More infos [here](https://eturnal.net/documentation/#relay_ipv6_addr)  |  | no default, auto-detected if possible |
| `ETURNAL_RELAY_MAX_PORT`  | More infos [here](https://eturnal.net/documentation/#relay_max_port)  | `65535` |  |
| `ETURNAL_RELAY_MIN_PORT`  | More infos [here](https://eturnal.net/documentation/#relay_min_port)  | `49152` |  |
| `ETURNAL_SECRET`  | More infos [here](https://eturnal.net/documentation/#secret)  |  | no default, auto-generated |

#### module `mod_stats_prometheus`

| Name  | Description  |  Default value | Additional notes  |
| ------------ | ------------ | ------------ | ------------ |
| `MOD_STATS_PROMETHEUS_ENABLE`  | enable (`true`) or disable the module  | `false` |  |
| `MOD_STATS_PROMETHEUS_IP`  | More infos [here](https://eturnal.net/documentation/#mod_stats_prometheus)  | `any` |  |
| `MOD_PROMETHEUS_PORT`  | see above  | `8081` |  |
| `MOD_PROMETHEUS_TLS`  | see above  | `false` |  |
| `MOD_PROMETHEUS_VM_METRICS`  | see above  | `true` |  |

#### additional configuration options

| Name  | Description  |  Default value | Additional notes  |
| ------------ | ------------ | ------------ | ------------ |
| `LOG_LEVEL`  | Sets [log level](https://eturnal.net/documentation/#log_level)  | `info` |  |
| `CREDENTIALS_STRICT_EXPIRY`  | More infos [here](https://eturnal.net/documentation/#strict_expiry)  | `false` |  |
| `STUN_SERVICE`  | External IP address lookup, more infos [here](https://github.com/processone/eturnal/tree/master/doc/CONTAINER.md#general-hints))  | `stun.conversations.im 3478` | Set to `false` to disable, or us another STUN service |
| `REALM`  | This option defines the [realm](https://eturnal.net/documentation/#realm)  | | no default |

### Limitations

* No support for providing custom TLS certificates.

## Examples

The image works with `docker` or `podman`.

```shell
docker run -d --rm \
    --name eturnal \
    --read-only \
    --cap-drop=ALL \
    --cap-add=NET_BIND_SERVICE \
    -p 80:80/udp \
    -e LISTEN_UDP_PORT=80 \
    -p 443:443 \
    -e LISTEN_TCP_TLS_PORT=443 \
    -p 50000-50500:50000-50500/udp \
    -e ETURNAL_RELAY_MIN_PORT=50000 \
    -e ETURNAL_RELAY_MAX_PORT=50500 \
    -e ETURNAL_SECRET=super-secret-password \
    -p 80:80 \
    -e ACME_CHALLENGE=http \
    -e ACME_EMAIL=admin@example.com \
    -e ACME_DOMAIN=turn.example.com \
  ghcr.io/processone/eturnal:acme
```

Or consider using the host network instead ([more info here](https://github.com/processone/eturnal/tree/master/doc/CONTAINER.md#usage-with-docker-or-podman)):

```shell
docker run -d --rm \
    --name eturnal \
    --read-only \
    --cap-drop=ALL \
    --cap-add=NET_BIND_SERVICE \
    --network=host \
    -e LISTEN_UDP_PORT=80 \
    -e LISTEN_TCP_TLS_PORT=443 \
    -e ETURNAL_RELAY_MIN_PORT=50000 \
    -e ETURNAL_RELAY_MAX_PORT=50500 \
    -e ETURNAL_SECRET=super-secret-password \
    -e ACME_CHALLENGE=http \
    -e ACME_EMAIL=admin@example.com \
    -e ACME_DOMAIN=turn.example.com \
  ghcr.io/processone/eturnal:acme
```

And an example with a custom `eturnal.yml` configuration file and `dns`:

```shell
docker run -d --rm \
    --name eturnal \
    --read-only \
    --cap-drop=ALL \
    --cap-add=NET_BIND_SERVICE \
    --network=host \
    -e ACME_CHALLENGE=dns \
    -e DNS_PROVIDER="dns_cf" \
    -e CF_Token="sdfsdfsdfljlbjkljlkjsdfoiwje" \
    -e CF_Account_ID="xxxxxxxxxxxxx" \
    -e ACME_EMAIL=admin@example.com \
    -e ACME_DOMAIN=turn.example.com \
    -v /path/to/eturnal.yml:/etc/eturnal.yml \
  ghcr.io/processone/eturnal:acme
```
