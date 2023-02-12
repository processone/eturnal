#!/bin/sh

set -e
set -u

myself=${0##*/}

info()
{
	echo "$myself: $*"
}

error()
{
	echo >&2 "$myself: $*"
}

info 'Start init script ...'
info "Container runs eturnal vsn $(eturnalctl version | awk '{{print $1}}') ..."
info 'Documentation on https://eturnal.net ...'
info 'Source code on https://github.com/processone/eturnal ...'

info 'Read secrets defined as "Docker secrets" - if any ...'
docker_secrets="$HOME/.docker-secrets"
for i in $(env | grep '__FILE')
do
        var_name="$(echo "$i" | sed -e 's|__FILE=| |' | awk '{print $1}')"
        var_file="$(echo "$i" | sed -e 's|__FILE=| |' | awk '{print $2}')"
        echo "$var_name=$(cat $var_file)" >> "$docker_secrets"
done

if [ -f "$docker_secrets" ]
then
        set -a
        source "$docker_secrets"
        set +a
        rm "$docker_secrets"
fi

s6_cron='/etc/s6.d/cron/run'
acme_sh="$(find $HOME -type f -executable -name acme.sh)"

info 'Apply environment variables settings ...'
sed -i -e "s|<ACME_CRON_PERIOD>|${ACME_CRON_PERIOD:-60d}|g" "$s6_cron" \
       -e "s|<ACME_SH>|$acme_sh|g" "$s6_cron"

info 'Finalize minimal configuration file ...'
## enable `mod_stats_prometheus`
if [ ${MOD_STATS_PROMETHEUS_ENABLE:-false} = 'true' ]
then cat >> $HOME/etc/eturnal.yml <<-EOF
    mod_stats_prometheus:
      ip: ${MOD_STATS_PROMETHEUS_IP:-any}
      port: ${MOD_PROMETHEUS_PORT:-8081}
      tls: ${MOD_PROMETHEUS_TLS:-false}
      vm_metrics: ${MOD_PROMETHEUS_VM_METRICS:-true}

EOF
fi

## adjust default listener ports and log level
cat >> $HOME/etc/eturnal.yml <<-EOF
  listen:
    -
      ip: "::"
      port: ${LISTEN_UDP_PORT:-3478}
      transport: udp
    -
      ip: "::"
      port: ${LISTEN_TCP_TLS_PORT:-3478}
      transport: auto

  log_level: ${LOG_LEVEL:-info}

  strict_expiry: ${CREDENTIALS_STRICT_EXPIRY:-false}

EOF

## realm: https://eturnal.net/documentation/#realm
if [ ! -z ${REALM-} ]
then cat >> $HOME/etc/eturnal.yml <<-EOF
  realm: ${REALM-}

EOF
fi

## include TLS certificate paths into configuration yml
cat >> $HOME/etc/eturnal.yml <<-EOF
  tls_crt_file: $(find $HOME -name fullchain.pem)
  tls_key_file: $(find $HOME -name key.pem)
EOF

info 'Start main processes ...'
exec /bin/s6-svscan /etc/s6.d
