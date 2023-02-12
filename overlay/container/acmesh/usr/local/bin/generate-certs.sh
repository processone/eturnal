#!/bin/sh

email="${ACME_EMAIL:-admin@example.com}"
domain="${ACME_DOMAIN:-turn.example.com}"
cert_dir="$HOME/tls"
key_size="${ACME_KEY_SIZE:-4096}"
acme_sh="$HOME/.acme.sh/acme.sh"
acme_ca="${ACME_CA:-zerossl}"
acme_ca_chain="${ACME_CA_CHAIN:-isrg}"

create_tls_certs()
{
        local email="$1"
        local domain="$2"
        local cert_dir="$3"
        local key_size="$4"
        local dns_provider="${DNS_PROVIDER-}"

        echo ">> Register an account at $acme_ca ..."
        "$acme_sh" --register-account --server "$acme_ca" -m "$email"

        if [ "$acme_ca" = 'letsencrypt' ]
        then
                echo ">> Set preferred certificate chain to $acme_ca_chain ..."
                "$acme_sh" --set-default-chain \
                        --preferred-chain "$acme_ca_chain" \
                        --server "$acme_ca"
        fi

        if [ "$acme_challenge" = 'dns' ]
        then acme_sh=""$acme_sh" --issue --"$challenge" "$dns_provider""
        else acme_sh=""$acme_sh" --issue --"$challenge""
        fi

        if [ "${ACME_OCSP_MUST_STAPLE:-false}" = 'true' ]
        then acme_sh="$acme_sh --ocsp"
        fi

        echo ">> Create certificates for domain(s) $domain ..."
        $acme_sh \
                --keylength "$key_size" \
                --always-force-new-domain-key \
                -d $(echo "$domain" | sed 's/,/ -d /g') \
                --server "$acme_ca" \
                --key-file $cert_dir/key.pem \
                --ca-file $cert_dir/ca.pem \
                --cert-file $cert_dir/crt.pem \
                --fullchain-file $cert_dir/fullchain.pem \
                --reloadcmd 'eturnalctl reload'
}
#.

if [ ${ACME_SH_UPGRADE:-true} = 'true' ]
then
        echo '>> Upgrading acme.sh ...'
        "$acme_sh" --upgrade
fi

echo '>> Print current acme.sh version ...'
 "$acme_sh" --version

acme_challenge="${ACME_CHALLENGE:-http}"
if [ "$acme_challenge" = 'http' ]
then challenge='standalone'
elif [ "$acme_challenge" = 'https' ]
then challenge='alpn'
elif [ "$acme_challenge" = 'dns' ]
then challenge='dns'
fi

[ ! -d "$HOME/tls" ] && mkdir -p "$HOME/tls"

create_tls_certs "$email" "$domain" "$cert_dir"

echo ">> Cron period set to ${ACME_CRON_PERIOD:-60d} ..."
