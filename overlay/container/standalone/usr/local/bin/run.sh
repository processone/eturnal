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

exec eturnalctl foreground
