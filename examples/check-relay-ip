#!/bin/sh

# Check our external IP address and if it changed, update the eturnal
# configuration and reload the server.

set -e
set -u

eturnal_yml='/etc/eturnal.yml'
url='https://ifconfig.co/ip'
old_ipv4_addr=$(sed -n 's/^[[:blank:]]\{1,\}relay_ipv4_addr:[[:blank:]]*"\?\([[:digit:].]*\)"\?.*$/\1/p' "$eturnal_yml")
old_ipv6_addr=$(sed -n 's/^[[:blank:]]\{1,\}relay_ipv6_addr:[[:blank:]]*"\?\([[:xdigit:]:]*\)"\?.*$/\1/p' "$eturnal_yml")
new_ipv4_addr=$(curl -fsS -4 "$url" || :)
new_ipv6_addr=$(curl -fsS -6 "$url" || :)
modified_config='false'
myself=${0##*/}

die()
{
	echo >&2 "$myself: $*"
	exit 1
}

test -n "$old_ipv4_addr" || die "Found no 'relay_ipv4_addr' in $eturnal_yml"
test -n "$new_ipv4_addr" || die "Cannot retrieve my IPv4 address from $url"

if [ "$old_ipv4_addr" != "$new_ipv4_addr" ]
then
	expr "$new_ipv4_addr" : \
	     '\([[:digit:]]\{1,3\}\.\)\{3\}[[:digit:]]\{1,3\}$' >'/dev/null' ||
	    die "Not a valid IPv4 address: $new_ipv4_addr"
	sed -i "s/$old_ipv4_addr/$new_ipv4_addr/" "$eturnal_yml"
	modified_config='true'
fi

if [ -n "$old_ipv6_addr" ]
then
	test -n "$new_ipv6_addr" || die "Cannot retrieve my IPv6 address from $url"

	if [ "$old_ipv6_addr" != "$new_ipv6_addr" ]
	then
		expr "$new_ipv6_addr" : \
		     '\([[:xdigit:]]\{1,4\}:\)\{7\}[[:xdigit:]]\{1,4\}$' >'/dev/null' ||
		    die "Not a valid IPv6 address: $new_ipv6_addr"
		sed -i "s/$old_ipv6_addr/$new_ipv6_addr/" "$eturnal_yml"
		modified_config='true'
	fi
fi

test "$modified_config" = 'false' || exec eturnalctl reload
