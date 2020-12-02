#!/bin/sh

# Create ephemeral TURN credentials as per:
#
# https://tools.ietf.org/html/draft-uberti-behave-turn-rest-00

set -e
set -u

expiry="${TURN_EXPIRY:-tomorrow}"
secret="${TURN_SECRET:-}"
suffix="${TURN_USER:-}"

die()
{
	test $# -gt 0 && echo >&2 "$0: $@"
	echo >&2 "Usage: $0 [-e expiry] [-s secret] [-u user]"
	exit 2
}

while getopts e:u:s: opt
do
	case $opt in
	e) expiry=$OPTARG;;
	s) secret=$OPTARG;;
	u) suffix=$OPTARG;;
	\?) die;;
	esac
done
shift $((OPTIND - 1))
test -n "$secret" || die 'No shared secret specified'

username="$(date '+%s' -d "$expiry")${suffix:+:$suffix}"
password="$(printf '%s' "$username" | openssl sha1 -binary -hmac "$secret" | base64)"

echo "Username: $username"
echo "Password: $password"
