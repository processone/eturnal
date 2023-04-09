#!/bin/bash

CHALLENGES="http"

# Parse command-line arguments
while [[ $# -gt 0 ]]
do
key="$1"

case $key in
  -c|--preferred-challenges)
  CHALLENGES="$2"
  shift # past argument
  shift # past value
  ;;
  -d|--domain)
  DOMAIN="$2"
  shift # past argument
  shift # past value
  ;;
  *)
  # unknown option
  shift # past argument
  ;;
esac
done

# Verify if the user is root (superuser)
if [ "$(id -u)" != "0" ]; then
  echo "This script must be run as root or with sudo."
  exit 1;
fi

# Update package list and install Certbot
dnf update
dnf install -y certbot

# Enable external repository for eturnal and install package
sudo dnf config-manager --add-repo https://eturnal.net/eturnal.repo
sudo dnf install eturnal
sudo systemctl --now enable eturnal

# Stop eturnal service
sudo systemctl stop eturnal

# Clear console
clear

# Ask user to enter domain name if not provided
if [ -z "$DOMAIN" ]; then
  if [ "$#" -eq 0 ]; then
    read -rp "Please enter domain name: " DOMAIN
  else
    DOMAIN="$1"
  fi
fi

# Generate SSL certificate using Certbot and set the preferred challenge type
sudo certbot certonly --standalone -d "$DOMAIN" --preferred-challenges "$CHALLENGES" --test-cert --deploy-hook "sudo ETURNAL_SECRET=${ETURNAL_SECRET_KEY:-my-secret-key} eturnalctl reload"

# Verify if the /etc/letsencrypt/live/${DOMAIN} folder exists
if [ ! -d "/etc/letsencrypt/live/${DOMAIN}" ]; then
  echo "The folder /etc/letsencrypt/live/${DOMAIN} was not found. Please ensure that the SSL certificate was generated successfully."
  exit 1
fi

fullchain_path="/etc/eturnal/tls/fullchain.pem"
privkey_path="/etc/eturnal/tls/privkey.pem"

# Ensure that the folder exists
mkdir -p /etc/eturnal/tls

# Copy .pem files
cp "/etc/letsencrypt/live/${DOMAIN}/fullchain.pem" "$fullchain_path"
cp "/etc/letsencrypt/live/${DOMAIN}/privkey.pem" "$privkey_path"

# Change owner to 'eturnal'
chown eturnal "$fullchain_path"
chown eturnal "$privkey_path"

# Replace /etc/eturnal.yml content with the provided content
cat > /tmp/eturnal.yml << EOM
eturnal:

  realm: \$DOMAIN
  listen:
    -
      ip: "::"
      port: 80
      transport: udp
    -
      ip: "::"
      port: 80
      transport: tcp
    -
      ip: "::"
      port: 443
      transport: tls

  tls_crt_file: /etc/eturnal/tls/fullchain.pem
  tls_key_file: /etc/eturnal/tls/privkey.pem

  relay_min_port: 49152
  relay_max_port: 65535

  strict_expiry: true

  log_level: critical
  log_rotate_size: 10485760
  log_rotate_count: 10
EOM

sed "s/\$DOMAIN/${DOMAIN}/g" /tmp/eturnal.yml > /tmp/eturnal.yml.modified
mv /tmp/eturnal.yml.modified /etc/eturnal.yml

# Restart eturnal
sudo systemctl restart eturnal

echo "Process completed for domain: ${DOMAIN}"
