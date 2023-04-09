#!/bin/bash

# Determine package manager (apt, dnf, or yum)
if command -v apt >/dev/null 2>&1; then
    PKG_MANAGER="apt"
elif command -v dnf >/dev/null 2>&1; then
    PKG_MANAGER="dnf"
elif command -v yum >/dev/null 2>&1; then
    PKG_MANAGER="yum"
else
    echo "No compatible package manager found (apt, dnf, or yum)."
    exit 1
fi

# Download the install script based on the package manager
INSTALL_SCRIPT_URL="https://eturnal.net/eturnal/install/${PKG_MANAGER}.sh"
INSTALL_SCRIPT_FILE="/tmp/eturnal_install_${PKG_MANAGER}.sh"

curl -o "$INSTALL_SCRIPT_FILE" "$INSTALL_SCRIPT_URL"

if [ ! -f "$INSTALL_SCRIPT_FILE" ]; then
    echo "Failed to download the install script from ${INSTALL_SCRIPT_URL}"
    exit 1
fi

# Make the downloaded script executable
chmod +x "$INSTALL_SCRIPT_FILE"

# Execute the downloaded script with the original arguments
./"$INSTALL_SCRIPT_FILE" "$@"
