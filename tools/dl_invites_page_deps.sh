#!/bin/bash

set -e

jquery_checksum='39a546ea9ad97f8bfaf5d3e0e8f8556adb415e470e59007ada9759dce472adaa';
bootstrap_checksum='3258c873cbcb1e2d81f4374afea2ea6437d9eee9077041073fd81dd579c5ba6b';

check() {
  echo "$1 $2" | sha256sum -c - || (echo "checksum failed: $2 (does not match $1)"; exit 1)
}

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <install_dir>"
    exit 1
fi
install_dir="$1"

mkdir -p "$install_dir/jquery"
jquery="$(mktemp /tmp/jquery.XXXXXXXXX)"
curl -s -o $jquery https://code.jquery.com/jquery-4.0.0.min.js
check $jquery_checksum $jquery
mv $jquery "$install_dir/jquery/jquery.min.js"

bootstrap="$(mktemp /tmp/bootstrap.XXXXXXXXX)"
curl -L -s -o $bootstrap https://github.com/twbs/bootstrap/releases/download/v5.3.8/bootstrap-5.3.8-dist.zip
check $bootstrap_checksum $bootstrap

rm -rf "$install_dir/bootstrap"
unzip -q -d "$install_dir" $bootstrap
mv "$install_dir/bootstrap-5.3.8-dist" "$install_dir/bootstrap"
rm $bootstrap
echo "landing page dependencies for mod_invites installed to $install_dir"
