#!/bin/bash

set -e

jquery_checksum='fc9a93dd241f6b045cbff0481cf4e1901becd0e12fb45166a8f17f95823f0b1a';
bootstrap4_checksum='dc9b29fe7100e69d1a512860497bd2237eadccde6e813e588416429359832dce';

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
curl -s -o $jquery https://code.jquery.com/jquery-3.7.1.min.js
check $jquery_checksum $jquery
mv $jquery "$install_dir/jquery/jquery.min.js"

bootstrap4="$(mktemp /tmp/bootstrap4.XXXXXXXXX)"
curl -L -s -o $bootstrap4 https://github.com/twbs/bootstrap/releases/download/v4.6.2/bootstrap-4.6.2-dist.zip
check $bootstrap4_checksum $bootstrap4

unzip -q -d "$install_dir" $bootstrap4
mv "$install_dir/bootstrap-4.6.2-dist" "$install_dir/bootstrap4"
rm $bootstrap4
echo "landing page dependencies for mod_invites installed to $install_dir"
