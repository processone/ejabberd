#!/bin/bash

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <install_dir>"
    exit 1
fi
install_dir="$1"

mkdir -p "$install_dir/jquery"
curl -s -o "$install_dir/jquery/jquery.min.js" https://code.jquery.com/jquery-3.7.1.min.js

curl -L -s -o /tmp/bootstrap4.zip https://github.com/twbs/bootstrap/releases/download/v4.6.2/bootstrap-4.6.2-dist.zip
unzip -d "$install_dir" /tmp/bootstrap4.zip
mv "$install_dir/bootstrap-4.6.2-dist" "$install_dir/bootstrap4"
rm /tmp/bootstrap4.zip
echo "landing page dependencies for mod_invites installed to $install_dir"
