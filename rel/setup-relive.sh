#!/bin/sh

PWD_DIR=$(pwd)
TARGET=$1
REL_DIR=$PWD_DIR/_build/$TARGET/
CON_DIR=$REL_DIR/conf/

[ -z "$REL_DIR_TEMP" ] && REL_DIR_TEMP=$REL_DIR
CON_DIR_TEMP=$REL_DIR_TEMP/conf/

copy_ctl_cfg ()
{
    cp ejabberdctl.cfg.example $CON_DIR/ejabberdctl.cfg.example
}

prepare_ctl_cfg ()
{
    sed -i "s|#' POLL|EJABBERD_BYPASS_WARNINGS=true\n\n#' POLL|g" ejabberdctl.cfg.example
    [ ! -f "$CON_DIR/ejabberdctl.cfg" ] \
        && printf "ejabberdctl.cfg " \
        && mv ejabberdctl.cfg.example ejabberdctl.cfg \
        || printf
}

make ejabberdctl.$TARGET
chmod +x ejabberdctl.$TARGET
mv ejabberdctl.$TARGET $REL_DIR/ejabberdctl

cp inetrc $CON_DIR/
[ "$TARGET" = "relivectl" ] && copy_ctl_cfg
cp ejabberd.yml.example $CON_DIR/ejabberd.yml.example
cp test/ejabberd_SUITE_data/ca.pem $CON_DIR
cp test/ejabberd_SUITE_data/cert.pem $CON_DIR

cd $CON_DIR_TEMP || exit

sed -i "s|# certfiles:|certfiles:\n  - $CON_DIR/cert.pem|g" ejabberd.yml.example
sed -i "s|certfiles:|ca_file: $CON_DIR/ca.pem\ncertfiles:|g" ejabberd.yml.example
sed -i 's|^acl:$|acl:\n  admin: [user: admin]|g' ejabberd.yml.example
[ ! -f "$CON_DIR/ejabberd.yml" ] \
    && printf "ejabberd.yml " \
    && mv ejabberd.yml.example ejabberd.yml

[ "$TARGET" = "relivectl" ] && prepare_ctl_cfg
