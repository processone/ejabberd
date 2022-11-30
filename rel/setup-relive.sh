PWD_DIR=`pwd`
REL_DIR=$PWD_DIR/_build/relive/
CON_DIR=$REL_DIR/conf/

[ -z "$REL_DIR_TEMP" ] && REL_DIR_TEMP=$REL_DIR
CON_DIR_TEMP=$REL_DIR_TEMP/conf/

make ejabberdctl.relive
chmod +x ejabberdctl.relive
mv ejabberdctl.relive $REL_DIR/ejabberdctl

cp inetrc $CON_DIR/
cp ejabberdctl.cfg.example $CON_DIR/ejabberdctl.cfg.example
cp ejabberd.yml.example $CON_DIR/ejabberd.yml.example
cp test/ejabberd_SUITE_data/ca.pem $CON_DIR
cp test/ejabberd_SUITE_data/cert.pem $CON_DIR

cd $CON_DIR_TEMP

sed -i "s|# certfiles:|certfiles:\n  - $CON_DIR/cert.pem|g" ejabberd.yml.example
sed -i "s|certfiles:|ca_file: $CON_DIR/ca.pem\ncertfiles:|g" ejabberd.yml.example
sed -i 's|^acl:$|acl:\n  admin: [user: admin]|g' ejabberd.yml.example
[ ! -f "$CON_DIR/ejabberd.yml" ] \
    && echo -n "ejabberd.yml " \
    && mv ejabberd.yml.example ejabberd.yml

sed -i "s|#' POLL|EJABBERD_BYPASS_WARNINGS=true\n\n#' POLL|g" ejabberdctl.cfg.example
[ ! -f "$CON_DIR/ejabberdctl.cfg" ] \
    && echo -n "ejabberdctl.cfg " \
    && mv ejabberdctl.cfg.example ejabberdctl.cfg \
    || echo -n
