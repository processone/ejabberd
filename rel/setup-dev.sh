echo -n "===> Preparing dev configuration files: "

PWD_DIR=`pwd`
REL_DIR=$PWD_DIR/_build/dev/rel/ejabberd/
CON_DIR=$REL_DIR/etc/ejabberd/
BIN_DIR=$REL_DIR/bin/
CTLPATH=$BIN_DIR/ejabberdctl

[ -z "$REL_DIR_TEMP" ] && REL_DIR_TEMP=$REL_DIR
CON_DIR_TEMP=$REL_DIR_TEMP/etc/ejabberd/
BIN_DIR_TEMP=$REL_DIR_TEMP/bin/

[ ! -e "ejabberdctl" ] \
    && echo -n "ejabberdctl " \
    && ln -s $CTLPATH ejabberdctl

# (cd $BIN_DIR_TEMP && sed -i "s|^SCRIPT_DIR=.*|SCRIPT_DIR=$REL_DIR|g" ejabberdctl)

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
    && mv ejabberdctl.cfg.example ejabberdctl.cfg

echo ""
echo "===> Now you can start this ejabberd dev with: ./ejabberdctl live"
