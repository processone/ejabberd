echo -n "===> Preparing dev configuration files: "

PWD=`pwd`
REL_DIR=$PWD/_build/dev/rel/
CON_DIR=$REL_DIR/ejabberd/etc/ejabberd/
BIN_DIR=$REL_DIR/ejabberd/bin/
CTLPATH=$BIN_DIR/ejabberdctl

[ ! -f "ejabberdctl" ] \
    && echo -n "ejabberdctl " \
    && ln -s $CTLPATH ejabberdctl

(cd $BIN_DIR && sed -i "s|^SCRIPT_DIR=.*|SCRIPT_DIR=`pwd`|g" ejabberdctl)

cd $CON_DIR

[ ! -f "ejabberd.yml" ] \
    && echo -n "ejabberd.yml " \
    && mv ejabberd.yml.example ejabberd.yml \
    && sed -i "s|# certfiles:|certfiles:\n  - $CON_DIR/cert.pem|g" ejabberd.yml \
    && sed -i "s|certfiles:|ca_file: $CON_DIR/ca.pem\ncertfiles:|g" ejabberd.yml \
    && sed -i 's|^acl:$|acl:\n  admin: [user: admin]|g' ejabberd.yml \
    || rm ejabberd.yml.example

[ ! -f "ejabberdctl.cfg" ] \
    && echo -n "ejabberdctl.cfg " \
    && mv ejabberdctl.cfg.example ejabberdctl.cfg \
    && sed -i "s|#' POLL|EJABBERD_BYPASS_WARNINGS=true\n\n#' POLL|g" ejabberdctl.cfg \
    || rm ejabberdctl.cfg.example

echo ""
echo "===> Now you can start this ejabberd dev with: ./ejabberdctl live"
