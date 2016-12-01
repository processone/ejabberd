#!/bin/sh
set -ex

export DEBIAN_FRONTEND="noninteractive"

readonly buildDeps='
    git-core
    build-essential
    automake
    libssl-dev
    zlib1g-dev
    libexpat-dev
    libyaml-dev
    libsqlite3-dev
    erlang-src erlang-dev'

readonly requiredAptPackages='
    locales
    ldnsutils
    python2.7
    python-jinja2
    ca-certificates
    libyaml-0-2
    erlang-base erlang-snmp erlang-ssl erlang-ssh erlang-webtool
    erlang-tools erlang-xmerl erlang-corba erlang-diameter erlang-eldap
    erlang-eunit erlang-ic erlang-odbc erlang-os-mon
    erlang-parsetools erlang-percept erlang-typer
    python-mysqldb
    imagemagick'

apt-key adv \
    --keyserver keys.gnupg.net \
    --recv-keys 434975BD900CCBE4F7EE1B1ED208507CA14F4FCA

apt-get update
apt-get install -y $buildDeps $requiredAptPackages --no-install-recommends
dpkg-reconfigure locales && locale-gen C.UTF-8
/usr/sbin/update-locale LANG=C.UTF-8
echo 'en_US.UTF-8 UTF-8' >> /etc/locale.gen
locale-gen

# add ejabberd user
useradd --home $EJABBERD_HOME -M --system ejabberd
mkdir $EJABBERD_HOME

cd /tmp/ejabberd
chmod +x ./autogen.sh
./autogen.sh
./configure --enable-user=ejabberd \
    --enable-all \
    --disable-tools \
    --disable-pam

make debug=$EJABBERD_DEBUG_MODE
make install

cd $EJABBERD_HOME
mkdir -p logs ssl backup upload module_source modules/conf
mv /tmp/ejabberd/docker $EJABBERD_HOME

# Move config to homedir
mv /etc/ejabberd conf
ln -s $EJABBERD_HOME/conf /etc/ejabberd

# rename original configs
mv conf/ejabberd.yml conf/ejabberd.yml.orig
mv conf/ejabberdctl.cfg conf/ejabberdctl.cfg.orig

# clean up
rm -rf /tmp/ejabberd
rm -rf /var/lib/apt/lists/*
apt-get purge -y --auto-remove $buildDeps

# change owner for ejabberd home
chown -R ejabberd $EJABBERD_HOME
