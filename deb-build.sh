#!/bin/bash
#
# You need to have installed rubygems, fpm[0] gem (gem install fpm) and build-essential
# [0] https://github.com/jordansissel/fpm/wiki

USER=ejabberd
GROUP=ejabberd

INSTDIR=$(pwd)/installdir
FPM=$(gem which fpm | sed 's/\/lib\/fpm.rb/\/bin\/fpm/g')
TAG=$(git describe --always --tag)

if [ ! -z "$1" ]; then
    TAG="$1"
fi

#check if gem and fpm are installed
echo "You must have rubygems, fpm, and build-essential installed..."

gem list --local | grep fpm

if [[ $? -ne 0 ]]; then
    echo "Please verify the output of: gem list --local | grep fpm , remember you need tubygems and fpm installed"
    exit 1
fi

#clean compile and make the package
rm -rf $INSTDIR
./autogen.sh
./configure --prefix=$INSTDIR
make clean
PREFIX=$INSTDIR make 

if [[ $? -ne 0 ]]; then
    echo "Please check dependencies, compelation went wrong"
    exit 1
fi

PREFIX=$INSTDIR make install

#build the package
cd $INSTDIR
# Debian has issues with package version numbers NOT starting with a number. Below I prepend a version number to TAG
$FPM -s dir -t deb -n ejabberd -v 0-${TAG} --description "Erlang XMPP Server" -p ejabberd-VERSION_ARCH.deb --prefix / --deb-user $USER --deb-group $GROUP --license gpl2 --url http://www.ejabberd.im --vendor process-one .
