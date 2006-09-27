#!/bin/sh

echo '1. fetch, compile, and install erlang'

if [ ! pkg_info erlang 1>/dev/null 2>&1 ]; then
    cd /usr/pkgsrc/lang/erlang
    make fetch-list|sh
    make
    make install
fi
if pkg_info erlang | grep -q erlang-9.1nb1; then
else
    echo "erlang-9.1nb1 not installed" 1>&2
    exit 1
fi


echo '2. install crypt_drv.so'
    
if [ ! -d    /usr/pkg/lib/erlang/lib/crypto-1.1.2.1/priv/lib ] ; then
    mkdir -p /usr/pkg/lib/erlang/lib/crypto-1.1.2.1/priv/lib
fi
if [ ! -f /usr/pkg/lib/erlang/lib/crypto-1.1.2.1/priv/lib/crypto_drv.so ]; then
    cp work/otp*/lib/crypto/priv/*/*/crypto_drv.so \
          /usr/pkg/lib/erlang/lib/crypto-1.1.2.1/priv/lib
fi


echo '3. compile and install elibcrypto.so'

if [ ! -f /usr/pkg/lib/erlang/lib/crypto-1.1.2.1/priv/lib/elibcrypto.so ]; then
cd /usr/pkgsrc/lang/erlang/work/otp_src_R9B-1/lib/crypto/c_src
ld -r -u CRYPTO_set_mem_functions -u MD5 -u MD5_Init -u MD5_Update \
   -u MD5_Final -u SHA1 -u SHA1_Init -u SHA1_Update -u SHA1_Final \
   -u des_set_key -u des_ncbc_encrypt -u des_ede3_cbc_encrypt \
   -L/usr/lib -lcrypto -o ../priv/obj/i386--netbsdelf/elibcrypto.o
cc -shared \
   -L/usr/pkgsrc/lang/erlang/work/otp_src_R9B-1/lib/erl_interface/obj/i386--netbsdelf \
    -o ../priv/obj/i386--netbsdelf/elibcrypto.so \
    ../priv/obj/i386--netbsdelf/elibcrypto.o -L/usr/lib -lcrypto
cp ../priv/obj/i386--netbsdelf/elibcrypto.so \
           /usr/pkg/lib/erlang/lib/crypto-1.1.2.1/priv/lib
fi
    

echo '4. compile and install ssl_esock'

if [ ! -f  /usr/pkg/lib/erlang/lib/ssl-2.3.5/priv/bin/ssl_esock ]; then
    cd     /usr/pkg/lib/erlang/lib/ssl-2.3.5/priv/obj/
    make
fi
    

echo '5. initial ejabberd configuration'
    
cd /usr/pkg/jabber/ejabberd/src
./configure


echo '6. edit ejabberd Makefiles'

for M in Makefile mod_*/Makefile; do
    if [ ! -f $M.orig ]; then
        mv $M $M.orig
        sed -e s%/usr/local%/usr/pkg%g < $M.orig > $M
    fi
done


echo '7. compile ejabberd'

gmake
for A in mod_irc mod_muc mod_pubsub; do
    (cd $A; gmake)
done


echo ''
echo 'now edit ejabberd.cfg'
echo ''
echo 'to start ejabberd: erl -sname ejabberd -s ejabberd'
