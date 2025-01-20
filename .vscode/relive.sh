[ ! -f Makefile ] \
    && ./autogen.sh \
    && ./configure --with-rebar=rebar3 \
    && make

make relive
