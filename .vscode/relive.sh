[ ! -f Makefile ] \
    && ./autogen.sh \
    && ./configure --with-rebar=./rebar3 \
    && make deps

make relive
