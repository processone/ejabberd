#!/bin/bash

erl -pa ebin \
deps/jiffy/ebin \
deps/fast_tls/ebin \
deps/jose/ebin \
deps/base64url/ebin \
deps/xmpp/ebin \
-noshell -s acme_experimental scenario -s erlang halt