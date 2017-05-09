#!/bin/bash

erl -pa ebin \
deps/jiffy/ebin \
deps/fast_tls/ebin \
deps/jose/ebin \
deps/base64url/ebin \
-noshell -s mod_acme scenario -s erlang halt