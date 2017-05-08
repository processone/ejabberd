#!/bin/bash

erl -pa ebin deps/jiffy/ebin deps/fast_tls/ebin -noshell -s mod_acme scenario -s erlang halt