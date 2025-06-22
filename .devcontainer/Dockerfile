# CodeSandbox supports debian & ubuntu based images
FROM mcr.microsoft.com/devcontainers/base:ubuntu-22.04

# Example; install default
RUN apt update -y 
RUN apt install -y \
        libexpat1-dev \ 
        libgd-dev \
        libpam0g-dev \
        libsqlite3-dev \
        libwebp-dev \
        libyaml-dev \
        autoconf \
        automake \
        erlang \
        elixir \
        rebar3
# The project will be mounted in /workspace by default

# Every new shell will open inside the built container

# On top of this build, we install `zsh`, `git` and `docker` as additional
# Docker layers.
