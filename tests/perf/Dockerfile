# The benchmark and both Pascal builds use the project's Linux CI toolchain.
ARG CI_IMAGE=registry.gitlab.com/slftp/ci-images/ubuntu-lts
FROM docker:27.5.1-cli AS docker
FROM ${CI_IMAGE}
COPY --from=docker /usr/local/bin/docker /usr/local/bin/docker
RUN apt-get update && apt-get install -y --no-install-recommends \
    tmux curl git python3 iproute2 libsqlite3-0 ca-certificates \
 && rm -rf /var/lib/apt/lists/*
WORKDIR /work
