# UnrealIRCd with a test client for the unrealircd skill. ircd.sh puts the extracted sources
# (unrealircd/) next to this file before it builds the image.

FROM ubuntu:24.04 AS build
# inherited by all stages, lets ircd.sh remove the build stages and images of older inputs
LABEL slftp-unrealircd=1
RUN apt-get update && apt-get install -y --no-install-recommends gcc libc6-dev make pkg-config libssl-dev openssl file \
 && useradd -m ircd
COPY --chown=ircd:ircd unrealircd/ /build/
USER ircd
# Config asks questions, the empty answers take the defaults. The bundled libraries keep the final
# image small, and Config generates the TLS certificate. UnrealIRCd refuses to run as root. The output of
# make shows that the long compile advances.
RUN cd /build && yes '' | ./Config -nointro >/dev/null && make -j"$(nproc)" && make install >/dev/null

FROM ubuntu:24.04
LABEL slftp-unrealircd=1
RUN apt-get update && apt-get install -y --no-install-recommends libssl3t64 python3 python3-cryptography \
 && rm -rf /var/lib/apt/lists/* && useradd -m ircd
COPY --from=build --chown=ircd:ircd /home/ircd/unrealircd/ /home/ircd/unrealircd/
COPY --chown=ircd:ircd unrealircd.conf /home/ircd/unrealircd/conf/unrealircd.conf
COPY ircclient.py /ircclient.py
USER ircd
# -F stays in the foreground, so the container lives as long as the server
CMD ["/home/ircd/unrealircd/bin/unrealircd", "-F"]
