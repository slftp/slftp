# glftpd with pzs-ng for the glftpd-e2e skill. e2e.sh puts the extracted glftpd tarball (glftpd/) and
# the pzs-ng sources (pzs-ng/) next to this file before it builds the image.

FROM ubuntu:24.04 AS base
# inherited by all stages, lets e2e.sh remove the build stages and images of older inputs
LABEL slftp-e2e=1
RUN apt-get update && apt-get install -y --no-install-recommends socat openssl \
 && rm -rf /var/lib/apt/lists/*
COPY glftpd/ /glftpd/

# pzs-ng is configured at compile time (zsconfig.h); make install only writes into /glftpd
FROM base AS pzs-ng
RUN apt-get update && apt-get install -y --no-install-recommends gcc make libc6-dev file
COPY pzs-ng/ /build/
COPY zsconfig.h /build/zipscript/conf/zsconfig.h
RUN cd /build && ./configure --enable-gl202-64 --disable-flac >/dev/null && make >/dev/null \
 && make install >/dev/null

FROM base
COPY --from=pzs-ng /glftpd/ /glftpd/
COPY glftpd.conf.add start.sh /
# the directories installgl.sh would create, the libraries the chrooted pzs-ng binaries need, a
# self-signed certificate, a user that may log in often and download without credits, and TLS
# required for logins, directory listings and data, as on most scene sites
RUN mkdir -p -m755 /glftpd/dev /glftpd/ftp-data/msgs /glftpd/ftp-data/logs /glftpd/tmp \
 && mkdir -p -m777 /glftpd/site \
 && chmod 755 /glftpd/ftp-data/users /glftpd/ftp-data/groups \
 && chmod 644 /glftpd/ftp-data/users/* /glftpd/ftp-data/groups/* \
 && /glftpd/libcopy.sh /glftpd >/dev/null \
 && openssl req -x509 -newkey ec -pkeyopt ec_paramgen_curve:secp384r1 -nodes -days 3650 -subj /CN=e2e \
      -keyout /tmp/k.pem -out /tmp/c.pem 2>/dev/null \
 && cat /tmp/k.pem /tmp/c.pem > /glftpd/ftpd-ecdsa.pem && rm /tmp/k.pem /tmp/c.pem \
 && chmod 600 /glftpd/ftpd-ecdsa.pem \
 && sed -i 's/^LOGINS .*/LOGINS 50 0 -1 -1/; s/^RATIO .*/RATIO 0 -1/' /glftpd/ftp-data/users/glftpd \
 && sed -i 's/^\(userrejectinsecure\|denydiruncrypted\|denydatauncrypted\)\s.*/\1\t*/' /glftpd/glftpd.conf \
 && cat /glftpd/glftpd.conf /glftpd.conf.add > /etc/glftpd.conf.base \
 && chmod +x /start.sh
CMD ["/start.sh"]
