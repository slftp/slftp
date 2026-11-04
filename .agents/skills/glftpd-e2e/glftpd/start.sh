#!/bin/sh
# Container entry: glftpd on $PORT with the passive ports $PASV_PORTS (both set by e2e.sh). With
# nofork, socat execs glftpd with the accepted socket as stdin/stdout, as inetd does; otherwise glftpd
# would get a socketpair and could not see the peer address.
set -e
# device nodes inside the chroot, used by the pzs-ng binaries
for node in null:3 zero:5 urandom:9; do
  [ -e "/glftpd/dev/${node%:*}" ] || mknod -m 666 "/glftpd/dev/${node%:*}" c 1 "${node#*:}"
done
{ cat /etc/glftpd.conf.base; echo "pasv_ports $PASV_PORTS"; } > /etc/glftpd.conf
exec socat -d TCP-LISTEN:"$PORT",bind=127.0.0.1,reuseaddr,fork EXEC:"/glftpd/bin/glftpd -l -o -i -r /etc/glftpd.conf",nofork
