SHELL = /bin/bash
SLFTPPATH = ~/slftp
CC = fpc
CFLAGS = -MDelphi -O3 -Xs
CINCLUDES = -Fulibs/FastMM4 -Fulibs/BeRoHighResolutionTimer
CDBFLAGS = -MDelphi -gl -gp -gs -gw3
default: clean slftp

all: slftp install

all_32: slftp_32 install

all_64: slftp_64 insall
slftp:
	make clean
	$(CC) $(CFLAGS) $(CINCLUDES) slftp.lpr

slftp_32:
	make clean
	$(CC) -Pi386 $(CFLAGS) $(CINCLUDES) slftp.lpr

slftp_64:
	make clean
	$(CC) -Px86_64 $(CFLAGS) $(CINCLUDES) slftp.lpr

slftp_debug:
	$(CC) $(CDBFLAGS) $(CINCLUDES) slftp.lpr

slftp_32_debug:
	$(CC) -Pi386 $(CDBFLAGS) $(CINCLUDES) slftp.lpr

slftp_64_debug:
	$(CC) -Px86_64 $(CDBFLAGS) $(CINCLUDES) slftp.lpr

clean:
	@rm -f *.ppu *.o slftp *.exe

install:
	@cp slftp $(SLFTPPATH)/slftp
