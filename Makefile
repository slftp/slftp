SLFTPPATH = ~/testsl/
SHELL = /bin/bash
CC = fpc
CFLAGS = -MDelphi
CDBFLAGS = -MDelphi -gl -gp -gs -gw3
default: clean slftp
all: clean slftp_svn_up slftp install
slftp:
	@rm -f *.ppu *.o slftp *.exe
	$(CC) $(CFLAGS) slftp.lpr
	@cp slftp $(SLFTPPATH)slftp
slftp_svn_up:
	@svn up
slftp_debug:
	$(CC) $(CDBFLAGS) slftp.lpr
cryptconf2:
	@rm -f *.ppu *.o cryptconf2 *.exe
	$(CC) $(CFLAGS) cryptconf2.dpr
	@cp cryptconf2 $(SLFTPPATH)cryptconf2
dbrepair:
	@rm -f *.ppu *.o dbrepair *.exe
	$(CC) $(CFLAGS) dbrepair.dpr
	@cp dbrepair $(SLFTPPATH)dbrepair
clean:
	@rm -f *.ppu *.o slftp cryprconf cryptconf2 dbrepair *.exe
install:
	@cp slftp $(SLFTPPATH)slftp
#	@cp cryptconf2 $(SLFTPPATH)cryptconf2
#	@cp dbrepair $(SLFTPPATH)dbrepair