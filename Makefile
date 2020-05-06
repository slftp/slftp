SHELL = /bin/bash
SLFTPPATH = ~/slftp
CC = fpc
CFLAGS = -MDelphi -O3 -Xs
CINCLUDES = -Fuirccommands -Furules -Fulibs/FastMM4 -Fulibs/BeRoHighResolutionTimer -Fulibs/FLRE -Fulibs/rcmdline -Fulibs/lkJSON -Fulibs/TRegExpr -Fulibs/pasmp -Fulibs/Indy10/* -Fulibs/LibTar -Fulibs/mORMot -Fulibs/mORMot/* -Fulibs/ZeosLib/*
CTESTINCLUDES = -dUNITTESTING -Futests/* -Futests/fptest/*
CDBFLAGS = -dDEBUG -MDelphi -gl -gp -gs -gw3
# flag for heaptrace output
# see http://wiki.freepascal.org/heaptrc & http://wiki.freepascal.org/leakview
HEAPTRACE = -gh
# flag for valgrind
# see http://wiki.lazarus.freepascal.org/Profiling#Using_Valgrind.2FCallgrind
VALGRIND = -gv

default: clean slftp

debug: clean slftp_debug
heaptrace: clean slftp_debug_heaptrace
valgrind: clean slftp_debug_valgrind

all: slftp install

all_32: slftp_32 install

all_64: slftp_64 install

slftp:
	make clean
	$(call revpatch)
	$(CC) $(CFLAGS) $(CINCLUDES) slftp.lpr
	$(call revpatchrevert)

slftp_32:
	make clean
	$(call revpatch)
	$(CC) -Pi386 $(CFLAGS) $(CINCLUDES) slftp.lpr
	$(call revpatchrevert)

slftp_64:
	make clean
	$(call revpatch)
	$(CC) -Px86_64 $(CFLAGS) $(CINCLUDES) slftp.lpr
	$(call revpatchrevert)

slftp_debug:
	$(call revpatch)
	$(CC) $(CDBFLAGS) $(CINCLUDES) slftp.lpr
	$(call revpatchrevert)

slftp_32_debug:
	$(call revpatch)
	$(CC) -Pi386 $(CDBFLAGS) $(CINCLUDES) slftp.lpr
	$(call revpatchrevert)

slftp_64_debug:
	$(call revpatch)
	$(CC) -Px86_64 $(CDBFLAGS) $(CINCLUDES) slftp.lpr
	$(call revpatchrevert)

slftp_debug_heaptrace:
	$(call revpatch)
	$(CC) $(CDBFLAGS) $(HEAPTRACE) $(CINCLUDES) slftp.lpr
	$(call revpatchrevert)

slftp_debug_valgrind:
	$(call revpatch)
	$(CC) $(CDBFLAGS) $(VALGRIND) $(CINCLUDES) slftp.lpr
	$(call revpatchrevert)

test:
	@make cleanuptestdir
	$(CC) $(CFLAGS) $(CINCLUDES) $(CTESTINCLUDES) tests/slftpUnitTests.lpr
	./tests/slftpUnitTests
	@make cleanuptestdir

clean:
	@find . -name "*.ppu" -type f -delete
	@find . \( -path "./libs/mORMot/static" -o -wholename "./libs/mORMot/SQLite3/sqlite3.o" -o -wholename "./libs/mORMot/padlock*" -o -wholename "./libs/mORMot/SynEcc64O2.o" \) -prune -o -name "*.o" -type f -exec rm {} +
	@rm -f slftp *.exe
	@make cleanuptestdir

cleanuptestdir:
	@find tests -name "*.ppu" -type f -delete
	@find tests -name "*.o" -type f -delete
	@rm -f tests/*.ppu tests/*.o tests/slftpUnitTests tests/*.exe

install:
	@cp slftp $(SLFTPPATH)/slftp

# patch used HEAD git-hash into slftp.inc
define revpatch
	@if [ -d ".git" ]; then \
        GIT_COMMIT=$(shell git rev-parse --short HEAD) ;\
		echo "patching SL_REV entry to $$GIT_COMMIT" ;\
		sed -i "s/SL_REV: string.*/SL_REV: string = '$$GIT_COMMIT';/g" slftp.inc ;\
    fi
endef

# restore default blank value of slftp.inc
define revpatchrevert
	@if [ -d ".git" ]; then \
        sed -i "s/SL_REV: string.*/SL_REV: string = '';/g" slftp.inc ;\
    fi
endef
