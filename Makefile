SHELL = bash
SLFTPPATH = ~/slftp
CC = fpc
CFLAGS = -MDelphi -O3 -Xs
CINCLUDES = -Fuirccommands -Furules -Fulibs/BeRoHighResolutionTimer -Fulibs/FLRE -Fulibs/rcmdline -Fulibs/lkJSON -Fulibs/TRegExpr -Fulibs/pasmp -Fulibs/Indy10/* -Fulibs/Indy10/Protocols -Fulibs/Indy10/Protocols/OpenSSL -Fulibs/Indy10/Protocols/OpenSSL/* -Fulibs/LibTar -Fulibs/mORMot2/src/core -Fulibs/mORMot2/src/lib -Fulibs/mORMot2/src/crypt -Fulibs/mORMot2/src/db -Fulibs/mORMot2/src/orm -Fulibs/mORMot2/src/rest -Fulibs/mORMot2/src/soa -Fulibs/ZeosLib/*
CTESTINCLUDES = -Futests/* -Futests/fptest/*
CDBFLAGS = -dDEBUG -MDelphi -gl -gp -gw3
# flag for heaptrace output
# see http://wiki.freepascal.org/heaptrc & http://wiki.freepascal.org/leakview
HEAPTRACE = -gh
# flag for valgrind
# see http://wiki.lazarus.freepascal.org/Profiling#Using_Valgrind.2FCallgrind
VALGRIND = -gv
GPROF = -pg
VTUNE = -dDEBUG -MDelphi -gl -gp -gw3 -O2

default: clean slftp

debug: clean slftp_debug
heaptrace: clean slftp_debug_heaptrace
valgrind: clean slftp_debug_valgrind
gprof: clean slftp_debug_gprof
vtune: clean slftp_debug_vtune

all: slftp install

all_32: slftp_32 install

all_64: slftp_64 install

slftp:	FORCE clean revpatch slftp_build revpatchrevert

slftp_build:
	$(CC) $(CFLAGS) $(CINCLUDES) slftp.lpr

slftp_32:	FORCE clean revpatch slftp_32_build revpatchrevert

slftp_32_build:
	$(CC) -Pi386 $(CFLAGS) $(CINCLUDES) slftp.lpr

slftp_64:	FORCE clean revpatch slftp_64_build revpatchrevert

slftp_64_build:
	$(CC) -Px86_64 $(CFLAGS) $(CINCLUDES) slftp.lpr

slftp_debug:	FORCE clean revpatch slftp_debug_build revpatchrevert

slftp_debug_build:
	$(CC) $(CDBFLAGS) $(CINCLUDES) slftp.lpr

slftp_32_debug:	FORCE clean revpatch slftp_32_debug_build revpatchrevert

slftp_32_debug_build:
	$(CC) -Pi386 $(CDBFLAGS) $(CINCLUDES) slftp.lpr

slftp_64_debug:	FORCE clean revpatch slftp_64_debug_build revpatchrevert

slftp_64_debug_build:
	$(CC) -Px86_64 $(CDBFLAGS) $(CINCLUDES) slftp.lpr

slftp_debug_heaptrace:	FORCE clean revpatch slftp_debug_heaptrace_build revpatchrevert

slftp_debug_heaptrace_build:
	$(CC) $(CDBFLAGS) $(HEAPTRACE) $(CINCLUDES) slftp.lpr

slftp_debug_valgrind:	FORCE clean revpatch slftp_debug_valgrind_build revpatchrevert

slftp_debug_valgrind_build:
	$(CC) $(CDBFLAGS) $(VALGRIND) $(CINCLUDES) slftp.lpr

slftp_debug_gprof:	FORCE clean revpatch slftp_debug_gprof_build revpatchrevert

slftp_debug_gprof_build:
	$(CC) $(CDBFLAGS) $(GPROF) $(CINCLUDES) slftp.lpr

slftp_debug_vtune:	FORCE clean revpatch slftp_debug_vtune_build revpatchrevert

slftp_debug_vtune_build:
	$(CC) $(VTUNE) $(CINCLUDES) slftp.lpr

test:	FORCE clean test_build_and_run cleanuptestdir

test_build_and_run:
	$(CC) $(CFLAGS) $(CINCLUDES) $(CTESTINCLUDES) tests/slftpUnitTests.lpr
	./tests/slftpUnitTests

clean:	cleanuptestdir
	@find . -name "*.ppu" -type f -delete
	@find . \( -path "./libs/mORMot2/static" \) -prune -o -name "*.o" -type f -exec rm {} +
	@rm -f slftp *.exe

cleanuptestdir:
	@find tests -name "*.ppu" -type f -delete
	@find tests -name "*.o" -type f -delete
	@rm -f tests/*.ppu tests/*.o tests/slftpUnitTests tests/*.exe
	@rm -f tests/*.res tests/*.or

install:
	@cp slftp $(SLFTPPATH)/slftp

# empty target to force execution
FORCE:

# patch used HEAD git-hash into slftp.inc
revpatch: FORCE
	@if [ -d ".git" ]; then \
        GIT_COMMIT=$(shell git rev-parse --short HEAD) ;\
		echo "patching SL_REV entry to $$GIT_COMMIT" ;\
		perl replace_git_commit.pl $$GIT_COMMIT ;\
    fi

# restore default blank value of slftp.inc
revpatchrevert: FORCE
	@if [ -d ".git" ]; then \
        perl replace_git_commit.pl ;\
    fi
