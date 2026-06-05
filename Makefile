SHELL = bash
SLFTPPATH = ~/slftp
CC = fpc
CFLAGS = -MDelphi -O3 -Xs -gl
CINCLUDES = -Fuapi -Fuirccommands -Furules -Fulibs/BeRoHighResolutionTimer -Fulibs/FLRE -Fulibs/rcmdline -Fulibs/lkJSON -Fulibs/TRegExpr -Fulibs/pasmp -Fulibs/Indy10/* -Fulibs/Indy10/Protocols -Fulibs/Indy10/Protocols/OpenSSL -Fulibs/Indy10/Protocols/OpenSSL/* -Fulibs/LibTar -Fulibs/mORMot2/src/core -Fulibs/mORMot2/src/lib -Fulibs/mORMot2/src/crypt -Fulibs/mORMot2/src/db -Fulibs/mORMot2/src/orm -Fulibs/mORMot2/src/rest -Fulibs/mORMot2/src/soa -Fulibs/ZeosLib/* -Fulibs/mORMot2/src/net/
WEB_DEPLOY_DIR = $(SLFTPPATH)/web-ui
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

slftp:	FORCE
	$(MAKE) clean
	$(MAKE) revpatch
	$(CC) $(CFLAGS) $(CINCLUDES) slftp.lpr
	$(MAKE) revpatchrevert

slftp_32:	FORCE
	$(MAKE) clean
	$(MAKE) revpatch
	$(CC) -Pi386 $(CFLAGS) $(CINCLUDES) slftp.lpr
	$(MAKE) revpatchrevert

slftp_64:	FORCE
	$(MAKE) clean
	$(MAKE) revpatch
	$(CC) -Px86_64 $(CFLAGS) $(CINCLUDES) slftp.lpr
	$(MAKE) revpatchrevert

slftp_debug:	FORCE
	$(MAKE) revpatch
	$(CC) $(CDBFLAGS) $(CINCLUDES) slftp.lpr
	$(MAKE) revpatchrevert

slftp_32_debug:	FORCE
	$(MAKE) revpatch
	$(CC) -Pi386 $(CDBFLAGS) $(CINCLUDES) slftp.lpr
	$(MAKE) revpatchrevert

slftp_64_debug:	FORCE
	$(MAKE) revpatch
	$(CC) -Px86_64 $(CDBFLAGS) $(CINCLUDES) slftp.lpr
	$(MAKE) revpatchrevert

slftp_debug_heaptrace:	FORCE
	$(MAKE) revpatch
	$(CC) $(CDBFLAGS) $(HEAPTRACE) $(CINCLUDES) slftp.lpr
	$(MAKE) revpatchrevert

slftp_debug_valgrind:	FORCE
	$(MAKE) revpatch
	$(CC) $(CDBFLAGS) $(VALGRIND) $(CINCLUDES) slftp.lpr
	$(MAKE) revpatchrevert

slftp_debug_gprof:	FORCE
	$(MAKE) revpatch
	$(CC) $(CDBFLAGS) $(GPROF) $(CINCLUDES) slftp.lpr
	$(MAKE) revpatchrevert

slftp_debug_vtune:	FORCE
	$(MAKE) revpatch
	$(CC) $(VTUNE) $(CINCLUDES) slftp.lpr
	$(MAKE) revpatchrevert

test:	FORCE
	$(MAKE) clean
	@mkdir -p tests/databases
	$(CC) $(CFLAGS) $(CINCLUDES) $(CTESTINCLUDES) tests/slftpUnitTests.lpr
	./tests/slftpUnitTests
	$(MAKE) cleanuptestdir

clean:
	@find . -name "*.ppu" -type f -delete
	@find . \( -path "./libs/mORMot2/static" \) -prune -o -name "*.o" -type f -exec rm {} +
	@rm -f slftp *.exe
	$(MAKE) cleanuptestdir

cleanuptestdir:
	@find tests -name "*.ppu" -type f -delete
	@find tests -name "*.o" -type f -delete
	@rm -f tests/*.ppu tests/*.o tests/slftpUnitTests tests/*.exe
	@rm -f tests/*.res tests/*.or
	@rm -rf tests/databases

install:
	@cp slftp $(SLFTPPATH)/slftp

# empty target to force execution
FORCE:

# patch used HEAD git-hash into slftp.inc
revpatch: FORCE
	@if [ -d ".git" ]; then \
        GIT_COMMIT=DEADC0DE-$(shell git rev-parse --short HEAD) ;\
		echo "patching SL_REV entry to $$GIT_COMMIT" ;\
		perl replace_git_commit.pl $$GIT_COMMIT ;\
    fi

# restore default blank value of slftp.inc
revpatchrevert: FORCE
	@if [ -d ".git" ]; then \
        perl replace_git_commit.pl ;\
    fi

# Build web UI (requires Node.js and npm)
web-ui-build: FORCE
	cd web-ui && npm install && npm run build

# Deploy built web UI to deployment directory
web-ui-deploy: FORCE
	if [ -z "$(WEB_DEPLOY_DIR)" ]; then echo "WEB_DEPLOY_DIR not set"; exit 1; fi
	mkdir -p $(WEB_DEPLOY_DIR)
	cp -r web-ui/dist/* $(WEB_DEPLOY_DIR)/

# Build and deploy web UI
web-ui-prod: web-ui-build web-ui-deploy

# Alias for web-ui-prod (shorthand)
webui-deploy: web-ui-prod

# Build slftp and web UI together
all-with-ui: slftp web-ui-prod
