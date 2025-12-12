SHELL = bash
SLFTPPATH = ~/slftp
CC = fpc
CFLAGS = -MDelphi -O3 -Xs
CINCLUDES = -Fuirccommands -Furules -Fulibs/BeRoHighResolutionTimer -Fulibs/FLRE -Fulibs/rcmdline -Fulibs/lkJSON -Fulibs/TRegExpr -Fulibs/pasmp -Fulibs/Indy10/* -Fulibs/Indy10/Protocols -Fulibs/Indy10/Protocols/OpenSSL -Fulibs/Indy10/Protocols/OpenSSL/* -Fulibs/LibTar -Fulibs/mORMot2/src/core -Fulibs/mORMot2/src/lib -Fulibs/mORMot2/src/crypt -Fulibs/mORMot2/src/db -Fulibs/mORMot2/src/orm -Fulibs/mORMot2/src/rest -Fulibs/mORMot2/src/soa -Fulibs/ZeosLib/* -Fulibs/mORMot2/src/net/
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

all-with-ui: slftp web-ui-prod

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

install:
	@if [ -d "$(SLFTPPATH)" ]; then \
		cp slftp $(SLFTPPATH)/slftp; \
		echo "Installed slftp to $(SLFTPPATH)/slftp"; \
	else \
		echo "Warning: $(SLFTPPATH) does not exist. Skipping install."; \
		echo "Copy manually: cp slftp /your/path/"; \
	fi

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

.PHONY: web-ui-build web-ui-deploy web-ui-prod

WEB_UI_DIR = web-ui
# WEB_DEPLOY_DIR can be overridden: make web-ui-deploy WEB_DEPLOY_DIR=/custom/path
WEB_DEPLOY_DIR ?= ./web

web-ui-build:
	@echo "Building Web UI..."
	@cd $(WEB_UI_DIR) && npm install && npm run build
	@echo "Web UI built successfully in $(WEB_UI_DIR)/dist/"
	@echo ""
	@echo "To deploy manually:"
	@echo "  cp -r $(WEB_UI_DIR)/dist/* /your/slftp/web/"

web-ui-deploy: web-ui-build
	@echo "Deploying Web UI to $(WEB_DEPLOY_DIR)..."
	@mkdir -p $(WEB_DEPLOY_DIR)
	@echo "Cleaning old files..."
	@rm -rf $(WEB_DEPLOY_DIR)/*
	@cp -r $(WEB_UI_DIR)/dist/* $(WEB_DEPLOY_DIR)/
	@echo "Web UI deployed to $(WEB_DEPLOY_DIR)"
	@echo "Note: Login with API key from slftp.ini [api] section"

web-ui-prod: web-ui-deploy
