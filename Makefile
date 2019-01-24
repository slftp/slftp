SHELL = /bin/bash
SLFTPPATH = ~/slftp
CC = fpc
CFLAGS = -MDelphi -O3 -Xs
CINCLUDES = -Fuirccommands -Fulibs/FastMM4 -Fulibs/BeRoHighResolutionTimer -Fulibs/FLRE -Fulibs/rcmdline -Fulibs/DFFLibV15_UIntList -Fulibs/lkJSON -Fulibs/TRegExpr -Fulibs/pasmp -Fulibs/Compvers -Fulibs/Indy10/* -Fulibs/LibTar -Fulibs/mORMot -Fulibs/ZeosLib/*
CDBFLAGS = -MDelphi -gl -gp -gs -gw3

default: clean slftp

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

clean:
	@rm -f irccommands/*.ppu irccommands/*.o
	@rm -f libs/FastMM4/*.ppu libs/FastMM4/*.o
	@rm -f libs/BeRoHighResolutionTimer/*.ppu libs/BeRoHighResolutionTimer/*.o
	@rm -f libs/FLRE/*.ppu libs/FLRE/*.o
	@rm -f libs/rcmdline/*.ppu libs/rcmdline/*.o
	@rm -f libs/DFFLibV15_UIntList/*.ppu libs/DFFLibV15_UIntList/*.o
	@rm -f libs/lkJSON/*.ppu libs/lkJSON/*.o
	@rm -f libs/TRegExpr/*.ppu libs/TRegExpr/*.o
	@rm -f libs/pasmp/*.ppu libs/pasmp/*.o
	@rm -f libs/Compvers/*.ppu libs/Compvers/*.o
	@rm -f libs/Indy10/Core/*.ppu libs/Indy10/Core/*.o libs/Indy10/Protocols/*.ppu libs/Indy10/Protocols/*.o libs/Indy10/System/*.ppu libs/Indy10/System/*.o
	@rm -f libs/LibTar/*.ppu libs/LibTar/*.o
	@rm -f libs/mORMot/*.ppu libs/mORMot/*.o libs/mORMot/CrossPlatform/*.ppu libs/mORMot/CrossPlatform/*.o libs/mORMot/SQLite3/*.ppu libs/mORMot/SQLite3/*.o libs/mORMot/SynDBDataset/*.ppu libs/mORMot/SynDBDataset/*.o
	@rm -f libs/ZeosLib/core/*.ppu libs/ZeosLib/core/*.o libs/ZeosLib/dbc/*.ppu libs/ZeosLib/dbc/*.o libs/ZeosLib/plain/*.ppu libs/ZeosLib/plain/*.o
	@rm -f *.ppu *.o slftp *.exe

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
