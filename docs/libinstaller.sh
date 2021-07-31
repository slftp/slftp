#!/bin/bash
##############
# slftpd install script for libaries
#
# dependencies on:
#   debian 10.4.0 (buster) amd64 - basic install with ssl only
# apt -y install ca-certificates openssl unzip cpp gcc manpages manpages-dev cmake make build-essential bzip2 dpkg-dev fakeroot g++ patch perl rename xz-utils libssl-dev libssl-doc curl libcurl4-openssl-dev git libncurses-dev 
##############
# changelog
# v20210721 - variable DEVRUN unused, removed
#           # fix `...` to $(...) http://mywiki.wooledge.org/BashFAQ/082
# v20210409 + slftp now supports openssl 1.1
#           # changelog from this point on will be covered in Gitlab
# v20200727 # bugfix for downloading mysql (github template has been changed)
#           # bugfix for invalid SQLite download options
#           # bugfix for SQLite sha3 chksum (now using sha3-256 instead of sha1sum)
# v20200117 # bugfix for libmysqlclient (reported by fated)
#           ^ optimized make on mysql and mariadb
#           + added DEVDIR for downloaded and compiled files.
# v20200111 # bugfix for more than nine options
#           + added HTTP support for MIRROR_OPENSSL
#           ^ default MIRROR_OPENSSL is now linked to */old/1.0.2
# v20181612 ^ initial release
##############


###
# config
###


#DEBUG=<true|false>
DEBUG=true
LOGFILE="/tmp/debug.log"

#here we will download/compile
DEVDIR="$HOME/_dev"

MIRROR_OPENSSL="http://artfiles.org/openssl.org/source/"
# https://www.openssl.org/source/mirror.html

MIRROR_SQLITE="https://www.sqlite.org/download.html"

MIRROR_MYSQL="https://github.com/mysql/mysql-server"

MIRROR_MARIADB="http://downloads.mariadb.com/Connectors/c/"


###
# end of config - if you know what you are doing, keep going.
###


LANG=C
LC_ALL="C"
export LC_ALL


# these are just some regular files which are used by the script.
# software developer will need some more libs/bins!
BINS_NEEDED="echo grep egrep wget basename cut sha256sum sha1sum md5sum printf tr head tail unzip cmake tar curl git"

#set maximum of ids to choose (LIMITED to 9) -> func_choose
IDS=4

#initiate ids(=x)
for i in $(seq 0 $IDS); do
 x[$i]=" "
done

function func_banner {
clear
 echo "
 _           _        _ _   _ _ _           
(_)_ __  ___| |_ __ _| | | | (_) |__  ___   
| | '_ \\/ __| __/ _\` | | | | | | '_ \\/ __|  
| | | | \\__ \\ || (_| | | | | | | |_) \\__ \\  
|_|_| |_|___/\\__\\__,_|_|_| |_|_|_.__/|___/              for debian and ubuntu"
 printf '%65s v20200727   \n' | tr ' ' =
}



function func_echo_debug {
 #if [ "$DEBUG" = true ]; then echo "[$(date)] [DEBUG] $@" 2>&1 | tee -a $LOGFILE; fi
 if [ "$DEBUG" = true ]; then echo "[$(date)] [DEBUG] $@"; fi
}



function func_maxnum {
 if [ "$i" -gt "9" ] ; then
  let MAXTEN=$i/10
  let MAXONE=$i-$MAXTEN*10
  MAXTWE="|"
  if [ "$i" -gt 19 ] ; then
   MAXTWE="[1-$(($MAXTEN-1))][0-9]|"
  fi
  MAXNUM="^([0-9]|$MAXTWE[1-$MAXTEN][0-$MAXONE])$"
 else
  MAXNUM="^[0-$i]$"
 fi
}



function func_openssl {
 OPENSSL_FILES=$(wget -O- -q "$MIRROR_OPENSSL" | grep -v "fips" | egrep "([0-9]{6,} bytes|[0-9]+\.?(0-9)*M)" | egrep -o [^\"]*openssl.+ | sed 's/".*//' | sed "s|^\([^fF][^tT][^pP][^:]\)|$MIRROR_OPENSSL\1|g")
 i=0
 echo "Available OpenSSL versions:"
 for FILE in $OPENSSL_FILES; do
  let i+=1
  echo "  [$i] $(basename "$FILE")"
 done
 echo "  --- -------------------------" 
 echo "  [0] Continue without OpenSSL."

 REPLY=
 while [ -z "$REPLY" ]; do
  echo " "
  read -p "Which OpenSSL do you want to install? "
  func_maxnum
  if ! [[ "$REPLY" =~ $MAXNUM ]] ; then
   REPLY=
   echo "Invalid input. Has to be a valid number."
  else
   if ! [[ "$REPLY" == 0 ]] ; then
    OPENSSL_FILE=$(echo $OPENSSL_FILES | cut -d' ' -f$REPLY)
    OPENSSL_FILENAME=$(basename "$OPENSSL_FILE")
   fi
  fi
 done
 OPENSSL_FILENAME=$(basename "$OPENSSL_FILE")
}



function func_openssl_dlinst {
 wget "$OPENSSL_FILE" -O "$DEVDIR/$OPENSSL_FILENAME"
 wget "${OPENSSL_FILE}.sha256" -O "$DEVDIR/${OPENSSL_FILENAME}.sha256"
 if ! [[ "$(sha256sum "$DEVDIR/${OPENSSL_FILENAME}"|cut -d' ' -f1)" == "$(cat "$DEVDIR/${OPENSSL_FILENAME}.sha256")" ]] ; then
  echo "[-] ERROR: Checksum does _NOT_ match."
  read -n 1 -s -r -p "Press CTRL+C to abort  OR  any key to continue."
  echo -ne "\033[0K\r"
 fi
 case "${OPENSSL_FILENAME##*.}" in
  [gG][zZ]     ) cd "$DEVDIR";tar xfz "$DEVDIR/$OPENSSL_FILENAME"   || echo "[-] ERROR: Could _NOT_ extract." ;;
  [zZ][iI][pP] ) cd "$DEVDIR";unzip -oq "$DEVDIR/$OPENSSL_FILENAME" || echo "[-] ERROR: Could _NOT_ extract." ;;
  *            ) echo "[-] ERROR: Could _NOT_ extract. Unknown fileformat." ;;
 esac

 if [ -d "$DEVDIR/${OPENSSL_FILENAME%.*}" ]; then
  cd "$DEVDIR/${OPENSSL_FILENAME%.*}"
  OPENSSL_LIBNAME="${OPENSSL_FILENAME%.*}"
 elif [ -d "$DEVDIR/${OPENSSL_FILENAME%.*.*}" ]; then
  cd "$DEVDIR/${OPENSSL_FILENAME%.*.*}"
  OPENSSL_LIBNAME="${OPENSSL_FILENAME%.*.*}"
 else
  echo "[-] ERROR: Could _NOT_ find extracted directory."
  exit 0
 fi
 ./config -shared && make clean && make
 if [[ -e "libssl.so" && -e "libcrypto.so" ]]; then
  cp -f libssl.so "$SL_DIR/libssl_$OPENSSL_LIBNAME"
  cp -f libcrypto.so "$SL_DIR/libcrypto_$OPENSSL_LIBNAME"
  cd -
  cd "$SL_DIR"
  rm libssl.so libcrypto.so
  ln -s libssl_$OPENSSL_LIBNAME libssl.so
  ln -s libcrypto_$OPENSSL_LIBNAME libcrypto.so
  OPENSSL_INSTALLED=1
 else
  echo "[-] ERROR: Could _NOT_ find compiled libaries."
  OPENSSL_INSTALLED=0
 fi
}



function func_sqlite {
 SQLITE_CONTENT=$(wget -O- -q "$MIRROR_SQLITE")
 #SQLITE_CONTENT=$(cat sqlite)
# SQLITE_FILES=$(echo "$SQLITE_CONTENT"  | egrep "sqlite\-amalgamation" | egrep -o "20[^']+")
 SQLITE_FILES=$(echo "$SQLITE_CONTENT"  | egrep "20[^']+sqlite\-amalgamation\-[0-9]+\.zip" | egrep -o "20[^']+")
 for FILE in $SQLITE_FILES; do
  TMP=$(echo $SQLITE_CONTENT | egrep -o "$(basename $FILE)[^\)]+\)[^\)]+" | egrep -o "[^ ][0-9a-f]+$")
  SQLITE_CHKSUM=$(echo $SQLITE_CHKSUM $FILE $TMP)
 done
 i=0
 echo "Available SQLite versions:"
 for FILE in $SQLITE_FILES; do
  let i+=1
  echo "  [$i] $(basename $FILE)"
 done
 echo "  --- -------------------------" 
 echo "  [0] Continue without SQLite."

 REPLY=
 while [ -z "$REPLY" ]; do
  echo " "
  read -p "Which SQLite do you want to install? "
  func_maxnum
  if ! [[ "$REPLY" =~ ^[0-$i]$ ]] ; then
   REPLY=
   echo "Invalid input. Has to be a valid number."
  else
   if ! [[ "$REPLY" == 0 ]] ; then
    SQLITE_FILE=$(echo $SQLITE_FILES | cut -d' ' -f$REPLY)
    SQLITE_FILENAME=$(basename $SQLITE_FILE)
    SQLITE_CHKSUM=$(echo $SQLITE_CHKSUM | egrep -o "$SQLITE_FILE [^ ]+" | cut -d' ' -f2)
   fi
  fi
 done
 SQLITE_BASEURL=$(echo "$MIRROR_SQLITE" | egrep -o "(f|ht)tps?://[^/]+")
}



function func_sqlite_dlinst {
 wget "${SQLITE_BASEURL}/${SQLITE_FILE}" -O "$DEVDIR/$SQLITE_FILENAME"

# if ! [[ "$(sha1sum "$DEVDIR/${SQLITE_FILENAME}"|cut -d' ' -f1)" == "$SQLITE_CHKSUM" ]] ; then
 if ! [[ "$(openssl dgst -sha3-256 "$DEVDIR/${SQLITE_FILENAME}"|cut -d' ' -f2)" == "$SQLITE_CHKSUM" ]] ; then
  echo "[-] ERROR: Checksum does _NOT_ match."
  read -n 1 -s -r -p "Press CTRL+C to abort  OR  any key to continue."
  echo -ne "\033[0K\r"
 fi

 case "${SQLITE_FILENAME##*.}" in
  [gG][zZ]     ) cd "$DEVDIR";tar xfz "$SQLITE_FILENAME"   || echo "[-] ERROR: Could _NOT_ extract." ;;
  [zZ][iI][pP] ) cd "$DEVDIR";unzip -oq "$SQLITE_FILENAME" || echo "[-] ERROR: Could _NOT_ extract." ;;
  *            ) echo "[-] ERROR: Could _NOT_ extract. Unknown fileformat." ;;
 esac

 if [ -d "$DEVDIR/${SQLITE_FILENAME%.*}" ]; then
  cd "$DEVDIR/${SQLITE_FILENAME%.*}"
  SQLITE_LIBNAME="${SQLITE_FILENAME%.*}"
 elif [ -d "$DEVDIR/${SQLITE_FILENAME%.*.*}" ]; then
  cd "$DEVDIR/${SQLITE_FILENAME%.*.*}"
  SQLITE_LIBNAME="${SQLITE_FILENAME%.*.*}"
 else
  echo "[-] ERROR: Could _NOT_ find extracted directory."
  exit 0
 fi
 # http://jqnotes.blogspot.com/2011/01/compile-sqlite3-to-be-shared-library.html
 gcc -c -fPIC sqlite3.c && gcc -shared -o libsqlite3.so -fPIC sqlite3.o -ldl -lpthread
 if [[ -e "libsqlite3.so" ]]; then
  cp -f libsqlite3.so "$SL_DIR"/libsqlite3_$SQLITE_LIBNAME
  cd -
  cd "$SL_DIR"
  rm libsqlite3.so
  ln -s libsqlite3_$SQLITE_LIBNAME libsqlite3.so
  SQLITE_INSTALLED=1
 else
  echo "[-] ERROR: Could _NOT_ find compiled libaries."
  SQLITE_INSTALLED=0
 fi
}



function func_mysql {
 MYSQL_CONTENT=$(wget -O- -q "$MIRROR_MYSQL")
# MYSQL_FILES=$(echo "$MYSQL_CONTENT" | grep "Download ZIP" | grep -o -E "mysql/mysql-server[^\"]+\.zip")
 MYSQL_FILES=$(echo "$MYSQL_CONTENT" | grep "DOWNLOAD_ZIP" | grep -o -E "mysql/mysql-server[^\"]+\.zip")
 MYSQL_VERSION=$(echo $(basename "$MYSQL_FILES") | sed 's/\.zip//')
#https://dev.mysql.com/get/Downloads/Connector-C++/mysql-connector-c++-8.0.13-src.tar.gz
 i=0
 echo "Available MySQL versions:"
 for FILE in $MYSQL_FILES; do
  let i+=1
  echo "  [$i] $(basename "$FILE")"
 done
 echo "  --- -------------------------" 
 echo "  [0] Continue without MySQL."

 REPLY=
 while [ -z "$REPLY" ]; do
  echo " "
  read -p "Which MySQL do you want to install? "
  func_maxnum
  if ! [[ "$REPLY" =~ $MAXNUM ]] ; then
   REPLY=
   echo "Invalid input. Has to be a valid number."
  else
   if ! [[ "$REPLY" == 0 ]] ; then
    MYSQL_FILE=$(echo "$MYSQL_FILES" | cut -d' ' -f"$REPLY")
    MYSQL_FILENAME=$(basename "$MYSQL_FILE")
   fi
  fi
 done
}



function func_mysql_dlinst {
 MIRROR_MYSQL_DL=$(echo "$MIRROR_MYSQL" | grep -o -E "https?://[^/]+")
 wget "${MIRROR_MYSQL_DL}/${MYSQL_FILE}" -O "$DEVDIR/$MYSQL_FILENAME"

#no chksum because of github.
#https://dev.mysql.com/downloads/mysql/#downloads does exist. but i am keeping it easy.

 case "${MYSQL_FILENAME##*.}" in
  [gG][zZ]     ) cd "$DEVDIR";tar xfz "$MYSQL_FILENAME"   || echo "[-] ERROR: Could _NOT_ extract." ;;
  [zZ][iI][pP] ) cd "$DEVDIR";unzip -oq "$MYSQL_FILENAME" || echo "[-] ERROR: Could _NOT_ extract." ;;
  *            ) echo "[-] ERROR: Could _NOT_ extract. Unknown fileformat." ;;
 esac

 if [ -d "$DEVDIR/${MYSQL_FILENAME%.*}" ]; then
  cd "$DEVDIR/${MYSQL_FILENAME%.*}"
  MYSQL_LIBNAME="${MYSQL_FILENAME%.*}"
 elif [ -d "$DEVDIR/${MYSQL_FILENAME%.*.*}" ]; then
  cd "$DEVDIR/${MYSQL_FILENAME%.*.*}"
  MYSQL_LIBNAME="${MYSQL_FILENAME%.*.*}"
 elif [ -d "$DEVDIR/mysql-server-${MYSQL_FILENAME%.*}" ]; then
  cd "$DEVDIR/mysql-server-${MYSQL_FILENAME%.*}"
  MYSQL_LIBNAME="mysql-server-${MYSQL_FILENAME%.*}"
 else
  echo "[-] ERROR: Could _NOT_ find extracted directory."
  exit 0
 fi
 mkdir -p "$DEVDIR/$MYSQL_LIBNAME/bld";cd "$DEVDIR/$MYSQL_LIBNAME/bld";cmake ../ -DDOWNLOAD_BOOST=1 -DWITH_BOOST=. -DWITH_UNIT_TESTS=OFF -DWITHOUT_SERVER=ON -DBUILD_SHARED_LIBS=ON -DCMAKE_INSTALL_PREFIX=. && make libmysql;
 if [[ -e "./library_output_directory/libmysqlclient.so" ]]; then
  cp -f "./library_output_directory/libmysqlclient.so" "$SL_DIR/libmysqlclient_$MYSQL_LIBNAME"
  cd -
  cd "$SL_DIR"
  rm libmysqlclient.so
  ln -s "libmysqlclient_$MYSQL_LIBNAME" libmysqlclient.so
  MYSQL_INSTALLED=1
 else
  echo "[-] ERROR: Could _NOT_ find compiled libaries."
  MYSQL_INSTALLED=0
 fi
}



function func_mariadb {
 MARIADB_VERSION=$(wget -O- -q "$MIRROR_MARIADB" | grep connector | tail -n1 | egrep -o "connector[^\"]+" | head -n1)
 #MARIADB_VERSION=$(cat maria | grep connector | tail -n1 | egrep -o "connector[^\"]+" | head -n1)
 MARIADB_FILES=$(echo "mariadb-${MARIADB_VERSION}-src.tar.gz")
#http://downloads.mariadb.com/Connectors/c/connector-c-3.0.7/mariadb-connector-c-3.0.7-src.tar.gz
 i=0
 echo "Available MariaDB versions:"
 for FILE in $MARIADB_FILES; do
  let i+=1
  echo "  [$i] $(basename "$FILE")"
 done
 echo "  --- -------------------------" 
 echo "  [0] Continue without MariaDB."

 REPLY=
 while [ -z "$REPLY" ]; do
  echo " "
  read -p "Which MariaDB do you want to install? "
  func_maxnum
  if ! [[ "$REPLY" =~ $MAXNUM ]] ; then
   REPLY=
   echo "Invalid input. Has to be a valid number."
  else
   if ! [[ "$REPLY" == 0 ]] ; then
    echo "$MARIADB_FILES" | cut -d' ' -f"$REPLY"
    MARIADB_FILE=$(echo "$MARIADB_FILES" | cut -d' ' -f"$REPLY")
    echo "$MARIADB_FILE"
    MARIADB_FILENAME=$(basename "$MARIADB_FILE")
   fi
  fi
 done
}



function func_mariadb_dlinst {
 wget "${MIRROR_MARIADB}/${MARIADB_VERSION}/${MARIADB_FILE}" -O "$DEVDIR/$MARIADB_FILENAME"

 MARIADB_CHKSUM=$(wget -O- -q "${MIRROR_MARIADB}/${MARIADB_VERSION}/sha256sums.txt" | grep "$MARIADB_FILENAME" | cut -d' ' -f1)
 if ! [[ "$(sha256sum "$DEVDIR/${MARIADB_FILENAME}"|cut -d' ' -f1)" == "$MARIADB_CHKSUM" ]] ; then
  echo "[-] ERROR: Checksum does _NOT_ match."
  read -n 1 -s -r -p "Press CTRL+C to abort  OR  any key to continue."
  echo -ne "\033[0K\r"
 fi

 case "${MARIADB_FILENAME##*.}" in
  [gG][zZ]     ) cd "$DEVDIR";tar xfz "$MARIADB_FILENAME"   || echo "[-] ERROR: Could _NOT_ extract." ;;
  [zZ][iI][pP] ) cd "$DEVDIR";unzip -oq "$MARIADB_FILENAME" || echo "[-] ERROR: Could _NOT_ extract." ;;
  *            ) echo "[-] ERROR: Could _NOT_ extract. Unknown fileformat." ;;
 esac

 if [ -d "$DEVDIR/${MARIADB_FILENAME%.*}" ]; then
  cd "$DEVDIR/${MARIADB_FILENAME%.*}"
  MARIADB_LIBNAME="${MARIADB_FILENAME%.*}"
 elif [ -d "$DEVDIR/${MARIADB_FILENAME%.*.*}" ]; then
  cd "$DEVDIR/${MARIADB_FILENAME%.*.*}"
  MARIADB_LIBNAME="${MARIADB_FILENAME%.*}"
 elif [ -d "$DEVDIR/${MARIADB_FILENAME%.*}" ]; then
  cd "$DEVDIR/${MARIADB_FILENAME%.*.*}"
 else
  echo "[-] ERROR: Could _NOT_ find extracted directory."
  exit 0
 fi
 cmake -G "Unix Makefiles" && make clean && make libmariadb
 if [[ -e "libmariadb/libmariadb.so" ]]; then
  cp -f "libmariadb/libmariadb.so" "$SL_DIR/libmariadb_$MARIADB_LIBNAME"
  cd -
  cd "$SL_DIR"
  rm libmariadb.so
  ln -s "libmariadb_$MARIADB_LIBNAME" libmariadb.so
  MARIADB_INSTALLED=1
 else
  echo "[-] ERROR: Could _NOT_ find compiled libaries."
  MARIADB_INSTALLED=0
 fi
}



function func_choose {
 REPLY=
 while [ -z "$REPLY" ]; do
  func_banner
  read -p "  id  x=selected to be installed
 /   /
[1] [${x[1]}] Lib of OpenSSL
[2] [${x[2]}] Lib of SQLite
[3] [${x[3]}] Lib of MySQL
[4] [${x[4]}] Lib of MariaDB
--- --- --------------
[0]     Continue

Deselect by typing the ID again.

Select your IDs (without any spaces or delimiter): "
  if ! [[ "$REPLY" =~ ^[0-$IDS]+$ ]] ; then
   REPLY=
  else
   if ! [[ "$REPLY" == 0 ]] ; then
    for i in $(seq 0 ${#REPLY}); do
     #y x=yes space=no
     y=${x[${REPLY:$i:1}]}
     #z = id
     z=${REPLY:$i:1}
     if [[ "$z" == "" ]]; then
      continue
     elif [[ "$y" == " " ]]; then
      x[$z]="x"
     else
      x[$z]=" "
     fi
    done
    if [[ "${x[0]}" == "x" ]] ; then
     continue
    else
     REPLY=
    fi
   elif [[ "${x[0]}" == "x" ]] ; then
    continue
   else
    x[0]="x"
   fi
  fi
 done
 if [ $(echo ${x[*]} | grep -o "x" | wc -l) -le 1 ];then
  echo "                  , _ ,
                 ( o o )
      OWL.      /'\` ' \`'\\
      RLY?      |'''''''|
                |\\\\'''//|
                   \"\"\"
Done with nothing. Hackerman."
  exit 0
 else
  continue
  # :)
 fi
}



function func_init {
 func_echo_debug "[*] START: ${FUNCNAME[0]}"
 if [ "$DEBUG" = true ]; then echo>$LOGFILE; fi

 func_banner
 # check for needed bins
 for BIN in $BINS_NEEDED; do
  if [[ "$(command -v "$BIN")" != "" ]]; then
   func_echo_debug "[+] FOUND: $BIN"
  else
   func_echo_debug "[-] NOT FOUND: $BIN"
   BINS_MISSING="$BINS_MISSING $BIN"
  fi
 done
 if ! [ -z "$BINS_MISSING" ]; then
  echo "[-] ERROR: Some binaries are missing:$BINS_MISSING"
  echo " "
  read -n 1 -s -r -p "Press CTRL+C to abort  OR  any key to continue."
  echo -ne "\033[0K\r"
 else
  mkdir -m700 "$DEVDIR"
  cd "$DEVDIR"
 fi

 func_banner
 func_choose

 func_banner
 # enter slftp dir
 REPLY=
 while [ -z "$REPLY" ]; do
  read -p "Where do you want to install your compiled lib/bin (e.g. $HOME/bin/)?  "
  if ! [[ -d "$REPLY" ]] ; then
   REPLY=
   echo "Invalid input. Has to be a valid directory."
  else
   SL_DIR=$REPLY
  fi
 done
 if [[ "${x[1]}" == "x" ]] ; then 
  func_banner
  func_openssl
 fi
 if [[ "${x[2]}" == "x" ]] ; then 
  func_banner
  func_sqlite
 fi
 if [[ "${x[3]}" == "x" ]] ; then 
  func_banner
  func_mysql
 fi
 if [[ "${x[4]}" == "x" ]] ; then 
  func_banner
  func_mariadb
 fi

 func_banner
 echo "times calculated on following specs:
=====================================
cpu.......: cpubenchmark 1000pts (one core)
bandwith..: 1MBit -> 0,125mb/s
=====================================

--------------,------------,---------
   Software   |  download  | compile
--------------+------------+---------
 OpenSSL      |       43s  | 01m:53s
 SQLite       |       19s  | 00m:04s
 MySQL+Boost  |   38m 52s  | 03m:20s
 MariaDB      |        6s  | 00m:20s
--------------'------------'---------"

 echo " "
 echo "Downloading and installing will start now. This will take some time."
 echo " "
 read -n 1 -s -r -p "Press CTRL+C to abort  OR  any key to continue."
 echo -ne "\033[0K\r"

 if [[ "${x[1]}" == "x" ]] ; then 
  func_banner
  func_openssl_dlinst
 fi
 if [[ "${x[2]}" == "x" ]] ; then 
  func_banner
  func_sqlite_dlinst
 fi
 if [[ "${x[3]}" == "x" ]] ; then 
  func_banner
  func_mysql_dlinst
 fi
 if [[ "${x[4]}" == "x" ]] ; then 
  func_banner
  func_mariadb_dlinst
 fi

 func_banner
 if [[ "$OPENSSL_INSTALLED" == "1" ]] ; then 
  echo "[+] Lib of OpenSSL has been installed."
 elif [[ "${x[1]}" == "x" ]] ; then 
  echo "[-] Lib of OpenSSL has _NOT_ been installed."
 fi
 if [[ "$SQLITE_INSTALLED" == "1" ]] ; then 
  echo "[+] Lib of SQLite has been installed."
 elif [[ "${x[2]}" == "x" ]] ; then 
  echo "[-] Lib of SQLite has _NOT_ been installed."
 fi
 if [[ "$MYSQL_INSTALLED" == "1" ]] ; then 
  echo "[+] Lib of MySQL has been installed."
 elif [[ "${x[3]}" == "x" ]] ; then 
  echo "[-] Lib of MySQL has _NOT_ been installed."
 fi
 if [[ "$MARIADB_INSTALLED" == "1" ]] ; then 
  echo "[+] Lib of MariaDB has been installed."
 elif [[ "${x[4]}" == "x" ]] ; then 
  echo "[-] Lib of MariaDB has _NOT_ been installed."
 fi
}



func_init

#EoF /1337? yea.. a bit.
