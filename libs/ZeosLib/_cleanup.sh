#!/bin/bash
#
# Remove all files we don't need to compile slftp
#

# cd into dir of execution
cd "$(dirname "$0")"

# delete unwanted folders
rm -r component
rm -r webservice

# delete unwanted file types
find * -iname "repl.*" -type f -delete

find * -iname "ZDbcAdo*" -type f -delete
find * -iname "ZDbcASA*" -type f -delete
find * -iname "ZDbcDbLib*" -type f -delete
find * -iname "ZDbcInterbase6*" -type f -delete
find * -iname "ZDbcODBC*" -type f -delete
find * -iname "ZDbcOleDB*" -type f -delete
find * -iname "ZDbcOracle*" -type f -delete
find * -iname "ZDbcPostgreSql*" -type f -delete
find * -iname "ZDbcSqLite*" -type f -delete

find * -iname "ZOleDB*" -type f -delete
find * -iname "ZPlainAdo*" -type f -delete
find * -iname "ZPlainASA*" -type f -delete
find * -iname "ZPlainDbLib*" -type f -delete
find * -iname "ZPlainFirebird*" -type f -delete
find * -iname "ZPlainODBC*" -type f -delete
find * -iname "ZPlainOleDB*" -type f -delete
find * -iname "ZPlainOracle*" -type f -delete
find * -iname "ZPlainPostgreSql*" -type f -delete
find * -iname "ZPlainSqLite*" -type f -delete

# set settings for ZeosLib as needed
sed -i 's/{.$DEFINE ZEOS_DISABLE_POSTGRESQL}/{$DEFINE ZEOS_DISABLE_POSTGRESQL}/' Zeos.inc
sed -i 's/{.$DEFINE ZEOS_DISABLE_DBLIB}/{$DEFINE ZEOS_DISABLE_DBLIB}/' Zeos.inc
sed -i 's/{.$DEFINE ZEOS_DISABLE_ADO}/{$DEFINE ZEOS_DISABLE_ADO}/' Zeos.inc
sed -i 's/{.$DEFINE ZEOS_DISABLE_INTERBASE}/{$DEFINE ZEOS_DISABLE_INTERBASE}/' Zeos.inc
sed -i 's/{.$DEFINE ZEOS_DISABLE_FIREBIRD}/{$DEFINE ZEOS_DISABLE_FIREBIRD}/' Zeos.inc
sed -i 's/{.$DEFINE ZEOS_DISABLE_SQLITE}/{$DEFINE ZEOS_DISABLE_SQLITE}/' Zeos.inc
sed -i 's/{.$DEFINE ZEOS_DISABLE_ORACLE}/{$DEFINE ZEOS_DISABLE_ORACLE}/' Zeos.inc
sed -i 's/{.$DEFINE ZEOS_DISABLE_ASA}/{$DEFINE ZEOS_DISABLE_ASA}/' Zeos.inc
sed -i 's/{.$DEFINE ZEOS_DISABLE_POOLED}/{$DEFINE ZEOS_DISABLE_POOLED}/' Zeos.inc
sed -i 's/{.$DEFINE ZEOS_DISABLE_OLEDB}/{$DEFINE ZEOS_DISABLE_OLEDB}/' Zeos.inc
sed -i 's/{.$DEFINE ZEOS_DISABLE_ODBC}/{$DEFINE ZEOS_DISABLE_ODBC}/' Zeos.inc

# copy ZEOS files which are needed for mORMot
cp Zeos.inc ../mORMot/Zeos.inc
cp ZeosLazarus.inc ../mORMot/ZeosLazarus.inc
