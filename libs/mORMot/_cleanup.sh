#!/bin/bash
#
# Remove all files we don't need to compile slftp
#

# cd into dir of execution
cd "$(dirname "$0")"

# delete unwanted folders
rm -r CrossPlatform/templates
rm -r SQLite3/DDD
rm -r SQLite3/Documentation
rm -r SQLite3/Samples
rm -r static
rm -r SyNode

# delete unwanted file types
find * -iname "*.tmpl" -type f -delete
find * -iname "*.dpk" -type f -delete
find * -iname "*.bdsproj" -type f -delete
find * -iname "*.proj" -type f -delete
find * -iname "*.rc" -type f -delete
find * -iname "*.dproj" -type f -delete
find * -iname "*.ico" -type f -delete
find * -iname "*.resources" -type f -delete
find * -iname "*.res" -type f -delete
find * -iname "*.bmp" -type f -delete
find * -iname "*.cfg*" -type f -delete
find * -iname "*.json*" -type f -delete
find * -iname "*.lpi*" -type f -delete
find * -iname "*.dpr*" -type f -delete
find * -iname "*.png*" -type f -delete
