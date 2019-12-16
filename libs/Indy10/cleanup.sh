#!/bin/bash
#
# Remove all files we don't need to compile slftp
#

# cd into dir of execution
cd "$(dirname "$0")"

# delete unwanted folders
rm -r Core/IconsDotNet
rm -r Core/Res
rm -r Protocols/IconsDotNet

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
find * -iname "*.dpkl*" -type f -delete
find * -iname "*.psd*" -type f -delete
find * -iname "*.dcr*" -type f -delete
find * -iname "*.xpm*" -type f -delete
find * -iname "*.lpk*" -type f -delete
find * -iname "*.fpc" -type f -delete
find * -iname "*.resx" -type f -delete
