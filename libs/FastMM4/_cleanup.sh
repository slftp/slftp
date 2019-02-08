#!/bin/bash
#
# Remove all files we don't need to compile slftp
#

# cd into dir of execution
cd "$(dirname "$0")"

# delete unwanted folders
rm -r CPP\ Builder\ Support/
rm -r Replacement\ BorlndMM\ DLL/
rm -r Translations
rm -r Demos
rm -r FullDebugMode\ DLL/CPP\ Builder\ Support/

# delete unwanted file types
find * -iname "*.dylib" -type f -delete
