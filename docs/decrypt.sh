#!/bin/bash
rm sites.dat.decrypt
./slftp -d --pf=masterpass.txt --infile=sites.dat --outfile=sites.dat.decrypt
