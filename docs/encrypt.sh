#!/bin/bash
mv sites.dat sites.dat.bac
./slftp -e --pf=masterpass.txt --infile=sites.dat.decrypt --outfile=sites.dat
