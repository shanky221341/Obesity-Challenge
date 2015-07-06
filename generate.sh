#!/bin/bash

# for each file in data, run line below
# Rscript data_clean.R "filename"
FILES="data/*"

#rm -rf parsed/*
#for f in $FILES
#do
#	echo "Processing $f file..."
	

	Rscript data_clean.R 

#done 
