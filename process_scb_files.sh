#!/usr/bin/env bash

wd=~/Documents/postal_codes
of=$wd/education_all.txt
echo "ID BYEAR CIVSTAND LKF EDU YEAR" > $of
for y in {1998..2013}
do
	awk -v y=${y} -F"\t" 'FNR>1{print $1, $2, $7, $8, $10, y}' \
		/home/sarah/Desktop/POSTAL/PostalCodes_2015_SCB/BoJ_lev_${y}.txt >> $of
done
