#!/bin/bash
# this bash is used to start ice cloud forcing calculation
cld_date="200701/"
#rm -rf filename 
rm -rf nohup.out
#for icehab in "solid-column" "hollow-column" "rough-aggregate" "rosette-6" "plate" "droxtal" "new.spheroid" 
#do
icehab="rough-aggregate"
make clean
make
echo -e "$cld_date\n$icehab" | ./ice_cf 
#done
