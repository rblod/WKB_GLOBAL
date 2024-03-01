#!/bin/bash

NSTART=1
NEND=10

TSTART=1
TEND=5

DATADIR='/Users/rblod/202302_WAGNER/Wagner_to_Rachid/'

# generate input bathy
./PREPRO/genebathy.py --datadir $DATADIR --nstart $NSTART --nend $NEND  || exit 1

#generate input namelists
./PREPRO/genedata.py --datadir $DATADIR --nstart $NSTART --nend $NEND  || exit 2


# run wkb 
for ((NITER=NSTART; NITER<=NEND; NITER++))
do
  echo 'PROFILE : ' $NITER
  for ((TITER=TSTART; TITER<=TEND; TITER++))
  do
  	var1=$(printf "%05d" $NITER)
   	var2=$(printf "%05d" $TITER)
 	NAM=namelist_p${var1}_t${var2}
 	echo '     TIME : ' $TITER
    ./wkb.exe $NAM || exit 3
  done
  echo " "      
done