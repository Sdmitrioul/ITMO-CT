#!/bin/bash
#2 arguments K, N
K=$1
N=$2
for ((i=0; i<K; i++))
do
	/bin/bash ./newmem.sh $N &
	sleep 1
done
