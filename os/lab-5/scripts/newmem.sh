#!/bin/bash
# 1 argument
N=$1
array=()
cnt=0
while true;
do
	let cnt+=1
	array+=( 1 2 3 4 5 6 7 8 9 10 )
	[ ${#array[@]} -gt $N ] && echo "googd" && exit
done
