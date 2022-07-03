#!/bin/bash
echo $$ > pids2
array=()
cnt=0
[ ! -f report2.log ] && touch report2.log
echo '' > report2.log
while true;
do
	let cnt+=1
	array+=( 1 2 3 4 5 6 7 8 9 10 )
	[ $(($cnt%100000)) -eq 0 ] && echo ${#array[@]} >> report2.log
done
