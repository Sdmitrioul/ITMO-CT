#!/bin/bash
echo $$ > pids
#/bin/bash ./follower.sh &
array=()
cnt=0
[ ! -f report.log ] && touch report.log
echo '' > report.log
while true;
do
	let cnt+=1
	array+=(1 2 3 4 5 6 7 8 9 10)
	[ $(($cnt%100000)) -eq 0 ] && echo ${#array[@]} >> report.log
done
