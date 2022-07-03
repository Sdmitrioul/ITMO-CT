#!/bin/bash
[ ! -f sys2.log ] && touch sys2.log
echo '' > sys2.log
pid=$(cat pids)
pid2=$(cat pids2)
while true;
do
	top -b -n 1 | grep -q -E "^[ ]*$pid|^[ ]*$pid2"
	[ $? -ne 0 ] && break
	top -b -n 1 | grep -E '^MiB|^[ ]*PID' >> sys2.log
	top -b -n 1 | grep -E "[ ]*$pid|^[ ]*$pid2" >> sys2.log
	echo '' >> sys2.log
	top -b -n 1 | grep -E '^[ ]*[0-9]' | head -n5 >> sys2.log
	echo '--------------------------------' >> sys2.log
	sleep 1
done
echo 'Ending'
echo 'Results: ' >> sys2.log
dmesg | grep -E "$pid|$pid2" >> sys2.log
echo 'Max size of array: ' $(cat report.log | tail -n1) $(cat report2.log | tail -n1) >> sys2.log
