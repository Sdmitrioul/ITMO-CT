#!/bin/bash
[ ! -f sys.log ] && touch sys.log
echo '' > sys.log
pid=$(cat pids)
while true;
do
	top -b -n 1 | grep -q "^[ ]*$pid"
	[ $? -ne 0 ] && break
	top -b -n 1 | grep -E '^MiB|^[ ]*PID' >> sys.log
	top -b -n 1 | grep -E "^[ ]*$pid" >> sys.log
	echo '' >> sys.log
	top -b -n 1 | grep -E '^[ ]*[0-9]' | head -n5 >> sys.log
	echo '--------------------------------' >> sys.log
	sleep 1
done
echo 'Results: ' >> sys.log
dmesg | grep "mem.sh" | tail -n2 >> sys.log
echo 'Max size of array: ' $(cat report.log | tail -n1) >> sys.log
