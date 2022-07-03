#!/bin/bash

for pid in $(ps -A -o pid | tail -n +2)
do
    file=/proc/$pid/io
    if [[ -f $file ]]
    then
        bytes=$(grep read_bytes $file | grep -E -o '[0-9]+')
        echo $pid $bytes >> tmp2.out
    fi
done
sleep 60
while read line
do
    pid=$(echo $line | awk '{print $1}')
    bytes=$(echo $line | awk '{print $2}')
    file=/proc/$pid/io
    if [[ -f file ]]
    then
        bytes2=$(grep bytes $file | grep -E -o '[0-9]+')
        echo $pid ":" $(echo $bytes2 $bytes | awk '{print $1-$2}') >> tmp3.out
    fi
done < tmp2.out
cat tmp3.out | sort -nkr 2 | head -3 > ans7.out
rm tmp2.out
rm tmp3.out
