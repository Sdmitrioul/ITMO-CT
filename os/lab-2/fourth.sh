#!/bin/bash

for pid in $(ps -A -o pid | tail -n +2)
do
    fileppid=/proc/$pid/status
    fileart=/proc/$pid/sched
    if [[ -f $fileppid ]]
    then
        ppid=$(grep PPid $fileppid | grep -E -o '[0-9]+')
        sum_exec_runtime=$(grep sum_exces_runtime $fileart | awk '{print $3}')
        nr_switches=$(grep nr_switches $fileart | awk '{print $3}')
        if [[ $nr_switches -ne 0 ]]
        then
            art=$(echo $sum_exces_runtime $nr_switches | awk '{print $1/$2}')
        fi
        echo 'ProcessID='$pid' : Parent_ProcessID='$ppid' : Average_Running_Time='$art' >> tmp.out
    fi
done
cat tmp.out | sort -n -k 3 > ans3.out
