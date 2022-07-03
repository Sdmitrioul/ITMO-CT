#!/bin/bash

N=0
M=0
count=0
while read line
do
    ppid=$(echo $line | grep -E -o 'Parent_ProcessID= [0-9]+')
    if [[$N -eq $ppid]]
    then
        count=$(echo $count | awk '{print $1+1}')
        line=$(echo $line | grep -E -o 'Average_Running_Time= [0-9]+,[0-9]+'| grep -E -o '[0-9]+,[0-9]*')
        M=$(echo $M $cur | awk '{print $1+$2}')
    else
        M=$(echo $M $count | awk '{print $1/$2}')
        echo 'Average_Sleeping_Children_of_ParentID=' $N ' is ' $M >> ans5.out
        N=$(echo $line | grep -E -o 'Parent_ProcessID= [0-9]+'| grep -E -o '[0-9]+')
        M=$(echo $line | grep -E -o 'Average_Running_Time= [0-9]+,[0-9]+'| grep -E -o '[0-9]+,[0-9]*')
        count=1
    fi
    echo $line >> ans5.out
done < ans4.out
M=$(echo $M $count | awk '{print $1/$2}')
echo 'Average_Sleeping_Children_of_ParentID=' $N ' is ' $M >> ans5.out
