#!/bin/bash

echo $$ > operationssixth

cur=1
mode="wait"

sigterm()
{
    echo "Stopped by SIGTERM signal"
    exit 0
}

sum()
{
    mode="+"
}

prod()
{
    mode="*"
}

trap 'sum' USR1
trap 'prod' USR2
trap 'sigterm' TERM

echo $cur

while true;
do
    case "$mode" in
        "+")
            cur=$(($cur + 2))
            echo $cur
            mode="wait"
            ;;
        "*")
            cur=$(($cur * 2))
            echo $cur
            mode="wait"
            ;;
        "wait")
            continue
            ;;
    esac
done
