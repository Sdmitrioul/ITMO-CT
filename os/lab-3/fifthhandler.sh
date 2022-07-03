#!/bin/bash

touch operationsfifth

num=1
mode="+"

tail -n 0 -f operationsfifth |
while true
do
    read line
    case $line in
        "+")
            mode="+"
            ;;
        "*")
            mode="*"
            ;;
        "QUIT")
            echo "QUIT"
            killall tail
            exit
            ;;
        [0-9]*)
            case $mode in
                "+")
                    echo $num " + " $line
                    num=$(($num + $line))
                    echo " = " $num
                    ;;
                "*")
                    echo $num " * " $line
                    num=$(($num * $line))
                    echo " = " $num
                    ;;
            esac
            ;;
        *)
            echo "InputException: input isn't correct"
            killall tail
            exit 1
            ;;
    esac
done
