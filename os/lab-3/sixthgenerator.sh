#!/bin/bash

while true;
do
read line
case "$line" in
    "+")
        kill -USR1 $(cat operationssixth)
        ;;
    "*")
        kill -USR2 $(cat operationssixth)
        ;;
    TERM)
        kill -TERM $(cat operationssixth)
        exit 0
        ;;
    *)
        continue
        ;;
esac
done
