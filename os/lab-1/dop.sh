#!/bin/bash

if [[ $# -ne 12 ]]
then
    echo "Exception: wrong number of args"
    echo "Input: $#; Neaded 12"
else
    for param in "$@"
    do
    let ans=$RANDOM\*$param
    echo "Обработка $param дала $ans"
    done
fi
