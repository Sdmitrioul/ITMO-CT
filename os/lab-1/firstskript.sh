#!/bin/bash

if [[ $# -ne 3 ]]
then
	echo "Exception: wrong number of args"
	echo "Input: $#"
else
    if [[ $1 -lt $2 ]]
    then
        if [[ $2 -lt $3 ]]
        then
            echo "$3"
        else
            echo "$2"
        fi
    else
        if [[ $1 -lt $3 ]]
        then
            echo "$3"
        else
            echo "$1"
        fi
    fi
fi
