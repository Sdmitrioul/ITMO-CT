#!/bin/bash

while true
do
    read line
    echo "$line" >> operationsfifth
    
    if [[ "$line" == "QUIT" ]]
    then
        echo "Exit"
        exit 0
    fi

    if [[ ! "$line" =~ [0-9]+ && "$line" != "+" && "$line" != "*" ]]
    then
        echo "InputException: input isn't correct"
        exit 1
    fi
done
