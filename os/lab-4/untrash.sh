#!/bin/bash

trash=~/.trash
log=~/.trash.log

if [[ $# -ne 1 ]]
then
    echo "Wrong number of args: $#"
    exit 1
fi

if [[ ! -d $trash ]]
then
    echo "`Trash isn`t found`"
    exit 1
fi

for f in $(grep -sh $1 $log)
do
    count=$(echo $f | awk -F ':' '{print $1}')
    path=$(echo $f | awk -F ':' '{print $2}')
    name=$(echo $f | awk -F ':' '{print $3}')
    
    if [[ ! -e "$trash/$count" ]]
    then
        continue
    fi
    
    echo "Restore? (y/n)"
    read answer
    
    if [[ "$answer" == "y" ]]
    then
        if [[ ! -d $path ]]
        then
            echo "Directory not found, restore in home directory"
            path=$HOME
        fi
        
        while [[ -e "$path/$name" ]]
        do
            echo "File with this name alerady exist. Enter new name"
            read name
        done
        
        ln "$trash/$count" "$path/$name"
        echo "Resoring succsessfully"
        exit 0
    fi
done

echo "Nothing happens"
