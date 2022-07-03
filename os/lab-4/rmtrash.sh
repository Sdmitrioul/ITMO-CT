#!/bin/bash

trash=~/.trash
log=~/.trash.log
counter=~/.trash.counter

if [[ $# -ne 1 ]]
then
    echo "Wrong number of args: $#"
    exit 1
fi

file="$1";

if [[ ! -f ./"$1" ]];
then
    echo "Usage: rmtrash <filename>
    echo "DESCRIPTION:";
    echo "filename: name of a existing file in current directory"
    exit 1;
fi;

if [[ ! -d trash ]];
then
    mkdir ~/.trash;
    touch log
    touch counter
    echo "0" > counter
fi;

count=$($(cat counter) + 1)

ln ./"$file" $trash/$count
rm ./"$file"

echo "$count:$(pwd):$file" >> $log
echo $count > $counter
