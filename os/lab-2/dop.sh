#!/bin/bash

for i in $(ls /proc/ | grep "[0-9]\+")
do
    echo $i | readlink /proc/$i/statm | grep "libc-2.5.so" | echo $i >> dop.log
done
