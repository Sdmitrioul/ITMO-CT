#!/bin/bash

nice -n 1 ./helperFourth.sh&pid1=$!
nice -n 3 ./helperFourth.sh&pid2=$!
nice -n 5 ./helperFourth.sh&pid3=$!

cpulimit --pid=$pid1 --limit=10

echo $pid1 $pid2 $pid3

top

sleep 20

kill $pid1
kill $pid2
kill $pid3
