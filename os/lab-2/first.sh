#!/bin/bash

ps | wc -l > ans1.out
ps | tr -s " " | awk -F " " '{ print $1 " " $4 }' >> ans1.out
