#!/bin/bash

d=$(date +"%F_%R")
mkdir ~/test && echo "catalog test was created successfully" >> ~/report.tmp ; echo > ~/test/$d.tmp
ping -c 1 "www.net_nikogo.ru" 2>> ~/report
