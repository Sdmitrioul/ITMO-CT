#!/bin/bash

#ps aux | awk '{ print $2 " " $9 }'| grep -E '[0-9]' | sort -k 2  |  tail -1 | awk '{ print $1}' > ans3.out

ps -Ao pid,stime | tail -n +2 | sort -k2 | head -1 | awk '{print $1}' > ans3.out
