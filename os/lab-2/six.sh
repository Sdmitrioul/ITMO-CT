#!/bin/bash

ps aux | sort -nrk 4 | head -1 | awk '{print($2" - "$4)}'
