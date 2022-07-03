#!/bin/bash

at -f ./first.sh now + 2 minutes
tail -n 0 -f ~/report.tmp
