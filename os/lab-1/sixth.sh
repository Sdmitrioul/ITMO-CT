#!/bin/bash

grep -E "\\<WW\\>" /var/log/anaconda/X.log | sed -E "s/\\<WW\\>/Warning: /" > full.log
grep -E "\\<II\\>" /var/log/anaconda/X.log | sed -E "s/\\<WW\\>/Information: /" >> full.log
