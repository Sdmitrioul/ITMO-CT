#!/bin/bash

man bash | grep -o -i "[a-z]\{4,\}" | sort | uniq -c | sort -r -n | head -3 | awk '{ print($2) }'
