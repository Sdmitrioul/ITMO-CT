#!/bin/bash

grep -E -h -s -o -r "[a-z0-9._-]+@[a-z0-9._-]+\.[a-z]+" /etc/* | sort | uniq > emails.lst
