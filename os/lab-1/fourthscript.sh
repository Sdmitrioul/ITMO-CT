#!/bin/bash

if [[ "$PWD" == "$HOME" ]]
then
    echo "$HOME"
    exit 0
else
    echo "You're not in the home directory ($PWD)"
    exit 1
fi
