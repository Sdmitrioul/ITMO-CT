#!/bin/bash

string=""
read input

while [[ "$input" != "q" ]]
do
    string="$string $input"
    read input
done

echo "inputs: $string"
