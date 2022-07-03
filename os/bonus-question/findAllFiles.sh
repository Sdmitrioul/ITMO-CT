#!/bin/bash

directory=""
temporaryFileName="SomeRandomStringfkjrnfjernfjere.txt"
echo > "$temporaryFileName"

for string in $(ls -F -R -1)
do
    if [[ -n $string && "${string: -1}" != "/" ]]
    then
        if [[ "${string: -1}" == ":" ]]
            then
                directory="$(echo $string | sed 's/.$//')/"
            else
                if [[ $string != $temporaryFileName &&  $string != "findAllFiles.sh" ]]
                then
                    echo "$directory$string" >> "$temporaryFileName"
                fi
        fi
    fi
done

wc -w $(cat "$temporaryFileName") | sort -nk1 -r | tail -n +2 |  awk '{print $2}'

rm $temporaryFileName
