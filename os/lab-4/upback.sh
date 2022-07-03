
#!/bin/bash

backup="/home/user/"
sourceDir="/home/user/restore"

if [[ ! -d "$sourceDir"]]
then
    mkdir $sourceDir
fi

latest=$(find $backup"Backup-"* -maxdepth 0 | sort -n | tail -n 1) "/"
for f in $(ls $latest -1)
do
    if [ $(echo $f | grep -E -o "Backup-[0-9]{4}-[0-9]{2}-[0-9]{2}") == "" ]
    then
        if [ $(ls $latest$f.* 2>/dev/null | grep -E -o "[0-9]{4}-[0-9]{2}-[0-9]{2}") == "" ]
        then
            cp $latest$f $restore$f
        fi
    fi
done

exit 0
