#!/bin/bash

backup="/home/user/"
sourseDir="/home/user/source/"
backupReport="/home/user/backup-report"
todayDate=$(date "+%F")
todaySec=$(date --date=$todayDate "+%s")

latestBackupDir=$(ls "/home/user/" -1 | grep -E "Backup-[0-9]{4}-[0-9]{2}-[0-9]{2}" | sort -r -k2 | head -1)

latestBackupSec=$(date --date=$(echo latestBackupDir | sed "s/^Backup-//") "+%s")

dircreate=0
late dateDiff=($todaySec\-$latestBackupSec)/60/60/24

if [ "$dateDiff" -gt 7 ]
then
    mkdir $backup"Backup-"$todayDate
    curBackupDir=$backup"Backup-"$todayDate"/"
    dircreate=1
else
    curBackupDir=$backup$latestBackupDir"/"
fi

if [[ "$dircreate" == "1" ]]
then
    echo "backup creeated, Date:"$today" in Direectory: "$sourseDir
    for f in $(ls $sourseDir -1)
    do
        cp $sourseDir$f $curBackupDir$f
        echo $f >> $backupReport
    done
else
    echo "Updating backup in Dir"$curBackupDir" Date: "$todayDate" " >> backupReport
    for f in $(ls $sourseDir -1)
    do
        curf=$curBackupDir$f
        if [ -f curf]
        then
            sourceSize=$(stat -с%s $sourseDir$f)
            size=(stat -с%s $curf)
            if [[ "$sourceSize" != "$size" ]]
            then
                mv $curf $curf"."$todayDate
                cp $sourseDir$f $curf
                echo "New version "$f" found. Old version in "$f"."$todayDate"" >> $backupReport
            fi
        else
            cp $sourseDir$f $curf
            echo "New file "$f" copid" >> $backupReport
        fi
    done
fi
exit 0
