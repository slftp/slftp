#/bin/bash

DATE="date"
bindir=$(pwd)
#run this as sudo
logfile="./restart.log"

ison=$(ps aux | grep slftp | awk '{print $11}' | grep "$bindir/slftp")

#restarting slftp
echo "$($DATE "+%d/%m/%Y %H:%M:%S"):Restarting slftp" >> "$logfile"
echo "$($DATE "+%d/%m/%Y %H:%M:%S"):slftp was not found running..." >> "$logfile"
echo "$($DATE "+%d/%m/%Y %H:%M:%S"):Starting slftp now" >> "$logfile"

while true; do
        ison=$(ps aux | grep slftp | awk '{print $11}' | grep "$bindir/slftp")
        if [ -z "$ison" ]; then
        echo "$($DATE "+%d/%m/%Y %H:%M:%S"):Restarting slftp" >> "$logfile"
        echo "$($DATE "+%d/%m/%Y %H:%M:%S"):slftp was not found running..." >> "$logfile"
        echo "$($DATE "+%d/%m/%Y %H:%M:%S"):Starting slftp now" >> "$logfile"
	./kill.sh
	sleep 5
        $bindir/slftp
        else
        echo "$($DATE "+%d/%m/%Y %H:%M:%S"):slftp is still running..." >> "$logfile"
        sleep 120
        fi
done
