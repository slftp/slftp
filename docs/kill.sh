#/bin/bash
DATE="date"
bindir=$(pwd)
#run this as sudo
logfile="./restart.log"

for pid in $(ps aux | grep $bindir/slftp | awk '{print $2}');
do
if [ -n "$pid" ]; then
#kills all processess
echo "$($DATE "+%d/%m/%Y %H:%M:%S"):Detecting slftp Processess" >> "$logfile"
echo "$($DATE "+%d/%m/%Y %H:%M:%S"):Terminating slftp PID $pid" >> "$logfile"
kill -9 $pid
echo "$($DATE "+%d/%m/%Y %H:%M:%S"):Waiting for Processes to terminate..." >> "$logfile"
sleep 2
fi
done

#Removing old files
echo "$($DATE "+%d/%m/%Y %H:%M:%S"):Removing old files" >> "$logfile"
rm ./slftp.kb
if [ -s ./slftp.news ]; then
echo "$($DATE "+%d/%m/%Y %H:%M:%S"):Found and Removed slftp.news" >> "$logfile"
rm ./slftp.news
else
echo "$($DATE "+%d/%m/%Y %H:%M:%S"):slftp.news does not exist" >> "$logfile"
fi
if [ -s ./slftp.log ]; then
echo "$($DATE "+%d/%m/%Y %H:%M:%S"):Found and Removed slftp.log" >> "$logfile"
rm ./slftp.log
else
echo "$($DATE "+%d/%m/%Y %H:%M:%S"):slftp.log does not exist" >> "$logfile"
fi
if [ -s ./slftp.speedstats ]; then
echo "$($DATE "+%d/%m/%Y %H:%M:%S"):Found and Removed slftp.speedstats" >> "$logfile"
rm ./slftp.speedstats
else
echo "$($DATE "+%d/%m/%Y %H:%M:%S"):slftp.speedstats does not exist" >> "$logfile"
fi
if [ -s ./slftp.kb ]; then
echo "$($DATE "+%d/%m/%Y %H:%M:%S"):Found and Removed slftp.kb" >> "$logfile"
rm ./slftp.kb
else
echo "$($DATE "+%d/%m/%Y %H:%M:%S"):slftp.kb does not exist" >> "$logfile"
fi
if [ -s ./databases/stats.db ]; then
rm ./databases/stats.db
"Found and Removed stats.db" >> "$logfile"
else
echo "$($DATE "+%d/%m/%Y %H:%M:%S"):stats.db does not exist" >> "$logfile"
fi
if [ -s ./databases/db_addpre.db ]; then
rm ./databases/db_addpre.db
 "Found and Removed db_addpre.db" >> "$logfile"
fi
