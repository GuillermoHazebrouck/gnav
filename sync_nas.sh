#!/bin/bash
if [ "$1" == "to" ]
then
   echo "sending to nas..."
   rm -r Software/obj/*
   rsync -a --delete --progress "$PWD/" "Guille@192.168.1.35:/volume1/Guillermo/Programas/gnav"
elif [ "$1" == "from" ]
then
    echo "receiving from nas..."
    rsync -a --delete --progress "Guille@192.168.1.35:/volume1/Guillermo/Programas/gnav/" $PWD    
fi
