#!/bin/bash
if [ $1 == "to" ]
then
   echo "sending to usb..."
   rm -r obj/*
   rsync -anv "$PWD/" "/run/media/$USER/$2/Gavilan"
elif [ $1 == "from" ]
then
    echo "receiving from usb..."
    if [ $3 == "all" ]
    then
        rsync -rP "/media/$USER/$2/Gavilan/" $PWD
    else
        rsync -rP "/media/$USER/$2/Gavilan/src/" "$PWD/src"
    fi
    
fi
