#!/bin/bash

GAVILAN_PATH=~/Gavilan/bin

if [ "$GAVILAN_OPTIONS" == "" ]
then
    GAVILAN_OPTIONS="SERIAL_STREAM" #"UDP_STREAM=4000"
fi

echo "Starting Gavilan ($GAVILAN_OPTIONS)"

# Protect agains multiple startup attempts
#---------------------------------------------
if [ "$(pidof gavilan)" != "" ]
then

    echo "(the program is already running)"
    exit 0
    
fi

# Launch for development
#---------------------------------------------
if [ "$1" == "DEVEL" ]
then
    
    echo "(development mode)"
    
    ./gavilan $GAVILAN_OPTIONS
    
    exit $?
        
# Launch for operational use
#---------------------------------------------
else

    echo "(operational mode)"

    cd $GAVILAN_PATH
    
    # Start front panel in the background
    #-------------------------------------
    sudo python frontpanel.py &
        
    # Use patched mesa library
    #-------------------------------------
    export LD_LIBRARY_PATH=/opt/mesa/lib/arm-linux-gnueabihf
    
    # Recover the application after errors
    # and turn off system when the user
    # closes the application.
    #-------------------------------------
    while : 
    do
        ./gavilan $GAVILAN_OPTIONS
        EXIT_CODE=$?
        if [ "$EXIT_CODE" == "9" ]
        then
            sudo shutdown -h now
            exit $EXIT_CODE
        fi;
    done  
    
fi
