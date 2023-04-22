#!/bin/bash

export LIBGL_ALWAYS_INDIRECT="";
export MESA_GL_VERSION_OVERRIDE="3.3";

if [ "$GNAV_PATH" == "" ]; then
    export GNAV_PATH="$HOME/gnav/Software"
fi;

GNAV_TRACE_FILE="$GNAV_PATH/trace.dat"
GNAV_SETUP_FILE="$GNAV_PATH/setup.dat"

#====================================================================
# Updates the binary and static data from a plugged USB drive
#====================================================================
upgrade () {
 
    # Replace upgraded binary
    
    echo "$(date) > checking upgrade..." >> $GNAV_TRACE_FILE
    
    if [ -f "$GNAV_PATH/bin/gnav_update" ]
    then
    
        mv -f "$GNAV_PATH/bin/gnav_update" "$GNAV_PATH/bin/gnav"
        
        echo "$(date) > upgraded" >> $GNAV_TRACE_FILE
        
    else
        
        echo "$(date) > nothing new found" >> $GNAV_TRACE_FILE
     
    fi
   
}

echo "$(date) > STARTING GNAV" >> $GNAV_TRACE_FILE

# Protect agains multiple startup attempts
#---------------------------------------------
if [ "$(pidof gnav)" != "" ]
then

    echo "$(date) > (the program is already running)" >> $GNAV_TRACE_FILE
    exit 0
    
fi

# Launch for development (never update)
#---------------------------------------------
if [ "$1" == "DEVEL" ]
then
    
    upgrade
 
    ./gnav
    
    echo "$(date) > application stopped" >> $GNAV_TRACE_FILE  
    
    exit $?
        
# Launch for operational use
#---------------------------------------------
else

    echo "$(date) > (operational mode)"   >> $GNAV_TRACE_FILE
    
    echo "$(date) > starting front panel" >> $GNAV_TRACE_FILE
    
    # Start front panel in the background
    #-------------------------------------
    sudo python frontpanel.py &
        
    # Use patched mesa library
    #-------------------------------------
    #export LD_LIBRARY_PATH=/opt/mesa/lib/arm-linux-gnueabihf
    
    # Recover the application after errors
    # and turn off system when the user
    # closes the application.
    #-------------------------------------
    while : 
    do
        cd "$GNAV_PATH/bin"
        ./gnav
        EXIT_CODE=$?
        
        # Shut down the system
        #-------------------------------------
        if [ "$EXIT_CODE" == "9" ]
        then        
            echo "$(date) > shutdown requested" >> $GNAV_TRACE_FILE  
            sudo shutdown -h now
            exit $EXIT_CODE
            
        # Reseting triggers update
        #-------------------------------------
        elif [ "$EXIT_CODE" == "8" ]
        then
            echo "$(date) > restart requested" >> $GNAV_TRACE_FILE
            upgrade
        fi
                    
        # Protect against tight loop
        #-------------------------------------
        delay 0.5
        
    done  
    
fi
