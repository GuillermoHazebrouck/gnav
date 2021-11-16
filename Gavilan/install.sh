#-------------------------------------------------------------------
# GAVILAN INSTALLATION SCRIPT
#
# Function: this scrip installs the Gavilan program and default
#           data. Gavilan must be located in the /home/pi/Gavilan
#           folder to work.
#           The script will install all dependencies and compile the
#           source code. Then it will apply the necessary desktop
#           configuration to launch the program at startup in full
#           scren mode, with the front panel active and the ability to
#           shut down and restart the whole computer.
# Warning:  once the script is run, Gavilan will automatically start
#           after reboot and hide the desktop. The application
#           will also persist to restart after program failures.
#           If you dont want Gavilan to do this, remove the autostart
#           script (see step 7). If you can't find the desktop,
#           use one of the system consoles (Ctrl+Alt+F<2..6>):
#           $ rm ~/.config/lxsession/LXDE-pi/autostart
#           In Pi-OS, Ctrl+Alt+F7 is the system graphical output.
#-------------------------------------------------------------------
sudo apt update

# 1. Get dependencies
#-------------------------------------------------------------------
sudo apt-get install libgles2
sudo apt-get install libgles-dev
sudo apt-get install libglfw3
sudo apt-get install libglfw3-dev

# 2. Get GNU Ada compiler
# Note: version 7 is used since it supports gnatmake -P
#-------------------------------------------------------------------
sudo apt-get install gnat-7

# 3. Unpack custom Mesa library to opt/mesa
# NOTE: this is only necessary to fix a bug in the geometry shaders
#       in the Mesa package that comes with Pi-OS
#-------------------------------------------------------------------
sudo mv ../etc/mesa-armhf-20201107.tar.gz /
sudo tar xfz /mesa-armhf-20201107.tar.gz
sudo rm /mesa-armhf-20201107.tar.gz

# 4. Build source code
#-------------------------------------------------------------------
chmod +x build_gavilan.sh
./build_gavilan.sh

# 5. Install the evdev package for the front panel
# (this is a pyton deamon that translates inputs in keyboard entries)
# Alternative method: sudo apt-get install python-evdev
#-------------------------------------------------------------------
sudo pip install evdev

# 6. Load the window configuration for openbox
# NOTE: this will replace the local script and might affect the 
#       default desktop (which is usless for the soaring computer).
# Details:
# Since GLFW 3.2 does not provide maximize and undecorate calls,
# this XML chunk is added on the "~/.config/openbox/lxde-pi-rc.xml" file
# to undecorate and maximize the Gavilan window.
# 
# <application name="gavilan">
#     <decor>no</decor>
#     <shade>no</shade>
#     <focus>yes</focus>
#     <desktop>all</desktop>
#     <layer>normal</layer>
#     <iconic>no</iconic>
#     <skip_pager>yes</skip_pager>
#     <skip_taskbar>yes</skip_taskbar>
#     <fullscreen>yes</fullscreen>
#     <maximized>yes</maximized>    
# </application>
#
# Reminders:
# > The original xml file is taken from "/etc/xdg/openbox/rc.xml"
# > To check the exact config file read by openbox, run:
#   $ ps ax | grep openbox
#-------------------------------------------------------------------
cp ../etc/lxde-pi-rc.xml ~/.config/openbox/lxde-pi-rc.xml

# 7. Install the automatic launch scrip (to be executed on LXDE startup)
# NOTE: this will cause Gavilan to automatically launch at startup and
#       to shut down the whole system when pressing the "power off" button.
#-------------------------------------------------------------------
chmod +x ../etc/autostart
cp ../etc/autostart ~/.config/lxsession/LXDE-pi/
