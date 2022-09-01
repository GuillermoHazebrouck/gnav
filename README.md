# G-NAV
G-NAV is an electronic flight instrument system (EFIS) for soaring based on Raspberry Pi. G-NAV is made by assembling together standard hardware and software modules, which means you can build (and repair) your own kit without little budget and without requiring a lot of technical skills.

> **NOTE:** G-NAV is not certified as an IFR equipment. Although a best effort is done to make the system as resilient and reliable as possible, you should never use this device as primary source of navigation or collision avoidance.

## Gavilan
The software solution is composed of the Gavilan graphical application and possibly a middleware data aquisition system.
The development of Gavilan is the main focus of this project. The software that is required in the external hardware (SoftRF, Stratux, etc.) is not developed here.
Gavilan is entirely written in Ada and it is powered by OpenGL and GLFW. It can run on any Linux system, including the Raspberry Pi (models 3 and 4).
Make sure to use a recent version of the Raspberry Pi OS. Both, 32 and 64 bit versions are supported.

## Recommended hardware
For the hardware part, check our [project blog](https://sites.google.com/view/thegavilanproject/main).
In short, the proposed hardware is made of:
- Raspberry Pi 4B (4GB RAM recommended).
- Raspberry 7" standard screen.
- Hardware encasing (3D printed or standard).
- 16 or 32 GB good quality micro SD.
- Data aquisition USB dongles (providing GPS, Flarm and other capabilities).

### SoftRF dongles:
Using the SoftRF dongles (T-Beam or T-Motion) directly is the default startup option for Gavilan and does not require of any middleware. Just plug one of the dongles on a free USB port and the application will start capturing and processing the NMEA and FLARM dataframes (make sure the dongle is correctly configured for NMEA/FLARM).

### Other possible solutions for data acquisition:
Any device that provides dataframes via UDP or SERIAL in NMEA/FLARM or G-NAV formats will work, for example:
- You can plug a GPS receiver via USB (like an U-BLOX) and have only positioning data in NMEA format.
- You can mount Stratux on a second Raspberry Pi or on the same Raspberry Pi as Gavilan and have your own middleware to generate a UDP datastream that Gavilan can capture and interprete. This should be a data format translator that pumps NMEA/FLARM or G-NAV dataframes on the Gavilan UDP socket.

## Installation of Gavilan on Raspberry Pi
To install the Gavilan application on your Rasperry Pi, simply clone this repository and then run the ./install.sh script:
```
$ git clone github.com/GuillermoHazebrouck/gnav/
$ cd gnav/Gavilan/
$ chmod +x install.sh
$ sudo ./install.sh
```
In the last command, use DEVEL as option for local development. Patches and replays can be done directly on the Raspberry Pi without the need of an extra PC.
When Gavilan is installed without the DEVEL option, it will run after each reboot in maximized screen size and it will restart automatically after any malfunction.

## Installation of Gavilan on your Linux PC
If you want to make serious changes to Gavilan, it is recommended you do it from a Linux PC. In that case, do not run the `install.sh` script, but simply use your package manager to install the OpenGL and GLFW development libraries. The Ada compiler can be obtained from [GNAT](https://www.adacore.com/download). It is recommended to use the Gnat community edition.

## Running a recorded file
Every time the progam is launched, a recording is started (independently on the type of data streaming). The recording files are stored in $GNAV_PATH/Gavilan/bin/replay/.

To replay a recorded file you need to set the GAVILAN_OPTIONS environmental variable to indicate Gavilan that the data stream is a file:
```
GAVILAN_OPTIONS=FILE_STREAM=<your file name>.dat ./run_gavilan.sh DEVEL
```

## Testing G-NAV in FlightGear

If you want to test the G-NAV computer in a simulated environment (to see if it is something for you before buying the hardware), you can use FlightGear in your Linux PC. This is actually very simple:

1. Install FlightGear.
2. Install G-NAV.
3. Copy the Gavilan/etc/gavilan_protocol.xml file into the FlightGear generic protocol configuration folder (typically "/usr/share/games/flightgear/Protocol/").
4. Launch FlightGear using the next options (note that FlightGear also provides a textbox in the graphic interface for this):
```
--generic=socket,out,4,gha_lnx,4000,udp,gavilan_protocol
```
5. Launch Gavilan with the command:
```
export GAVILAN_OPTIONS="UDP_STREAM=4000 PROTOCOL=G-NAV"; ./run_gavilan DEVEL
```

