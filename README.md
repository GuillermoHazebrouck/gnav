# G-NAV
G-NAV is an electronic flight instrument system (EFIS) for soaring based on Raspberry Pi. G-NAV is made from hardware and software. For the hardware part, check our [project blog](https://sites.google.com/view/thegavilanproject/main).

The software solution is composed of the **Gavilan** graphical application and the **Stratux** tracker. Both solutions can be installed and run independently from eachother.
The entire development of Gavilan is part of this project, while Stratux is developed by a separate team (and serves to many other purposes). 
Gavilan is written in Ada and it is powered by OpenGL and GLFW.

## Installation of Gavilan on Raspberry Pi
To install the Gavilan application on your Rasperry Pi, simply clone this repository and then run the ./install.sh script:
```
$ git clone github.com/GuillermoHazebrouck/gnav/
$ cd gnav/Gavilan/
$ chmod +x install.sh
$ sudo ./install.sh
```
When Gavilan is installed, it will run after each reboot in maximized screen size and it will restart automatically after any malfunction.

## Installation of Gavilan on your Linux PC
If you want to develop or change Gavilan, it is recommended to do it from a Linux PC. In that case, do not run the `install.sh` script, but simply use your package manager to install the OpenGL and GLFW development libraries. The Ada compiler can be obtained from [GNAT](https://www.adacore.com/download). It is recommended to use the Gnat community edition.
