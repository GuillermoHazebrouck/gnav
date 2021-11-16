#cd src
#gnatmake -D ../obj/ -aIutility -aIopengl -aIglfw -aIdisplay -aIwidgets -aImaps -lm -lx11 -lGL -lglfw gavilan.adb

gnatmake -P gavilan.gpr
mv obj/gavilan bin/gavilan
