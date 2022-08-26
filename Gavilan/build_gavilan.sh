# Make sure there is a folder where to dump the objects
#------------------------------------------------------
if [ ! -d "obj" ]; then
    mkdir obj
fi

# Compile and link the executable
#------------------------------------------------------
cd src
gnatmake gavilan.adb -aIopengl -aIglfw -aItiming -aIstacks -aImath -aIutility -aImaps -aIflight -aIdisplay -Idisplay/pages -aIwidgets  -D ../obj/ -largs -lGL -lglfw

# Copy executable to bin folder
#------------------------------------------------------
if [ -f "gavilan" ]; then
    mv gavilan ../bin/gavilan
    echo "done!"
else
    echo "compilation failed"
fi
