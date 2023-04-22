# Make sure there is a folder where to dump the objects
#---------------------------------------------------------------------------------
if [ ! -d "obj" ]; then
    mkdir obj
fi

if [[ $3 == "REBUILD" ]] || [[ $2 == "REBUILD" ]]; then
    rm -f obj/*
    rm -f lib/*
fi
    
# Compile and link the lib or executable (default to GLFW EXE)
#---------------------------------------------------------------------------------
cd src

GNAV_SRC="-aIopengl 
          -aIdisplay 
          -Idisplay/pages 
          -Idisplay/panels 
          -aItiming 
          -aIstacks 
          -aImath 
          -aIutility 
          -aImaps 
          -aIflight 
          -aIwidgets"

GLFW_SRC="-aIglfw"

SDL_SRC= "-aIsdl 
          -Isdl/linux 
          -Isdl/image 
          -Isdl/ttf"

if [[ $1 == "EXE" ]] || [[ $1 == "" ]]; then

    # NOTE: for the executables you only need gnatmake (no gprbuild required)
    
    if [[ $2 == "GLFW" ]] || [[ $2 == "" ]]; then
    
        echo "************************"
        echo "Building G-NAV for GLFW"
        echo "************************"
        
        EXE_NAME="gnav_glfw"
        gnatmake "$EXE_NAME.adb" $GNAV_SRC $GLFW_SRC -D ../obj/ -largs -lGLESv2 -lglfw
        
    elif [[ $2 == "SDL" ]]; then
  
        echo "**********************"
        echo "Building G-NAV for SDL"
        echo "**********************"
        
        EXE_NAME="gnav_sdl"
        gnatmake "$EXE_NAME.adb" $GNAV_SRC $SDL_SRC -D ../obj/ -largs -lGLESv2 -lSDL2
        
    else
        echo "please, choose a base API for OpenGL (either GLFW or SDL)"
    fi

    # NOTE: the new executable is 'gavilan_update', so that it can be compiled while the app is running
    
    if [ -f $EXE_NAME ]; then
        mv -f $EXE_NAME ../bin/gnav_update
        echo "executable done!"
    else
        echo "excecutable compilation failed"
    fi
    
elif [[ $1 == "LIB" ]]; then
    
    # NOTE: gprbuild is required for the standalone library

    echo "******************************************"
    echo "Building G-NAV standalone library for Java"
    echo "******************************************"
            
    gprbuild -P gnavlib.gpr
    
fi
    
