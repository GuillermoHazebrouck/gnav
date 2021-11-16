from gpiozero import Button
from signal   import pause
from evdev    import UInput, ecodes as e

ui = UInput()
Channel_A = Button(26, False) # Wheel scroll - to 3v3
Channel_B = Button(19, False) # Wheel scroll - to 3v3
Channel_C = Button(13, False) # Wheel button - to 3v3
Channel_D = Button(27, False) # Left  button - to 3v3
Channel_E = Button(03, True)  # Right button - to GND

print("front panel running")

def On_Channel_A_Down():
    if Channel_B.is_pressed:
        ui.write(e.EV_KEY, e.KEY_KPPLUS, 1)
        ui.write(e.EV_KEY, e.KEY_KPPLUS, 0)
        ui.syn()
    else:
        ui.write(e.EV_KEY, e.KEY_KPMINUS, 1)
        ui.write(e.EV_KEY, e.KEY_KPMINUS, 0)
        ui.syn()
 
Channel_A.when_pressed  = On_Channel_A_Down

def On_Channel_C_Down():
    ui.write(e.EV_KEY, e.KEY_ENTER, 1)
    ui.write(e.EV_KEY, e.KEY_ENTER, 0)
    ui.syn()
    
Channel_C.when_pressed  = On_Channel_C_Down

def On_Channel_D_Down():
    ui.write(e.EV_KEY, e.KEY_L, 1)
    ui.write(e.EV_KEY, e.KEY_L, 0)
    ui.syn()
    
Channel_D.when_pressed  = On_Channel_D_Down

def On_Channel_E_Down():
    ui.write(e.EV_KEY, e.KEY_R, 1)
    ui.write(e.EV_KEY, e.KEY_R, 0)
    ui.syn()
    
Channel_E.when_pressed  = On_Channel_E_Down

pause()


