from gpiozero import Button
from signal   import pause
from evdev    import UInput, ecodes as e

ui = UInput()
Channel_A = Button(10, False) # Wheel scroll - to 3v3
Channel_B = Button( 9, False) # Wheel scroll - to 3v3
Channel_C = Button( 3, True)  # Wheel button - to GND

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

pause()


