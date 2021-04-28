#!/usr/bin/env python
# -*- coding: utf-8 -*-
## lcdccmex.py
## Tinkerforge LCD Custom Character example - Python Script
## Tested with Master Brick 2.1, LCD 20x4 Bricklet 1.2
## IMPORTANT=Change #UID# to the UID of the LCD 20x4 Bricklet

HOST = "localhost"
PORT = 4223
UID = "#UID#"

from tinkerforge.ip_connection import IPConnection
from tinkerforge.bricklet_lcd_20x4 import BrickletLCD20x4

if __name__ == "__main__":
    # Create IP connection
    ipcon = IPConnection()
    # Create device object
    lcd = BrickletLCD20x4(UID, ipcon)
    # Connect to brickd
    ipcon.connect(HOST, PORT)
    # Turn backlight on
    lcd.backlight_on()
    # Clear screen
    lcd.clear_display()
    # Write text at pos row=0,col=0
    lcd.write_line(0, 0, "Hello World")
    # Define custom character
    #CUSTOMCHARNAME# = [#CUSTOMCHARPIXELS#]
    # Set the custom char at index 0 (from max 7)
    lcd.set_custom_character(0, #CUSTOMCHARNAME#)
    # Write custom char at pos row=1,col=0
    lcd.write_line(1,0,"\u0008")
    # Disconnect
    ipcon.disconnect()
    