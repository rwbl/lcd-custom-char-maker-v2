#!/bin/bash
# lcdccmex.sh
# Tinkerforge LCD Custom Character example - Bash script
# Tested with Master Brick 2.1, LCD 20x4 Bricklet 1.2, Tinkerforge MQTT2 API
# IMPORTANT:
# Change #UID# to the UID of the LCD 20x4 Bricklet
# Save the file as uNIX to be able to run after making executable: sudo chmod +x lcdcustomcharex.sh

echo Set custom character
mosquitto_pub -t tinkerforge/request/lcd_20x4_bricklet/#UID#/set_custom_character -m '{"index":0, "character":[#CUSTOMCHARPIXELS#]}'

echo Set backlight on
mosquitto_pub -t tinkerforge/request/lcd_20x4_bricklet/#UID#/backlight_on -m ''

echo Clear display
mosquitto_pub -t tinkerforge/request/lcd_20x4_bricklet/#UID#/clear_display -m ''

echo Write text followed by custom character
mosquitto_pub -t tinkerforge/request/lcd_20x4_bricklet/#UID#/write_line -m '{"line": 0, "position": 0, "text": "#CUSTOMCHARNAME#: \u0008"}'

echo Done

