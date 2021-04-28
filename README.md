# lcd-custom-char-maker-v2
Create Custom Characters for LCD displays connected to Arduino, Raspberry Pi, Tinkerforge or other.

## Functionality
* Create custom LCD character with 5 pixel horizontal (cols), 8 pixel vertical (rows).
* Each row is represented by a byte with 5 bits.
* Create DEC, HEX and BIN arrays.
* Save / open the character to / from a textfile located in the application folder.
* Import 8 bytes array string in format 0xNN,0xNN... where NN is HEX value.
* Convert LCD character table high & low bits to DEC & HEX values.
* Various language specific examples
* Some example chars in the project source/objects folder.
**Developed for personal use only under the GNU GENERAL PUBLIC LICENSE Version 3.**

## Development
* Application developed with [Lazarus](https://www.lazarus-ide.org/) v2.0.12, [Free Pascal Compiler](https://www.freepascal.org/) 3.2.0.
* Source included and well documented.
* Application executable not included. Use Lazarus to build.
* Developed and tested under both Windows 10 and Ubuntu 20.04.

## Install
Unpack the ZIP archive to a folder of choice.
Load the Lazarus project file **lcdccm.lpi** in the Lazarus IDE and build.

## Example Pixel Arrays Custom Character Battery
For a character 5x8, the 8 entries of the array are holding 5 bits for each row.
```
DEC: 14,27,17,17,17,17,17,31
HEX: 0x0E,0x1B,0x11,0x11,0x11,0x11,0x11,0x1F
BIN: 0b01110,0b11011,0b10001,0b10001,0b10001,0b10001,0b10001,0b11111
```

## Generated Code Examples
### CPP
```
// lcdcustomcharex.pde
// Arduino CPP LCD Custom Char example 
// Tested with Arduino UNO, MEGA and I2C LCD Display 20x4 (LCD2004)
// Additional Libraries: LiquidCrystal_I2C - LiquidCrystal Arduino library for DFRobot I2C LCD displays (https://github.com/marcoschwartz/LiquidCrystal_I2C)
// Wiring: Arduino = LCD I2C: GND = GND,5v = VCC,SDA = SDA,SCL = SCL
#End Region

#include <Wire.h>
#include <LiquidCrystal_I2C.h>

#if defined(ARDUINO) && ARDUINO >= 100
#define printByte(args)  write(args);
#else
#define printByte(args)  print(args,BYTE);
#endif

// Define the custom character starting with index 0 (of max 7)
uint8_t battery[8]  = {0b01110,0b11011,0b10001,0b10001,0b10001,0b10001,0b10001,0b11111};
// Create the lcd object - Address (1602 = 0x3F,2004 = 0x27)
LiquidCrystal_I2C lcd(0x27,20,4);

void setup()
{
  // Init the lcd and turn the backlight on
  lcd.init();
  lcd.backlight();
  // Create the custom character at index 0
  lcd.createChar(0, battery);
  // Move to home position and write some text
  lcd.home();
  lcd.print("Hello World");
  // Set the cursor at col 0 of line 2 (index 1)
  lcd.setCursor(0, 1);
  // write the custom character with index 0
  lcd.printByte(0);
}

void loop() {

}
```

### B4R
```
#Region Notes
'lcdcustomcharex.b4r
'B4R LCD Custom Char example 
'Tested with Arduino UNO, MEGA and I2C LCD Display 20x4 (LCD2004)
'Additional Libraries: rLiquidCrystalI2CEx for DFRobot I2C LCD displays (see www.b4x.com >B4R Forum >B4R Libraries)
'Wiring: Arduino = LCD I2C: GND = GND,5v = VCC,SDA = SDA,SCL = SCL
#End Region

#Region Project Attributes
    #AutoFlushLogs: True
    #CheckArrayBounds: True
    #StackBufferSize: 300
#End Region

Sub Process_Globals
    Public serialLine As Serial
    Private serialLineBaudRate As ULong = 115000
    Private lcd As LiquidCrystalI2CEX
    ' Special characters with location index 0,1...
    Private locationbattery As Byte = 0
    Private arraybattery() As Byte = Array As Byte (0x0E,0x1B,0x11,0x11,0x11,0x11,0x11,0x1F)
End Sub

Private Sub AppStart
    serialLine.Initialize(serialLineBaudRate)
    Log("AppStart")
    'Init the LCD: Address (1602 = 0x3F,2004 = 0x27), Columns, Rows
    lcd.Initialize(0x27, 20, 4)
    'lcd.Initialize(0x3F, 16, 2)
    'Turn the backlight on (default is turned off), set cursor 0,0, write some text
    lcd.Backlight = True
    'Write text at row 1 (index 0)
    lcd.SetCursor(0,0)
    lcd.Write("Hello World")
    'Define Special Chars
    lcd.CreateChar(locationbattery, arraybattery)
    'Write Special Chars at row 2 (index 1)
    lcd.WriteCharAt(0,1,locationbattery)
End Sub
```

### Python Tinkerforge
The LCD 20x4 Bricklet supports up to 8 custom characters 0-7 which are mapped to \u0008-\u000F (\u takes hex numbers).
Tested defining and writing custom characters with an LCD 20x4 v1.2 Bricklet having UID=BHN.
```
#!/usr/bin/env python
# -*- coding: utf-8 -*-
#lcdcustomcharex.py
#Tinkerforge LCD Custom Char example  Python
#Tested with Master Brick 2.1, LCD 20x4 Bricklet 1.2
#IMPORTANT=Change #UID# to the UID of the LCD 20x4 Bricklet

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
    battery = [14,27,17,17,17,17,17,31]
    # Set the custom char at index 0 (from max 7)
    lcd.set_custom_character(0, battery)
    # Write custom char at pos row=1,col=0
    lcd.write_line(1,0,"\u0008")
    # Disconnect
    ipcon.disconnect()
```

### MQTT Tinkerforge
```
#!/bin/bash
# lcdcustomcharex.sh
# Tinkerforge LCD Custom Char example - Bash script
# Tested with Master Brick 2.1, LCD 20x4 Bricklet 1.2, Tinkerforge MQTT2 API
# IMPORTANT:
# Change #UID# to the UID of the LCD 20x4 Bricklet
# Save the file as uNIX to be able to run after making executable: sudo chmod +x lcdcustomcharex.sh

echo Set custom character
mosquitto_pub -t tinkerforge/request/lcd_20x4_bricklet/#UID#/set_custom_character -m '{"index":0, "character":[14,27,17,17,17,17,17,31]}'

echo Set backlight on
mosquitto_pub -t tinkerforge/request/lcd_20x4_bricklet/#UID#/backlight_on -m ''

echo Clear display
mosquitto_pub -t tinkerforge/request/lcd_20x4_bricklet/#UID#/clear_display -m ''

echo Write text followed by custom character
mosquitto_pub -t tinkerforge/request/lcd_20x4_bricklet/#UID#/write_line -m '{"line": 0, "position": 0, "text": "battery: \u0008"}'

echo Done
```

### Hint
To write a character from the character table of the LCD Display driver.
The Degree Character ° is located at position upper 4 bits 1101 and lower 4 bits 1111.
The 8 bits 1101 1111 are HEX DF and DEC 223.
To write the character to the LCD use lcd.Write(Array As Byte(223)).

### LCD Character Table
Convert Upper 4 & Lower 4 bits to HEX and Unicode
Example Cent Character: Upper 4 bits = 1110, Lower 4 bits = 1111
Converted:
11101111 = EF , 0xEF , \u00EF, 239
Use the LCD display datasheet accordingly.

## Licence
This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
You should have received a copy of the GNU General Public License along with the samples.  If not, see [GNU Licenses](http://www.gnu.org/licenses/).
