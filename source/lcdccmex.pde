// lcdccmex.pde
// Arduino CPP LCD Custom Character example 
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
uint8_t #CUSTOMCHARNAME#[8]  = {#CUSTOMCHARPIXELS#};
// Create the lcd object - Address (1602 = 0x3F,2004 = 0x27)
LiquidCrystal_I2C lcd(0x27,20,4);

void setup()
{
  // Init the lcd and turn the backlight on
  lcd.init();
  lcd.backlight();
  // Create the custom character at index 0
  lcd.createChar(0, #CUSTOMCHARNAME#);
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
