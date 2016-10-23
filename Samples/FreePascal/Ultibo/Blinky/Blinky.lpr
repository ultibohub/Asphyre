program Blinky;
{
  This file is part of Asphyre Framework, also known as Platform eXtended Library (PXL).
  Copyright (c) 2000 - 2016  Yuriy Kotsarenko
  Ultibo port Copyright (c) 2016 Garry Wood <garry@softoz.com.au>
  
  The contents of this file are subject to the Mozilla Public License Version 2.0 (the "License");
  you may not use this file except in compliance with the License. You may obtain a copy of the
  License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
  KIND, either express or implied. See the License for the specific language governing rights and
  limitations under the License.
}
{
  This example illustrates a simple use of GPIO with Ultibo on Raspberry Pi to blink a LED.

  Make sure to connect the LED to RPi physical pin #16 (GPIO pin 23), please take a look at the accompanying diagram and photo.
  
  The example has been created for a Raspberry Pi 2 but will also run on a Raspberry Pi 2.
  
  To convert ths example to Raspberry Pi A/B/A+/B+/Zero create a new project then copy and
  paste this code into it taking care to adjust the RaspberryPi2 unit in the uses clause as
  required.
}
uses
  RaspberryPi2, 
  GlobalConst, 
  Threads,
  Console, 
  PXL.Boards.Types, 
  PXL.Boards.Ultibo;

const
  PinLED = GPIO_PIN_23;

var
  SystemCore: TUltiboSystemCore = nil;
  GPIO: TUltiboGPIO = nil;
  TurnedOn: Boolean = False;
begin
  // Create a console window
  ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_FULLSCREEN, True);

  SystemCore := TUltiboSystemCore.Create;
  try
    GPIO := TUltiboGPIO.Create(SystemCore);
    try
      // Switch LED pin for output.
      GPIO.PinMode[PinLED] := TPinMode.Output;

      ConsoleWriteLn('Blinking LED, press any key to exit...');

      while not ConsoleKeyPressed do
      begin
        TurnedOn := not TurnedOn;

        if TurnedOn then
          GPIO.PinValue[PinLED] := TPinValue.High
        else
          GPIO.PinValue[PinLED] := TPinValue.Low;

        SystemCore.Delay(500000); // wait for 500 ms
      end;

      // Eat the key pressed so it won't go to terminal after we exit.
      ConsoleReadKey;

      // Turn the LED off after we are done and switch it to "Input" just to be safe.
      GPIO.PinMode[PinLED] := TPinMode.Input;
    finally
      GPIO.Free;
    end;
  finally
    SystemCore.Free;
  end;
  
  ThreadHalt(0); 
end.

