program DisplaySPI;
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
  This example illustrates usage of SPI protocol and drawing on a TFT display with HX8357 driver.

  Attention! Please follow these instructions before running the sample:

   1. PinRST and PinDC constants should contain the corresponding pin numbers to which these lines are connected.

   2. Remember to copy accompanying files "corbel.font", "consolas.font" and "smalllenna.png" from the 
      folder \Samples\FreePascal\Ultibo\Media to your SD card as well.

   3. Check the accompanying diagram and photo to see an example on how this can be connected on Raspberry PI.
   
  The example has been created for a Raspberry Pi 2 but will also run on a Raspberry Pi 2.
  
  To convert ths example to Raspberry Pi A/B/A+/B+/Zero create a new project then copy and
  paste this code into it taking care to adjust the RaspberryPi2 unit in the uses clause as
  required.
}

{$mode delphi}{$H+}

uses
  RaspberryPi2, 
  GlobalConst,
  Threads,
  Console, 
  SysUtils,
  PXL.TypeDef,
  PXL.Types,
  PXL.Fonts,
  PXL.Boards.Types,
  PXL.Boards.Ultibo,
  PXL.Displays.Types,
  PXL.Displays.HX8357;
  
const
  // Please make sure to specify the following pins according to Raspberry PI pin numbering scheme.
  PinRST = Integer(GPIO_PIN_UNKNOWN);
  PinDC = Integer(GPIO_PIN_25);
  
type
  TApplication = class
  private
    FSystemCore: TUltiboSystemCore;
    FGPIO: TUltiboGPIO;
    FDataPort: TCustomDataPort;
    FDisplay: TCustomDisplay;

    FDisplaySize: TPoint2px;

    FFontCorbel: Integer;
    FFontConsolas: Integer;
    FImageLenna: Integer;

    procedure LoadGraphics;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute;
  end;

constructor TApplication.Create;
begin
  FSystemCore := TUltiboSystemCore.Create;
  FGPIO := TUltiboGPIO.Create(FSystemCore);
  FDataPort := TUltiboSPI.Create(nil, 0, 32000000, 0);

  FDisplay := TDisplay.Create(FGPIO, FDataPort, PinDC, PinRST);

  FDisplaySize := (FDisplay as TDisplay).PhysicalSize;

  FDisplay.Initialize;
  FDisplay.Clear;

  LoadGraphics;
end;

destructor TApplication.Destroy;
begin
  FDataPort.Free;
  FGPIO.Free;
  FSystemCore.Free;

  inherited;
end;

procedure TApplication.LoadGraphics;
const
  CorbelFontName: StdString = 'C:\corbel.font';
  ConsolasFontName: StdString = 'C:\consolas.font';
  LennaFileName: StdString = 'C:\smalllenna.png';
begin
  FFontCorbel := FDisplay.Fonts.AddFromBinaryFile(CorbelFontName);
  if FFontCorbel = -1 then
    raise Exception.CreateFmt('Could not load %s.', [CorbelFontName]);

  FFontConsolas := FDisplay.Fonts.AddFromBinaryFile(ConsolasFontName);
  if FFontConsolas = -1 then
    raise Exception.CreateFmt('Could not load %s.', [ConsolasFontName]);

  FImageLenna := FDisplay.Images.AddFromFile(LennaFileName);
  if FImageLenna = -1 then
    raise Exception.CreateFmt('Could not image %s.', [LennaFileName]);
end;

procedure TApplication.Execute;
var
  J, I: Integer;
  Ticks: Integer = 0;
  Quad: Integer = 16;
  Omega, Kappa: Single;
begin
  ConsoleWriteLn('Showing animation on display, press any key to exit...');

  while not ConsoleKeyPressed do
  begin
    Inc(Ticks);
    FDisplay.Clear;

    // Draw some background.
    for J := 0 to FDisplaySize.Y div Quad do
      for I := 0 to FDisplaySize.X div Quad do
        FDisplay.Canvas.FillQuad(
          FloatRect4(I * Quad, J * Quad, Quad, Quad),
          IntColor4($FF101010, $FF303030, $FF585858, $FF303030));

    // Draw an animated Arc.
    Omega := Ticks * 0.0274;
    Kappa := 1.25 * Pi + Sin(Ticks * 0.01854) * 0.5 * Pi;

    FDisplay.Canvas.FillArc(
      Point2(FDisplaySize.X * 0.2, FDisplaySize.Y * 0.5),
      Point2(16.0, 15.0),
      Omega, Omega + Kappa, 16,
      IntColor4($FFA4E581, $FFFF9C00, $FF7728FF, $FFFFFFFF));

    // Draw an animated Ribbon.
    Omega := Ticks * 0.02231;
    Kappa := 1.25 * Pi + Sin(Ticks * 0.024751) * 0.5 * Pi;

    FDisplay.Canvas.FillRibbon(
      Point2(FDisplaySize.X * 0.8, FDisplaySize.Y * 0.5),
      Point2(7.0, 3.0),
      Point2(14.0, 16.0),
      Omega, Omega + Kappa, 16,
      IntColor4($FFFF244F, $FFACFF0D, $FF2B98FF, $FF7B42FF));

    // Draw the image of famous Lenna.
    FDisplay.Canvas.UseImage(FDisplay.Images[FImageLenna]);
    FDisplay.Canvas.TexQuad(FloatRect4RC(
      Point2(FDisplaySize.X * 0.5, FDisplaySize.Y * 0.5),
      Point2(64.0, 64.0),
      Ticks * 0.01),
      IntColorAlpha(128));

    // Draw some text.
    FDisplay.Fonts[FFontCorbel].DrawTextAligned(
      Point2(FDisplaySize.X * 0.5, 2.0),
      'Hello Ultibo.',
      IntColor2($FFFFE000, $FFFF0000),
      TTextAlignment.Middle, TTextAlignment.Start);

    FDisplay.Fonts[FFontConsolas].DrawText(
      Point2(1.0, FDisplaySize.Y - 14.0),
      'Frame #: ' + UnicodeString(IntToStr(Ticks)),
      IntColor2($FFD6F5FC, $FF3E0DDC));

    // Send picture to the display.
    FDisplay.Present;
  end;

  ConsoleReadKey;
end;

var
  Application: TApplication = nil;

begin
  // Create a console window
  ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_FULLSCREEN, True);

  // Wait for C: drive to be ready
  ConsoleWriteLn('Waiting for C:\ drive.');
  while not DirectoryExists('C:\') do
  begin
    // Sleep for a second
    Sleep(1000);
  end;
  ConsoleWriteLn('C:\ drive is ready.');
  
  try
    Application := TApplication.Create;
    try
      ConsoleWriteLn('Starting example.');
      
      Application.Execute;
      
      ConsoleWriteLn('Example completed.');
    finally
      Application.Free;
    end;
  except
    on E: Exception do
    begin
      ConsoleWriteLn('Exception: ' + E.Message);
    end;
  end;  
  
 ThreadHalt(0); 
end.

