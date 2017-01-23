program DisplayFB;
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
  This example illustrates drawing on the framebuffer in Ultibo.
  
  Attention! Please follow these instructions before running the sample:
  
   1. Remember to copy accompanying files "corbel.font", "consolas.font" and "lenna.png" from the 
      folder \Samples\FreePascal\Ultibo\Media to your SD card as well.
  
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
  Framebuffer,  
  Console, 
  SysUtils, 
  PXL.TypeDef, 
  PXL.Types, 
  PXL.Fonts, 
  PXL.Boards.Types, 
  PXL.Boards.Ultibo, 
  PXL.Displays.Types,
  PXL.Displays.FB.Ultibo;
  
type
  TApplication = class
  private
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
  FDisplay := TDisplay.Create;

  FDisplaySize := (FDisplay as TDisplay).PhysicalSize;

  FDisplay.Initialize;
  FDisplay.Clear;

  LoadGraphics;
end;

destructor TApplication.Destroy;
begin

  inherited;
end;

procedure TApplication.LoadGraphics;
const
  CorbelFontName: StdString = 'C:\corbel.font';
  ConsolasFontName: StdString = 'C:\consolas.font';
  LennaFileName: StdString = 'C:\lenna.png';
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
  Quad: Integer = 64;
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
      //Point2(16.0, 15.0),
      Point2(64.0, 60.0),
      Omega, Omega + Kappa, 16,
      IntColor4($FFA4E581, $FFFF9C00, $FF7728FF, $FFFFFFFF));
    
    // Draw an animated Ribbon.
    Omega := Ticks * 0.02231;
    Kappa := 1.25 * Pi + Sin(Ticks * 0.024751) * 0.5 * Pi;

    FDisplay.Canvas.FillRibbon(
      Point2(FDisplaySize.X * 0.8, FDisplaySize.Y * 0.5),
      Point2(28.0, 12.0),
      Point2(56.0, 64.0),
      Omega, Omega + Kappa, 16,
      IntColor4($FFFF244F, $FFACFF0D, $FF2B98FF, $FF7B42FF));
    
    // Draw the image of famous Lenna.
    FDisplay.Canvas.UseImage(FDisplay.Images[FImageLenna]);
    FDisplay.Canvas.TexQuad(FloatRect4RC(
      Point2(FDisplaySize.X * 0.5, FDisplaySize.Y * 0.5),
      Point2(256.0, 256.0),
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
  FramebufferDevice: PFramebufferDevice; 
  FramebufferProperties: TFramebufferProperties; 
  
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
  
  // Get framebuffer device
  FramebufferDevice := FramebufferDeviceGetDefault;
  if FramebufferDevice <> nil then
   begin
     // Sleep for a second
     Sleep(1000);
     
     // Get framebuffer properties
     FramebufferDeviceGetProperties(FramebufferDevice, @FramebufferProperties);
     
     // Release framebuffer
     FramebufferDeviceRelease(FramebufferDevice);
     
     // Adjust framebuffer properties
     if FramebufferProperties.PhysicalWidth > 640 then
       FramebufferProperties.PhysicalWidth := 640;
       
     if FramebufferProperties.PhysicalHeight > 480 then
       FramebufferProperties.PhysicalHeight := 480;
       
     FramebufferProperties.VirtualWidth := FramebufferProperties.PhysicalWidth;
     FramebufferProperties.VirtualHeight := FramebufferProperties.PhysicalHeight;
     
     // Reallocate framebuffer
     FramebufferDeviceAllocate(FramebufferDevice, @FramebufferProperties);
     
     // Recreate the console window
     ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_FULLSCREEN, True);
   end;
  
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

