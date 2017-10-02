program DisplaySPIAndGPU;
{
  This file is part of Asphyre Framework, also known as Platform eXtended Library (PXL).
  Copyright (c) 2000 - 2016  Yuriy Kotsarenko
  Copyright (c) 2016 Garry Wood (garry@softoz.com.au)

  The contents of this file are subject to the Mozilla Public License Version 2.0 (the "License");
  you may not use this file except in compliance with the License. You may obtain a copy of the
  License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
  KIND, either express or implied. See the License for the specific language governing rights and
  limitations under the License.
}
{
  This example illustrates usage of Raspberry PI integrated GPU for rendering off-screen scene, which
  can then be uploaded to an external HX8357-driven display using SPI protocol.

  Attention! Please follow these instructions before running the sample:

   1. Remember to copy accompanying files "Lenna.png" and "Tahoma9b.font" from the
      folder \Samples\Media to your SD card as well.

  The example has been created for a Raspberry Pi 2 but will also run on a Raspberry Pi 3.
  
  To convert this example to Raspberry Pi A/B/A+/B+/Zero create a new project then copy and
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
  Classes,
  SysUtils, 
  PXL.TypeDef,
  PXL.Types,
  PXL.ImageFormats,
  PXL.ImageFormats.FCL,
  PXL.Boards.Types,
  PXL.Boards.Ultibo,
  PXL.Displays.Types,
  PXL.Displays.HX8357,
  PXL.Bitmaps,
  PXL.Devices,
  PXL.Canvas,
  PXL.SwapChains,
  PXL.Images,
  PXL.Fonts,
  PXL.Providers,
  PXL.Providers.GLES,
  PXL.Devices.GLES.Ultibo,
  VC4;                {Include the VC4 unit to enable OpenGL ES support}
  
const
  // Please make sure to specify the following pins according to Raspberry PI pin numbering scheme.
  // These values are valid for an Adafruit PiTFT 3.5" LCD (https://www.adafruit.com/products/2441).
  // Adjust as required for other displays using the same HX8357 chipset.
  PinDC = Integer(GPIO_PIN_25);
  PinRST = Integer(GPIO_PIN_UNKNOWN);
  
type
  TApplication = class
  private
    FImageFormatManager: TImageFormatManager;
    FImageFormatHandler: TCustomImageFormatHandler;

    FAcceleratedProvider: TGraphicsDeviceProvider;

    FAcceleratedDevice: TGLESDevice;
    FAcceleratedImages: TAtlasImages;
    FAcceleratedFonts: TBitmapFonts;

    FCurrentFrame: Integer;

    FImageLenna: Integer;
    FFontTahoma: Integer;

    FHardwareBitmap: TBitmap;
    FSoftwareBitmap: TBitmap;

    FDisplay: TCustomDisplay;
    FSystemCore: TUltiboSystemCore;
    FGPIO: TUltiboGPIO;
    FDataPort: TCustomDataPort;

    procedure RenderSceneGPU;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute;
  end;

constructor TApplication.Create;
begin
  inherited;
  
  FSystemCore := TUltiboSystemCore.Create;
  FGPIO := TUltiboGPIO.Create(FSystemCore);
  FDataPort := TUltiboSPI.Create(nil, TUltiboSPI.DefaultChipSelect, 32000000);

  // Create and configure embedded display.
  FDisplay := TDisplay.Create(FGPIO, FDataPort, PinDC, PinRST);
  FDisplay.LogicalOrientation := TDisplay.TOrientation.Landscape;

  // Create infrastructure for loading and saving different image formats.
  FImageFormatManager := TImageFormatManager.Create;
  FImageFormatHandler := TFCLImageFormatHandler.Create(FImageFormatManager);

  // Create and configure GPU rendering provider.
  FAcceleratedProvider := TGLESProvider.Create(FIMageFormatManager);

  FAcceleratedDevice := FAcceleratedProvider.CreateDevice as TGLESDevice;
  FAcceleratedDevice.SwapChains.Add(INVALID_HANDLE_VALUE, FDisplay.PhysicalSize);
  
  if not FAcceleratedDevice.Initialize then
    raise Exception.Create('Failed to initialize PXL Device.');

  // Create and configure GPU images and fonts.
  FAcceleratedImages := TAtlasImages.Create(FAcceleratedDevice);

  FImageLenna := FAcceleratedImages.AddFromFile('C:\Lenna.png');
  if FImageLenna = -1 then
    raise Exception.Create('Could not load Lenna image.');

  FAcceleratedFonts := TBitmapFonts.Create(FAcceleratedDevice);

  FFontTahoma := FAcceleratedFonts.AddFromBinaryFile('C:\Tahoma9b.font');
  if FFontTahoma = -1 then
    raise Exception.Create('Could not load Tahoma font.');

  // GPU hardware-accelerated bitmap.
  FHardwareBitmap := TBitmap.Create(FAcceleratedDevice);
  FHardwareBitmap.SetSize(FDisplay.LogicalSize);

  // Display-bound bitmap that uses software rendering.
  FSoftwareBitmap := TBitmap.Create(FDisplay.Device);
end;

destructor TApplication.Destroy;
begin
  FSoftwareBitmap.Free;
  FHardwareBitmap.Free;
  FAcceleratedFonts.Free;
  FAcceleratedImages.Free;
  FAcceleratedDevice.Free;
  FAcceleratedProvider.Free;
  FImageFormatHandler.Free;
  FImageFormatManager.Free;
  FDisplay.Free;
  FDataPort.Free;
  FGPIO.Free;
  FSystemCore.Free;

  inherited;
end;

procedure TApplication.RenderSceneGPU;
var
  J, I: Integer;
  Omega, Kappa: Single;
begin
  FHardwareBitmap.Canvas.BeginScene;
  try
    // Draw gray background.
    for J := 0 to FHardwareBitmap.Height div 40 do
      for I := 0 to FHardwareBitmap.Width div 40 do
        FHardwareBitmap.Canvas.FillQuad(
          FloatRect4(I * 40, J * 40, 40, 40),
          IntColor4($FF585858, $FF505050, $FF484848, $FF404040));

    for I := 0 to FHardwareBitmap.Width div 40 do
      FHardwareBitmap.Canvas.Line(
        Point2(I * 40.0, 0.0),
        Point2(I * 40.0, FHardwareBitmap.Size.Y),
        $FF555555);

    for J := 0 to FHardwareBitmap.Height div 40 do
      FHardwareBitmap.Canvas.Line(
        Point2(0.0, J * 40.0),
        Point2(FHardwareBitmap.Size.X, J * 40.0),
        $FF555555);

    // Draw an animated hole.
    FHardwareBitmap.Canvas.QuadHole(
      Point2(0.0, 0.0),
      FHardwareBitmap.Size,
      Point2(
        FHardwareBitmap.Width * 0.5 + Cos(FCurrentFrame * 0.073) * FHardwareBitmap.Width * 0.25,
        FHardwareBitmap.Height * 0.5 + Sin(FCurrentFrame * 0.0312) * FHardwareBitmap.Height * 0.25),
      Point2(80.0, 100.0),
      $20FFFFFF, $80955BFF, 16);

    // Draw the image of famous Lenna.
    FHardwareBitmap.Canvas.UseImage(FAcceleratedImages[FImageLenna]);
    FHardwareBitmap.Canvas.TexQuad(FloatRect4RC(
      TPoint2(FHardwareBitmap.Size) * 0.5,
      Point2(300.0, 300.0),
      FCurrentFrame * 0.1),
      IntColorAlpha(128));

    // Draw an animated Arc.
    Omega := FCurrentFrame * 0.274;
    Kappa := 1.25 * Pi + Sin(FCurrentFrame * 0.1854) * 0.5 * Pi;

    FHardwareBitmap.Canvas.FillArc(
      Point2(FHardwareBitmap.Width * 0.15, FHardwareBitmap.Height * 0.8),
      Point2(75.0, 50.0),
      Omega, Omega + Kappa, 32,
      IntColor4($FFFF0000, $FF00FF00, $FF0000FF, $FFFFFFFF));

    // Draw an animated Ribbon.
    Omega := FCurrentFrame * 0.2231;
    Kappa := 1.25 * Pi + Sin(FCurrentFrame * 0.24751) * 0.5 * Pi;

    FHardwareBitmap.Canvas.FillRibbon(
      Point2(FHardwareBitmap.Width * 0.75, FHardwareBitmap.Height * 0.8),
      Point2(25.0, 20.0),
      Point2(70.0, 80.0),
      Omega, Omega + Kappa, 32,
      IntColor4($FFFF0000, $FF00FF00, $FF0000FF, $FFFFFFFF));

    // Draw some text.
    FAcceleratedFonts.Canvas := FHardwareBitmap.Canvas;

    FAcceleratedFonts[FFontTahoma].DrawText(
      Point2(4.0, 4.0),
      'GPU rendering on SPI display demo application.',
      IntColor2($FFFFE887, $FFFF0000));

    FAcceleratedFonts[FFontTahoma].DrawText(
      Point2(4.0, 24.0),
      'Technology: ' + UniString(GetFullDeviceTechString(FAcceleratedDevice)),
      IntColor2($FFE8FFAA, $FF12C312));
  finally
    FHardwareBitmap.Canvas.EndScene;
  end;
end;

procedure TApplication.Execute;
var
  Key: Char;
begin
  ConsoleWriteLn('Initializing display...');
  FDisplay.Initialize;
  
  ConsoleWriteLn('Showing animation on display, press ESC to stop...');

  while true do
  begin
    if ConsoleKeyPressed then
    begin
      Key := ConsoleReadKey;
      if Key = #27 then
        Break;
    end;

    // Render scene using GPU.
    RenderSceneGPU;

    { Copy from hardware-accelerated bitmap to software bitmap. Note that software bitmap is bound to the same
      provider as the display, so it can be used for drawing there. Trying to draw hardware-accelerated bitmap on the
      display directly would not work as they are not bound to the same provider. }
    FSoftwareBitmap.CopyFrom(FHardwareBitmap);

    // Draw software bitmap on display.
    FDisplay.Canvas.UseImage(FSoftwareBitmap);
    FDisplay.Canvas.TexQuad(FloatRect4(0.0, 0.0, FSoftwareBitmap.Width, FSoftwareBitmap.Height), IntColorWhite4);

    // Present picture on the display.
    FDisplay.Present;

    Inc(FCurrentFrame);
  end;
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

