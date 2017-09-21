program TunnelGLES;
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
  This example was adapted for Ultibo from the FreePascal desktop example Tunnel.
  
   1. Remember to copy accompanying files "TempusSans.font" from the folder \Samples\Media to
      your SD card as well.
  
  The example has been created for a Raspberry Pi 2 but will also run on a Raspberry Pi 2.
  
  To convert this example to Raspberry Pi A/B/A+/B+/Zero create a new project then copy and
  paste this code into it taking care to adjust the RaspberryPi2 unit in the uses clause as
  required.
  
  This version of the example uses OpenGL ES and will only work on platforms where OpenGL ES
  is supported.
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
  PXL.Timing,
  PXL.Devices,
  PXL.ImageFormats,
  PXL.Canvas,
  PXL.SwapChains,
  PXL.Fonts,
  PXL.Providers,
  PXL.Bitmaps,
  
  PXL.Classes,
  PXL.Providers.GLES, {Include the GLES provider instead of the Auto provider}
  PXL.ImageFormats.Auto,
  
  VC4;                {Include the VC4 unit to enable OpenGL ES support}

type
  TMainForm = class(TObject)
    function FormCreate(Sender: TObject): Boolean;
    procedure FormDestroy(Sender: TObject);
  private
    Name: String;
    Handle: THandle;
    
    WindowX: Integer;
    WindowY: Integer;
    
    ClientWidth: Integer;
    ClientHeight: Integer;

    { private declarations }
    ImageFormatManager: TImageFormatManager;
    ImageFormatHandler: TCustomImageFormatHandler;

    DeviceProvider: TGraphicsDeviceProvider;

    EngineDevice: TCustomSwapChainDevice;
    EngineCanvas: TCustomCanvas;
    EngineFonts: TBitmapFonts;
    EngineTimer: TMultimediaTimer;

    DisplaySize: TPoint2px;
    EngineTicks: Integer;

    FontTempusSans: Integer;

    BitmapMain: TBitmap;
    BitmapCopy: TBitmap;

    procedure ApplicationIdle(Sender: TObject);

    procedure EngineTiming(const Sender: TObject);
    procedure EngineProcess(const Sender: TObject);

    procedure PrepareBitmaps;

    procedure RenderWindow;
    procedure RenderScene;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

function TMainForm.FormCreate(Sender: TObject): Boolean;  
var
  FramebufferDevice: PFramebufferDevice = nil;
  FramebufferProperties: TFramebufferProperties;
begin
  Result := False;
  
  if Length(Name) <> 0 then
  begin
   FramebufferDevice := FramebufferDeviceFindByName(Name);
   if FramebufferDevice = nil then
     FramebufferDevice := FramebufferDeviceFindByDescription(Name);
  end;
  
  if FramebufferDevice = nil then
    FramebufferDevice := FramebufferDeviceGetDefault;
    
  if FramebufferDevice = nil then
  begin
    ConsoleWriteLn('Failed to locate Framebuffer Device.');
    Exit;
  end;
  ConsoleWriteLn('Located Framebuffer Device.');
  
  if FramebufferDeviceGetProperties(FramebufferDevice, @FramebufferProperties) <> ERROR_SUCCESS then
  begin
    ConsoleWriteLn('Failed to get Framebuffer properties.');
    Exit;
  end;
  ConsoleWriteLn('Got Framebuffer properties.');
  
  Handle := THandle(FramebufferDevice);
  ClientWidth := FramebufferProperties.PhysicalWidth;
  if ClientWidth > 512 then ClientWidth := 512;
  ClientHeight := FramebufferProperties.PhysicalHeight;
  if ClientHeight > 512 then ClientHeight := 512;

  ImageFormatManager := TImageFormatManager.Create;
  ImageFormatHandler := CreateDefaultImageFormatHandler(ImageFormatManager);

  DeviceProvider := TGLESProvider.Create(ImageFormatManager); //Force use of the GLES provider
  EngineDevice := DeviceProvider.CreateDevice as TCustomSwapChainDevice;

  DisplaySize := Point2px(ClientWidth, ClientHeight);
  EngineDevice.SwapChains.Add(Handle, DisplaySize, 0, True);

  if ClientWidth < FramebufferProperties.PhysicalWidth then
    WindowX := (FramebufferProperties.PhysicalWidth - ClientWidth) div 2;
  if ClientHeight < FramebufferProperties.PhysicalHeight then
    WindowY := (FramebufferProperties.PhysicalHeight - ClientHeight) div 2;
    
  if (WindowX <> 0) or (WindowY <> 0) then
    EngineDevice.Move(0, Point2px(WindowX, WindowY));
  
  if not EngineDevice.Initialize then
  begin
    ConsoleWriteLn('Failed to initialize PXL Device.');
    Exit;
  end;
  ConsoleWriteLn('Initialized PXL Device.');

  EngineCanvas := DeviceProvider.CreateCanvas(EngineDevice);
  if not EngineCanvas.Initialize then
  begin
    ConsoleWriteLn('Failed to initialize PXL Canvas.');
    Exit;
  end;
  ConsoleWriteLn('Initialized PXL Canvas.');
  
  EngineFonts := TBitmapFonts.Create(EngineDevice);
  EngineFonts.Canvas := EngineCanvas;

  FontTempusSans := EngineFonts.AddFromBinaryFile(CrossFixFileName('C:\Media\TempusSans.font'));
  if FontTempusSans = -1 then
  begin
    ConsoleWriteLn('Could not load TempusSans font.');
    Exit;
  end;
  ConsoleWriteLn('Loaded TempusSans font.');

  EngineTimer := TMultimediaTimer.Create;
  EngineTimer.OnTimer := EngineTiming;
  EngineTimer.OnProcess := EngineProcess;
  EngineTimer.MaxFPS := 4000;

  EngineTicks := 0;

  BitmapMain := TBitmap.Create(EngineDevice);
  BitmapMain.SetSize(512, 512);

  BitmapCopy := TBitmap.Create(EngineDevice);
  BitmapCopy.Storage := TBitmapStorage.Drawable;
  
  Result := True;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  BitmapCopy.Free;
  BitmapMain.Free;
  EngineTimer.Free;
  EngineFonts.Free;
  EngineCanvas.Free;
  EngineDevice.Free;
  DeviceProvider.Free;
  ImageFormatHandler.Free;
  ImageFormatManager.Free;
end;

procedure TMainForm.ApplicationIdle(Sender: TObject);
begin
  EngineTimer.NotifyTick;
end;

procedure TMainForm.EngineTiming(const Sender: TObject);
begin
  PrepareBitmaps;

  RenderWindow;
end;

procedure TMainForm.EngineProcess(const Sender: TObject);
begin
  Inc(EngineTicks);
end;

procedure TMainForm.PrepareBitmaps;
const
  SourceMapping: TFloatRect4 = (
    TopLeft    : (X:   0 + 4; Y: 0 + 4);
    TopRight   : (X: 511 - 1; Y: 0 + 3);
    BottomRight: (X: 511 - 3; Y: 511 - 4);
    BottomLeft : (X:   0 + 1; Y: 511 - 8));
var
  Theta, RibbonLength: Single;
begin
  if BitmapMain.Canvas.BeginScene then
  try
    // Copy previous scene, englarged and slightly rotated.
    BitmapMain.Canvas.UseImagePx(BitmapCopy, SourceMapping);
    BitmapMain.Canvas.TexQuad(FloatRect4(0.0, 0.0, 512.0, 512.0), IntColorWhite4);

    // Darken the area slightly, to avoid color mess :)
    // Replace color parameter to $FFF0F0F0 to reduce the effect.
    BitmapMain.Canvas.FillRect(0, 0, 512, 512, $FFFCFCFC, TBlendingEffect.Multiply);

    // Add the "motion scene" to the working surface.
    Theta := (EngineTicks mod 200) * Pi / 100;
    RibbonLength := (1.0 + Sin(EngineTicks / 50.0)) * Pi * 2 / 3 + (Pi / 3);

    BitmapMain.Canvas.FillRibbon(
      Point2(256, 256 - 32), Point2(32.0, 48.0), Point2(96.0, 64.0),
      Theta, Theta + RibbonLength, 64,
      $7F7E00FF, $7F75D3FF, $7FD1FF75, $7FFFC042, $7F00FF00, $7FFF0000);
  finally
    BitmapMain.Canvas.EndScene;
  end;

  // Copy newly created scene to auxiliary bitmap.
  BitmapCopy.CopyFrom(BitmapMain);
end;

procedure TMainForm.RenderWindow;
begin
  EngineTimer.Enabled := False;
  try
    if EngineDevice.BeginScene then
    try
      EngineDevice.Clear([TClearType.Color], $FF404040);
    
      if EngineCanvas.BeginScene then
      try
        RenderScene;
      finally
        EngineCanvas.EndScene;
      end;
    
      { Invoke "EngineProcess" event (60 times per second, independently of rendering speed) to do processing and calculations
        while GPU is busy rendering the scene. }
      EngineTimer.Process;
    finally
      EngineDevice.EndScene;
    end;
  finally  
    EngineTimer.Enabled := True; 
  end;  
end;

procedure TMainForm.RenderScene;
begin
  // Simply draw the bitmap on the screen.
  EngineCanvas.UseImage(BitmapMain);
  EngineCanvas.TexQuad(
    FloatRect4(
      (DisplaySize.X - BitmapMain.Width) * 0.5, (DisplaySize.Y - BitmapMain.Height) * 0.5,
      BitmapMain.Width, BitmapMain.Height), IntColorWhite4, TBlendingEffect.Add);

  // Display the information text.
  EngineFonts[FontTempusSans].DrawText(
    Point2(4.0, 4.0),
    'Frame Rate: ' + UniString(IntToStr(EngineTimer.FrameRate)),
    IntColor2($FFEDF8FF, $FFA097FF));

  EngineFonts[FontTempusSans].DrawText(
    Point2(4.0, 24.0),
    'Technology: ' + UniString(GetFullDeviceTechString(EngineDevice)),
    IntColor2($FFE8FFAA, $FF12C312));
end;
  
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
  ConsoleWriteLn('Starting example.');
  
  MainForm := TMainForm.Create;
  if MainForm.FormCreate(nil) then
  begin
    ConsoleWriteLn('MainForm Created.');
    
    while not ConsoleKeyPressed do
    begin
      // Refresh the image
      MainForm.ApplicationIdle(nil);
     end;
     
    ConsoleReadKey;
    
    MainForm.FormDestroy(nil);
    ConsoleWriteLn('MainForm Destroyed.');
    
    ConsoleWriteLn('Example completed.');
  end;  
  MainForm.Free;
  
  ThreadHalt(0);
end.

