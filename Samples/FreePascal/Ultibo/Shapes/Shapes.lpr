program Shapes;
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
  This example was adapted for Ultibo from the FreePascal desktop example Shapes.
  
  Attention! Please follow these instructions before running the sample:
  
   1. Remember to copy accompanying files "Kristen.font" from the folder \Samples\Media to
      your SD card as well.
  
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
  Classes,
  SysUtils,
  PXL.TypeDef,
  PXL.Types,
  PXL.Timing,
  PXL.Devices,
  PXL.Canvas,
  PXL.SwapChains,
  PXL.Fonts,
  PXL.Providers,
  
  PXL.Classes,
  PXL.Providers.Auto;

type
  TMainForm = class(TObject)
    function FormCreate(Sender: TObject): Boolean;
    procedure FormDestroy(Sender: TObject);
  private
    Name: String;
    Handle: THandle;
    ClientWidth: Integer;
    ClientHeight: Integer;

    { private declarations }
    DeviceProvider: TGraphicsDeviceProvider;

    EngineDevice: TCustomSwapChainDevice;
    EngineCanvas: TCustomCanvas;
    EngineFonts: TBitmapFonts;
    EngineTimer: TMultimediaTimer;

    DisplaySize: TPoint2px;
    EngineTicks: Integer;
    CacheStall: Integer;

    FontKristen: Integer;

    procedure ApplicationIdle(Sender: TObject);

    procedure EngineTiming(const Sender: TObject);
    procedure EngineProcess(const Sender: TObject);

    procedure RenderWindow;
    procedure RenderScene;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

function TMainForm.FormCreate(Sender: TObject): Boolean;  
const
  DefaultMultisamples = 8;
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
  if ClientWidth > 640 then ClientWidth := 640;
  ClientHeight := FramebufferProperties.PhysicalHeight;
  if ClientHeight > 480 then ClientHeight := 480;

  DeviceProvider := CreateDefaultProvider;
  EngineDevice := DeviceProvider.CreateDevice as TCustomSwapChainDevice;

  DisplaySize := Point2px(ClientWidth, ClientHeight);
  EngineDevice.SwapChains.Add(Handle, DisplaySize, DefaultMultisamples);

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

  FontKristen := EngineFonts.AddFromBinaryFile(CrossFixFileName('C:\Kristen.font'));
  if FontKristen = -1 then
  begin
    ConsoleWriteLn('Could not load Kristen font.');
    Exit;
  end;
  ConsoleWriteLn('Loaded Kristen font.');
  
  EngineTimer := TMultimediaTimer.Create;
  EngineTimer.OnTimer := EngineTiming;
  EngineTimer.OnProcess := EngineProcess;
  EngineTimer.MaxFPS := 4000;

  EngineTicks := 0;
  
  Result := True;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  EngineTimer.Free;
  EngineFonts.Free;
  EngineCanvas.Free;
  EngineDevice.Free;
  DeviceProvider.Free;
end;

procedure TMainForm.ApplicationIdle(Sender: TObject);
begin
  EngineTimer.NotifyTick;
end;

procedure TMainForm.EngineTiming(const Sender: TObject);
begin
  RenderWindow;
end;

procedure TMainForm.EngineProcess(const Sender: TObject);
begin
  Inc(EngineTicks);
end;

procedure TMainForm.RenderWindow;
begin
  EngineTimer.Enabled := False;
  try
    if EngineDevice.BeginScene then
    try
      EngineDevice.Clear([TClearType.Color], $FF4E4433);
    
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
var
 HexMtx: TMatrix3;
 Omega, Kappa: Single;
 HoleAt, HoleSize: TPoint2;
 I: Integer;
begin
  // Draw gradient lines.
  for I := 0 to DisplaySize.Y div 20 do
    EngineCanvas.Line(
      Point2(0.0, 0.0), Point2(DisplaySize.X, I * 20.0),
      IntColor2($FF837256, $FF4E4433));

    for I := 0 to DisplaySize.X div 20 do
      EngineCanvas.Line(
        Point2(0.0, 0.0), Point2(I * 20.0, DisplaySize.Y),
        IntColor2($FF837256, $FF4E4433));

  // Draw Hexagon.
  HexMtx :=
    // Make hexagon with dimensions of 50x50.
    ScaleMtx3(Point2(50.0, 50.0)) *
    // Rotate hexagon with time.
    RotateMtx3(EngineTicks * 0.00371) *
    // Position hexagon at one quarter of screen.
    TranslateMtx3(Point2(DisplaySize.X * 0.25, DisplaySize.Y * 0.25));

  EngineCanvas.FillHexagon(HexMtx, $00FF0000, $FFFFD728, $00FF0000, $FFFFD728, $00FF0000, $FFFFD728);

  // Draw Arc.
  Omega := EngineTicks * 0.01892;
  Kappa := 1.25 * Pi + Sin(EngineTicks * 0.01241) * 0.5 * Pi;

  EngineCanvas.FillArc(
    Point2(DisplaySize.X * 0.75, DisplaySize.Y * 0.25),
    Point2(70.0, 50.0), Omega, Omega + Kappa, 64,
    IntColor4($FFA4E581, $FFFF9C00, $FF7728FF, $FFFFFFFF));

  // Draw small Ribbon.
  Omega := EngineTicks * 0.01134;
  Kappa := 1.25 * Pi + Sin(EngineTicks * 0.014751) * 0.5 * Pi;

  EngineCanvas.FillRibbon(
    Point2(DisplaySize.X * 0.25, DisplaySize.Y * 0.75),
    Point2(25.0, 20.0), Point2(45.0, 40.0), Omega, Omega + Kappa, 64,
    IntColor4($FFFF244F, $FFACFF0D, $FF2B98FF, $FF7B42FF));

  // Draw large Ribbon.
  Omega := EngineTicks * 0.01721;
  Kappa := 1.25 * Pi + Sin(EngineTicks * 0.01042) * 0.5 * Pi;

  EngineCanvas.FillRibbon(
    Point2(DisplaySize.X * 0.25, DisplaySize.Y * 0.75),
    Point2(50.0, 45.0), Point2(70.0, 65.0),
    Omega, Omega + Kappa, 64,
    $FFFF244F, $FFACFF0D, $FF2B98FF, $FFA4E581, $FFFF9C00, $FF7728FF);

  // Draw hole with smooth internal border (using tape).
  HoleAt := Point2(
   DisplaySize.X * 0.75 + Cos(EngineTicks * 0.00718) * DisplaySize.X * 0.15,
   DisplaySize.Y * 0.75 + Sin(EngineTicks * 0.00912) * DisplaySize.Y * 0.15);

  HoleSize := Point2(40.0, 40.0);

  EngineCanvas.QuadHole(
    Point2(DisplaySize.X * 0.5, DisplaySize.Y * 0.5),
    Point2(DisplaySize.X * 0.5, DisplaySize.Y * 0.5),
    HoleAt, HoleSize,
    $004E4433, $FFE4DED5, 64);

  EngineCanvas.FillRibbon(
    HoleAt, HoleSize * 0.75, HoleSize, 0.0, 2.0 * Pi, 64,
    $004E4433, $004E4433, $004E4433, $FFE4DED5, $FFE4DED5, $FFE4DED5);

  // Draw information text.
  EngineFonts[FontKristen].DrawTextCentered(
    Point2ToPx(Point2(DisplaySize.X * 0.25, DisplaySize.Y * 0.25 + 70.0)),
    'Hexagon',
    IntColor2($FFFFD25D, $FFFF0036));

  EngineFonts[FontKristen].DrawTextCentered(
    Point2ToPx(Point2(DisplaySize.X * 0.75, DisplaySize.Y * 0.25 + 70.0)),
    'Arc',
    IntColor2($FFE5FF3B, $FF00FF00));

  EngineFonts[FontKristen].DrawTextCentered(
    Point2ToPx(Point2(DisplaySize.X * 0.25, DisplaySize.Y * 0.75 + 80.0)),
    'Tapes',
    IntColor2($FFEAFAFF, $FF7B42FF));

  EngineFonts[FontKristen].DrawTextCentered(
    Point2ToPx(Point2(DisplaySize.X * 0.75, DisplaySize.Y * 0.75 + 80.0)),
    'Hole + tape',
    IntColor2($FFFFF4B3, $FFA9824C));

  EngineFonts[FontKristen].DrawText(
    Point2(4.0, 4.0),
    'FPS: ' + IntToStr(EngineTimer.FrameRate) + ', Cache Stall: ' + IntToStr(CacheStall),
    IntColor2($FFFFFF62, $FFFF8424), 1.0);

  EngineFonts[FontKristen].DrawText(
    Point2(4.0, 34.0),
    'Technology: ' + GetFullDeviceTechString(EngineDevice),
    IntColor2($FFE8FFAA, $FF12C312));

  EngineCanvas.Flush;
  CacheStall := EngineCanvas.CacheStall;
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

