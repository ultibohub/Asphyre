program Combustion;
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
  This example was adapted for Ultibo from the FreePascal desktop example Combustion.
  
  Attention! Please follow these instructions before running the sample:

   1. Remember to copy accompanying files "media.vtdb" and "Tahoma9b.font" from the 
      folder \Samples\Media to your SD card as well.
  
  The example has been created for a Raspberry Pi 2 but will also run on a Raspberry Pi 2.
  
  To convert ths example to Raspberry Pi A/B/A+/B+/Zero create a new project then copy and
  paste this code into it taking care to adjust the RaspberryPi2 unit in the uses clause as
  required.
}

{
  An old demo that originally appeared in Asphyre's predecessor library called "PowerDraw" version 2, published
  on Delphi news site "Turbo" somewhere around 2002. The example has been reworked to use PXL's new "script object"
  mechanism.

  This sample also illustrates one of PXL features - support loading images and fonts from older archives.
  PXL supports older Asphyre archives such VTDb, ASDb and ASVF in addition to its own format, PXLA. Note that
  earlier archive formats such as VTD2 from very first Asphyre releases are not supported.
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
  PXL.Types,
  PXL.Timing,
  PXL.Devices,
  PXL.Canvas,
  PXL.SwapChains,
  PXL.Images,
  PXL.Fonts,
  PXL.Providers,
  PXL.Archives,
  PXL.Scripts,
  PXL.Classes,
  
  PXL.Providers.Auto,
  PXL.Archives.Loading;
  
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
    EngineFonts: TBitmapFonts;
    EngineTimer: TMultimediaTimer;

    EngineTicks: Integer;

    EngineArchive: TArchive;

    ImagePowerDraw: Integer;
    ImageScanline: Integer;
    FontVerdana: Integer;

    EngineParticles: TScriptObject;

    BackColor1: TIntColor;
    BackColor2: TIntColor;
    BackAlpha: Integer;

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
  
const
  // Defines the overall size of the combustion particle.
  ParticleSize = 128;

var
  // The following variables are made global so they can be accessed by particle objects.
  DisplaySize: TPoint2px;

  EngineCanvas: TCustomCanvas;
  EngineImages: TAtlasImages;

  ImageCombustion: Integer = -1;
  
type
  // This is a simplest particle object that moves with constant acceleration and has fixed life.
  TParticle = class(TScriptObject)
  private
    FPosition: TPoint2;
    FVelocity: TPoint2;
    FAcceleration: TPoint2;
    FCurrentLife: Integer;
    FMaximumLife: Integer;
    FAngle: Single;
    FScale: Single;
  protected
    procedure DoUpdate; override;
    procedure DoDraw; override;
  public
    constructor Create(const AOwner: TScriptObject);

    property Position: TPoint2 read FPosition;
    property Velocity: TPoint2 read FVelocity;
    property Acceleration: TPoint2 read FAcceleration;

    property CurrentLife: Integer read FCurrentLife;
    property MaximumLife: Integer read FMaximumLife;

    property Angle: Single read FAngle;
    property Scale: Single read FScale;
  end;

constructor TParticle.Create(const AOwner: TScriptObject);
begin
  inherited Create(AOwner);

  FPosition := Point2(Random(DisplaySize.X + 1), DisplaySize.Y);
  FVelocity := Point2((Random(10) - 5) / 20.0, -(Random(20) / 5.0));
  FAcceleration := Point2(0.0, -(0.001 + (Random(15) / 100)));
  FMaximumLife := 56 + Random(32);
  FAngle := Random * 2.0 * Pi;
  FScale := 0.75 + Random * 0.5;
end;

procedure TParticle.DoUpdate;
begin
  FVelocity := FVelocity + FAcceleration;
  FPosition := FPosition + FVelocity;

  Inc(FCurrentLife);
  if FCurrentLife >= FMaximumLife then
    ObjDispose;
end;

procedure TParticle.DoDraw;
var
  Image: TAtlasImage;
  Region: Integer;
begin
  Image := EngineImages[ImageCombustion];
  Region := (FCurrentLife * Image.Regions.Count) div FMaximumLife;

  EngineCanvas.UseImageRegion(Image, Region);
  EngineCanvas.TexQuad(
    FloatRect4RC(FPosition, Point2(ParticleSize, ParticleSize), FAngle, FScale),
    IntColorAlpha(0.75));
end;

function TMainForm.FormCreate(Sender: TObject): Boolean;  
var
  FramebufferDevice: PFramebufferDevice = nil;
  FramebufferProperties: TFramebufferProperties;
begin
  Result := False;
  
  Randomize;

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
  EngineDevice.SwapChains.Add(Handle, DisplaySize, 0, True);

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
  
  EngineArchive := TArchive.Create;
  EngineArchive.OpenMode := TArchive.TOpenMode.ReadOnly;

  if not EngineArchive.OpenFile('C:\media.vtdb') then
  begin
    ConsoleWriteLn('Failed to open media archive.');
    Exit;
  end;
  ConsoleWriteLn('Opened media archive.');

  EngineImages := TAtlasImages.Create(EngineDevice);

  ImagePowerDraw := LoadImageFromArchive('PowerDraw logo', EngineImages, EngineArchive);
  if ImagePowerDraw = -1 then
  begin
    ConsoleWriteLn('Could not load PowerDraw logo image.');
    Exit;
  end;
  ConsoleWriteLn('Loaded PowerDraw logo image.');
  
  ImageScanline := LoadImageFromArchive('Scanline Tex', EngineImages, EngineArchive);
  if ImageScanline = -1 then
  begin
    ConsoleWriteLn('Could not load Scanline Texture image.');
    Exit;
  end;
  ConsoleWriteLn('Loaded Scanline Texture image.');
  
  ImageCombustion := LoadImageFromArchive('Fire Combustion', EngineImages, EngineArchive);
  if ImageCombustion = -1 then
  begin
    ConsoleWriteLn('Could not load Combustion image.');
    Exit;
  end;
  ConsoleWriteLn('Loaded Combustion image.');
  
  EngineFonts := TBitmapFonts.Create(EngineDevice);
  EngineFonts.Canvas := EngineCanvas;

  //FontVerdana := LoadFontFromArchive('Verdana.font', EngineFonts, EngineArchive); //To Do //Currently fails to load
  FontVerdana := EngineFonts.AddFromBinaryFile(CrossFixFileName('C:\Tahoma9b.font'));
  if FontVerdana = -1 then
  begin
    ConsoleWriteLn('Could not load Verdana font from archive.');
    Exit;
  end;
  ConsoleWriteLn('Loaded Verdana font from archive.');
 
  EngineTimer := TMultimediaTimer.Create;
  EngineTimer.OnTimer := EngineTiming;
  EngineTimer.OnProcess := EngineProcess;
  EngineTimer.MaxFPS := 4000;

  EngineTicks := 0;

  EngineParticles := TScriptObject.Create;

  BackColor1 := IntColorBlack;
  BackColor2 := IntColorRGB(Random(256), Random(256), Random(256));
  
  Result := True;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  EngineParticles.Free;
  EngineTimer.Free;
  EngineFonts.Free;
  EngineImages.Free;
  EngineArchive.Free;
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
var
  Particle: TParticle;
  I: Integer;
begin
  Inc(EngineTicks);

  for I := 0 to 18 do
    if Random(2) = 0 then
    begin
      Particle := TParticle.Create(EngineParticles);
      Particle.DrawOrder := -EngineTicks;
    end;

  EngineParticles.Update;

  Inc(BackAlpha);
  if BackAlpha >= 255 then
  begin
    BackColor1 := BackColor2;
    BackColor2 := IntColorRGB(Random(256), Random(256), Random(256));
    BackAlpha := 0;
  end;
end;

procedure TMainForm.RenderWindow;
begin
  EngineTimer.Enabled := False;
  try
    if EngineDevice.BeginScene then
    try
      EngineDevice.Clear([TClearType.Color], BlendPixels(BackColor1, BackColor2, BackAlpha));
    
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
const
  LogoSize: TPoint2px = (X: 480; Y: 128);
  StatusSize: TPoint2px = (X: 400; Y: 32);
var
 DrawAt: TPoint2px;
 I, J: Integer;
begin
  // Draw particles according to their order.
  EngineParticles.DrawSorted;

  // Show "PowerDraw" logo on top of the particles.
  DrawAt.X := (DisplaySize.X - LogoSize.X) div 2;
  DrawAt.Y := (DisplaySize.Y - LogoSize.Y) div 2;

  EngineCanvas.UseImageRegion(EngineImages[ImagePowerDraw], 0);
  EngineCanvas.TexQuad(FloatRect4(DrawAt.X, DrawAt.Y, LogoSize.X div 2, LogoSize.Y), IntColorWhite4);

  EngineCanvas.UseImageRegion(EngineImages[ImagePowerDraw], 1);
  EngineCanvas.TexQuad(FloatRect4(DrawAt.X + (LogoSize.X div 2), DrawAt.Y, LogoSize.X div 2, LogoSize.Y),
    IntColorWhite4);

  // Apply "Scanline" effect to the whole scene.
  for J := 0 to DisplaySize.Y div 64 do
    for I := 0 to DisplaySize.X div 64 do
    begin
      EngineCanvas.UseImage(EngineImages[ImageScanline]);
      EngineCanvas.TexQuad(FloatRect4(I * 64, J * 64, 64, 64), IntColorWhite4, TBlendingEffect.Add); //To Do //Previously TBlendingEffect.Multiply which is not implemented for software rendering
    end;

  // Draw some fill for status background.
  DrawAt.X := (DisplaySize.X - StatusSize.X) div 2;
  DrawAt.Y := LogoSize.Y + ((DisplaySize.Y - StatusSize.Y) div 2);

  EngineCanvas.FillRect(DrawAt.X, DrawAt.Y, StatusSize.X, StatusSize.Y, $FF5F5F5F, TBlendingEffect.Add); //To Do //Previously TBlendingEffect.Multiply which is not implemented for software rendering
  EngineCanvas.FillRect(DrawAt.X, DrawAt.Y, StatusSize.X, StatusSize.Y, $FF1F1F1F, TBlendingEffect.Add);
  EngineCanvas.FrameRect(FloatRect(DrawAt.X, DrawAt.Y, StatusSize.X, StatusSize.Y), $FF3F3F3F, TBlendingEffect.Add);

  // Show current status.
  EngineFonts[FontVerdana].DrawText(
    Point2(DrawAt.X + 2, DrawAt.Y),
    'Frame Rate: ' + IntToStr(EngineTimer.FrameRate) + ', Particle Count: ' + IntToStr(EngineParticles.ComputeTotalNodeCount),
    IntColor2($FF00FF00, $FFFFFFFF));

  EngineFonts[FontVerdana].DrawText(
    Point2(DrawAt.X + 2, DrawAt.Y + 14),
    'Technology: ' + GetFullDeviceTechString(EngineDevice),
    IntColor2($FFFF00FF, $FFFFFFFF));
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

