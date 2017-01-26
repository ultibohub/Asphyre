program Plasma;
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
  This example was adapted for Ultibo from the FreePascal desktop example Plasma.
  
  Attention! Please follow these instructions before running the sample:
  
   1. Remember to copy accompanying files "TranceForm.font" and "Scanline.png" from the 
      folder \Samples\Media to your SD card as well.
  
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
  PXL.Types,
  PXL.Timing,
  PXL.ImageFormats,
  PXL.Devices,
  PXL.Canvas,
  PXL.SwapChains,
  PXL.Surfaces,
  PXL.Images,
  PXL.Fonts,
  PXL.Providers,
  PXL.Palettes,
  
  PXL.Classes,
  PXL.Providers.Auto,
  PXL.ImageFormats.Auto;

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
    ImageFormatManager: TImageFormatManager;
    ImageFormatHandler: TCustomImageFormatHandler;

    DeviceProvider: TGraphicsDeviceProvider;

    EngineDevice: TCustomSwapChainDevice;
    EngineCanvas: TCustomCanvas;
    EngineImages: TAtlasImages;
    EngineFonts: TBitmapFonts;
    EngineTimer: TMultimediaTimer;

    DisplaySize: TPoint2px;

    SinLookup: array[0..1023] of Word;
    CosLookup: array[0..1023] of Word;
    PaletteLookup: array[0..1023] of TIntColor;
    ShiftX: Integer;
    ShiftY: Integer;
    PaletteIndex: Integer;

    PlasmaSurface: TPixelSurface;

    ImagePlasma: Integer;
    ImageScanline: Integer;

    FontTranceForm: Integer;

    procedure ApplicationIdle(Sender: TObject);

    procedure EngineTiming(const Sender: TObject);
    procedure EngineProcess(const Sender: TObject);

    procedure InitPlasma;
    procedure InitPalette;
    function CreatePlasmaImage: Integer;
    procedure PreparePlasma(const ShiftX, ShiftY: Integer);
    procedure UpdatePlasmaImage;

    procedure RenderWindow;
    procedure RenderScene;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

const
  PlasmaSize: TPoint2px = (X: 256; Y: 256);
 
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
  if ClientWidth > 640 then ClientWidth := 640;
  ClientHeight := FramebufferProperties.PhysicalHeight;
  if ClientHeight > 480 then ClientHeight := 480;

  ImageFormatManager := TImageFormatManager.Create;
  ImageFormatHandler := CreateDefaultImageFormatHandler(ImageFormatManager);

  DeviceProvider := CreateDefaultProvider(ImageFormatManager);
  EngineDevice := DeviceProvider.CreateDevice as TCustomSwapChainDevice;

  DisplaySize := Point2px(ClientWidth, ClientHeight);
  EngineDevice.SwapChains.Add(Handle, DisplaySize);

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

  EngineImages := TAtlasImages.Create(EngineDevice);

  ImagePlasma := CreatePlasmaImage;
  if ImagePlasma = -1 then
  begin
    ConsoleWriteLn('Could not create Plasma image.');
    Exit;
  end;
  ConsoleWriteLn('Created Plasma image.');
  
  ImageScanline := EngineImages.AddFromFile(CrossFixFileName('C:\Scanline.png'));
  if ImageScanline = -1 then
  begin
    ConsoleWriteLn('Could not load Scanline image.');
    Exit;
  end;
  ConsoleWriteLn('Loaded Scanline image.');
  
  EngineFonts := TBitmapFonts.Create(EngineDevice);
  EngineFonts.Canvas := EngineCanvas;

  FontTranceForm := EngineFonts.AddFromBinaryFile(CrossFixFileName('C:\TranceForm.font'));
  if FontTranceForm = -1 then
  begin
    ConsoleWriteLn('Could not load TranceForm font.');
    Exit;
  end;
  ConsoleWriteLn('Loaded TranceForm font.');
  
  EngineTimer := TMultimediaTimer.Create;
  EngineTimer.OnTimer := EngineTiming;
  EngineTimer.OnProcess := EngineProcess;
  EngineTimer.MaxFPS := 4000;

  PlasmaSurface := TPixelSurface.Create;
  PlasmaSurface.SetSize(PlasmaSize, TPixelFormat.A8R8G8B8);
  
  InitPlasma;
  InitPalette;
  
  Result := True;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  PlasmaSurface.Free;
  EngineTimer.Free;
  EngineFonts.Free;
  EngineImages.Free;
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
  PreparePlasma(ShiftX, ShiftY);
  UpdatePlasmaImage;

  RenderWindow;
end;

procedure TMainForm.EngineProcess(const Sender: TObject);
begin
  Inc(ShiftX);
  Dec(ShiftY);
  Inc(PaletteIndex);
end;

procedure TMainForm.InitPlasma;
var
  I: Integer;
begin
  for I := 0 to 1023 do
  begin
    SinLookup[I] := (Trunc(Sin(2.0 * Pi * I / 1024.0) * 512.0) + 512) and $3FF;
    CosLookup[I] := (Trunc(Cos(2.0 * Pi * I / 1024.0) * 512.0) + 512) and $3FF;
  end;

  ShiftX := 0;
  ShiftY := 0;
  PaletteIndex := 0;
end;

procedure TMainForm.InitPalette;
var
  Palette: TFloatPalette;
  I: Integer;
begin
  Palette:= TFloatPalette.Create;
  try
    Palette.Add(FloatColor($FF000000), TFloatNodeType.Sine, 0.0);
    Palette.Add(FloatColor($FF7E00FF), TFloatNodeType.Sine, 0.1);
    Palette.Add(FloatColor($FFE87AFF), TFloatNodeType.Sine, 0.2);
    Palette.Add(FloatColor($FF7E00FF), TFloatNodeType.Sine, 0.3);
    Palette.Add(FloatColor($FFFFFFFF), TFloatNodeType.Sine, 0.4);
    Palette.Add(FloatColor($FF000000), TFloatNodeType.Plain, 0.5);
    Palette.Add(FloatColor($FF0500A8), TFloatNodeType.Brake, 0.6);
    Palette.Add(FloatColor($FFBEFF39), TFloatNodeType.Accel, 0.7);
    Palette.Add(FloatColor($FFFFC939), TFloatNodeType.Brake, 0.8);
    Palette.Add(FloatColor($FFFFF58D), TFloatNodeType.Sine,  0.9);
    Palette.Add(FloatColor($FF000000), TFloatNodeType.Plain, 1.0);

    for I := 0 to 1023 do
      PaletteLookup[I] := FloatToIntColor(Palette.Color[I / 1023.0]);
  finally
    Palette.Free;
  end;
end;

function TMainForm.CreatePlasmaImage: Integer;
var
  Image: TAtlasImage;
begin
  Image := TAtlasImage.Create(EngineDevice, False);
  Image.MipMapping  := False;
  Image.PixelFormat := TPixelFormat.A8R8G8B8;
  Image.DynamicImage:= True;

  if Image.InsertTexture(PlasmaSize.X, PlasmaSize.Y) = nil then
  begin
    Image.Free;
    Exit(-1);
  end;

  Result := EngineImages.Include(Image);
end;

procedure TMainForm.PreparePlasma(const ShiftX, ShiftY: Integer);
var
  I, J, Xadd, Cadd: Integer;
  DestPixel: PIntColor;
  Index: Integer;
begin
  for J := 0 to PlasmaSurface.Height - 1 do
  begin
    DestPixel := PlasmaSurface.Scanline[J];

    Xadd := SinLookup[((J shl 2) + ShiftX) and $3FF];
    Cadd := CosLookup[((J shl 2) + ShiftY) and $3FF];

    for I := 0 to PlasmaSurface.Width - 1 do
    begin
      Index := (SinLookup[((I shl 2) + Xadd) and $3FF] + Cadd + (PaletteIndex * 4)) and $3FF;
      if Index > 511 then
        Index := 1023 - Index;

      DestPixel^ := PaletteLookup[((Index div 4) + PaletteIndex) and $3FF];
      Inc(DestPixel);
    end;
  end;
end;

procedure TMainForm.UpdatePlasmaImage;
var
  PlasmaImage: TAtlasImage;
  LockSurface: TPixelSurface;
begin
  PlasmaImage := EngineImages[ImagePlasma];
  if (PlasmaImage = nil) or (PlasmaImage.TextureCount < 1) then
    Exit;

  if not PlasmaImage.Texture[0].Lock(LockSurface) then
    Exit;
  try
    LockSurface.CopyFrom(PlasmaSurface);
  finally
    LockSurface.Free;
  end;
end;

procedure TMainForm.RenderWindow;
begin
  EngineTimer.Enabled := False;
  try
    if EngineDevice.BeginScene then
    try
      EngineDevice.Clear([TClearType.Color], 0);
    
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
  J, I: Integer;
begin
  for j := 0 to DisplaySize.Y div PlasmaSize.Y do
    for i := 0 to DisplaySize.X div PlasmaSize.X do
    begin
      EngineCanvas.UseImage(EngineImages[ImagePlasma]);
      EngineCanvas.TexQuad(
        FloatRect4(I * PlasmaSize.X, J * PlasmaSize.Y, PlasmaSize.X, PlasmaSize.Y),
        IntColorWhite4);
    end;

  for J := 0 to DisplaySize.Y div 64 do
    for I:= 0 to DisplaySize.X div 64 do
    begin
      EngineCanvas.UseImage(EngineImages[ImageScanline]);
      EngineCanvas.TexQuad(
        FloatRect4(I * 64, J * 64, 64, 64),
        //IntColorWhite4, TBlendingEffect.Multiply); //To Do //TBlendingEffect.Multiply is not implemented for software rendering
        IntColorWhite4, TBlendingEffect.Add);
    end;

  EngineFonts[FontTranceForm].DrawText(
    Point2(4.0, 4.0),
    'fps: ' + UnicodeString(IntToStr(EngineTimer.FrameRate)),
    IntColor2($FFD1FF46, $FF3EB243));
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

