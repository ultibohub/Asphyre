unit PXL.Displays.FB.Ultibo;
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
interface

{$INCLUDE PXL.Config.inc}

uses
  GlobalConst, GlobalConfig, Platform, Framebuffer, PXL.Types, PXL.Boards.Types, PXL.Displays.Types;

type
  TDisplay = class(TCustomDisplay)
  private
    FFramebuffer: PFramebufferDevice;
    FScreenBufferFormat: LongWord;
    FScreenBufferReverse: Boolean;
    FScreenBufferBytes: LongWord;
  protected
    procedure Reset; override;
    procedure InitSequence; override;
    procedure PresentBuffer(const Rect: TIntRect); override;

    function ReadPixel(const X, Y: Integer): TIntColor; override;
    procedure WritePixel(const X, Y: Integer; const Color: TIntColor); override;

    function GetScanline(const Index: Integer): Pointer; override;
  public
    constructor Create(AFramebuffer: PFramebufferDevice = nil);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;
  
constructor TDisplay.Create(AFramebuffer: PFramebufferDevice = nil);
var
  Properties: TFramebufferProperties;
begin
  FFramebuffer := AFramebuffer;
  if FFramebuffer = nil then
    FFramebuffer := FramebufferDeviceGetDefault;
  
  FScreenBufferFormat := COLOR_FORMAT_UNKNOWN;
  FScreenBufferReverse := False;
  FScreenBufferBytes := 0;
  
  FPhysicalOrientation := TOrientation.Landscape;
  FPhysicalSize := Point2px(0, 0);

  FScreenBufferSize := 0;
  FScreenBuffer := nil;
  
  if FramebufferDeviceGetProperties(FFramebuffer, @Properties) = ERROR_SUCCESS then
  begin
   FScreenBufferFormat := Properties.Format;            
   FScreenBufferReverse := (Properties.Flags and FRAMEBUFFER_FLAG_SWAP) <> 0;
   FScreenBufferBytes := ColorFormatToBytes(FScreenBufferFormat);
   
   case Properties.Rotation of
    FRAMEBUFFER_ROTATION_0:
      FPhysicalOrientation := TOrientation.Landscape;
    FRAMEBUFFER_ROTATION_90:
      FPhysicalOrientation := TOrientation.Portrait;
    FRAMEBUFFER_ROTATION_180:
      FPhysicalOrientation := TOrientation.InverseLandscape;
    FRAMEBUFFER_ROTATION_270:
      FPhysicalOrientation := TOrientation.InversePortrait;
   end;
   
   FPhysicalSize := Point2px(Properties.PhysicalWidth, Properties.PhysicalHeight);
 
   FScreenBufferSize := (FPhysicalSize.X * FPhysicalSize.Y) * FScreenBufferBytes; 
   if DMAAvailable then
    FScreenBuffer := DMAAllocateBuffer(FScreenBufferSize)
   else
    FScreenBuffer := AllocMem(FScreenBufferSize);
  end;
  
  inherited Create;
end;

destructor TDisplay.Destroy;
begin
 if FScreenBuffer <> nil then
 begin
  if DMAAvailable then
   DMAReleaseBuffer(FScreenBuffer)
  else
   FreeMem(FScreenBuffer);
   
  FScreenBuffer := nil; 
 end;
 
 inherited Destroy;
end;

procedure TDisplay.Reset; 
begin
 
end;

procedure TDisplay.InitSequence;
begin
 
end;

procedure TDisplay.PresentBuffer(const Rect: TIntRect);
var
 Skip: LongWord;
 Width: LongWord;
 Height: LongWord;
 Address: Pointer;
begin
  Width := Rect.Right - Rect.Left;
  Height := Rect.Bottom - Rect.Top;
  Skip := FPhysicalSize.X - Width;
  Address := Pointer(PtrUInt(FScreenBuffer) + (Rect.Top * FPhysicalSize.X + Rect.Left) * FScreenBufferBytes);
  
  FramebufferDevicePutRect(FFramebuffer, Rect.Left, Rect.Top, Address, Width, Height, Skip, FRAMEBUFFER_TRANSFER_DMA);
end;

function TDisplay.ReadPixel(const X, Y: Integer): TIntColor;
begin
  ColorFormatAltToDefault(FScreenBufferFormat, Pointer(PtrUInt(FScreenBuffer) + (Cardinal(Y) * Cardinal(FPhysicalSize.X)
                          + Cardinal(X)) * FScreenBufferBytes), LongWord(Result), FScreenBufferReverse); 
end;

procedure TDisplay.WritePixel(const X, Y: Integer; const Color: TIntColor);
begin
  ColorDefaultToFormat(FScreenBufferFormat, LongWord(Color), Pointer(PtrUInt(FScreenBuffer) + (Cardinal(Y) *
                       Cardinal(FPhysicalSize.X) + Cardinal(X)) * FScreenBufferBytes), FScreenBufferReverse);
end;

function TDisplay.GetScanline(const Index: Integer): Pointer;
begin
  if (Index >= 0) and (Index < FPhysicalSize.Y) then
    Result := Pointer(PtrUInt(FScreenBuffer) + Cardinal(Index) * Cardinal(FPhysicalSize.X) * FScreenBufferBytes)
  else
    Result := nil;
end;

end.
