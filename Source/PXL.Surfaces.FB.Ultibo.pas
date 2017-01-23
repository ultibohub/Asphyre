unit PXL.Surfaces.FB.Ultibo;
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
  GlobalConst, GlobalConfig, Platform, Threads, Framebuffer, PXL.TypeDef, PXL.Types, PXL.Surfaces;

type
  TFBPixelSurface = class(TPixelSurface)
  private
    FFormat: LongWord;
  protected
    procedure ResetAllocation; override;
    function Reallocate(const NewWidth, NewHeight: Integer; const NewPixelFormat: TPixelFormat): Boolean; override;
  public
    function ApproximatePixelFormat(const NewPixelFormat: TPixelFormat): TPixelFormat; override;
   
    procedure PutRect(const Handle: THandle; const DestAt, DestSize: TPoint2px; const SrcAt, SrcSize: TPoint2px); 
  end;

  TFBPixelMipMapSurface = class(TPixelMipMapSurface)
  protected
    function CreatePixelSurfaces: TPixelSurfaces; override;
  end;

  TFBPixelSurfaces = class(TPixelSurfaces)
  protected
    function CreatePixelSurface(const SurfaceName: StdString): TPixelSurface; override;
  end;

implementation

uses
  PXL.Formats;
  
procedure TFBPixelSurface.ResetAllocation;
begin
  if FBits <> nil then
  begin
   if DMAAvailable then
    DMAReleaseBuffer(FBits)
   else
    FreeMem(FBits);
  end;
  
  FFormat := COLOR_FORMAT_UNKNOWN;
  
  FBits := nil;
  FPitch := 0;
  FWidth := 0;
  FHeight := 0;
  FBufferSize := 0;
  FBytesPerPixel := 0;
  FPixelFormat := TPixelFormat.Unknown;
end;
  
function TFBPixelSurface.Reallocate(const NewWidth, NewHeight: Integer; const NewPixelFormat: TPixelFormat): Boolean;
begin
  FPixelFormat := ApproximatePixelFormat(NewPixelFormat);
  FBytesPerPixel := FPixelFormat.Bytes;

  case FPixelFormat of
    TPixelFormat.A8R8G8B8:
     FFormat := COLOR_FORMAT_ARGB32;
    TPixelFormat.X8R8G8B8:
     FFormat := COLOR_FORMAT_URGB32;
    TPixelFormat.R8G8B8:
     FFormat := COLOR_FORMAT_RGB24;
    TPixelFormat.X1R5G5B5:
     FFormat := COLOR_FORMAT_RGB15;
    TPixelFormat.R5G6B5:
     FFormat := COLOR_FORMAT_RGB16;
    TPixelFormat.A8B8G8R8:
     FFormat := COLOR_FORMAT_ABGR32;
    TPixelFormat.X8B8G8R8:
     FFormat := COLOR_FORMAT_UBGR32;
    TPixelFormat.B8G8R8A8:
     FFormat := COLOR_FORMAT_BGRA32;
    TPixelFormat.B8G8R8X8:
     FFormat := COLOR_FORMAT_BGRU32;
  else
   Result := False;
   Exit;
  end;  
  
  FWidth := NewWidth;
  FHeight := NewHeight;
  FPitch := FWidth * FBytesPerPixel;
  FBufferSize := FHeight * FPitch;

  if DMAAvailable then
   FBits := DMAAllocateBuffer(FBufferSize)
  else
   FBits := AllocMem(FBufferSize);
   
  Result := True;
end;

function TFBPixelSurface.ApproximatePixelFormat(const NewPixelFormat: TPixelFormat): TPixelFormat;
const
  DefaultPixelFormat = TPixelFormat.A8R8G8B8;
var
  FormatList: TPixelFormatList;
begin
  if NewPixelFormat = TPixelFormat.Unknown then
    Exit(DefaultPixelFormat);

  FormatList := TPixelFormatList.Create;
  try
    FormatList.Insert(TPixelFormat.A8R8G8B8);
    FormatList.Insert(TPixelFormat.X8R8G8B8);
    FormatList.Insert(TPixelFormat.R8G8B8);
    FormatList.Insert(TPixelFormat.X1R5G5B5);
    FormatList.Insert(TPixelFormat.R5G6B5);
    FormatList.Insert(TPixelFormat.A8B8G8R8);
    FormatList.Insert(TPixelFormat.X8B8G8R8);
    FormatList.Insert(TPixelFormat.B8G8R8A8);
    FormatList.Insert(TPixelFormat.B8G8R8X8);

    Result := FindClosestPixelFormat(NewPIxelFormat, FormatList);

    if Result = TPixelFormat.Unknown then
      Result := DefaultPixelFormat;
  finally
    FormatList.Free;
  end;
end;
  
procedure TFBPixelSurface.PutRect(const Handle: THandle; const DestAt, DestSize: TPoint2px; const SrcAt, SrcSize: TPoint2px); 
var
 Count: LongWord;
 Offset: LongWord;
 Line: Pointer;
 Address: Pointer; 
 
 Skip: LongWord;
 Width: LongWord;
 Height: LongWord;
 Buffer: Pointer;
 Format: LongWord;
 Bytes: LongWord;
 Reverse: Boolean;
 FramebufferDevice: PFramebufferDevice;
begin
  // Check handle
  if Handle = INVALID_HANDLE_VALUE then 
   Exit;
   
  // Get framebuffer device 
  FramebufferDevice := PFramebufferDevice(Handle);
  if FramebufferDevice = nil then
   Exit;
  
  // Get width
  Width := SrcSize.X;
  if Width > FWidth then Width := FWidth;
  if (DestAt.X + Width) > DestSize.X then Width := DestSize.X - DestAt.X; 
  
  // Get height
  Height := SrcSize.Y;
  if Height > FHeight then Height := FHeight;
  if (DestAt.Y + Height) > DestSize.Y then Height := DestSize.Y - DestAt.Y; 
  
  // Get skip and buffer
  Skip := FWidth - Width;
  Buffer := FBits + (SrcAt.Y * FPitch) + (SrcAt.X * FBytesPerPixel);

  // Get framebuffer format
  Format := FramebufferDeviceGetFormat(FramebufferDevice);
  Reverse := FramebufferDeviceCheckFlag(FramebufferDevice, FRAMEBUFFER_FLAG_SWAP);
  
  // Compare surface format with framebuffer format
  if (FFormat = Format) and not(Reverse) then
  begin
    // Put Rect 
    FramebufferDevicePutRect(FramebufferDevice, DestAt.X, DestAt.Y, Buffer, Width, Height, Skip, FRAMEBUFFER_TRANSFER_DMA);
  end
  else
  begin
    // Get framebuffer format bytes
    Bytes := ColorFormatToBytes(Format);
    
    // Lock framebuffer
    if MutexLock(FramebufferDevice.Lock) <> ERROR_SUCCESS then
      Exit;
    
    // Set buffer offset
    Offset := 0;
    
    // Check surface format
    if FFormat = COLOR_FORMAT_DEFAULT then
    begin
      for Count := 0 to Height - 1 do
      begin
        // Get line
        Address := FramebufferDeviceGetPoint(FramebufferDevice, DestAt.X, DestAt.Y + Count);
        if Address <> nil then
        begin
          // Memory barrier
          DataMemoryBarrier;
         
          // Convert from surface format (COLOR_FORMAT_DEFAULT) to framebuffer format
          PixelsDefaultToFormat(Format, Pointer(Buffer + Offset), Address, Width, Reverse);
          
          // Check commit
          if ((FramebufferDevice.Device.DeviceFlags and FRAMEBUFFER_FLAG_COMMIT) <> 0) and Assigned(FramebufferDevice.DeviceCommit) then
          begin
            FramebufferDevice.DeviceCommit(FramebufferDevice, LongWord(Address), Width * Bytes, FRAMEBUFFER_TRANSFER_NONE);
          end;
           
          // Check mark
          if ((FramebufferDevice.Device.DeviceFlags and FRAMEBUFFER_FLAG_MARK) <> 0) and Assigned(FramebufferDevice.DeviceMark) then
          begin
            FramebufferDevice.DeviceMark(FramebufferDevice, DestAt.X, DestAt.Y + Count, Width, 1, FRAMEBUFFER_TRANSFER_NONE);
          end;
        end
        else
        begin
          Exit;
        end;
         
        // Update offset
        Inc(Offset,(Width + Skip) * FBytesPerPixel);
      end;
    end
    // Check framebuffer format
    else if Format = COLOR_FORMAT_DEFAULT then
    begin
      for Count := 0 to Height - 1 do
      begin
        // Get line
        Address := FramebufferDeviceGetPoint(FramebufferDevice, DestAt.X, DestAt.Y + Count);
        if Address <> nil then
        begin
          // Memory barrier
          DataMemoryBarrier;
          
          // Convert from surface format to framebuffer format (COLOR_FORMAT_DEFAULT)
          PixelsFormatToDefault(FFormat, Pointer(Buffer + Offset), Address, Width, Reverse);
          
          // Check commit
          if ((FramebufferDevice.Device.DeviceFlags and FRAMEBUFFER_FLAG_COMMIT) <> 0) and Assigned(FramebufferDevice.DeviceCommit) then
          begin
            FramebufferDevice.DeviceCommit(FramebufferDevice, LongWord(Address), Width * Bytes, FRAMEBUFFER_TRANSFER_NONE);
          end;
           
          // Check mark
          if ((FramebufferDevice.Device.DeviceFlags and FRAMEBUFFER_FLAG_MARK) <> 0) and Assigned(FramebufferDevice.DeviceMark) then
          begin
            FramebufferDevice.DeviceMark(FramebufferDevice, DestAt.X, DestAt.Y + Count, Width, 1, FRAMEBUFFER_TRANSFER_NONE);
          end;
        end
        else
        begin
          Exit;
        end;
         
        // Update offset
        Inc(Offset,(Width + Skip) * FBytesPerPixel);
      end;
    end
    else
    begin
      // Get line
      Line:=GetMem(Width * ColorFormatToBytes(COLOR_FORMAT_DEFAULT));
      if Line <> nil then
      begin
        for Count:=0 to Height - 1 do
        begin
          // Get address
          Address:=FramebufferDeviceGetPoint(FramebufferDevice, DestAt.X, DestAt.Y + Count);
          if Address <> nil then
           begin
             // Memory barrier
             DataMemoryBarrier;
             
             // Convert from surface format to default format (COLOR_FORMAT_DEFAULT)
             PixelsFormatToDefault(FFormat, Pointer(Buffer + Offset), Line, Width, False);
             
             // Convert from default format (COLOR_FORMAT_DEFAULT) to framebuffer format
             PixelsDefaultToFormat(Format, Line, Address, Width, Reverse);
             
             // Check commit
             if ((FramebufferDevice.Device.DeviceFlags and FRAMEBUFFER_FLAG_COMMIT) <> 0) and Assigned(FramebufferDevice.DeviceCommit) then
             begin
               FramebufferDevice.DeviceCommit(FramebufferDevice, LongWord(Address), Width * Bytes, FRAMEBUFFER_TRANSFER_NONE);
             end;
              
             // Check mark
             if ((FramebufferDevice.Device.DeviceFlags and FRAMEBUFFER_FLAG_MARK) <> 0) and Assigned(FramebufferDevice.DeviceMark) then
             begin
               FramebufferDevice.DeviceMark(FramebufferDevice, DestAt.X, DestAt.Y + Count, Width, 1, FRAMEBUFFER_TRANSFER_NONE);
             end;
           end
          else
           begin
             Exit;
           end;
           
          // Update offset
          Inc(Offset,(Width + Skip) * FBytesPerPixel);
        end;
                
        // Free line
        FreeMem(Line);
      end;         
    end;
    
    // Unlock framebuffer
    MutexUnlock(FramebufferDevice.Lock);
  end;   
end;

function TFBPixelMipMapSurface.CreatePixelSurfaces: TPixelSurfaces;
begin
  Result := TFBPixelSurfaces.Create;
end;

function TFBPixelSurfaces.CreatePixelSurface(const SurfaceName: StdString): TPixelSurface;
begin
  Result := TFBPixelSurface.CreateNamed(SurfaceName);
end;

end.
