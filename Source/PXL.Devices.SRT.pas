unit PXL.Devices.SRT;
{
  This file is part of Asphyre Framework, also known as Platform eXtended Library (PXL).
  Copyright (c) 2000 - 2016  Yuriy Kotsarenko

  The contents of this file are subject to the Mozilla Public License Version 2.0 (the "License");
  you may not use this file except in compliance with the License. You may obtain a copy of the
  License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
  KIND, either express or implied. See the License for the specific language governing rights and
  limitations under the License.
}
interface

{$INCLUDE PXL.Config.inc}

{ Enable the following option to render directly to form's surface (as with any other provider) by using GDI.
  Note that this option works only on Windows. }
{.$DEFINE SRT_RENDER_TO_GDI}

{$IF DEFINED(MSWINDOWS) AND DEFINED(SRT_RENDER_TO_GDI)}
  {$DEFINE SRT_RENDER_TO_GDI_ENABLED}
{$ENDIF}

{ Enable the following option to render directly to a framebuffer surface.
  Note that this option currently only works on Ultibo. }
{$DEFINE SRT_RENDER_TO_FB}

{$IF DEFINED(ULTIBO) AND DEFINED(SRT_RENDER_TO_FB)}
  {$DEFINE SRT_RENDER_TO_FB_ULTIBO}
  {$DEFINE SRT_RENDER_TO_FB_ENABLED}
{$ENDIF}

uses
{$IFDEF SRT_RENDER_TO_GDI_ENABLED}
  PXL.Surfaces.GDI,
{$ENDIF}

{$IFDEF SRT_RENDER_TO_FB_ULTIBO}
  PXL.Surfaces.FB.Ultibo,
{$ENDIF}

  PXL.Types, PXL.Devices, PXL.Surfaces, PXL.SwapChains, PXL.Types.SRT;

type
  TSRTDevice = class(TCustomSwapChainDevice)
  private
    FContext: TSRTDeviceContext;
    FContextWriter: TSRTDeviceContextWriter;

    FSurface: TPixelSurface;
  {$IFDEF SRT_RENDER_TO_GDI_ENABLED}
    FPrimarySurface: TGDIPixelSurface;
  {$ENDIF}
  {$IFDEF SRT_RENDER_TO_FB_ENABLED}
    FPrimarySurface: TFBPixelSurface;
  {$ENDIF}
  protected
    function GetDeviceContext: TCustomDeviceContext; override;

    function InitDevice: Boolean; override;
    procedure DoneDevice; override;
  public
    constructor Create(const AProvider: TCustomDeviceProvider);
    destructor Destroy; override;

    function Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single;
      const StencilValue: Cardinal): Boolean; override;

    function BeginScene(const SwapChainIndex: Integer): Boolean; override;
    function EndScene: Boolean; override;

    property Surface: TPixelSurface read FSurface;

  {$IFDEF SRT_RENDER_TO_GDI_ENABLED}
    property PrimarySurface: TGDIPixelSurface read FPrimarySurface;
  {$ENDIF}
  {$IFDEF SRT_RENDER_TO_FB_ENABLED}
    property PrimarySurface: TFBPixelSurface read FPrimarySurface;
  {$ENDIF}
  end;

implementation

{$IFDEF SRT_RENDER_TO_GDI_ENABLED}
uses
  Windows;
{$ENDIF}

{$IFDEF SRT_RENDER_TO_FB_ULTIBO}
uses
  GlobalConst, Framebuffer;
{$ENDIF}

constructor TSRTDevice.Create(const AProvider: TCustomDeviceProvider);
begin
  inherited;

  FContext := TSRTDeviceContext.Create(Self, FContextWriter);

  FTechnology := TDeviceTechnology.Software;
  FTechVersion := $100;

{$IFDEF SRT_RENDER_TO_GDI_ENABLED}
  FPrimarySurface := TGDIPixelSurface.Create;
{$ELSE}
  {$IFDEF SRT_RENDER_TO_FB_ENABLED}
    FPrimarySurface := TFBPixelSurface.Create;
  {$ELSE}
    FSurface := TPixelSurface.Create;
  {$ENDIF}
{$ENDIF}
end;

destructor TSRTDevice.Destroy;
begin
  inherited;
  
{$IFDEF SRT_RENDER_TO_GDI_ENABLED}
  if FSurface <> FPrimarySurface then
    FSurface.Free;
  FPrimarySurface.Free;
{$ELSE}
  {$IFDEF SRT_RENDER_TO_FB_ENABLED}
    if FSurface <> FPrimarySurface then
      FSurface.Free;
    FPrimarySurface.Free;
  {$ELSE}
    FSurface.Free;
  {$ENDIF}
{$ENDIF}

  FContext.Free;
end;

function TSRTDevice.GetDeviceContext: TCustomDeviceContext;
begin
  Result := FContext;
end;

function TSRTDevice.InitDevice: Boolean;
{$IFDEF SRT_RENDER_TO_GDI_ENABLED}
var
  SwapChainInfo: PSwapChainInfo;
{$ENDIF}

{$IFDEF SRT_RENDER_TO_FB_ULTIBO}
var
  SwapChainInfo: PSwapChainInfo;
{$ENDIF}

begin
{$IFDEF SRT_RENDER_TO_GDI_ENABLED}
  SwapChainInfo := SwapChains[0];
  if SwapChainInfo = nil then
    Exit(False);

  FPrimarySurface.SetSize(SwapChainInfo.Width, SwapChainInfo.Height, SwapChainInfo.Format);

  if (SwapChainInfo.Format <> TPixelFormat.Unknown) and (FPrimarySurface.PixelFormat <> SwapChainInfo.Format) then
  begin
    if (FSurface = nil) or (FSurface = FPrimarySurface) then
      FSurface := TPixelSurface.Create;

    FSurface.SetSize(FPrimarySurface.Size, SwapChainInfo.Format);
  end
  else
  begin
    if (FSurface <> FPrimarySurface) and (FSurface <> nil) then
      FSurface.Free;

    FSurface := FPrimarySurface;
  end;

  FSurface.Clear(0);
{$ENDIF}

{$IFDEF SRT_RENDER_TO_FB_ULTIBO}
  SwapChainInfo := SwapChains[0];
  if SwapChainInfo = nil then
    Exit(False);

  FPrimarySurface.SetSize(SwapChainInfo.Width, SwapChainInfo.Height, SwapChainInfo.Format);

  if (SwapChainInfo.Format <> TPixelFormat.Unknown) and (FPrimarySurface.PixelFormat <> SwapChainInfo.Format) then
  begin
    if (FSurface = nil) or (FSurface = FPrimarySurface) then
      FSurface := TPixelSurface.Create;

    FSurface.SetSize(FPrimarySurface.Size, SwapChainInfo.Format);
  end
  else
  begin
    if (FSurface <> FPrimarySurface) and (FSurface <> nil) then
      FSurface.Free;

    FSurface := FPrimarySurface;
  end;

  FSurface.Clear(0);
{$ENDIF}

  FContextWriter.Surface := FSurface;
  FContextWriter.SurfaceSize := FSurface.Size;

  Result := True;
end;

procedure TSRTDevice.DoneDevice;
begin
  FContextWriter.Surface := nil;
end;

function TSRTDevice.Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single;
  const StencilValue: Cardinal): Boolean;
begin
  if ClearTypes <> [TClearType.Color] then
    Exit(False);

  if (FSurface = nil) or FSurface.IsEmpty then
    Exit(False);

  FSurface.Clear(ColorValue);

  Result := True;
end;

function TSRTDevice.BeginScene(const SwapChainIndex: Integer): Boolean;
{$IFNDEF SRT_RENDER_TO_GDI_ENABLED}
{$IFNDEF SRT_RENDER_TO_FB_ULTIBO}
var
  SwapChainInfo: PSwapChainInfo;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF SRT_RENDER_TO_GDI_ENABLED}
  Result := SwapChainIndex = 0;
{$ELSE}
  {$IFDEF SRT_RENDER_TO_FB_ULTIBO}
    Result := SwapChainIndex = 0;
  {$ELSE}
    SwapChainInfo := SwapChains[SwapChainIndex];
    if SwapChainInfo = nil then
      Exit(False);
    
    FSurface.SetSize(SwapChainInfo.Width, SwapChainInfo.Height, SwapChainInfo.Format);
    SwapChainInfo.Format := FSurface.PixelFormat;
    
    FContextWriter.SurfaceSize := FSurface.Size;
    
    Result := True;
  {$ENDIF}  
{$ENDIF}
end;

function TSRTDevice.EndScene: Boolean;
{$IFDEF SRT_RENDER_TO_GDI_ENABLED}
var
  SwapChainInfo: PSwapChainInfo;
  DestDC: TUntypedHandle;
{$ENDIF}

{$IFDEF SRT_RENDER_TO_FB_ULTIBO}
var
  SwapChainInfo: PSwapChainInfo;
{$ENDIF}

begin
{$IFDEF SRT_RENDER_TO_GDI_ENABLED}
  SwapChainInfo := SwapChains[0];
  if SwapChainInfo = nil then
    Exit(False);

  if FSurface <> FPrimarySurface then
    FPrimarySurface.CopyFrom(FSurface);

  FPrimarySurface.ResetAlpha(False);

  DestDC := GetDC(SwapChainInfo.WindowHandle);
  if DestDC <> 0 then
  try
    FPrimarySurface.BitBlt(DestDC, ZeroPoint2px, FPrimarySurface.Size, ZeroPoint2px);
  finally
    ReleaseDC(SwapChainInfo.WindowHandle, DestDC);
  end;
{$ENDIF}

{$IFDEF SRT_RENDER_TO_FB_ULTIBO}
  SwapChainInfo := SwapChains[0];
  if SwapChainInfo = nil then
    Exit(False);

  if FSurface <> FPrimarySurface then
    FPrimarySurface.CopyFrom(FSurface);

  FPrimarySurface.ResetAlpha(False);

  FPrimarySurface.PutRect(SwapChainInfo.WindowHandle, ZeroPoint2px, Point2px(SwapChainInfo.Width, SwapChainInfo.Height), ZeroPoint2px, FPrimarySurface.Size);
{$ENDIF}

  Result := True;
end;

end.
