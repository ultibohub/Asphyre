unit PXL.Devices.GLES.Ultibo;
{
  This file is part of Asphyre Framework, also known as Platform eXtended Library (PXL).
  Copyright (c) 2000 - 2016  Yuriy Kotsarenko
  Ultibo port Copyright (c) 2017 Garry Wood <garry@softoz.com.au>

  The contents of this file are subject to the Mozilla Public License Version 2.0 (the "License");
  you may not use this file except in compliance with the License. You may obtain a copy of the
  License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
  KIND, either express or implied. See the License for the specific language governing rights and
  limitations under the License.
}
interface

{$INCLUDE PXL.Config.inc}

{ These libraries need to be linked in the following order, which is opposite to how they are loaded in "gles20.pas".
  If they are loaded in different order, it will cause linking failure as libEGL.so requires libGLESv2.so. }
{$LINKLIB libGLESv2}
{$LINKLIB libEGL}

uses
  gles20, PXL.TypeDef, PXL.Types, PXL.Devices, PXL.SwapChains, PXL.Types.GLES, PXL.Boards.PI.BCM;

type
  TGLESDevice = class(TCustomSwapChainDevice)
  private
    FDeviceContext: TGLESDeviceContext;

    FNativeWindow: EGL_DISPMANX_WINDOW_T;
    FDispmanElement: DISPMANX_ELEMENT_HANDLE_T;
    FDispmanDisplay: DISPMANX_DISPLAY_HANDLE_T;
    FDispmanUpdate: DISPMANX_UPDATE_HANDLE_T;

    FDisplay: EGLDisplay;
    FSurface: EGLSurface;
    FContext: EGLContext;

    FScreenSize: TPoint2px;
    FViewportSize: TPoint2px;
    FViewportPosition: TPoint2px;

    function InitializeDisplay: Boolean;
    procedure FinalizeDisplay;
  protected
    function GetDeviceContext: TCustomDeviceContext; override;

    function InitDevice: Boolean; override;
    procedure DoneDevice; override;
    
    function ResizeSwapChain(const SwapChainIndex: Integer; const NewSwapChainInfo: PSwapChainInfo): Boolean; override;
    
    procedure SetViewportPosition(const Position: TPoint2px);
    procedure UpdateViewport(var SrcRect, DestRect: VC_RECT_T); 
  public
    constructor Create(const AProvider: TCustomDeviceProvider);
    destructor Destroy; override;

    function Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single = 1.0;
      const StencilValue: Cardinal = 0): Boolean; override;
    
    function Move(const SwapChainIndex: Integer; const NewPosition: TPoint2px): Boolean; override;
    
    function BeginScene(const SwapChainIndex: Integer = 0): Boolean; override;
    function EndScene: Boolean; override;

    property Display: EGLDisplay read FDisplay;
    property Surface: EGLSurface read FSurface;
    property Context: EGLContext read FContext;

    property ScreenSize: TPoint2px read FScreenSize;
    property WindowSize: TPoint2px read FViewportSize;
    property WindowPosition: TPoint2px read FViewportPosition write SetViewportPosition;
  end;

implementation

uses
  SysUtils;

constructor TGLESDevice.Create(const AProvider: TCustomDeviceProvider);
begin
  inherited;

  bcm_host_init;

  FDeviceContext := TGLESDeviceContext.Create(Self);

  FTechnology := TDeviceTechnology.OpenGL_ES;
  FTechVersion := $200;
end;

destructor TGLESDevice.Destroy;
begin
  inherited;
  
  FreeAndNil(FDeviceContext);
  bcm_host_deinit;
end;

function TGLESDevice.GetDeviceContext: TCustomDeviceContext;
begin
  Result := FDeviceContext;
end;

function TGLESDevice.InitializeDisplay: Boolean;
const
  ContextAttribs: array[0..2] of EGLint = (EGL_CONTEXT_CLIENT_VERSION, 2, EGL_NONE);
const
  ConfigAttribs: array[0..10] of EGLint = (
    EGL_RENDERABLE_TYPE, EGL_OPENGL_ES2_BIT,
    EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
    EGL_BLUE_SIZE, 8,
    EGL_GREEN_SIZE, 8,
    EGL_RED_SIZE, 8,
    EGL_NONE);
var
  SwapChainInfo: PSwapChainInfo;
  Config: EGLConfig;
  NumConfigs: EGLint;
  DisplayWidth, DisplayHeight: LongWord;
  SrcRect, DestRect: VC_RECT_T;
  Alpha: VC_DISPMANX_ALPHA_T;
begin
  SwapChainInfo := SwapChains[0];
  if SwapChainInfo = nil then
    Exit(False);
    
  FDisplay := eglGetDisplay(EGL_DEFAULT_DISPLAY);
  if FDisplay = nil then
    Exit(False);

  if eglInitialize(FDisplay, nil, nil) <> EGL_TRUE then
    Exit(False);

  if eglChooseConfig(FDisplay, @ConfigAttribs[0], @Config, 1, @NumConfigs) <> EGL_TRUE then
  begin
    FinalizeDisplay;
    Exit(False);
  end;

  if eglBindAPI(EGL_OPENGL_ES_API) <> EGL_TRUE then
  begin
    FinalizeDisplay;
    Exit(False);
  end;

  FContext := eglCreateContext(FDisplay, Config, EGL_NO_CONTEXT, @ContextAttribs[0]);
  if FContext = EGL_NO_CONTEXT then
  begin
    FinalizeDisplay;
    Exit(False);
  end;

  if graphics_get_display_size(DISPMANX_ID_MAIN_LCD, @DisplayWidth, @DisplayHeight) < 0 then
  begin
    FinalizeDisplay;
    Exit(False);
  end;

  FScreenSize := Point2px(DisplayWidth, DisplayHeight);

  FDispmanDisplay := vc_dispmanx_display_open(DISPMANX_ID_MAIN_LCD);
  if FDispmanDisplay = DISPMANX_NO_HANDLE then
  begin
    FinalizeDisplay;
    Exit(False);
  end;

  FDispmanUpdate := vc_dispmanx_update_start(0);
  if FDispmanUpdate = DISPMANX_NO_HANDLE then
  begin
    FinalizeDisplay;
    Exit(False);
  end;

  FViewportSize := Point2px(SwapChainInfo.Width, SwapChainInfo.Height);
  
  UpdateViewport(SrcRect, DestRect);

  FillChar(Alpha, SizeOf(VC_DISPMANX_ALPHA_T), 0);
  Alpha.flags := DISPMANX_FLAGS_ALPHA_FIXED_ALL_PIXELS;
  Alpha.opacity := 255;

  FDispmanElement := vc_dispmanx_element_add(FDispmanUpdate, FDispmanDisplay, 0, @DestRect, 0, @SrcRect,
    DISPMANX_PROTECTION_NONE, @Alpha, nil, 0);
  if FDispmanElement = DISPMANX_NO_HANDLE then
  begin
    FinalizeDisplay;
    Exit(False);
  end;

  FNativeWindow.element := FDispmanElement;
  FNativeWindow.width := FViewportSize.X;
  FNativeWindow.height := FViewportSize.Y;

  vc_dispmanx_update_submit_sync(FDispmanUpdate);

  FSurface := eglCreateWindowSurface(FDisplay, Config, EGLNativeWindowType(@FNativeWindow), nil);
  if FSurface = EGL_NO_SURFACE then
  begin
    FinalizeDisplay;
    Exit(False);
  end;

  if eglMakeCurrent(FDisplay, FSurface, FSurface, FContext) <> EGL_TRUE then
  begin
    FinalizeDisplay;
    Exit(False);
  end;

  Result := True;
end;

procedure TGLESDevice.FinalizeDisplay;
begin
  if FDispmanDisplay <> DISPMANX_NO_HANDLE then
  begin
    vc_dispmanx_display_close(FDispmanDisplay);
    FDispmanDisplay := DISPMANX_NO_HANDLE;
  end;

  if FDisplay <> EGL_NO_DISPLAY then
  begin
    eglMakeCurrent(FDisplay, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);

    if FContext <> EGL_NO_CONTEXT then
    begin
      eglDestroyContext(FDisplay, FContext);
      FContext := EGL_NO_CONTEXT;
    end;

    if FSurface <> EGL_NO_SURFACE then
    begin
      eglDestroySurface(FDisplay, FSurface);
      FSurface := EGL_NO_SURFACE;
    end;

    eglTerminate(FDisplay);
    FDisplay := EGL_NO_DISPLAY;
  end;
end;

function TGLESDevice.InitDevice: Boolean;
begin
  if not InitializeDisplay then
    Exit(False);

  Result := True;
end;

procedure TGLESDevice.DoneDevice;
begin
  FinalizeDisplay;
end;

function TGLESDevice.ResizeSwapChain(const SwapChainIndex: Integer; const NewSwapChainInfo: PSwapChainInfo): Boolean; 
begin
  if SwapChainIndex <> 0 then
    Exit(False);

  Result := False;
end;

procedure TGLESDevice.SetViewportPosition(const Position: TPoint2px);
var
  SrcRect, DestRect: VC_RECT_T;
begin
  FViewportPosition := Position;
  
  if FDispmanElement = DISPMANX_NO_HANDLE then
    Exit;
   
  UpdateViewport(SrcRect, DestRect);
  
  FDispmanUpdate := vc_dispmanx_update_start(0);
  if FDispmanUpdate = DISPMANX_NO_HANDLE then
    Exit;
    
  vc_dispmanx_element_change_attributes(FDispmanUpdate, FDispmanElement, 0, 0, 0, @DestRect, @SrcRect, 0, DISPMANX_NO_ROTATE);
  
  vc_dispmanx_update_submit_sync(FDispmanUpdate);
end;

procedure TGLESDevice.UpdateViewport(var SrcRect, DestRect: VC_RECT_T); 
begin
  //Check Size
  if FViewportSize.X > FScreenSize.X then
    FViewportSize.X := FScreenSize.X;

  if FViewportSize.Y > FScreenSize.Y then
    FViewportSize.Y := FScreenSize.Y;
  
  //Check Position
  if FViewportPosition.X < 0 then
    FViewportPosition.X := 0;
    
  if FViewportPosition.Y < 0 then
    FViewportPosition.Y := 0;

  if (FViewportPosition.X + FViewportSize.X) > FScreenSize.X then
    FViewportPosition.X := FScreenSize.X - FViewportSize.X;

  if (FViewportPosition.Y + FViewportSize.Y) > FScreenSize.Y then
    FViewportPosition.Y := FScreenSize.Y - FViewportSize.Y;
    
  //Fill Rects
  DestRect.x := FViewportPosition.X;
  DestRect.y := FViewportPosition.Y;
  DestRect.width := FViewportSize.X;
  DestRect.height := FViewportSize.Y;

  SrcRect.x := FViewportPosition.X;
  SrcRect.y := FViewportPosition.Y;
  SrcRect.width := FViewportSize.X shl 16;
  SrcRect.height := FViewportSize.Y shl 16;
end;

function TGLESDevice.Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor;
  const DepthValue: Single; const StencilValue: Cardinal): Boolean;
var
  Flags: Cardinal;
begin
  if ClearTypes = [] then
    Exit(False);

  Flags := 0;

  if TClearType.Color in ClearTypes then
  begin
    glClearColor(TIntColorRec(ColorValue).Red / 255.0, TIntColorRec(ColorValue).Green / 255.0,
      TIntColorRec(ColorValue).Blue / 255.0, TIntColorRec(ColorValue).Alpha / 255.0);
    Flags := Flags or GL_COLOR_BUFFER_BIT;
  end;

  if TClearType.Depth in ClearTypes then
  begin
    glClearDepthf(DepthValue);
    Flags := Flags or GL_DEPTH_BUFFER_BIT;
  end;

  if TClearType.Stencil in ClearTypes then
  begin
    glClearStencil(StencilValue);
    Flags := Flags or GL_STENCIL_BUFFER_BIT;
  end;

  glClear(Flags);

  Result := glGetError = GL_NO_ERROR;
end;

function TGLESDevice.Move(const SwapChainIndex: Integer; const NewPosition: TPoint2px): Boolean; 
begin 
  if SwapChainIndex <> 0 then
    Exit(False);
    
  SetViewportPosition(NewPosition);
  
  Result := True;
end;

function TGLESDevice.BeginScene(const SwapChainIndex: Integer): Boolean;
begin
  if SwapChainIndex <> 0 then
    Exit(False);

  glViewport(0, 0, FViewportSize.X, FViewportSize.Y);
  Result := True;
end;

function TGLESDevice.EndScene: Boolean;
begin
  eglSwapBuffers(FDisplay, FSurface);
  Result := glGetError = GL_NO_ERROR;
end;

end.
