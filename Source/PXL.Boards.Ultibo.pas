unit PXL.Boards.Ultibo;
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
  SysUtils, GlobalConst, GlobalConfig, Platform, GPIO, UART, I2C, SPI, PXL.TypeDef, PXL.Boards.Types;

type
  TUltiboSystemCore = class(TCustomSystemCore)
  strict private 
    FBaseClock: LongWord;
    FPageSize: LongWord;
  public
    property BaseClock:LongWord read FBaseClock;
    property PageSize:LongWord read FPageSize;
  public
    constructor Create;
    destructor Destroy; override;

    { Returns the current value of system timer as 64-bit unsigned integer, in microseconds. }
    function GetTickCount: UInt64; override;

    { Waits for the specified amount of microseconds, calling NanoSleep if waiting time is long enough for the most
      portion of wait time, while the remaining part doing a busy wait to provide greater accuracy. }
    procedure Delay(const MicroSeconds: Cardinal); override;
  end;
  
  TPinModeEx = (Input = GPIO_FUNCTION_IN,
                Output = GPIO_FUNCTION_OUT,
                Alt0 = GPIO_FUNCTION_ALT0,
                Alt1 = GPIO_FUNCTION_ALT1,
                Alt2 = GPIO_FUNCTION_ALT2,
                Alt3 = GPIO_FUNCTION_ALT3,
                Alt4 = GPIO_FUNCTION_ALT4,
                Alt5 = GPIO_FUNCTION_ALT5);
  
  TUltiboGPIO = class(TCustomGPIO)
  strict private
    FSystemCore: TUltiboSystemCore;
    FGPIO: PGPIODevice;
    
    function GetPinModeEx(const Pin: Integer): TPinModeEx;
    procedure SetPinModeEx(const Pin: Integer; const Value: TPinModeEx);
  protected
    function GetPinMode(const Pin: Integer): TPinMode; override;
    procedure SetPinMode(const Pin: Integer; const Mode: TPinMode); override;

    function GetPinValue(const Pin: Integer): TPinValue; override;
    procedure SetPinValue(const Pin: Integer; const Value: TPinValue); override;
  public
    constructor Create(const ASystemCore: TUltiboSystemCore; AGPIO: PGPIODevice = nil);
    destructor Destroy; override;

    { Quickly changes specified pin value (assuming it is set for output).}
    procedure SetFastValue(const Pin: Integer; const Value: TPinValue);

    { Reference to @link(TUltiboSystemCore), which provides timing and delay utilities. }
    property SystemCore: TUltiboSystemCore read FSystemCore;
    
    { Determine what GPIO device this instance is connected to. }
    property GPIO: PGPIODevice read FGPIO;

    { Provides control and feedback of currently selected mode for the given pin, including alternative functions. }
    property PinModeEx[const Pin: Integer]: TPinModeEx read GetPinModeEx write SetPinModeEx;
  end;
  
  TUltiboSPI = class(TCustomPortSPI)
  public const
    DefaultChipSelect = 0;
    DefaultFrequency = 8000000;
    DefaultMode = 0;
  strict private
    FSPI: PSPIDevice;
    
    FClockPhase: LongWord;
    FClockPolarity: LongWord;
  private
    FMode: Integer;
    FFrequency: Integer;
    FChipSelectAttributes: TChipSelectAttributes;
    FChipSelect: Integer;
  
    procedure SetChipSelect(const Value: Integer);
  protected
    function GetMode: Integer; override;
    procedure SetMode(const Value: Integer); override;
    function GetBitsPerWord: Integer; override;
    procedure SetBitsPerWord(const Value: Integer); override;
    function GetFrequency: Integer; override;
    procedure SetFrequency(const Value: Integer); override;
    function GetChipSelectAttributes: TChipSelectAttributes; override;
    procedure SetChipSelectAttributes(const Value: TChipSelectAttributes); override;
  public
    constructor Create(ASPI: PSPIDevice = nil; const AChipSelect: Integer = DefaultChipSelect;
                       const AFrequency: Integer = DefaultFrequency; const AMode: Integer = DefaultMode);
    destructor Destroy; override;

    function Read(const Buffer: Pointer; const BufferSize: Integer): Integer; override;
    function Write(const Buffer: Pointer; const BufferSize: Integer): Integer; override;
    function Transfer(const ReadBuffer, WriteBuffer: Pointer; const BufferSize: Integer): Integer; override;

    { Determine what SPI device this instance is connected to. }
    property SPI: PSPIDevice read FSPI;
    
    { Defines clock polarity and phase for SPI operation. }
    property Mode: Integer read FMode write SetMode;

    { Controls the operating frequency of SPI bus in Hz, default value for SPI is 8 mHz. }
    property Frequency: Integer read FFrequency write SetFrequency;

    { Defines what Chip Select line to enable during transfers. Supported values are SPI_CS_0 to SPI_CS_15. }
    property ChipSelect: Integer read FChipSelect write SetChipSelect;

    { Determines how Chip Select line is handled by protocol. }
    property ChipSelectAttributes: TChipSelectAttributes read FChipSelectAttributes write SetChipSelectAttributes;
  end;

  TUltiboI2C = class(TCustomPortI2C)
  public const
    DefaultFrequency = 100000;
  strict private
    FI2C: PI2CDevice;
  
    FAddress: Integer;
    FFrequency: Integer;
  
    procedure SetFrequency(const Value: Integer);
  public
    constructor Create(AI2C: PI2CDevice = nil; const AFrequency: Integer = DefaultFrequency);
    destructor Destroy; override;

    procedure SetAddress(const Address: Integer); override;

    function Read(const Buffer: Pointer; const BufferSize: Integer): Integer; override;
    function Write(const Buffer: Pointer; const BufferSize: Integer): Integer; override;

    function ReadBlockData(const Command: Byte; const Buffer: Pointer; const BufferSize: Integer): Integer; override;
    function WriteBlockData(const Command: Byte; const Buffer: Pointer; const BufferSize: Integer): Integer; override;

    { Determine what I2C device this instance is connected to. }
    property I2C: PI2CDevice read FI2C;
    
    { Controls the operating frequency of I2C bus in Hz. }
    property Frequency: Integer read FFrequency write SetFrequency;
    
    { Configures the address to read and write from and to.}
    property Address: Integer read FAddress write SetAddress;
  end;
  
  TUltiboUART = class(TCustomPortUART)
  strict private
    FUART: PUARTDevice;
    
    FMaxSupportedBaudRate: LongWord;
  private
    FOpen: Boolean;
    
    FBaudRate: Integer;
    FBitsPerWord: Integer;
    FParity: TParity;
    FStopBits: TStopBits;
    
    procedure UpdateState;
  protected
    function GetBaudRate: Integer; override;
    procedure SetBaudRate(const Value: Integer); override;
    function GetBitsPerWord: Integer; override;
    procedure SetBitsPerWord(const Value: Integer); override;
    function GetParity: TParity; override;
    procedure SetParity(const Value: TParity); override;
    function GetStopBits: TStopBits; override;
    procedure SetStopBits(const Value: TStopBits); override;
  public 
    property MaxSupportedBaudRate:LongWord read FMaxSupportedBaudRate;
  public
    constructor Create(AUART: PUARTDevice = nil);
    destructor Destroy; override;

    function Read(const Buffer: Pointer; const BufferSize: Integer): Integer; override;
    function Write(const Buffer: Pointer; const BufferSize: Integer): Integer; override;
    procedure Flush; override;

    { Determine what UART device this instance is connected to. }
    property UART: PUARTDevice read FUART;
    
    property BaudRate: Integer read FBaudRate write SetBaudRate;
    property BitsPerWord: Integer read FBitsPerWord write SetBitsPerWord;
    property Parity: TParity read FParity write SetParity;
    property StopBits: TStopBits read FStopBits write SetStopBits;
  end;
  
  EUltiboGeneric = class(Exception);
  
  EUltiboInvalidParams = class(EUltiboGeneric);
  
  EGPIOGeneric = class(EUltiboGeneric);
  EGPIOUnsupported = class(EGPIOGeneric);
  
  EGPIOInvalidPin = class(EGPIOGeneric);
  EGPIOAlternateFunctionPin = class(EGPIOGeneric);
  
  ESPIGeneric = class(EUltiboGeneric);
  ESPIUnsupportedGeneric = class(ESPIGeneric);
  ESPIUnsupportedMode = class(ESPIUnsupportedGeneric);
  ESPIUnsupportedBitsPerWord = class(ESPIUnsupportedGeneric);
  ESPIUnsupportedFrequency = class(ESPIUnsupportedGeneric);
  ESPIUnsupportedChipSelect = class(ESPIUnsupportedGeneric);
  
  EI2CGeneric = class(EUltiboGeneric);
  EI2CUnsupportedGeneric = class(EI2CGeneric);
  EI2CUnsupportedFrequency = class(EI2CUnsupportedGeneric);
  
  EUARTGeneric = class(EUltiboGeneric);
  EUARTOpen = class(EUARTGeneric);
  EUARTClose = class(EUARTGeneric);
  
  ESystemCoreRefRequired = class(EUltiboGeneric);
  EGPIORefRequired = class(EUltiboGeneric);
  
resourcestring
  SInvalidParameters = 'The specified parameters are invalid.';
  
  SGPIOSpecifiedPinInvalid = 'The specified GPIO pin <%d> is invalid.';
  SGPIOSpecifiedPinAlternativeMode = 'The specified GPIO pin <%d> has non-basic alternative mode.';

  SGPIOUnsupported = 'The requested feature is unsupported.';

  SSPIUnsupportedMode = 'Specified SPI mode <%d> is not supported.';
  SSPIUnsupportedBitsPerWord = 'Specified SPI number of bits per word <%d> is not supported.';
  SSPIUnsupportedFrequency = 'Specified SPI frequency <%d> is not supported.';
  SSPIUnsupportedChipSelect = 'Specified SPI chip select line <%d> is not supported.';

  SI2CUnsupportedFrequency = 'Specified I2C frequency <%d> is not supported.';

  SUARTOpen = 'Cannot open UART.';
  SUARTClose = 'Cannot close UART.';
  
  SSystemCoreRefNotProvided = 'Reference to TUltiboSystemCore has not been provided.';
  SGPIORefNotProvided = 'Reference to TUltiboGPIO has not been provided.';

implementation
  
{$REGION 'Global Types and Functions'}
  
{$ENDREGION}
{$REGION 'TUltiboSystemCore'}

constructor TUltiboSystemCore.Create;
begin
  inherited;
  
  FBaseClock := ClockGetRate(CLOCK_ID_CORE);
  FPageSize := MemoryGetPageSize;
end;

destructor TUltiboSystemCore.Destroy;
begin
  inherited;
end;
  
function TUltiboSystemCore.GetTickCount: UInt64;
begin
  if CLOCK_CYCLES_PER_MICROSECOND > 0 then
    Result := ClockGetTotal div CLOCK_CYCLES_PER_MICROSECOND
  else
    Result := ClockGetTotal;
end;

procedure TUltiboSystemCore.Delay(const MicroSeconds: Cardinal);
var
  Remain: Cardinal;
begin
  Remain := MicroSeconds;
  
  if MicroSeconds > 1000 then
  begin
   Sleep(MicroSeconds div 1000);
   Remain := MicroSeconds mod 1000;
  end;
  
  if Remain > 0 then
  begin
   MicrosecondDelay(Remain);
  end;
end;

{$ENDREGION}
{$REGION 'TUltiboGPIO'}
  
constructor TUltiboGPIO.Create(const ASystemCore: TUltiboSystemCore; AGPIO: PGPIODevice);
begin
  inherited Create;

  FSystemCore := ASystemCore;
  if FSystemCore = nil then
    raise ESystemCoreRefRequired.Create(SSystemCoreRefNotProvided);

  FGPIO := AGPIO;
  if FGPIO = nil then
    FGPIO := GPIODeviceGetDefault;
    
end;

destructor TUltiboGPIO.Destroy;
begin
  inherited;
end;
  
function TUltiboGPIO.GetPinModeEx(const Pin: Integer): TPinModeEx;
begin
  case GPIODeviceFunctionGet(FGPIO, Pin) of
    GPIO_FUNCTION_IN:
      Result := TPinModeEx.Input;
    GPIO_FUNCTION_OUT:
      Result := TPinModeEx.Output;
    GPIO_FUNCTION_ALT0:
      Result := TPinModeEx.Alt0;
    GPIO_FUNCTION_ALT1:
      Result := TPinModeEx.Alt1;
    GPIO_FUNCTION_ALT2:
      Result := TPinModeEx.Alt2;
    GPIO_FUNCTION_ALT3:
      Result := TPinModeEx.Alt3;
    GPIO_FUNCTION_ALT4:
      Result := TPinModeEx.Alt4;
    GPIO_FUNCTION_ALT5:
      Result := TPinModeEx.Alt5;
  else
   raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));
  end;
end;

procedure TUltiboGPIO.SetPinModeEx(const Pin: Integer; const Value: TPinModeEx);
begin
  GPIODeviceFunctionSelect(FGPIO, Pin, LongWord(Value));
end;
  
function TUltiboGPIO.GetPinMode(const Pin: Integer): TPinMode; 
begin
  case GetPinModeEx(Pin) of
    TPinModeEx.Input:
      Result := TPinMode.Input;
    TPinModeEx.Output:
      Result := TPinMode.Output;
  else
    raise EGPIOAlternateFunctionPin.Create(Format(SGPIOSpecifiedPinAlternativeMode, [Pin]));
  end;
end;

procedure TUltiboGPIO.SetPinMode(const Pin: Integer; const Mode: TPinMode); 
begin
  if Mode = TPinMode.Output then
    SetPinModeEx(Pin, TPinModeEx.Output)
  else
    SetPinModeEx(Pin, TPinModeEx.Input);
end;

function TUltiboGPIO.GetPinValue(const Pin: Integer): TPinValue; 
begin
  case GPIODeviceInputGet(FGPIO, Pin) of
    GPIO_LEVEL_LOW:
      Result := TPinValue.Low;
    GPIO_LEVEL_HIGH:
      Result := TPinValue.High;
  else
    raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));
  end;  
end;

procedure TUltiboGPIO.SetPinValue(const Pin: Integer; const Value: TPinValue); 
begin
  if Value = TPinValue.Low then
    GPIODeviceOutputSet(FGPIO, Pin, GPIO_LEVEL_LOW)
  else
    GPIODeviceOutputSet(FGPIO, Pin, GPIO_LEVEL_HIGH);
end;
  
procedure TUltiboGPIO.SetFastValue(const Pin: Integer; const Value: TPinValue);
begin
  if Value = TPinValue.Low then
    GPIODeviceOutputSet(FGPIO, Pin, GPIO_LEVEL_LOW)
  else
    GPIODeviceOutputSet(FGPIO, Pin, GPIO_LEVEL_HIGH);
end;
  
{$ENDREGION}
{$REGION 'TUltiboSPI'}
  
constructor TUltiboSPI.Create(ASPI: PSPIDevice; const AChipSelect: Integer;
                   const AFrequency: Integer; const AMode: Integer);
begin
  inherited Create;
  
  FSPI := ASPI;
  if FSPI = nil then
    FSPI := SPIDeviceGetDefault;
  
  FClockPhase := SPI_CLOCK_PHASE_UNKNOWN;
  FClockPolarity := SPI_CLOCK_POLARITY_UNKNOWN;
  
  FChipSelect := -1;
  FMode := -1;
  FFrequency := -1;

  SetChipSelect(AChipSelect);
  SetMode(AMode);
  SetFrequency(AFrequency);
  
  SPIDeviceStart(FSPI, SPI_MODE_4WIRE, FFrequency, FClockPhase, FClockPolarity);
end;

destructor TUltiboSPI.Destroy;
begin
  SPIDeviceStop(FSPI);

  inherited;
end;
  
procedure TUltiboSPI.SetChipSelect(const Value: Integer);
begin
  if FChipSelect <> Value then
  begin
    FChipSelect := Value;
  end;  
end;
  
function TUltiboSPI.GetMode: Integer; 
begin
  Result := FMode;
end;
  
procedure TUltiboSPI.SetMode(const Value: Integer); 
begin
  if FMode <> Value then
  begin
    if Value = 0 then
    begin
     {Value 0 (CPOL = 0 / CPHA = 0)}
     FClockPolarity := SPI_CLOCK_POLARITY_LOW;
     FClockPhase := SPI_CLOCK_PHASE_LOW;
    end
    else if Value = 1 then
    begin
     {Value 1 (CPOL = 0 / CPHA = 1)}
     FClockPolarity := SPI_CLOCK_POLARITY_LOW;
     FClockPhase := SPI_CLOCK_PHASE_HIGH;
    end
    else if Value = 2 then
    begin
     {Value 2 (CPOL = 1 / CPHA = 0)}
     FClockPolarity := SPI_CLOCK_POLARITY_HIGH;
     FClockPhase := SPI_CLOCK_PHASE_LOW;
    end
    else if Value = 3 then
    begin
     {Value 3 (CPOL = 1 / CPHA = 1)}
     FClockPolarity := SPI_CLOCK_POLARITY_HIGH;
     FClockPhase := SPI_CLOCK_PHASE_HIGH;
    end;  
  
    if SPIDeviceSetClockPolarity(FSPI, FClockPolarity) <> ERROR_SUCCESS then
      raise ESPIUnsupportedMode.Create(Format(SSPIUnsupportedMode, [Value]));

    if SPIDeviceSetClockPhase(FSPI, FClockPhase) <> ERROR_SUCCESS then
      raise ESPIUnsupportedMode.Create(Format(SSPIUnsupportedMode, [Value]));
    
    FMode := Value;
  end;
end;
  
function TUltiboSPI.GetBitsPerWord: Integer; 
begin
  Result := 8;
end;
  
procedure TUltiboSPI.SetBitsPerWord(const Value: Integer); 
begin
  if Value <> 8 then
    raise ESPIUnsupportedBitsPerWord.Create(Format(SSPIUnsupportedBitsPerWord, [Value]));
end;
  
function TUltiboSPI.GetFrequency: Integer; 
begin
  Result := FFrequency;
end;
  
procedure TUltiboSPI.SetFrequency(const Value: Integer); 
begin
  if Value < 1 then
    raise ESPIUnsupportedFrequency.Create(Format(SSPIUnsupportedFrequency, [Value]));
    
  if FFrequency <> Value then
  begin
    if SPIDeviceSetClockRate(FSPI, FChipSelect, Value) <> ERROR_SUCCESS then
      raise ESPIUnsupportedFrequency.Create(Format(SSPIUnsupportedFrequency, [Value]));
    
    FFrequency := Value;
  end;
end;
  
function TUltiboSPI.GetChipSelectAttributes: TChipSelectAttributes; 
begin
  Result := FChipSelectAttributes;
end;
  
procedure TUltiboSPI.SetChipSelectAttributes(const Value: TChipSelectAttributes); 
var
  Polarity: LongWord;
begin
  if FChipSelectAttributes <> Value then
  begin
    if TChipSelectAttribute.ActiveHigh in Value then
      Polarity := SPI_CS_POLARITY_HIGH
    else
      Polarity := SPI_CS_POLARITY_LOW;
    
    SPIDeviceSetSelectPolarity(FSPI, FChipSelect, Polarity);
    
    FChipSelectAttributes := Value;
  end;
end;
  
function TUltiboSPI.Read(const Buffer: Pointer; const BufferSize: Integer): Integer; 
begin
  Result := Transfer(Buffer, nil, BufferSize);
end;
  
function TUltiboSPI.Write(const Buffer: Pointer; const BufferSize: Integer): Integer; 
begin
  Result := Transfer(nil, Buffer, BufferSize);
end;
  
function TUltiboSPI.Transfer(const ReadBuffer, WriteBuffer: Pointer; const BufferSize: Integer): Integer; 
var
  Flags: LongWord;
  Count: LongWord;
begin
  Flags := SPI_TRANSFER_NONE;
  if (ReadBuffer = nil) and (WriteBuffer <> nil) and (BufferSize > SIZE_1K) then
  begin
   Flags := SPI_TRANSFER_DMA;
  end;
  
  SPIDeviceWriteRead(FSPI, FChipSelect, WriteBuffer, ReadBuffer, BufferSize, Flags, Count);
  
  Result := Count;
end;

{$ENDREGION}
{$REGION 'TUltiboI2C'}
  
constructor TUltiboI2C.Create(AI2C: PI2CDevice; const AFrequency: Integer);
begin
  inherited Create;
  
  FI2C := AI2C;
  if FI2C = nil then
    FI2C := I2CDeviceGetDefault;
   
  FAddress := I2C_ADDRESS_INVALID;
  FFrequency := -1;
  
  SetFrequency(AFrequency);
  
  I2CDeviceStart(FI2C, AFrequency);
end;

destructor TUltiboI2C.Destroy;
begin
  I2CDeviceStop(FI2C);

  inherited;
end;

procedure TUltiboI2C.SetFrequency(const Value: Integer);
begin
  if Value < 1 then
    raise EI2CUnsupportedFrequency.Create(Format(SI2CUnsupportedFrequency, [Value]));
    
  if FFrequency <> Value then
  begin
    if I2CDeviceSetRate(FI2C, Value) <> ERROR_SUCCESS then
      raise EI2CUnsupportedFrequency.Create(Format(SI2CUnsupportedFrequency, [Value]));
    
    FFrequency := Value;
  end;  
end;
  
procedure TUltiboI2C.SetAddress(const Address: Integer); 
begin
  if FAddress <> Address then
   FAddress := Address;
end;

function TUltiboI2C.Read(const Buffer: Pointer; const BufferSize: Integer): Integer; 
var 
  Count: LongWord;
begin
  I2CDeviceRead(FI2C, FAddress, Buffer, BufferSize, Count);
  
  Result := Count;
end;

function TUltiboI2C.Write(const Buffer: Pointer; const BufferSize: Integer): Integer; 
var 
  Count: LongWord;
begin
  I2CDeviceWrite(FI2C, FAddress, Buffer, BufferSize, Count);
  
  Result := Count;
end;

function TUltiboI2C.ReadBlockData(const Command: Byte; const Buffer: Pointer; const BufferSize: Integer): Integer; 
var 
  Count: LongWord;
begin
  I2CDeviceWriteRead(FI2C, FAddress, @Command, SizeOf(Byte), Buffer, BufferSize, Count);
  
  Result := Count;
end;

function TUltiboI2C.WriteBlockData(const Command: Byte; const Buffer: Pointer; const BufferSize: Integer): Integer; 
var 
  Count: LongWord;
begin
  I2CDeviceWriteWrite(FI2C, FAddress, @Command, SizeOf(Byte), Buffer, BufferSize, Count);
  
  Result := Count;
end;
  
{$ENDREGION}
{$REGION 'TUltiboUART'}
  
constructor TUltiboUART.Create(AUART: PUARTDevice);
var
  Properties: TUARTProperties;
begin
  inherited Create;
  
  FUART := AUART;
  if FUART = nil then
    FUART := UARTDeviceGetDefault;
  
  FMaxSupportedBaudRate := 0;
  if UARTDeviceProperties(FUART, @Properties) = ERROR_SUCCESS then
  begin
    FMaxSupportedBaudRate := Properties.MaxRate;
  end; 
  
  FOpen := False;
  
  FBaudRate := FMaxSupportedBaudRate;
  FBitsPerWord := 8;
  FParity := TParity.None;
  FStopBits := TStopBits.One;
  
  UpdateState;
end;

destructor TUltiboUART.Destroy;
begin
  UARTDeviceClose(FUART);
  
  inherited;
end;
  
procedure TUltiboUART.UpdateState;
var
 Properties: TUARTProperties;
begin
  if FOpen then
  begin
    if UARTDeviceClose(FUART) <> ERROR_SUCCESS then
      raise EUARTClose.Create(SUARTClose);
      
    FOpen := False;
  end;  
  
  FillChar(Properties, SizeOf(TUARTProperties), 0);
  
  Properties.BaudRate := FBaudRate;
  
  case FBitsPerWord of
    5: Properties.DataBits := SERIAL_DATA_5BIT;
    6: Properties.DataBits := SERIAL_DATA_6BIT;
    7: Properties.DataBits := SERIAL_DATA_7BIT;
  else
    Properties.DataBits := SERIAL_DATA_8BIT;
  end;
  
  case FStopBits of
    TStopBits.Two: Properties.StopBits := SERIAL_STOP_2BIT;
    TStopBits.OneDotFive: Properties.StopBits := SERIAL_STOP_1BIT5; 
  else
    Properties.StopBits := SERIAL_STOP_1BIT;  
  end;
  
  case FParity of
    TParity.Even: Properties.Parity := SERIAL_PARITY_EVEN;
    TParity.Odd: Properties.Parity := SERIAL_PARITY_ODD;
  else
     Properties.Parity := SERIAL_PARITY_NONE;
  end;
  
  if UARTDeviceOpen(FUART, Properties.BaudRate, Properties.DataBits, Properties.StopBits,
                    Properties.Parity, SERIAL_FLOW_NONE) <> ERROR_SUCCESS then
    raise EUARTOpen.Create(SUARTOpen);
    
  FOpen := True;
end;

function TUltiboUART.GetBaudRate: Integer; 
begin
  Result := FBaudRate;
end;

procedure TUltiboUART.SetBaudRate(const Value: Integer); 
begin
  if FBaudRate <> Value then
  begin
    FBaudRate := Value;
    
    UpdateState;
  end;
end;

function TUltiboUART.GetBitsPerWord: Integer; 
begin
  Result := FBitsPerWord;
end;

procedure TUltiboUART.SetBitsPerWord(const Value: Integer); 
begin
  if FBitsPerWord <> Value then
  begin
    FBitsPerWord := Value;
    
    UpdateState;
  end;
end;

function TUltiboUART.GetParity: TParity; 
begin
  Result := FParity;
end;

procedure TUltiboUART.SetParity(const Value: TParity); 
begin
  if FParity <> Value then
  begin
    FParity := Value;
    
    UpdateState;
  end;
end;

function TUltiboUART.GetStopBits: TStopBits; 
begin
  Result := FStopBits;
end;

procedure TUltiboUART.SetStopBits(const Value: TStopBits); 
begin
  if FStopBits <> Value then
  begin
    FStopBits := Value;
    
    UpdateState;
  end;
end;

function TUltiboUART.Read(const Buffer: Pointer; const BufferSize: Integer): Integer; 
var
  Count: LongWord;
begin
  if (Buffer = nil) or (BufferSize <= 0) then
    raise EUltiboInvalidParams.Create(SInvalidParameters);
  
  UARTDeviceRead(FUART, Buffer, BufferSize, UART_READ_NONE, Count);
  
  Result := Count;
end;

function TUltiboUART.Write(const Buffer: Pointer; const BufferSize: Integer): Integer; 
var
  Count: LongWord;
begin
  if (Buffer = nil) or (BufferSize <= 0) then
    raise EUltiboInvalidParams.Create(SInvalidParameters);
    
  UARTDeviceWrite(FUART, Buffer,  BufferSize, UART_WRITE_NONE, Count);
  
  Result := Count;
end;

procedure TUltiboUART.Flush; 
begin
  //Not supported
end;
  
{$ENDREGION}

end.
  