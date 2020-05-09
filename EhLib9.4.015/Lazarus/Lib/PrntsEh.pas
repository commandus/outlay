{*******************************************************}
{                                                       }
{                       EhLib 9.4                       }
{                 TVirtualPrinter object                }
{                    (Build 9.4.01)                     }
{                                                       }
{   Copyright (c) 1998-2019 by Dmitry V. Bolshakov      }
{                                                       }
{*******************************************************}

unit PrntsEh {$IFDEF CIL} platform{$ENDIF};

{$I EhLib.Inc}

interface

uses
  {$IFDEF FPC}
    OsPrinters,
    {$IFDEF FPC_CROSSP}
      LCLIntf, LCLType,
    {$ELSE}
      Windows,
    {$ENDIF}
  {$ELSE}
    Windows,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Printers;

type

  TVirtualPrinter = class(TObject)
  protected
    function GetAborted: Boolean; virtual;
    function GetCanvas: TCanvas; virtual;
    {$IFDEF FPC}
    {$ELSE}
    function GetCapabilities: TPrinterCapabilities; virtual;
    {$ENDIF}
    function GetFonts: TStrings; virtual;
    function GetHandle: HDC; virtual;
    function GetCopies: Integer; virtual;
    function GetOrientation: TPrinterOrientation; virtual;
    function GetPageHeight: Integer; virtual;
    function GetPageWidth: Integer; virtual;
    function GetPageNumber: Integer; virtual;
    function GetPrinting: Boolean; virtual;
    function GetTitle: String; virtual;
    function GetFullPageWidth: Integer; virtual;
    function GetFullPageHeight: Integer; virtual;
    function GetPrinterIndex: Integer; virtual;
    function GetPrinters: TStrings; virtual;
    function GetPixelsPerInchX: Integer; virtual;
    function GetPixelsPerInchY: Integer; virtual;

    procedure SetPrinterIndex(const Value: Integer); virtual;
    procedure SetCopies(const Value: Integer); virtual;
    procedure SetOrientation(const Value: TPrinterOrientation); virtual;
    procedure SetTitle(const Value: string); virtual;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Abort; virtual;
    procedure BeginDoc; virtual;
    procedure EndDoc; virtual;
    procedure NewPage; virtual;
{$IFDEF CIL}
    procedure GetPrinter(ADevice, ADriver, APort: String; var ADeviceMode: IntPtr); virtual;
    procedure SetPrinter(ADevice, ADriver, APort: String; ADeviceMode: IntPtr); virtual;
{$ELSE}
    procedure GetPrinter(ADevice, ADriver, APort: PChar; var ADeviceMode: THandle); virtual;
    procedure SetPrinter(ADevice, ADriver, APort: PChar; ADeviceMode: THandle); virtual;
{$ENDIF}

    property Aborted: Boolean read GetAborted;
    property Canvas: TCanvas read GetCanvas;
    {$IFDEF FPC}
    {$ELSE}
    property Capabilities: TPrinterCapabilities read GetCapabilities;
    {$ENDIF}
    property Copies: Integer read GetCopies write SetCopies;
    property Fonts: TStrings read GetFonts;
    property Handle: HDC read GetHandle;
    property Orientation: TPrinterOrientation read GetOrientation write SetOrientation;
    property PageHeight: Integer read GetPageHeight;
    property PageWidth: Integer read GetPageWidth;
    property PageNumber: Integer read GetPageNumber;
    property PrinterIndex: Integer read GetPrinterIndex write SetPrinterIndex;
    property FullPageWidth: Integer read GetFullPageWidth;
    property FullPageHeight: Integer read GetFullPageHeight;
    property Printing: Boolean read GetPrinting;
    property Printers: TStrings read GetPrinters;
    property Title: String read GetTitle write SetTitle;
    property PixelsPerInchX: Integer read GetPixelsPerInchX;
    property PixelsPerInchY: Integer read GetPixelsPerInchY;
  end;

  {$IFDEF FPC}
  {$ELSE}
  TWinPrinter = TPrinter;
  {$ENDIF}

var
  VirtualPrinter: TVirtualPrinter;

implementation

{ TVirtualPrinter }

procedure TVirtualPrinter.Abort;
begin
  Printer.Abort;
end;

procedure TVirtualPrinter.BeginDoc;
begin
  Printer.BeginDoc;
end;

constructor TVirtualPrinter.Create;
begin
  inherited Create;
end;

destructor TVirtualPrinter.Destroy;
begin
  inherited Destroy;
end;

procedure TVirtualPrinter.EndDoc;
begin
  Printer.EndDoc;
end;

function TVirtualPrinter.GetCanvas: TCanvas;
begin
  Result := Printer.Canvas;
end;

function TVirtualPrinter.GetFonts: TStrings;
begin
  Result := Printer.Fonts;
end;

function TVirtualPrinter.GetFullPageWidth: Integer;
begin
  {$IFDEF FPC_CROSSP}
  Result := Printer.PageWidth;
  {$ELSE}
  Result := Printer.PageWidth + GetDeviceCaps(TWinPrinter(Printer).Handle, PHYSICALOFFSETX) * 2;
  {$ENDIF}
end;

function TVirtualPrinter.GetFullPageHeight: Integer;
begin
  {$IFDEF FPC_CROSSP}
  Result := Printer.PageHeight;
  {$ELSE}
  Result := Printer.PageHeight + GetDeviceCaps(TWinPrinter(Printer).Handle, PHYSICALOFFSETY) * 2;
  {$ENDIF} 
end;

function TVirtualPrinter.GetCopies: Integer;
begin
  Result := Printer.Copies;
end;

function TVirtualPrinter.GetOrientation: TPrinterOrientation;
begin
  Result := Printer.Orientation;
end;

function TVirtualPrinter.GetPageHeight: Integer;
begin
  Result := Printer.PageHeight;
end;

function TVirtualPrinter.GetPageNumber: Integer;
begin
  Result := Printer.PageNumber;
end;

function TVirtualPrinter.GetPageWidth: Integer;
begin
  Result := Printer.PageWidth;
end;

function TVirtualPrinter.GetPrinting: Boolean;
begin
  Result := Printer.Printing;
end;

function TVirtualPrinter.GetTitle: string;
begin
  Result := Printer.Title;
end;

procedure TVirtualPrinter.NewPage;
begin
  Printer.NewPage;
end;

procedure TVirtualPrinter.SetCopies(const Value: Integer);
begin
  Printer.Copies := Value;
end;

procedure TVirtualPrinter.SetOrientation(const Value: TPrinterOrientation);
begin
  Printer.Orientation := Value;
end;

procedure TVirtualPrinter.SetTitle(const Value: string);
begin
  Printer.Title := Value;
end;

function TVirtualPrinter.GetAborted: Boolean;
begin
  Result := Printer.Aborted;
end;

function TVirtualPrinter.GetHandle: HDC;
begin
  {$IFDEF FPC_CROSSP}
  Result := 0;
  {$ELSE}
  Result := TWinPrinter(Printer).Handle;
  {$ENDIF} 
end;

{$IFDEF CIL}
procedure TVirtualPrinter.GetPrinter(ADevice, ADriver, APort: String; var ADeviceMode: IntPtr);
{$ELSE}
procedure TVirtualPrinter.GetPrinter(ADevice, ADriver, APort: PChar; var ADeviceMode: THandle);
{$ENDIF}
begin
  {$IFDEF FPC}
  raise Exception.Create('GetPrinter is not supported in FPC');
  {$ELSE}
  Printer.GetPrinter(ADevice, ADriver, APort, ADeviceMode);
  {$ENDIF}
end;

{$IFDEF CIL}
procedure TVirtualPrinter.SetPrinter(ADevice, ADriver, APort: String; ADeviceMode: IntPtr);
{$ELSE}
procedure TVirtualPrinter.SetPrinter(ADevice, ADriver, APort: PChar; ADeviceMode: THandle);
{$ENDIF}
begin
  {$IFDEF FPC}
  raise Exception.Create('SetPrinter is not supported in FPC');
  {$ELSE}
  Printer.SetPrinter(ADevice, ADriver, APort, ADeviceMode);
  {$ENDIF}
end;

function TVirtualPrinter.GetPrinterIndex: Integer;
begin
  Result := Printer.PrinterIndex;
end;

function TVirtualPrinter.GetPrinters: TStrings;
begin
  Result := Printer.Printers;
end;

procedure TVirtualPrinter.SetPrinterIndex(const Value: Integer);
begin
  Printer.PrinterIndex := Value;
end;

{$IFDEF FPC}
{$ELSE}
function TVirtualPrinter.GetCapabilities: TPrinterCapabilities;
begin
  Result := Printer.Capabilities;
end;
{$ENDIF}

function TVirtualPrinter.GetPixelsPerInchX: Integer;
{$IFDEF FPC_CROSSP}
begin
  Result := Printer.XDPI;
end;
{$ELSE}
begin
  Result := GetDeviceCaps(TWinPrinter(Printer).Handle, LOGPIXELSX)
end;
{$ENDIF}

function TVirtualPrinter.GetPixelsPerInchY: Integer;
{$IFDEF FPC_CROSSP}
begin
  Result := Printer.YDPI;
end;
{$ELSE}
begin
  Result := GetDeviceCaps(TWinPrinter(Printer).Handle, LOGPIXELSY)
end;
{$ENDIF}

initialization
  VirtualPrinter := TVirtualPrinter.Create;
finalization
  FreeAndNil(VirtualPrinter);
end.
