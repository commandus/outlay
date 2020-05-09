{*******************************************************}
{                                                       }
{                      EhLib 9.4                        }
{                                                       }
{           Classes to work w ith ZipingProvider        }
{                     Build 9.4.01                      }
{                                                       }
{     Copyright (c) 2019-2020 by Dmitry V. Bolshakov    }
{                                                       }
{*******************************************************}

unit ZipFileProviderEh;

{$I EhLib.Inc}

interface

uses
  SysUtils, Classes, Graphics, Dialogs, GridsEh, Controls,
  {$IFDEF EH_LIB_16} System.Zip, {$ENDIF}
  {$IFDEF FPC}
    zipper, EhLibLCL,
  {$ELSE}
    EhLibVCL,
  {$ENDIF}
  Variants, Types;

type

  TCustomFileZipingProviderEh = class;

{ TCustomFileZipingProviderEh }

  TCustomFileZipingProviderEh = class(TObject)
  public
    class function CreateInstance: TCustomFileZipingProviderEh; virtual;
    function InitZipFile(FileName: String): TStream; virtual; abstract;
    procedure AddFile(Data: TStream; const FilePathAndName: string); virtual; abstract;
    procedure FinalizeZipFile; overload; virtual; abstract;
    procedure FinalizeZipFile(AStream: TStream); overload; virtual; abstract;
  end;

  TCustomFileZipingProviderEhClass = class of TCustomFileZipingProviderEh;

{$IFDEF EH_LIB_ZIP} 

{ TSystemZipFileProvider }

  TSystemZipFileProviderEh = class(TCustomFileZipingProviderEh)
  private
    FStream: TStream;
    FFileName: String;
    {$IFDEF FPC}
    FZipFile: TZipper;
    {$ELSE}
    FZipFile: TZipFile;
    {$ENDIF}
    FWriteStreams: TObjectListEh;
  public
    constructor Create;
    destructor Destroy; override;

    class function CreateInstance: TCustomFileZipingProviderEh; override;
    function InitZipFile(FileName: String): TStream; override;
    procedure AddFile(Data: TStream; const FilePathAndName: string); override;
    procedure FinalizeZipFile; override;
    procedure FinalizeZipFile(AStream: TStream); override;

    property Stream: TStream read FStream write FStream;
  end;

{$ENDIF} 

function ZipFileProviderClass: TCustomFileZipingProviderEhClass;
function RegisterZipFileProviderClass(AZipFileProviderClass: TCustomFileZipingProviderEhClass): TCustomFileZipingProviderEhClass;

implementation

var
  FZipFileProviderClass: TCustomFileZipingProviderEhClass;

{ TCustomFileZipingProviderEh }

function ZipFileProviderClass: TCustomFileZipingProviderEhClass;
begin
  Result := FZipFileProviderClass;
end;

function RegisterZipFileProviderClass(AZipFileProviderClass: TCustomFileZipingProviderEhClass):
  TCustomFileZipingProviderEhClass;
begin
  Result := FZipFileProviderClass;
  FZipFileProviderClass := AZipFileProviderClass;
end;

class function TCustomFileZipingProviderEh.CreateInstance: TCustomFileZipingProviderEh;
begin
  Result := nil;
end;

{$IFDEF EH_LIB_ZIP} 

{ TSystemZipFileProvider }

constructor TSystemZipFileProviderEh.Create;
begin
  inherited Create;
  FWriteStreams := TObjectListEh.Create;
end;

destructor TSystemZipFileProviderEh.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FWriteStreams);
  FreeAndNil(FZipFile);
  FreeAndNil(FStream);
end;

class function TSystemZipFileProviderEh.CreateInstance: TCustomFileZipingProviderEh;
begin
  Result := TSystemZipFileProviderEh.Create;
end;

function TSystemZipFileProviderEh.InitZipFile(FileName: String): TStream;
begin
  if FStream <> nil then
    raise Exception.Create('ZipFile is already Initialized.');
  if FZipFile <> nil then
    raise Exception.Create('ZipFile is already Initialized.');
  if FileName = '' then
    FStream := TMemoryStream.Create
  else
    FStream := TFileStream.Create(FileName, fmCreate);
  Result := FStream;
  FFileName := FileName;
  {$IFDEF FPC}
  FZipFile := TZipper.Create;
  {$ELSE}
  FZipFile := TZipFile.Create;
  FZipFile.Open(Result, zmWrite);
  {$ENDIF}
end;

procedure TSystemZipFileProviderEh.AddFile(Data: TStream; const FilePathAndName: string);
{$IFDEF FPC}
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create();
  ms.CopyFrom(Data, Data.Size);
  ms.Position := 0;
  FZipFile.Entries.AddFileEntry(ms, FilePathAndName);
  FWriteStreams.Add(ms);
end;
{$ELSE}
begin
  FZipFile.Add(Data, FilePathAndName);
end;
{$ENDIF}

procedure TSystemZipFileProviderEh.FinalizeZipFile;
{$IFDEF FPC}
var
  i: Integer;
begin
  if FZipFile <> nil then
    FZipFile.SaveToStream(FStream);

  for i := 0 to FWriteStreams.Count-1 do
    FWriteStreams[i].Free;
{$ELSE}
begin
  if FZipFile <> nil then
    FZipFile.Close;
{$ENDIF}
  FreeAndNil(FZipFile);
  FreeAndNil(FStream);
end;

procedure TSystemZipFileProviderEh.FinalizeZipFile(AStream: TStream);
begin
  {$IFDEF FPC}
  if FZipFile <> nil then
    FZipFile.SaveToStream(AStream);
  {$ELSE}
  if FZipFile <> nil then
    FZipFile.Close;
  AStream.CopyFrom(FStream, 0);
  {$ENDIF}
  FreeAndNil(FZipFile);
  FreeAndNil(FStream);
end;

{$ENDIF} 

procedure InitUnit;
begin
{$IFDEF EH_LIB_ZIP} 
  RegisterZipFileProviderClass(TSystemZipFileProviderEh);
{$ENDIF} 
end;

procedure FinalizeUnit;
begin
{$IFDEF EH_LIB_ZIP} 
  RegisterZipFileProviderClass(nil);
{$ENDIF} 
end;

initialization
  InitUnit;
finalization
  FinalizeUnit;
end.
