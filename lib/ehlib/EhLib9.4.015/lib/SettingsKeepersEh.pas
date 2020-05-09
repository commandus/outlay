{*******************************************************}
{                                                       }
{                      EhLib 9.4                        }
{                                                       }
{                  TSettingsKeeper class                }
{                     Build 9.4.001                     }
{                                                       }
{    Copyright (c) 2017-2019 by Dmitry V. Bolshakov     }
{                                                       }
{*******************************************************}

{$I EhLib.Inc}

unit SettingsKeepersEh;

interface

uses
  Windows, Messages, SysUtils, Classes, Vcl.Graphics,
{$IFDEF EH_LIB_17} System.UITypes, System.Generics.Collections, System.Generics.Defaults, {$ENDIF}
{$IFDEF EH_LIB_20} System.JSON, {$ENDIF} 
  System.Variants;

{$IFDEF EH_LIB_20} 
type
  TSettingsKeeperDicEh = class(TDictionary<String, TObject>)
  protected
    procedure KeyNotify(const Key: String; Action: TCollectionNotification); override;
    procedure ValueNotify(const Value: TObject; Action: TCollectionNotification); override;
  end;

{ TSettingsKeeperEh }

  TSettingsKeeperEh = class(TObject)
  private
    FDic: TSettingsKeeperDicEh;
  public
    function ToArray: TArray<TPair<String, TObject>>;
    function TryGetValue(const Key: String; out Value: TObject): Boolean;
    function TryGetIntegerValue(const Key: String; out Value: Integer): Boolean;
    function TryGetStringValue(const Key: String; out Value: String): Boolean;
    function TryGetSubsettingsValue(const Key: String; out Value: TSettingsKeeperEh): Boolean;

    procedure Add(const Key: String; const Value: String); overload;
    procedure Add(const Key: String; const Value: Integer); overload;
    procedure Add(const Key: String; const Value: TSettingsKeeperEh); overload;

    constructor Create();
    destructor Destroy(); override;
  end;

{ TJSONSettingsWriterEh }

  TJSONSettingsWriterEh = class(TObject)
    function GetAsJSON(Keeper: TSettingsKeeperEh):String;
  end;

{ TJSONSettingsReaderEh }

  TJSONSettingsReaderEh = class(TObject)
    procedure FillByJSON(Keeper: TSettingsKeeperEh; JSon: String);
  end;

{ TSettingsKeeperCollectionByIndexComparer }

  TSettingsKeeperEhCollectionByIndexComparer = class(TCustomComparer<TPair<String,TObject>>)
  private
  public
    class function Ordinal: TSettingsKeeperEhCollectionByIndexComparer;
    function Compare(const Left, Right: TPair<String,TObject>): Integer; override;
    function Equals(const Left, Right: TPair<String,TObject>): Boolean; override;
    function GetHashCode(const Value: TPair<String,TObject>): Integer; override;
  end;

  function SettingsKeeperToJSONString(Keeper: TSettingsKeeperEh): String;
  procedure JSONStringToSettingsKeeper(Keeper: TSettingsKeeperEh; JSon: String);

{$ENDIF}

implementation

{$IFDEF EH_LIB_20} 

var
  FSettingsKeeperEhCollectionByIndexComparer: TSettingsKeeperEhCollectionByIndexComparer;

function SettingsKeeperEhCollectionByIndexComparer: TSettingsKeeperEhCollectionByIndexComparer;
begin
  if FSettingsKeeperEhCollectionByIndexComparer = nil then
    FSettingsKeeperEhCollectionByIndexComparer := TSettingsKeeperEhCollectionByIndexComparer.Create;
  Result := FSettingsKeeperEhCollectionByIndexComparer;
end;

function SettingsKeeperToJSONString(Keeper: TSettingsKeeperEh): String;
var
  Writer: TJSONSettingsWriterEh;
begin
  Writer := TJSONSettingsWriterEh.Create;
  Result := Writer.GetAsJSON(Keeper);
  FreeAndNIl(Writer);
end;

procedure JSONStringToSettingsKeeper(Keeper: TSettingsKeeperEh; JSon: String);
var
  Reader: TJSONSettingsReaderEh;
begin
  Reader := TJSONSettingsReaderEh.Create;
  Reader.FillByJSON(Keeper, JSon);
  FreeAndNIl(Reader);
end;

{ TColumnIndexComparer }

function TSettingsKeeperEhCollectionByIndexComparer.Compare(const Left,
  Right: TPair<String, TObject>): Integer;
var
  ColKeeper: TSettingsKeeperEh;
  IntValue: Integer;
  LeftIndex, RightIndex: Integer;
begin
  LeftIndex := 0;
  RightIndex := 0;
  ColKeeper := Left.Value as TSettingsKeeperEh;
  if ColKeeper.TryGetIntegerValue('ColIndex', IntValue) then
    LeftIndex := IntValue;

  ColKeeper := Right.Value as TSettingsKeeperEh;
  if ColKeeper.TryGetIntegerValue('ColIndex', IntValue) then
    RightIndex := IntValue;

  Result := LeftIndex - RightIndex;
end;

function TSettingsKeeperEhCollectionByIndexComparer.Equals(const Left,
  Right: TPair<String, TObject>): Boolean;
begin
  Result := Compare(Left, Right) = 0;
end;

function TSettingsKeeperEhCollectionByIndexComparer.GetHashCode(
  const Value: TPair<String, TObject>): Integer;
begin
  Result := 0;
end;

class function TSettingsKeeperEhCollectionByIndexComparer.Ordinal: TSettingsKeeperEhCollectionByIndexComparer;
begin
  Result := SettingsKeeperEhCollectionByIndexComparer;
end;

type
  TStringObject = class(TObject)
    Value: String;
    function ToString: string; override;
    constructor Create(Value: String);
  end;

{ TStringObject }

constructor TStringObject.Create(Value: String);
begin
  Self.Value := Value;
end;

function TStringObject.ToString: string;
begin
  Result := Value;
end;

type
  TIntegerObject = class(TObject)
    Value: Integer;
    function ToString: string; override;
    constructor Create(Value: Integer);
  end;

{ TIntegerObject }

constructor TIntegerObject.Create(Value: Integer);
begin
  Self.Value := Value;
end;

function TIntegerObject.ToString: string;
begin
  Result := Value.ToString;
end;

{ TSettingsKeeperDic }

procedure TSettingsKeeperDicEh.KeyNotify(const Key: String;
  Action: TCollectionNotification);
begin
  inherited;
end;

procedure TSettingsKeeperDicEh.ValueNotify(const Value: TObject;
  Action: TCollectionNotification);
begin
  inherited;
  if (Action = cnRemoved) then
    PObject(@Value)^.DisposeOf;
end;

{ TSettingsKeeperEh }

constructor TSettingsKeeperEh.Create;
begin
  inherited Create();
  FDic := TSettingsKeeperDicEh.Create();
end;

destructor TSettingsKeeperEh.Destroy;
begin
  FreeAndNil(FDic);
  inherited Destroy;
end;

procedure TSettingsKeeperEh.Add(const Key, Value: String);
begin
  FDic.Add(Key, TStringObject.Create(Value));
end;

procedure TSettingsKeeperEh.Add(const Key: String; const Value: Integer);
begin
  FDic.Add(Key, TIntegerObject.Create(Value));
end;

procedure TSettingsKeeperEh.Add(const Key: String; const Value: TSettingsKeeperEh);
begin
  FDic.Add(Key, Value);
end;

function TSettingsKeeperEh.ToArray: TArray<TPair<String, TObject>>;
begin
  Result := FDic.ToArray;
end;

function TSettingsKeeperEh.TryGetValue(const Key: String; out Value: TObject): Boolean;
begin
  Result := FDic.TryGetValue(Key, Value);
end;

function TSettingsKeeperEh.TryGetSubsettingsValue(const Key: String; out Value: TSettingsKeeperEh): Boolean;
var
  ObjValue: TObject;
begin
  Result := FDic.TryGetValue(Key, ObjValue);
  if Result and (ObjValue is TSettingsKeeperEh) then
    Value := ObjValue as TSettingsKeeperEh
  else
    Result := False;
end;

function TSettingsKeeperEh.TryGetIntegerValue(const Key: String; out Value: Integer): Boolean;
var
  StrVal: String;
  ObjValue: TObject;
begin
  Result := FDic.TryGetValue(Key, ObjValue);
  if Result then
  begin
    if (ObjValue is TStringObject) then
    begin
      StrVal := (ObjValue as TStringObject).Value;
      if Integer.TryParse(StrVal, Value) then
      begin
        Result := True;
        Exit;
      end;
    end
    else if (ObjValue is TIntegerObject) then
    begin
      Value := (ObjValue as TIntegerObject).Value;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TSettingsKeeperEh.TryGetStringValue(const Key: String;
  out Value: String): Boolean;
var
  ObjValue: TObject;
begin
  Result := FDic.TryGetValue(Key, ObjValue);
  if Result and (ObjValue is TStringObject) then
  begin
    Value := (ObjValue as TStringObject).Value;
    Result := True;
    Exit;
  end;
  Result := False;
end;

{ TJSONSettingsWriter }

procedure WriteKeeper(JSONObject: TJSONObject; Keeper: TSettingsKeeperEh);
var
  i: Integer;
  KeeperArray: TArray<TPair<String,TObject>>;
  SetPair: TPair<String,TObject>;
  SubKeeper: TSettingsKeeperEh;
  InnerObject : TJSONObject;
  JSSONStr : TJSONString;
  StrValue: String;
begin
  KeeperArray := Keeper.ToArray();
  for i := 0 to Length(KeeperArray)-1 do
  begin
    SetPair := KeeperArray[i];
    if SetPair.Value is TSettingsKeeperEh then
    begin
      SubKeeper := (SetPair.Value as TSettingsKeeperEh);
      InnerObject := TJSONObject.Create;
      WriteKeeper(InnerObject, SubKeeper);
      JSONObject.AddPair(SetPair.Key, InnerObject);
    end else
    begin
      StrValue := SetPair.Value.ToString;
      JSSONStr := TJSONString.Create(StrValue);
      JSONObject.AddPair(SetPair.Key, JSSONStr);
    end;
  end;
end;

function TJSONSettingsWriterEh.GetAsJSON(Keeper: TSettingsKeeperEh): String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;

  WriteKeeper(JSONObject, Keeper);

  Result := JSONObject.ToString();

  FreeAndNil(JSONObject);
end;

{ TJSONSettingsReader }

procedure ReadKeeper(JSONObject: TJSONObject; Keeper: TSettingsKeeperEh);
var
  i: Integer;
  SubKeeper: TSettingsKeeperEh;
  JSPair: TJSONPair;
begin
  for i := 0 to JSONObject.Size-1 do
  begin
    JSPair := JSONObject.Get(i);
    if JSPair.JsonValue is TJSONObject then
    begin
      SubKeeper := TSettingsKeeperEh.Create;
      ReadKeeper(JSPair.JsonValue as TJSONObject, SubKeeper);
      Keeper.Add(JSPair.JsonString.Value, SubKeeper);
    end else
    begin
      Keeper.Add(JSPair.JsonString.Value, (JSPair.JsonValue as TJSONString).Value);
    end;
  end;
end;

procedure TJSONSettingsReaderEh.FillByJSON(Keeper: TSettingsKeeperEh; JSon: String);
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.ParseJSONValue(JSon) as TJSONObject;
  if JSONObject = nil then Exit;

  ReadKeeper(JSONObject, Keeper);

  FreeAndNil(JSONObject);
end;

{$ENDIF}

procedure InitUnit;
begin
end;

procedure FinalUnit;
begin
{$IFDEF EH_LIB_20} 
  FreeAndNil(FSettingsKeeperEhCollectionByIndexComparer);
{$ENDIF}
end;

initialization
  InitUnit;
finalization
  FinalUnit;
end.
