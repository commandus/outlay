{*******************************************************}
{                                                       }
{                      EhLib 9.4                        }
{                                                       }
{           Classes to work w ith Xlsx Format           }
{                     Build 9.4.01                      }
{                                                       }
{     Copyright (c) 2019-2020 by Dmitry V. Bolshakov    }
{                                                       }
{*******************************************************}

unit XlsMemFilesEh;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  {$IFDEF FPC}
    EhLibLCL,
  {$ELSE}
    EhLibVCL,
  {$ENDIF}
  Contnrs, Controls, Forms, Dialogs;

type
  TXlsWorkbookEh = class;
  TXlsWorksheetEh = class;
  TXlsFileColumnEh = class;
  TXlsFileColumnsEh = class;
  TXlsFileCellEh = class;
  TXlsFileWorksheetDimensionEh = class;
  TXlsFileStylesEh = class;
  TXlsFileCellsRangeEh = class;
  TXlsFileCellsRangeLinesEh = class;
  TXlsFileWorksheetCellsRectEh = class;
  IXlsFileCellsRangeEh = interface;

  TXlsFileCellLineStyleEh = (clsNoneEh, clsThinEh, clsMediumEh, clsThickEh, clsDoubleEh,
    clsDashDotEh, clsDashDotDotEh, clsDashedEh, clsDottedEh, clsHairEh,
    clsMediumDashDotEh, clsMediumDashDotDotEh, clsMediumDashedEh, clsSlantDashDotEh);
  TXlsFileCellHorzAlign = (chaUnassignedEh, chaLeftEh, chaRightEh, chaCenterEh);
  TXlsFileCellVertAlign = (cvaUnassignedEh, cvaTopEh, cvaBottomEh, cvaCenterEh);

{ TXlsFileEh }

  TXlsMemFileEh = class(TObject)
  private
    FWorkbook: TXlsWorkbookEh;
    function GetWorkbook: TXlsWorkbookEh;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveToFile(FileName: String);

    property Workbook: TXlsWorkbookEh read GetWorkbook;
  end;

{ TXlsWorkbookEh }

  TXlsWorkbookEh = class(TObject)
  private
    FWorkSheets: TObjectListEh;
    FStyles: TXlsFileStylesEh;

    function GetWorksheet(WorksheetId: Variant): TXlsWorksheetEh;
    function GetWorksheetCount: Integer;
    function GetStyles: TXlsFileStylesEh;
  protected
    procedure RenameWorksheet(Worksheet: TXlsWorksheetEh; NewName: string);

  public
    constructor Create;
    destructor Destroy; override;

    function AddWorksheet(WorksheetName: string): TXlsWorksheetEh;
    function FindWorksheet(WorksheetName: string): TXlsWorksheetEh;

    procedure MoveWorksheet(FromIndex, ToIndex: Integer);

    property Worksheets[WorksheetId: Variant]: TXlsWorksheetEh read GetWorksheet;
    property WorksheetCount: Integer read GetWorksheetCount;

    property Styles: TXlsFileStylesEh read GetStyles;
  end;

{ TXlsWorksheetEh }

  TXlsWorksheetEh = class(TObject)
  private
    FName: String;
    FWorkbook: TXlsWorkbookEh;
    FColumns:  TXlsFileColumnsEh;
    FCells: array of array of TXlsFileCellEh;
    FDimension: TXlsFileWorksheetDimensionEh;
    FFrozenColCount: Integer;
    FFrozenRowCount: Integer;
    FAutoFilterRange: TXlsFileWorksheetCellsRectEh;
    //FUnderMergerCell: TXlsFileCell;
    function GetName: String;
    procedure SetName(const Value: String);
    function GetColumns: TXlsFileColumnsEh;
    function GetCell(Col, Row: Integer): TXlsFileCellEh;
    function GetCellDataExists(Col, Row: Integer): Boolean;
  public
    constructor Create(AWorkbook: TXlsWorkbookEh);
    destructor Destroy; override;

    function GetCellsRange(FromCol, FromRow, ToCol, ToRow: Integer): IXlsFileCellsRangeEh;
    procedure MergeCell(Col, Row, ColCount, RowCount: Integer);
    procedure UnmergerCell(Col, Row: Integer);

    property Name: String read GetName write SetName;
    property Columns: TXlsFileColumnsEh read GetColumns;
    property Cells[Col, Row: Integer]: TXlsFileCellEh read GetCell;
    property CellDataExists[Col, Row: Integer]: Boolean read GetCellDataExists;
    property Dimension: TXlsFileWorksheetDimensionEh read FDimension;
    property AutoFilterRange: TXlsFileWorksheetCellsRectEh read FAutoFilterRange write FAutoFilterRange;
    property FrozenColCount: Integer read FFrozenColCount write FFrozenColCount;
    property FrozenRowCount: Integer read FFrozenRowCount write FFrozenRowCount;
  end;

{ TXlsFileColumns }

  TXlsFileColumnsEh = class(TObject)
  private
    FColumns: TObjectListEh;
    function GetColumn(ColumnIndex: Integer): TXlsFileColumnEh;
    function GetCurrentCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function ScreenToXlsWidth(ScreenWidth: Integer): Single;
    function ColumnIsCreated(ColumnIndex: Integer): Boolean;

    property Column[ColumnIndex: Integer]: TXlsFileColumnEh read GetColumn; default;
    property CurrentCount: Integer read GetCurrentCount;
  end;

{ TXlsFileColumn }

  TXlsFileColumnEh = class(TObject)
  private
    FWidth: Single;
    FVisible: Boolean;

    function GetVisible: Boolean;
    function GetWidth: Single;
    procedure SetVisible(const Value: Boolean);
    procedure SetWidth(const Value: Single);
  public
    constructor Create;
    destructor Destroy; override;

    property Width: Single read GetWidth write SetWidth;
    property Visible: Boolean read GetVisible write SetVisible;
  end;

{ TXlsFileStyleFont }

  TXlsFileStyleFont = class(TObject)
  private
    FName: String;
    FIsUnderline: Boolean;
    FColor: TColor;
    FIsItalic: Boolean;
    FIsBold: Boolean;
    FSize: Integer;
    FIndex: Integer;
  public
    property Name: String read FName write FName;
    property Size: Integer read FSize write FSize;
    property Color: TColor read FColor write FColor;
    property IsBold: Boolean read FIsBold write FIsBold;
    property IsItalic: Boolean read FIsItalic write FIsItalic;
    property IsUnderline: Boolean read FIsUnderline write FIsUnderline;
    property Index: Integer read FIndex;
  end;

  TXlsFileStyleFillPatternTypeEh = (fptNoneEh, fptSolidEh, fptGray125Eh);

{ TXlsFileStyleFill }

  TXlsFileStyleFill = class(TObject)
  private
    FColor: TColor;
    FPatternType: TXlsFileStyleFillPatternTypeEh;
    FIndex: Integer;
  public
    property Color: TColor read FColor write FColor;
    property PatternType: TXlsFileStyleFillPatternTypeEh read FPatternType;
    property Index: Integer read FIndex;
  end;

{ TXlsFileStyleLineEh }

  TXlsFileStyleLineEh = class(TObject)
  private
    FColor: TColor;
    FStyle: TXlsFileCellLineStyleEh;

  public
    property Color: TColor read FColor write FColor;
    property Style: TXlsFileCellLineStyleEh read FStyle write FStyle;
  end;

{ TXlsFileStyleLinesEh }

  TXlsFileStyleLinesEh = class(TObject)
  private
    FRight: TXlsFileStyleLineEh;
    FBottom: TXlsFileStyleLineEh;
    FTop: TXlsFileStyleLineEh;
    FLeft: TXlsFileStyleLineEh;
    FIndex: Integer;
  public
    constructor Create();
    destructor Destroy; override;

    property Left: TXlsFileStyleLineEh read FLeft;
    property Right: TXlsFileStyleLineEh read FRight;
    property Top: TXlsFileStyleLineEh read FTop;
    property Bottom: TXlsFileStyleLineEh read FBottom;

    property Index: Integer read FIndex;
  end;

{ TXlsFileStyleLineEh }

  TXlsFileStyleNumberFormatEh = class(TObject)
  private
    FFormatId: Integer;
    FFormatStr: String;

  public
    property FormatStr: String read FFormatStr write FFormatStr;
    property FormatId: Integer read FFormatId write FFormatId;
  end;

{ TXlsFileCellStyle }

  TXlsFileCellStyle = class(TObject)
  private
    FNumberFormat: TXlsFileStyleNumberFormatEh;
    FFont: TXlsFileStyleFont;
    FFill: TXlsFileStyleFill;
    FIndex: Integer;
    FBorder: TXlsFileStyleLinesEh;
    FHorzAlign: TXlsFileCellHorzAlign;
    FVertAlign: TXlsFileCellVertAlign;
    FWrapText: Boolean;
    FRotation: Integer;
  public
    property NumberFormat: TXlsFileStyleNumberFormatEh read FNumberFormat;
    property Font: TXlsFileStyleFont read FFont;
    property Fill: TXlsFileStyleFill read FFill;
    property Border: TXlsFileStyleLinesEh read FBorder;
    property HorzAlign: TXlsFileCellHorzAlign read FHorzAlign;
    property VertAlign: TXlsFileCellVertAlign read FVertAlign;
    property WrapText: Boolean read FWrapText;
    property Rotation: Integer read FRotation; //Degrees
    property Index: Integer read FIndex;
  end;

{ TXlsFileStyles }

  TXlsFileStylesEh = class(TObject)
  private
    FCellStyles: TObjectListEh;
    FFonts: TObjectListEh;
    FFills: TObjectListEh;
    FBorders: TObjectListEh;
    FNumberFormats: TObjectListEh;

    function GetCellStyle(Index: Integer): TXlsFileCellStyle;
    function GetFill(Index: Integer): TXlsFileStyleFill;
    function GetFont(Index: Integer): TXlsFileStyleFont;
    function GetFontCount: Integer;
    function GetFillCount: Integer;
    function GetCellStyleCount: Integer;
    function GetBorder(Index: Integer): TXlsFileStyleLinesEh;
    function GetBorderCount: Integer;
    function GetNumberFormat(Index: Integer): TXlsFileStyleNumberFormatEh;
    function GetNumberFormatCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function GetOrCreateNumberFormat(FormatStr: String; FormatId: Integer): TXlsFileStyleNumberFormatEh;
    function GetOrCreateFont(FontName: String; FontSize: Integer; FontColor: TColor; FontIsBold: Boolean; FontIsItalic: Boolean; FontIsUnderline: Boolean): TXlsFileStyleFont;
    function GetOrCreateFill(FillColor: TColor; FillPatternType: TXlsFileStyleFillPatternTypeEh): TXlsFileStyleFill;
    function GetOrCreateBorder(LeftLineColor: TColor; LeftLineStyle: TXlsFileCellLineStyleEh; RightLineColor: TColor; RightLineStyle: TXlsFileCellLineStyleEh; TopLineColor: TColor; TopLineStyle: TXlsFileCellLineStyleEh; BottomLineColor: TColor; BottomLineStyle: TXlsFileCellLineStyleEh): TXlsFileStyleLinesEh;
    function GetOrCreateCellStyle(ANumberFormat: TXlsFileStyleNumberFormatEh; AFont: TXlsFileStyleFont; AFill: TXlsFileStyleFill; ABorder: TXlsFileStyleLinesEh; AHorzAlign: TXlsFileCellHorzAlign; AVertAlign: TXlsFileCellVertAlign; AWrapText: Boolean; ARotation: Integer): TXlsFileCellStyle;

    property CellStyle[Index: Integer]: TXlsFileCellStyle read GetCellStyle;
    property Font[Index: Integer]: TXlsFileStyleFont read GetFont;
    property Fill[Index: Integer]: TXlsFileStyleFill read GetFill;
    property Border[Index: Integer]: TXlsFileStyleLinesEh read GetBorder;
    property NumberFormat[Index: Integer]: TXlsFileStyleNumberFormatEh read GetNumberFormat;

    property FontCount: Integer read GetFontCount;
    property FillCount: Integer read GetFillCount;
    property BorderCount: Integer read GetBorderCount;
    property CellStyleCount: Integer read GetCellStyleCount;
    property NumberFormatCount: Integer read GetNumberFormatCount;
  end;

{ TXlsFileCellMergeRangeEh }

  TXlsFileCellMergeRangeEh = class(TObject)
  private
    FColCount: Integer;
    FRowCount: Integer;
  public
    property ColCount: Integer read FColCount;
    property RowCount: Integer read FRowCount;
  end;

  TCellValueType = (cvtEmpty, cvtBoolean, cvtDate, cvtError, cvtInlineStr, cvtNumber,
    cvtSharedString, cvtString);

{ TXlsFileColumn }

  TXlsFileCellEh = class(TObject)
  private
    FValue: Variant;
    FStyle: TXlsFileCellStyle;
    FMergeRange: TXlsFileCellMergeRangeEh;
    FWorksheet: TXlsWorksheetEh;
    //FUnderMerger: Boolean;
    FFormula: String;

    function GetStyle: TXlsFileCellStyle;
    function GetValue: Variant;
    function GetValueType: TCellValueType;
    procedure SetValue(const Value: Variant);
    procedure SetValueType(const Value: TCellValueType);

  public
    constructor Create(AWorksheet: TXlsWorksheetEh);
    destructor Destroy; override;

    property ValueType: TCellValueType read GetValueType write SetValueType;
    property Value: Variant read GetValue write SetValue;
    property Formula: String read FFormula write FFormula;
    property Style: TXlsFileCellStyle read GetStyle;
    property MergeRange: TXlsFileCellMergeRangeEh read FMergeRange;
    //property UnderMerger: Boolean read FUnderMerger;
  end;

{ TXlsFileWorksheetCellsRectEh }

  TXlsFileWorksheetCellsRectEh = class(TObject)
  private
    FFromCol: Integer;
    FToCol: Integer;
    FFromRow: Integer;
    FToRow: Integer;
    FWorksheet: TXlsWorksheetEh;

  protected
  public
    constructor Create(Worksheet: TXlsWorksheetEh);
    destructor Destroy; override;

    procedure Clear;
    function IsEmpty: Boolean;

    property FromCol: Integer read FFromCol write FFromCol;
    property ToCol: Integer read FToCol write FToCol;
    property FromRow: Integer read FFromRow write FFromRow;
    property ToRow: Integer read FToRow write FToRow;
  end;

{ TXlsFileWorksheetDimensionEh }

  TXlsFileWorksheetDimensionEh = class(TObject)
  private
    FFromCol: Integer;
    FToCol: Integer;
    FFromRow: Integer;
    FToRow: Integer;
    FWorksheet: TXlsWorksheetEh;

    function GetFromCol: Integer;
    function GetFromRow: Integer;
    function GetToCol: Integer;
    function GetToRow: Integer;
  protected
    procedure Update;
  public
    constructor Create(Worksheet: TXlsWorksheetEh);
    destructor Destroy; override;

    property FromCol: Integer read GetFromCol;
    property ToCol: Integer read GetToCol;
    property FromRow: Integer read GetFromRow;
    property ToRow: Integer read GetToRow;
  end;

//{ TXlsFileCellsRangeNumberFormatEh }
//
//  TXlsFileCellsRangeNumberFormatEh = class(TObject)
//  private
//    FNumberFormat: String;
//    FNumberFormatChanged: Boolean;
//
//    procedure SetNumberFormat(const Value: String);
//
//  public
//    function HasChanges: Boolean;
//
//    property NumberFormat: String read FNumberFormat write SetNumberFormat;
//  end;

{ TXlsFileCellsRangeFontEh }

  TXlsFileCellsRangeFontEh = class(TObject)
  private
    FName: String;
    FNameChanged: Boolean;
    FIsUnderline: Boolean;
    FIsUnderlineChanged: Boolean;
    FColor: TColor;
    FColorChanged: Boolean;
    FIsItalic: Boolean;
    FIsItalicChanged: Boolean;
    FIsBold: Boolean;
    FIsBoldChanged: Boolean;
    FSize: Integer;
    FSizeChanged: Boolean;

    procedure SetColor(const Value: TColor);
    procedure SetIsBold(const Value: Boolean);
    procedure SetIsItalic(const Value: Boolean);
    procedure SetIsUnderline(const Value: Boolean);
    procedure SetName(const Value: String);
    procedure SetSize(const Value: Integer);

  public
    function HasChanges: Boolean;

    property Name: String read FName write SetName;
    property Size: Integer read FSize write SetSize;
    property Color: TColor read FColor write SetColor;
    property IsBold: Boolean read FIsBold write SetIsBold;
    property IsItalic: Boolean read FIsItalic write SetIsItalic;
    property IsUnderline: Boolean read FIsUnderline write SetIsUnderline;
  end;

{ TXlsFileCellsRangeFillEh }

  TXlsFileCellsRangeFillEh = class(TObject)
  private
    FColor: TColor;
    FColorChanged: Boolean;

    procedure SetColor(const Value: TColor);

  public
    function HasChanges: Boolean;

    property Color: TColor read FColor write SetColor;
  end;

{ TXlsFileCellsRangeLineEh }

  TXlsFileCellsRangeLineEh = class(TObject)
  private
    FColor: TColor;
    FColorChanged: Boolean;
    FStyle: TXlsFileCellLineStyleEh;
    FStyleChanged: Boolean;

    procedure SetColor(const Value: TColor);
    procedure SetStyle(const Value: TXlsFileCellLineStyleEh);

  public
    constructor Create;

    function HasChanges: Boolean;

    property Color: TColor read FColor write SetColor;
    property Style: TXlsFileCellLineStyleEh read FStyle write SetStyle;
  end;

{ TXlsFileCellsRangeLinesEh }

  TXlsFileCellsRangeLinesEh = class(TObject)
  private
    FRight: TXlsFileCellsRangeLineEh;
    FBottom: TXlsFileCellsRangeLineEh;
    FTop: TXlsFileCellsRangeLineEh;
    FLeft: TXlsFileCellsRangeLineEh;
  public
    constructor Create();
    destructor Destroy; override;

    function HasChanges: Boolean;

    property Left: TXlsFileCellsRangeLineEh read FLeft;
    property Right: TXlsFileCellsRangeLineEh read FRight;
    property Top: TXlsFileCellsRangeLineEh read FTop;
    property Bottom: TXlsFileCellsRangeLineEh read FBottom;
  end;

  IXlsFileCellsRangeEh = interface
    function GetFill: TXlsFileCellsRangeFillEh;
    function GetFont: TXlsFileCellsRangeFontEh;
    function GetBorder: TXlsFileCellsRangeLinesEh;
    function GetInsideBorder: TXlsFileCellsRangeLinesEh;
    function GetHorzAlign: TXlsFileCellHorzAlign;
    function GetRotation: Integer;
    function GetVertAlign: TXlsFileCellVertAlign;
    function GetWrapText: Boolean;
    function GetNumberFormat: String;

    procedure SetHorzAlign(const Value: TXlsFileCellHorzAlign);
    procedure SetRotation(const Value: Integer);
    procedure SetVertAlign(const Value: TXlsFileCellVertAlign);
    procedure SetWrapText(const Value: Boolean);
    procedure SetNumberFormat(const Value: String);

    procedure ApplyChages;

    property Font: TXlsFileCellsRangeFontEh read GetFont;
    property Fill: TXlsFileCellsRangeFillEh read GetFill;
    property Border: TXlsFileCellsRangeLinesEh read GetBorder;
    property InsideBorder: TXlsFileCellsRangeLinesEh read GetInsideBorder;

    property HorzAlign: TXlsFileCellHorzAlign read GetHorzAlign write SetHorzAlign;
    property VertAlign: TXlsFileCellVertAlign read GetVertAlign write SetVertAlign;
    property WrapText: Boolean read GetWrapText write SetWrapText;
    property Rotation: Integer read GetRotation write SetRotation; //Degrees

    property NumberFormat: String read GetNumberFormat write SetNumberFormat;
  end;

{ TXlsFileCellsRangeEh }

  TXlsFileCellsRangeEh = class(TInterfacedObject, IXlsFileCellsRangeEh)
  private
    FFromCol: Integer;
    FToCol: Integer;
    FFromRow: Integer;
    FToRow: Integer;
    FWorksheet: TXlsWorksheetEh;
    FFont: TXlsFileCellsRangeFontEh;
    FFill: TXlsFileCellsRangeFillEh;
    FBorder: TXlsFileCellsRangeLinesEh;
    FInsideBorder: TXlsFileCellsRangeLinesEh;
    FHorzAlign: TXlsFileCellHorzAlign;
    FHorzAlignChanged: Boolean;
    FVertAlign: TXlsFileCellVertAlign;
    FVertAlignChanged: Boolean;
    FWrapText: Boolean;
    FWrapTextChanged: Boolean;
    FRotation: Integer;
    FRotationChanged: Boolean;
    FNumberFormat: String;
    FNumberFormatChanged: Boolean;

    function GetFromCol: Integer;
    function GetFromRow: Integer;
    function GetToCol: Integer;
    function GetToRow: Integer;
    function GetFill: TXlsFileCellsRangeFillEh;
    function GetFont: TXlsFileCellsRangeFontEh;

    procedure UpdateStyleFromChangedRange(Cell: TXlsFileCellEh; ACol, ARow: Integer);
    function GetNewNumberFormat(Cell: TXlsFileCellEh): TXlsFileStyleNumberFormatEh;
    function GetNewFont(Cell: TXlsFileCellEh): TXlsFileStyleFont;
    function GetNewFill(Cell: TXlsFileCellEh): TXlsFileStyleFill;
    function GetNewBorder(Cell: TXlsFileCellEh; UseLeftOutsideBorder, UseRightOutsideBorder, UseTopOutsideBorder, UseBottomOutsideBorder: Boolean): TXlsFileStyleLinesEh;
    function GetNewStyle(NewNumberFormat: TXlsFileStyleNumberFormatEh; NewFont: TXlsFileStyleFont; NewFill: TXlsFileStyleFill; NewBorder: TXlsFileStyleLinesEh; AHorzAlign: TXlsFileCellHorzAlign; AVertAlign: TXlsFileCellVertAlign; AWrapText: Boolean; ARotation: Integer): TXlsFileCellStyle;
    function GetBorder: TXlsFileCellsRangeLinesEh;
    function GetInsideBorder: TXlsFileCellsRangeLinesEh;
    function GetHorzAlign: TXlsFileCellHorzAlign;
    function GetRotation: Integer;
    function GetVertAlign: TXlsFileCellVertAlign;
    function GetWrapText: Boolean;
    procedure SetHorzAlign(const Value: TXlsFileCellHorzAlign);
    procedure SetRotation(const Value: Integer);
    procedure SetVertAlign(const Value: TXlsFileCellVertAlign);
    procedure SetWrapText(const Value: Boolean);
    function GetNumberFormat: String;
    procedure SetNumberFormat(const Value: String);

  public
    constructor Create(Worksheet: TXlsWorksheetEh);
    destructor Destroy; override;

    procedure ApplyChages;

    property FromCol: Integer read GetFromCol;
    property ToCol: Integer read GetToCol;
    property FromRow: Integer read GetFromRow;
    property ToRow: Integer read GetToRow;

    property Font: TXlsFileCellsRangeFontEh read GetFont;
    property Fill: TXlsFileCellsRangeFillEh read GetFill;
    property Border: TXlsFileCellsRangeLinesEh read GetBorder;
    property InsideBorder: TXlsFileCellsRangeLinesEh read GetInsideBorder;

    property HorzAlign: TXlsFileCellHorzAlign read GetHorzAlign write SetHorzAlign;
    property VertAlign: TXlsFileCellVertAlign read GetVertAlign write SetVertAlign;
    property WrapText: Boolean read GetWrapText write SetWrapText;
    property Rotation: Integer read GetRotation write SetRotation; //Degrees

    property NumberFormat: String read GetNumberFormat write SetNumberFormat;
  end;

function AlignmentToXlsFileCellHorzAlign(Alignment: TAlignment): TXlsFileCellHorzAlign;

implementation

uses XlsFileWritersEh;

function AlignmentToXlsFileCellHorzAlign(Alignment: TAlignment): TXlsFileCellHorzAlign;
begin
 if Alignment = taLeftJustify then
   Result := chaLeftEh
 else if Alignment = taRightJustify then
   Result := chaRightEh
 else if Alignment = taCenter then
   Result := chaCenterEh
 else
  Result := chaUnassignedEh;
end;

{ TXlsFileEh }

constructor TXlsMemFileEh.Create;
begin
  FWorkbook := TXlsWorkbookEh.Create;
end;

destructor TXlsMemFileEh.Destroy;
begin
  FreeAndNil(FWorkbook);
  inherited Destroy;
end;

function TXlsMemFileEh.GetWorkbook: TXlsWorkbookEh;
begin
  Result := FWorkbook;
end;

procedure TXlsMemFileEh.SaveToFile(FileName: String);
var
  FileWriter: TXlsxFileWriterEh;
begin
  FileWriter := TXlsxFileWriterEh.Create(Self);
  FileWriter.WritetToFile(FileName);
  FileWriter.Free;
end;

{ TXlsWorkbookEh }

constructor TXlsWorkbookEh.Create;
begin
  FWorkSheets := TObjectListEh.Create;
  FStyles := TXlsFileStylesEh.Create;

  AddWorksheet('Sheet1');
end;

destructor TXlsWorkbookEh.Destroy;
var
  i: Integer;
begin
  for i := 0 to FWorkSheets.Count-1 do
    FWorkSheets[i].Free;
  FreeAndNil(FWorkSheets);
  FreeAndNil(FStyles);
  inherited Destroy;
end;

function TXlsWorkbookEh.AddWorksheet(WorksheetName: string): TXlsWorksheetEh;
begin
  Result := TXlsWorksheetEh.Create(Self);
  Result.Name := WorksheetName;
  FWorkSheets.Add(Result);
end;

function TXlsWorkbookEh.FindWorksheet(WorksheetName: string): TXlsWorksheetEh;
var
  i: Integer;
begin
  for i := 0 to FWorkSheets.Count - 1 do
  begin
    if TXlsWorksheetEh(FWorkSheets[i]).Name = WorksheetName then
    begin
      Result := TXlsWorksheetEh(FWorkSheets[i]);
      Exit;
    end;
  end;
  Result := nil;
end;

function TXlsWorkbookEh.GetStyles: TXlsFileStylesEh;
begin
  Result := FStyles;
end;

function TXlsWorkbookEh.GetWorksheetCount: Integer;
begin
  Result := FWorkSheets.Count;
end;

procedure TXlsWorkbookEh.MoveWorksheet(FromIndex, ToIndex: Integer);
begin
  FWorkSheets.Move(FromIndex, ToIndex);
end;

procedure TXlsWorkbookEh.RenameWorksheet(Worksheet: TXlsWorksheetEh;
  NewName: string);
begin
  if (FindWorksheet(NewName) <> nil) then
    raise Exception.Create('Worksheet ''' + NewName + ''' already exists');
  Worksheet.FName := NewName;
end;

function TXlsWorkbookEh.GetWorksheet(WorksheetId: Variant): TXlsWorksheetEh;
var
  wsIndex: Integer;
begin
  if (VarIsStr(WorksheetId)) then
    Result := FindWorksheet(VarToStr(WorksheetId))
  else
  begin
    wsIndex := WorksheetId;
    Result := TXlsWorksheetEh(FWorkSheets[wsIndex]);
  end;
end;

{ TXlsWorksheet }

constructor TXlsWorksheetEh.Create(AWorkbook: TXlsWorkbookEh);
begin
  FWorkbook := AWorkbook;
  FColumns := TXlsFileColumnsEh.Create;
  FDimension := TXlsFileWorksheetDimensionEh.Create(Self);
  FAutoFilterRange := TXlsFileWorksheetCellsRectEh.Create(Self);
end;

destructor TXlsWorksheetEh.Destroy;
var
  ic, ir: Integer;
begin
  FreeAndNil(FColumns);
  FreeAndNil(FDimension);
  FreeAndNil(FAutoFilterRange);

  for ic := 0 to Length(FCells) - 1 do
  begin
    for ir := 0 to Length(FCells[ic]) - 1 do
    begin
      if (FCells[ic, ir] <> nil) then
      begin
        FCells[ic, ir].Free;
        FCells[ic, ir] := nil;
      end;
    end;
  end;

  inherited Destroy;
end;

function TXlsWorksheetEh.GetCell(Col, Row: Integer): TXlsFileCellEh;
var
  DimChanged: Boolean;
begin
  DimChanged := False;

  if Length(FCells) <= Col then
  begin
    SetLength(FCells, Col + 1);
    DimChanged := True;
  end;

  if Length(FCells[Col]) <= Row then
  begin
    SetLength(FCells[Col], Row + 1);
    DimChanged := True;
  end;

  if (FCells[Col, Row] = nil) then
  begin
    FCells[Col, Row] := TXlsFileCellEh.Create(Self);
    FCells[Col, Row].FStyle := FWorkbook.Styles.CellStyle[0];
  end;

  if (DimChanged) then
    FDimension.Update;

  Result := FCells[Col, Row];
end;

function TXlsWorksheetEh.GetCellDataExists(Col, Row: Integer): Boolean;
begin
  if (Col < Length(FCells)) and
     (Row < Length(FCells[Col])) and
     (FCells[Col, Row] <> nil)
  then
    Result := True
  else
    Result := False;
end;

function TXlsWorksheetEh.GetCellsRange(FromCol, FromRow, ToCol,
  ToRow: Integer): IXlsFileCellsRangeEh;
var
  Range: TXlsFileCellsRangeEh;
begin
  Range := TXlsFileCellsRangeEh.Create(Self);
  Range.FFromCol := FromCol;
  Range.FFromRow := FromRow;
  Range.FToCol := ToCol;
  Range.FToRow := ToRow;
  Result := Range;
end;

function TXlsWorksheetEh.GetColumns: TXlsFileColumnsEh;
begin
  Result := FColumns;
end;

function TXlsWorksheetEh.GetName: String;
begin
  Result := FName;
end;

procedure TXlsWorksheetEh.SetName(const Value: String);
begin
  if (Name <> Value) then
  begin
    FWorkbook.RenameWorksheet(Self, Value);
  end;
end;

procedure TXlsWorksheetEh.MergeCell(Col, Row, ColCount, RowCount: Integer);
var
  ic, ir: Integer;
  Cell, iCell: TXlsFileCellEh;
  MasterStyle: TXlsFileCellStyle;
begin
  if (ColCount < 0) then
    raise Exception.Create('TXlsWorksheetEh.MergeCell: ColCount < 0');
  if (RowCount < 0) then
    raise Exception.Create('TXlsWorksheetEh.MergeCell: RowCount < 0');

  Cell := Cells[Col, Row];
  if (Cell.MergeRange.ColCount > 0) or
     (Cell.MergeRange.RowCount > 0) then
  begin
    UnmergerCell(Col, Row);
  end;

  MasterStyle := Cell.Style;

  for ic := Col to Col + ColCount do
  begin
    for ir := Row to Row + RowCount do
    begin
      iCell := Cells[ic, ir]; //Force to create cell
      if (ic = Col) and (ir = Row) then
      begin
        iCell.MergeRange.FColCount := ColCount;
        iCell.MergeRange.FRowCount := RowCount;
      end else
      begin
        FCells[ic, ir].Value := Unassigned;
        FCells[ic, ir].Formula := '';
        FCells[ic, ir].FStyle := MasterStyle;
      end;
    end;
  end;
end;

procedure TXlsWorksheetEh.UnmergerCell(Col, Row: Integer);
var
  ic, ir: Integer;
  Cell: TXlsFileCellEh;
begin
  if not CellDataExists[Col, Row] then Exit;
  Cell := Cells[Col, Row];
  if (Cell.MergeRange.ColCount = 0) and (Cell.MergeRange.RowCount = 0) then Exit;

  for ic := Col to Col + Cell.MergeRange.ColCount do
  begin
    for ir := Row to Row + Cell.MergeRange.RowCount do
    begin
      if (ic = Col) and (ir = Row) then
      begin
        FCells[ic, ir].MergeRange.FColCount := 0;
        FCells[ic, ir].MergeRange.FRowCount := 0;
      end else
      begin
        FCells[ic, ir] := nil;
      end;
    end;
  end;
end;

{ TXlsFileColumn }

constructor TXlsFileColumnEh.Create;
begin
  FWidth := 8.43;
  FVisible := True;
end;

destructor TXlsFileColumnEh.Destroy;
begin
  inherited Destroy;
end;

function TXlsFileColumnEh.GetVisible: Boolean;
begin
  Result := FVisible
end;

procedure TXlsFileColumnEh.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

function TXlsFileColumnEh.GetWidth: Single;
begin
  Result := FWidth;
end;

procedure TXlsFileColumnEh.SetWidth(const Value: Single);
begin
  FWidth := Value;
end;

{ TXlsFileColumns }

constructor TXlsFileColumnsEh.Create;
begin
  FColumns := TObjectListEh.Create;
end;

destructor TXlsFileColumnsEh.Destroy;
var
  i: Integer;
begin
  for i := 0 to FColumns.Count-1 do
  begin
    FColumns[i].Free;
    FColumns[i] := nil;
  end;

  FreeAndNil(FColumns);

  inherited Destroy;
end;

function TXlsFileColumnsEh.GetColumn(ColumnIndex: Integer): TXlsFileColumnEh;
begin
  if (ColumnIndex >= FColumns.Count) then
    FColumns.Count := ColumnIndex + 1;

  if (FColumns[ColumnIndex] = nil) then
    FColumns[ColumnIndex] := TXlsFileColumnEh.Create;

  Result := TXlsFileColumnEh(FColumns[ColumnIndex]);
end;

function TXlsFileColumnsEh.GetCurrentCount: Integer;
begin
  Result := FColumns.Count;
end;

function TXlsFileColumnsEh.ColumnIsCreated(ColumnIndex: Integer): Boolean;
begin
  if (ColumnIndex < FColumns.Count) and (FColumns[ColumnIndex] <> nil) then
    Result := True
  else
    Result := False;
end;

function TXlsFileColumnsEh.ScreenToXlsWidth(ScreenWidth: Integer): Single;
begin
  Result := ScreenWidth / 7;
end;

{ TXlsFileCell }

constructor TXlsFileCellEh.Create(AWorksheet: TXlsWorksheetEh);
begin
  FWorksheet := AWorksheet;
  FMergeRange := TXlsFileCellMergeRangeEh.Create;
end;

destructor TXlsFileCellEh.Destroy;
begin
  FreeAndNil(FMergeRange);
  inherited Destroy;
end;

function TXlsFileCellEh.GetStyle: TXlsFileCellStyle;
begin
  Result := FStyle;
end;

function TXlsFileCellEh.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TXlsFileCellEh.SetValue(const Value: Variant);
var
  AVarType: TVarType;
  NewNumberFormat: TXlsFileStyleNumberFormatEh;
  NewStyle: TXlsFileCellStyle;
begin
  FValue := Value;
  AVarType := VarType(FValue);
  if (AVarType = varDate) and (Style.NumberFormat.FormatId = 0) then
  begin
    NewNumberFormat := FWorksheet.FWorkbook.Styles.GetOrCreateNumberFormat('dd/mm/yyyy;@', -1);
    NewStyle := FWorksheet.FWorkbook.Styles.GetOrCreateCellStyle(
                   NewNumberFormat,
                   Style.Font,
                   Style.Fill,
                   Style.Border,
                   Style.HorzAlign,
                   Style.VertAlign,
                   Style.WrapText,
                   Style.Rotation);

    FStyle := NewStyle;
  end;
end;

function TXlsFileCellEh.GetValueType: TCellValueType;
var
  AVarType: TVarType;
begin
  AVarType := VarType(FValue);

  if AVarType in [varEmpty, varNull] then
    Result := cvtEmpty
  else
  begin
    if (AVarType = varDate) and (FValue < EncodeDate(1900, 1, 1)) then
    begin 
      Result := cvtDate;
    end
    else if VarIsNumeric(FValue) then
    begin
      Result := cvtNumber
    end else
    begin
      Result := cvtString;
    end;
  end;
end;

procedure TXlsFileCellEh.SetValueType(const Value: TCellValueType);
begin

end;

{ TXlsFileWorksheetDimensionEh }

constructor TXlsFileWorksheetDimensionEh.Create(Worksheet: TXlsWorksheetEh);
begin
  FWorksheet := Worksheet;
  FFromCol := -1;
  FFromRow := -1;
  FToCol := -1;
  FToRow := -1;
end;

destructor TXlsFileWorksheetDimensionEh.Destroy;
begin
  inherited Destroy;
end;

function TXlsFileWorksheetDimensionEh.GetFromCol: Integer;
begin
  Result := FFromCol;
end;

function TXlsFileWorksheetDimensionEh.GetFromRow: Integer;
begin
  Result := FFromRow;
end;

function TXlsFileWorksheetDimensionEh.GetToCol: Integer;
begin
  Result := FToCol;
end;

function TXlsFileWorksheetDimensionEh.GetToRow: Integer;
begin
  Result := FToRow;
end;

procedure TXlsFileWorksheetDimensionEh.Update;
var
  ic, maxRows: Integer;
begin
  maxRows := 0;

  for ic := 0 to Length(FWorksheet.FCells)-1 do
  begin
    if Length(FWorksheet.FCells[ic]) > maxRows then
      maxRows := Length(FWorksheet.FCells[ic]);
  end;

  if (maxRows > 0) then
  begin
    FFromCol := 0;
    FFromRow := 0;
    FToCol := Length(FWorksheet.FCells);
    FToRow := maxRows;
  end else
  begin
    FFromCol := -1;
    FFromRow := -1;
    FToCol := -1;
    FToRow := -1;
  end;
end;

{ TXlsFileStyles }

constructor TXlsFileStylesEh.Create;
begin
  FNumberFormats := TObjectListEh.Create;
  GetOrCreateNumberFormat('', 0);

  FFonts := TObjectListEh.Create;
  GetOrCreateFont('Calibri', 11, clNone, False, False, False);

  FFills := TObjectListEh.Create;
  GetOrCreateFill(clNone, fptNoneEh);
  GetOrCreateFill(clNone, fptGray125Eh);

  FBorders := TObjectListEh.Create;
  GetOrCreateBorder(clNone, clsNoneEh, clNone, clsNoneEh, clNone, clsNoneEh, clNone, clsNoneEh);

  FCellStyles := TObjectListEh.Create;
  GetOrCreateCellStyle(NumberFormat[0], Font[0], Fill[0], Border[0],
    chaUnassignedEh, cvaUnassignedEh, False, 0);
end;

destructor TXlsFileStylesEh.Destroy;
var
  i: Integer;
begin
  for i := 0 to FCellStyles.Count-1 do
    FCellStyles[i].Free;
  FreeAndNil(FCellStyles);

  for i := 0 to FFonts.Count-1 do
    FFonts[i].Free;
  FreeAndNil(FFonts);

  for i := 0 to FFills.Count-1 do
    FFills[i].Free;
  FreeAndNil(FFills);

  for i := 0 to FBorders.Count-1 do
    FBorders[i].Free;
  FreeAndNil(FBorders);

  for i := 0 to FNumberFormats.Count-1 do
    FNumberFormats[i].Free;
  FreeAndNil(FNumberFormats);

  inherited Destroy;
end;

function TXlsFileStylesEh.GetBorder(Index: Integer): TXlsFileStyleLinesEh;
begin
  Result := TXlsFileStyleLinesEh(FBorders[Index]);
end;

function TXlsFileStylesEh.GetBorderCount: Integer;
begin
  Result := FBorders.Count;
end;

function TXlsFileStylesEh.GetCellStyle(Index: Integer): TXlsFileCellStyle;
begin
  Result := TXlsFileCellStyle(FCellStyles[Index]);
end;

function TXlsFileStylesEh.GetCellStyleCount: Integer;
begin
  Result := FCellStyles.Count;
end;

function TXlsFileStylesEh.GetFill(Index: Integer): TXlsFileStyleFill;
begin
  Result := TXlsFileStyleFill(FFills[Index]);
end;

function TXlsFileStylesEh.GetFillCount: Integer;
begin
  Result := FFills.Count;
end;

function TXlsFileStylesEh.GetFont(Index: Integer): TXlsFileStyleFont;
begin
  Result := TXlsFileStyleFont(FFonts[Index]);
end;

function TXlsFileStylesEh.GetFontCount: Integer;
begin
  Result := FFonts.Count;
end;

function TXlsFileStylesEh.GetNumberFormat(Index: Integer): TXlsFileStyleNumberFormatEh;
begin
  Result := TXlsFileStyleNumberFormatEh(FNumberFormats[Index]);
end;

function TXlsFileStylesEh.GetNumberFormatCount: Integer;
begin
  Result := FNumberFormats.Count;
end;

function TXlsFileStylesEh.GetOrCreateFont(FontName: String; FontSize: Integer;
  FontColor: TColor; FontIsBold, FontIsItalic,
  FontIsUnderline: Boolean): TXlsFileStyleFont;
var
  i: Integer;
  iFnt: TXlsFileStyleFont;
begin
  for i := 0 to FFonts.Count-1 do
  begin
    iFnt := Font[i];
    if (SameText(iFnt.Name, FontName)) and
       (iFnt.Size = FontSize) and
       (iFnt.Color = FontColor) and
       (iFnt.IsBold = FontIsBold) and
       (iFnt.IsItalic = FontIsItalic) and
       (iFnt.IsUnderline = FontIsUnderline)
    then
    begin
      Result := iFnt;
      Exit;
    end;
  end;

  Result := TXlsFileStyleFont.Create;
  Result.Name := FontName;
  Result.Size := FontSize;
  Result.Color := FontColor;
  Result.IsBold := FontIsBold;
  Result.IsItalic := FontIsItalic;
  Result.IsUnderline := FontIsUnderline;
  Result.FIndex := FFonts.Count;

  FFonts.Add(Result);
end;

function TXlsFileStylesEh.GetOrCreateNumberFormat(FormatStr: String; FormatId: Integer): TXlsFileStyleNumberFormatEh;
var
  i: Integer;
  iNf: TXlsFileStyleNumberFormatEh;
begin
  for i := 0 to FNumberFormats.Count-1 do
  begin
    iNf:= NumberFormat[i];
    if (iNf.FormatStr = FormatStr) then
    begin
      Result := iNf;
      Exit;
    end;
  end;

  Result := TXlsFileStyleNumberFormatEh.Create;
  Result.FormatStr := FormatStr;
  if (FormatId >= 0)
    then Result.FormatId := FormatId
    else Result.FormatId := FNumberFormats.Count + 163;

  FNumberFormats.Add(Result);
end;

function TXlsFileStylesEh.GetOrCreateFill(FillColor: TColor; FillPatternType: TXlsFileStyleFillPatternTypeEh): TXlsFileStyleFill;
var
  i: Integer;
  iFl: TXlsFileStyleFill;
begin
  for i := 0 to FFills.Count-1 do
  begin
    iFl := Fill[i];
    if (iFl.Color = FillColor) and
       (iFl.PatternType = FillPatternType)
    then
    begin
      Result := iFl;
      Exit;
    end;
  end;

  Result := TXlsFileStyleFill.Create;
  Result.Color := FillColor;
  Result.FPatternType := fptSolidEh;
  Result.FIndex := FFills.Count;

  FFills.Add(Result);
end;

function TXlsFileStylesEh.GetOrCreateBorder(LeftLineColor: TColor;
  LeftLineStyle: TXlsFileCellLineStyleEh; RightLineColor: TColor;
  RightLineStyle: TXlsFileCellLineStyleEh; TopLineColor: TColor;
  TopLineStyle: TXlsFileCellLineStyleEh; BottomLineColor: TColor;
  BottomLineStyle: TXlsFileCellLineStyleEh): TXlsFileStyleLinesEh;
var
  i: Integer;
  iFnt: TXlsFileStyleLinesEh;
begin
  for i := 0 to FBorders.Count-1 do
  begin
    iFnt := Border[i];
    if (iFnt.Left.Color = LeftLineColor) and
       (iFnt.Left.Style = LeftLineStyle) and
       (iFnt.Right.Color = RightLineColor) and
       (iFnt.Right.Style = RightLineStyle) and
       (iFnt.Top.Color = TopLineColor) and
       (iFnt.Top.Style = TopLineStyle) and
       (iFnt.Bottom.Color = BottomLineColor) and
       (iFnt.Bottom.Style = BottomLineStyle)
    then
    begin
      Result := iFnt;
      Exit;
    end;
  end;

  Result := TXlsFileStyleLinesEh.Create;
  Result.Left.Color := LeftLineColor;
  Result.Left.Style := LeftLineStyle;
  Result.Right.Color := RightLineColor;
  Result.Right.Style := RightLineStyle;
  Result.Top.Color := TopLineColor;
  Result.Top.Style := TopLineStyle;
  Result.Bottom.Color := BottomLineColor;
  Result.Bottom.Style := BottomLineStyle;
  Result.FIndex := FBorders.Count;

  FBorders.Add(Result);
end;

function TXlsFileStylesEh.GetOrCreateCellStyle(
  ANumberFormat: TXlsFileStyleNumberFormatEh;
  AFont: TXlsFileStyleFont;
  AFill: TXlsFileStyleFill; ABorder: TXlsFileStyleLinesEh;
  AHorzAlign: TXlsFileCellHorzAlign; AVertAlign: TXlsFileCellVertAlign;
  AWrapText: Boolean; ARotation: Integer): TXlsFileCellStyle;
var
  i: Integer;
  iStl: TXlsFileCellStyle;
begin
  for i := 0 to FCellStyles.Count-1 do
  begin
    iStl := CellStyle[i];
    if (iStl.NumberFormat = ANumberFormat) and
       (iStl.Font = AFont) and
       (iStl.Fill = AFill) and
       (iStl.Border = ABorder) and
       (iStl.HorzAlign = AHorzAlign) and
       (iStl.VertAlign = AVertAlign) and
       (iStl.WrapText = AWrapText) and
       (iStl.Rotation = ARotation)
    then
    begin
      Result := iStl;
      Exit;
    end;
  end;

  Result := TXlsFileCellStyle.Create;
  Result.FNumberFormat := ANumberFormat;
  Result.FFont := AFont;
  Result.FFill := AFill;
  Result.FBorder := ABorder;
  Result.FIndex := FCellStyles.Count;
  Result.FHorzAlign := AHorzAlign;
  Result.FVertAlign := AVertAlign;
  Result.FWrapText := AWrapText;
  Result.FRotation := ARotation;

  FCellStyles.Add(Result);
end;

{ TXlsFileCellsRangeEh }

constructor TXlsFileCellsRangeEh.Create(Worksheet: TXlsWorksheetEh);
begin
  inherited Create;

  FFont := TXlsFileCellsRangeFontEh.Create;
  FFill := TXlsFileCellsRangeFillEh.Create;
  FBorder := TXlsFileCellsRangeLinesEh.Create;
  FInsideBorder := TXlsFileCellsRangeLinesEh.Create;
  FWorksheet := Worksheet;
end;

destructor TXlsFileCellsRangeEh.Destroy;
begin
  FreeAndNil(FFont);
  FreeAndNil(FFill);
  FreeAndNil(FBorder);
  FreeAndNil(FInsideBorder);

  inherited Destroy;
end;

procedure TXlsFileCellsRangeEh.ApplyChages;
var
  ic, ir: Integer;
  Cell: TXlsFileCellEh;
begin
  if (FFont.HasChanges or
      FFill.HasChanges or
      FBorder.HasChanges or
      FInsideBorder.HasChanges or
      FHorzAlignChanged or
      FVertAlignChanged or
      FWrapTextChanged or
      FRotationChanged or
      FNumberFormatChanged
      ) then
  begin
    for ic := FromCol to ToCol do
    begin
      for ir := FromRow to ToRow do
      begin
        Cell := FWorksheet.Cells[ic, ir];
        UpdateStyleFromChangedRange(Cell, ic, ir);
      end;
    end;
  end;
end;

function TXlsFileCellsRangeEh.GetBorder: TXlsFileCellsRangeLinesEh;
begin
  Result := FBorder;
end;

function TXlsFileCellsRangeEh.GetInsideBorder: TXlsFileCellsRangeLinesEh;
begin
  Result := FInsideBorder;
end;

function TXlsFileCellsRangeEh.GetFill: TXlsFileCellsRangeFillEh;
begin
  Result := FFill;
end;

function TXlsFileCellsRangeEh.GetFont: TXlsFileCellsRangeFontEh;
begin
  Result := FFont;
end;

function TXlsFileCellsRangeEh.GetFromCol: Integer;
begin
  Result := FFromCol;
end;

function TXlsFileCellsRangeEh.GetFromRow: Integer;
begin
  Result := FFromRow;
end;

function TXlsFileCellsRangeEh.GetToCol: Integer;
begin
  Result := FToCol;
end;

function TXlsFileCellsRangeEh.GetToRow: Integer;
begin
  Result := FToRow;
end;

function TXlsFileCellsRangeEh.GetHorzAlign: TXlsFileCellHorzAlign;
begin
  Result := FHorzAlign;
end;

procedure TXlsFileCellsRangeEh.SetHorzAlign(const Value: TXlsFileCellHorzAlign);
begin
  FHorzAlign := Value;
  FHorzAlignChanged := True;
end;

function TXlsFileCellsRangeEh.GetVertAlign: TXlsFileCellVertAlign;
begin
  Result := FVertAlign;
end;

procedure TXlsFileCellsRangeEh.SetVertAlign(const Value: TXlsFileCellVertAlign);
begin
  FVertAlign := Value;
  FVertAlignChanged := True;
end;

function TXlsFileCellsRangeEh.GetWrapText: Boolean;
begin
  Result := FWrapText;
end;

procedure TXlsFileCellsRangeEh.SetWrapText(const Value: Boolean);
begin
  FWrapText := Value;
  FWrapTextChanged := True;
end;

function TXlsFileCellsRangeEh.GetRotation: Integer;
begin
  Result := FRotation;
end;

procedure TXlsFileCellsRangeEh.SetRotation(const Value: Integer);
begin
  FRotation := Value;
  FRotationChanged := True;
end;

function TXlsFileCellsRangeEh.GetNumberFormat: String;
begin
  Result := FNumberFormat;
end;

procedure TXlsFileCellsRangeEh.SetNumberFormat(const Value: String);
begin
  FNumberFormat := Value;
  FNumberFormatChanged := True;
end;

procedure TXlsFileCellsRangeEh.UpdateStyleFromChangedRange(Cell: TXlsFileCellEh; ACol, ARow: Integer);
var
  NewNumberFormat: TXlsFileStyleNumberFormatEh;
  NewFont: TXlsFileStyleFont;
  NewFill: TXlsFileStyleFill;
  NewBorder: TXlsFileStyleLinesEh;
  NewHorzAlign: TXlsFileCellHorzAlign;
  NewVertAlign: TXlsFileCellVertAlign;
  NewWrapText: Boolean;
  NewRotation: Integer;
begin
  NewNumberFormat := GetNewNumberFormat(Cell);

  NewFont := GetNewFont(Cell);
  NewFill := GetNewFill(Cell);
  NewBorder := GetNewBorder(Cell, ACol = FromCol, ACol = ToCol, ARow = FromRow, ARow = ToRow);

  if (FHorzAlignChanged)
    then NewHorzAlign := HorzAlign
    else NewHorzAlign := Cell.Style.HorzAlign;

  if (FVertAlignChanged)
    then NewVertAlign := VertAlign
    else NewVertAlign := Cell.Style.VertAlign;

  if (FWrapTextChanged)
    then NewWrapText := WrapText
    else NewWrapText := Cell.Style.WrapText;

  if (FRotationChanged)
    then NewRotation := Rotation
    else NewRotation := Cell.Style.Rotation;

  Cell.FStyle := GetNewStyle(NewNumberFormat, NewFont, NewFill, NewBorder,
    NewHorzAlign, NewVertAlign, NewWrapText, NewRotation);
end;

function TXlsFileCellsRangeEh.GetNewNumberFormat(Cell: TXlsFileCellEh): TXlsFileStyleNumberFormatEh;
var
  FormatStr: String;
begin

  if (FNumberFormatChanged) then
  begin
    FormatStr := NumberFormat;
  end else
  begin
    FormatStr := Cell.Style.NumberFormat.FormatStr;
  end;

  Result := FWorksheet.FWorkbook.Styles.GetOrCreateNumberFormat(FormatStr, -1);
end;

function TXlsFileCellsRangeEh.GetNewFont(Cell: TXlsFileCellEh): TXlsFileStyleFont;
var
  FontName: String;
  FontSize: Integer;
  FontColor: TColor;
  FontIsBold: Boolean;
  FontIsItalic: Boolean;
  FontIsUnderline: Boolean;
begin
  if Font.FNameChanged
    then FontName := Font.Name
    else FontName := Cell.Style.Font.Name;

  if Font.FSizeChanged
    then FontSize := Font.Size
    else FontSize := Cell.Style.Font.Size;

  if Font.FColorChanged
    then FontColor := Font.Color
    else FontColor := Cell.Style.Font.Color;

  if Font.FIsBoldChanged
    then FontIsBold := Font.IsBold
    else FontIsBold := Cell.Style.Font.IsBold;

  if Font.FIsItalicChanged
    then FontIsItalic := Font.IsItalic
    else FontIsItalic := Cell.Style.Font.IsItalic;

  if Font.FIsUnderlineChanged
    then FontIsUnderline := Font.IsUnderline
    else FontIsUnderline := Cell.Style.Font.IsUnderline;

  Result := FWorksheet.FWorkbook.Styles.
    GetOrCreateFont(FontName, FontSize, FontColor, FontIsBold, FontIsItalic, FontIsUnderline);
end;

function TXlsFileCellsRangeEh.GetNewFill(Cell: TXlsFileCellEh): TXlsFileStyleFill;
var
  FillColor: TColor;
  FillPatternType: TXlsFileStyleFillPatternTypeEh;
begin
  if (Fill.FColorChanged) then
  begin
    FillColor := Fill.Color;
    FillPatternType := fptSolidEh;
  end else
  begin
    FillColor := Cell.Style.Fill.Color;
    FillPatternType := Cell.Style.Fill.PatternType;
  end;

  Result := FWorksheet.FWorkbook.Styles.GetOrCreateFill(FillColor, FillPatternType);
end;

function TXlsFileCellsRangeEh.GetNewBorder(Cell: TXlsFileCellEh; UseLeftOutsideBorder, UseRightOutsideBorder, UseTopOutsideBorder, UseBottomOutsideBorder: Boolean): TXlsFileStyleLinesEh;
var
  LeftBorderColor: TColor;
  LeftBorderStyle: TXlsFileCellLineStyleEh;
  RightBorderColor: TColor;
  RightBorderStyle: TXlsFileCellLineStyleEh;
  TopBorderColor: TColor;
  TopBorderStyle: TXlsFileCellLineStyleEh;
  BottomBorderColor: TColor;
  BottomBorderStyle: TXlsFileCellLineStyleEh;
begin
  //Left
  if (Border.Left.FColorChanged and UseLeftOutsideBorder) then
    LeftBorderColor := Border.Left.Color
  else if (InsideBorder.Left.FColorChanged and not UseLeftOutsideBorder) then
    LeftBorderColor := InsideBorder.Left.Color
  else
    LeftBorderColor := Cell.Style.Border.Left.Color;

  if (Border.Left.FStyleChanged and UseLeftOutsideBorder) then
    LeftBorderStyle := Border.Left.Style
  else if (InsideBorder.Left.FColorChanged and not UseLeftOutsideBorder) then
    LeftBorderStyle := InsideBorder.Left.Style
  else
    LeftBorderStyle := Cell.Style.Border.Left.Style;

  //Right
  if (Border.Right.FColorChanged and UseRightOutsideBorder) then
    RightBorderColor := Border.Right.Color
  else if (InsideBorder.Right.FColorChanged and not UseRightOutsideBorder) then
    RightBorderColor := InsideBorder.Right.Color
  else
    RightBorderColor := Cell.Style.Border.Right.Color;

  if (Border.Right.FStyleChanged and UseRightOutsideBorder) then
    RightBorderStyle := Border.Right.Style
  else if (InsideBorder.Right.FColorChanged and not UseRightOutsideBorder) then
    RightBorderStyle := InsideBorder.Right.Style
  else
    RightBorderStyle := Cell.Style.Border.Right.Style;

  //Top
  if (Border.Top.FColorChanged and UseTopOutsideBorder) then
    TopBorderColor := Border.Top.Color
  else if (InsideBorder.Top.FColorChanged and not UseTopOutsideBorder) then
    TopBorderColor := InsideBorder.Top.Color
  else
    TopBorderColor := Cell.Style.Border.Top.Color;

  if (Border.Top.FStyleChanged and UseTopOutsideBorder) then
    TopBorderStyle := Border.Top.Style
  else if (InsideBorder.Top.FColorChanged and not UseTopOutsideBorder) then
    TopBorderStyle := InsideBorder.Top.Style
  else
    TopBorderStyle := Cell.Style.Border.Top.Style;

  //Bottom
  if (Border.Bottom.FColorChanged and UseBottomOutsideBorder) then
    BottomBorderColor := Border.Bottom.Color
  else if (InsideBorder.Bottom.FColorChanged and not UseBottomOutsideBorder) then
    BottomBorderColor := InsideBorder.Bottom.Color
  else
    BottomBorderColor := Cell.Style.Border.Bottom.Color;

  if (Border.Bottom.FStyleChanged and UseBottomOutsideBorder) then
    BottomBorderStyle := Border.Bottom.Style
  else if (InsideBorder.Bottom.FColorChanged and not UseBottomOutsideBorder) then
    BottomBorderStyle := InsideBorder.Bottom.Style
  else
    BottomBorderStyle := Cell.Style.Border.Bottom.Style;

  Result := FWorksheet.FWorkbook.Styles.GetOrCreateBorder(LeftBorderColor, LeftBorderStyle,
                                                          RightBorderColor, RightBorderStyle,
                                                          TopBorderColor, TopBorderStyle,
                                                          BottomBorderColor, BottomBorderStyle);
end;

function TXlsFileCellsRangeEh.GetNewStyle(NewNumberFormat: TXlsFileStyleNumberFormatEh;
  NewFont: TXlsFileStyleFont;
  NewFill: TXlsFileStyleFill; NewBorder: TXlsFileStyleLinesEh;
  AHorzAlign: TXlsFileCellHorzAlign; AVertAlign: TXlsFileCellVertAlign;
  AWrapText: Boolean; ARotation: Integer): TXlsFileCellStyle;
begin
  Result := FWorksheet.FWorkbook.Styles.
    GetOrCreateCellStyle(NewNumberFormat, NewFont, NewFill, NewBorder, AHorzAlign, AVertAlign, AWrapText, ARotation);
end;

{ TXlsFileCellsRangeFontEh }

function TXlsFileCellsRangeFontEh.HasChanges: Boolean;
begin
  Result := FNameChanged or
            FIsUnderlineChanged or
            FColorChanged or
            FIsItalicChanged or
            FIsBoldChanged or
            FSizeChanged;
end;

procedure TXlsFileCellsRangeFontEh.SetColor(const Value: TColor);
begin
  FColor := Value;
  FColorChanged := True;
end;

procedure TXlsFileCellsRangeFontEh.SetIsBold(const Value: Boolean);
begin
  FIsBold := Value;
  FIsBoldChanged := True;
end;

procedure TXlsFileCellsRangeFontEh.SetIsItalic(const Value: Boolean);
begin
  FIsItalic := Value;
  FIsItalicChanged := True;
end;

procedure TXlsFileCellsRangeFontEh.SetIsUnderline(const Value: Boolean);
begin
  FIsUnderline := Value;
  FIsUnderlineChanged := True;
end;

procedure TXlsFileCellsRangeFontEh.SetName(const Value: String);
begin
  FName := Value;
  FNameChanged := True;
end;

procedure TXlsFileCellsRangeFontEh.SetSize(const Value: Integer);
begin
  FSize := Value;
  FSizeChanged := True;
end;

{ TXlsFileCellsRangeFillEh }

function TXlsFileCellsRangeFillEh.HasChanges: Boolean;
begin
  Result := FColorChanged;
end;

procedure TXlsFileCellsRangeFillEh.SetColor(const Value: TColor);
begin
  FColor := Value;
  FColorChanged := True;
end;

{ TXlsFileCellsRangeLineEh }

constructor TXlsFileCellsRangeLineEh.Create;
begin
  FColor := clNone;
end;

function TXlsFileCellsRangeLineEh.HasChanges: Boolean;
begin
  Result := FColorChanged or FStyleChanged;
end;

procedure TXlsFileCellsRangeLineEh.SetColor(const Value: TColor);
begin
  FColor := Value;
  if (FStyle = clsNoneEh) then
    Style := clsThinEh;

  FColorChanged := True;
end;

procedure TXlsFileCellsRangeLineEh.SetStyle(const Value: TXlsFileCellLineStyleEh);
begin
  FStyle := Value;
  if (Color = clNone) then
    Color := clBlack;

  FStyleChanged := True;
end;

{ TXlsFileCellsRangeLinesEh }

constructor TXlsFileCellsRangeLinesEh.Create;
begin
  FLeft := TXlsFileCellsRangeLineEh.Create;
  FRight := TXlsFileCellsRangeLineEh.Create;
  FTop := TXlsFileCellsRangeLineEh.Create;
  FBottom := TXlsFileCellsRangeLineEh.Create;
end;

destructor TXlsFileCellsRangeLinesEh.Destroy;
begin
  FreeAndNil(FLeft);
  FreeAndNil(FRight);
  FreeAndNil(FTop);
  FreeAndNil(FBottom);
  inherited Destroy;
end;

function TXlsFileCellsRangeLinesEh.HasChanges: Boolean;
begin
  Result := Left.HasChanges or Right.HasChanges or Top.HasChanges or Bottom.HasChanges;
end;

{ TXlsFileStyleLinesEh }

constructor TXlsFileStyleLinesEh.Create;
begin
  FRight := TXlsFileStyleLineEh.Create;
  FBottom := TXlsFileStyleLineEh.Create;
  FTop := TXlsFileStyleLineEh.Create;
  FLeft := TXlsFileStyleLineEh.Create;
end;

destructor TXlsFileStyleLinesEh.Destroy;
begin
  FreeAndNil(FRight);
  FreeAndNil(FBottom);
  FreeAndNil(FTop);
  FreeAndNil(FLeft);
  inherited Destroy;
end;

{ TXlsFileWorksheetCellsRangeEh }

constructor TXlsFileWorksheetCellsRectEh.Create(Worksheet: TXlsWorksheetEh);
begin
  FWorksheet := Worksheet;
end;

destructor TXlsFileWorksheetCellsRectEh.Destroy;
begin
  inherited Destroy;
end;

function TXlsFileWorksheetCellsRectEh.IsEmpty: Boolean;
begin
  if (FFromCol = 0) and
     (FToCol = 0) and
     (FFromRow = 0) and
     (FToRow = 0)
  then
    Result := True
  else
    Result := False;
end;

procedure TXlsFileWorksheetCellsRectEh.Clear;
begin
  FFromCol := 0;
  FToCol := 0;
  FFromRow := 0;
  FToRow := 0;
end;

end.
