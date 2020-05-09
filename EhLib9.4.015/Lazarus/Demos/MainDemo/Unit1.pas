unit Unit1;

{$MODE Delphi}

//{$I EhLib.Inc}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
{$IFDEF EH_LIB_7}
  {$IFNDEF EH_LIB_16}
    XPMan,
  {$ENDIF}
{$ENDIF}
{$IFDEF CIL}
  Types, System.ComponentModel, Variants, System.Runtime.InteropServices,
{$ELSE}
{$ENDIF}
{$IFDEF EH_LIB_16} Themes, {$ENDIF}
  Grids,
  Dialogs, Buttons, ExtCtrls, ComCtrls, ToolWin, DBGridsEh, Menus,
  DataDriverEh, EhLibLCL, FrameMainGrid, FmtBCD,
//  PrnDbgeh,

  EhLibMTE, DBGridEhGrouping, ToolCtrlsEh, DBGridEhToolCtrls, MemTableDataEh,
  Db, odbcconn, sqldb, MemTableEh, GridsEh, StdCtrls, DBCtrlsEh, ImgList,
  StdActns, ActnList, FileUtil, DBAxisGridsEh, DynVarsEh, SQLDBDataDriverEh;

type

  { TForm1 }

  TForm1 = class(TForm)
    DataStruct: TMTDataStructEh;
    MemTableData: TMemTableDataEh;
    MenuName: TMTStringDataFieldEh;
    Panel1: TPanel;
    RecordsList: TRecordsListEh;
    RefFrame: TMTRefObjectFieldEh;
    RefFrameClass: TMTRefObjectFieldEh;
    RowDetailData: TRowDetailPanelControlEh;
    SQLDBConnectionProviderEh1: TSQLDBConnectionProviderEh;
    SQLDBConnectionProviderEh2: TSQLDBConnectionProviderEh;
    SQLQuery1: TSQLQuery;
    SQLQuery2: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    SQLTransaction2: TSQLTransaction;
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    Exit1: TMenuItem;
    About1: TMenuItem;
    Splitter1: TSplitter;
    MemTableEh1: TMemTableEh;
    DataSource1: TDataSource;
    MemTableEh1MenuName: TStringField;
    MemTableEh1RefFrameClass: TRefObjectField;
    MemTableEh1RefFrame: TRefObjectField;
    DBGridEh1: TDBGridEh;
    Bevel1: TBevel;
    Panel2: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    PanelXEStyles: TPanel;
    DBComboBoxEh1: TDBComboBoxEh;
    PaintBox1: TPaintBox;
    StaticText1: TStaticText;
    ImageList1: TImageList;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ActionList1: TActionList;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    actPreview: TAction;
//    PrintDBGridEh1: TPrintDBGridEh;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    actExit: TAction;
    actAbout: TAction;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    actShowObjInspector: TAction;
    procedure ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ODBCConnection1BeforeConnect(Sender: TObject);
    procedure ODBCConnection2BeforeConnect(Sender: TObject);
    procedure ScrollBox1Resize(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ADOConnectionProviderEh1InlineConnectionBeforeConnect(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
//    procedure ToolBar1CustomDraw(Sender: TToolBar; const ARect: TRect; var DefaultDraw: Boolean);
    procedure DBGridEh1GetCellParams(Sender: TObject; Column: TColumnEh;
      AFont: TFont; var Background: TColor; State: TGridDrawState);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
    procedure About1Click(Sender: TObject);
    procedure ADOConnectionProviderEh2InlineConnectionBeforeConnect(Sender: TObject);
    function SQLConnectionProviderEh1ExecuteCommand(
      SQLDataDriver: TCustomSQLDataDriverEh; Command: TCustomSQLCommandEh;
      var Cursor: TDataSet; var FreeOnEof: Boolean; var Processed: Boolean
      ): Integer;
    function SQLConnectionProviderEh2ExecuteCommand(
      SQLDataDriver: TCustomSQLDataDriverEh; Command: TCustomSQLCommandEh;
      var Cursor: TDataSet; var FreeOnEof: Boolean; var Processed: Boolean
      ): Integer;
    function SQLDBConnectionProviderEh1ExecuteCommand(
      SQLDataDriver: TCustomSQLDataDriverEh; Command: TCustomSQLCommandEh;
      var Cursor: TDataSet; var FreeOnEof: Boolean; var Processed: Boolean
      ): Integer;
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure DBComboBoxEh1Change(Sender: TObject);
//    procedure ActionList1Update(Action: TBasicAction;
//      var Handled: Boolean);
    procedure actPreviewExecute(Sender: TObject);
    procedure MemTableEh1AfterScroll(DataSet: TDataSet);
    procedure actExitExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure actShowObjInspectorExecute(Sender: TObject);
    procedure ToolButton12Click(Sender: TObject);
  private
  public
    Frame: TFrame;
    CurPan: TPanel;
    FrameTitleFont: TFont;
    FrameTitlePos: TPoint;

    frMainGrid: TfrMainGrid;
//    frMasterDetail: TfrMasterDetail;

    UseDfmDataTablesForConnectionProvider: Boolean;
    DfmDataTablesPath: String;

    function TestInitXE2: Boolean;
    procedure FillFrameTopPanel(ACanvas: TCanvas; const ARect: TRect);
    procedure ScaleFontHeight(Control: TControl; M, D: Integer; SrcH, ChH: Integer);

    procedure InitDataBase;
    procedure InitConnectionProvider1;
    procedure InitConnectionProvider2;
  end;

var
  Form1: TForm1;


type
  TGradientDirection = (gdHorizontal, gdVertical);

procedure GradientFillCanvas(const ACanvas: TCanvas;
const AStartColor, AEndColor: TColor; const ARect: TRect;
  const Direction: TGradientDirection);

implementation

{$R *.lfm}

uses ObjectInspectorEh, FormAbout, FrameEditControls, FrameVertGrid,
  FrameDropDownForms, FrameRowDetailPanel,
  FrameTreeView,   FrameMasterDetail, FrameFishFact,
  FrameMailBox,
  FrameCellDataIsLink,
  FramePivotGrid,
  FramePlanner,
  FrameDataGrouping, FrameBackgroundImages, FrameRowAsPanel,
  FrameLiveRestructure, FrameWorkingWithHugeData, FrameMemTableSaveLoad,
  FrameImportExport, FrameSearchPanel,
  FrameDataSetTextExporter,
  FrameDataSetTextImporter;

procedure GradientFillCanvas(const ACanvas: TCanvas;
  const AStartColor, AEndColor: TColor; const ARect: TRect;
  const Direction: TGradientDirection);
begin
end;

procedure TForm1.ScrollBox1Resize(Sender: TObject);
begin
end;

procedure TForm1.ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
begin
  actPreview.Enabled := (ActiveControl is TCustomDBGridEh);
  actExit.Enabled := True;
end;

procedure TForm1.Button1Click(Sender: TObject);
//var
//  ABlobType: TBlobType;
begin
//  ShowMessage('To Do');
  //SQLQuery2.Close;
  //SQLQuery2.SQL.Text := 'select Notes from biolife';
  //SQLQuery2.Open;
  //ABlobType := TMemoField(SQLQuery2.Fields[0]).BlobType;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.ScaleFontHeight(Control: TControl; M, D: Integer; SrcH, ChH: Integer);
begin
//  if Control.ParentFont
end;

function TForm1.TestInitXE2: Boolean;
{$IFDEF EH_LIB_16}
var
  sa: TArray<string>;
  i: Integer;
begin
  Result := True;
  sa := TStyleManager.StyleNames;
  for I := 0 to Length(sa)-1 do
    DBComboBoxEh1.Items.Add(sa[i]);
  DBComboBoxEh1.ItemIndex := 0;
end;
{$ELSE}
begin
  Result := False;
end;
{$ENDIF}

procedure TForm1.FillFrameTopPanel(ACanvas: TCanvas; const ARect: TRect);
begin
  GradientFillCanvas(ACanvas,
    ApproximateColor(StyleServices.GetSystemColor(clBtnFace), StyleServices.GetSystemColor(clWindow), 170),
    StyleServices.GetSystemColor(clBtnFace),
    ARect, gdVertical);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  FormCreate(Sender);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
//  Align := alClient;
//  SetEhLibDebugFocus(2000);

  PanelXEStyles.Visible := TestInitXE2;
  Caption := Caption + ' (' + EhLibVerInfo + ' ' + EhLibBuildInfo + ')';
  //StatusBar1.SimpleText := StatusBar1.SimpleText + ' ' + EhLibVerInfo + ' ' + EhLibBuildInfo;

  InitDataBase;

  DBGridEh1.Font.Color := clNavy;

  frMainGrid := TfrMainGrid.Create(Self);
  frMainGrid.Parent := Panel1;
  Frame := frMainGrid;
  FrameTitleFont := TFont.Create;
  FrameTitleFont.Assign(frMainGrid.FrameTitleLabel.Font);
  FrameTitlePos := Point(frMainGrid.FrameTitleLabel.Left, frMainGrid.FrameTitleLabel.Top);

//  CurPan := SpeedBPanel1;

//  SpeedBPanel1.Color :=  ApproximateColor(clBtnShadow, clBtnFace, 170);
//  SpeedBPanel2.Color := clBtnShadow;
//  Edit1.Text := Frame21.Text;
//  Exit;

  DataSource1.OnDataChange := nil;
  MemTableEh1.CreateDataSet;
  MemTableEh1.First;
  MemTableEh1.Append;
  MemTableEh1MenuName.AsString := 'Main Grid';
  TRefObjectField(MemTableEh1.FieldByName('RefFrameClass')).Value := TObject(TfrMainGrid);
  MemTableEh1RefFrame.Value := Frame;
  MemTableEh1.Post;

  MemTableEh1.Next;
  MemTableEh1.Append;
  MemTableEh1MenuName.AsString := '  Master Detail';
  TRefObjectField(MemTableEh1.FieldByName('RefFrameClass')).Value := TObject(TfrMasterDetail);
  MemTableEh1.Post;

  MemTableEh1.Next;
  MemTableEh1.Append;
  MemTableEh1MenuName.AsString := '  Edit Controls';
  TRefObjectField(MemTableEh1.FieldByName('RefFrameClass')).Value := TObject(TfrEditControls);
  MemTableEh1.Post;

  MemTableEh1.Next;
  MemTableEh1.Append;
  MemTableEh1MenuName.AsString := '  Fish fact';
  TRefObjectField(MemTableEh1.FieldByName('RefFrameClass')).Value := TObject(TfrFishFact);
  MemTableEh1.Post;

  MemTableEh1.Next;
  MemTableEh1.Append;
  MemTableEh1MenuName.AsString := '  Vertical Grid';
  TRefObjectField(MemTableEh1.FieldByName('RefFrameClass')).Value := TObject(TfrVertGrid);
  MemTableEh1.Post;

  MemTableEh1.Next;
  MemTableEh1.Append;
  MemTableEh1MenuName.AsString := '  Cell Data Is Link';
  TRefObjectField(MemTableEh1.FieldByName('RefFrameClass')).Value := TObject(TfrCellDataIsLink);
  MemTableEh1.Post;

  MemTableEh1.Next;
  MemTableEh1.Append;
  MemTableEh1MenuName.AsString := '  FrameDropDownForms';
  TRefObjectField(MemTableEh1.FieldByName('RefFrameClass')).Value := TObject(TfrFrameDropDownForms);
  MemTableEh1.Post;

  MemTableEh1.Next;
  MemTableEh1.Append;
  MemTableEh1MenuName.AsString := '  RowDetailPanel';
  TRefObjectField(MemTableEh1.FieldByName('RefFrameClass')).Value := TObject(TfrRowDetailPanel);
  MemTableEh1.Post;

  MemTableEh1.Next;
  MemTableEh1.Append;
  MemTableEh1MenuName.AsString := '  RowAsPanel';
  TRefObjectField(MemTableEh1.FieldByName('RefFrameClass')).Value := TObject(TfrRowAsPanel);
  MemTableEh1.Post;

  MemTableEh1.Next;
  MemTableEh1.Append;
  MemTableEh1MenuName.AsString := '  TreeView';
  TRefObjectField(MemTableEh1.FieldByName('RefFrameClass')).Value := TObject(TfrTreeView);
  MemTableEh1.Post;

  MemTableEh1.Next;
  MemTableEh1.Append;
  MemTableEh1MenuName.AsString := '  DataGrouping';
  TRefObjectField(MemTableEh1.FieldByName('RefFrameClass')).Value := TObject(TfrDataGrouping);
  MemTableEh1.Post;

  MemTableEh1.Next;
  MemTableEh1.Append;
  MemTableEh1MenuName.AsString := '  BackgroundImages';
  TRefObjectField(MemTableEh1.FieldByName('RefFrameClass')).Value := TObject(TfrBackgroundImages);
  MemTableEh1.Post;

  MemTableEh1.Next;
  MemTableEh1.Append;
  MemTableEh1MenuName.AsString := '  SearchPanel';
  TRefObjectField(MemTableEh1.FieldByName('RefFrameClass')).Value := TObject(TfrSearchPanel);
  MemTableEh1.Post;

  MemTableEh1.Next;
  MemTableEh1.Append;
  MemTableEh1MenuName.AsString := '  MailBox';
  TRefObjectField(MemTableEh1.FieldByName('RefFrameClass')).Value := TObject(TfrMailBox);
  MemTableEh1.Post;

  MemTableEh1.Next;
  MemTableEh1.Append;
  MemTableEh1MenuName.AsString := '  ImportExport';
  TRefObjectField(MemTableEh1.FieldByName('RefFrameClass')).Value := TObject(TfrImportExport);
  MemTableEh1.Post;

  MemTableEh1.Next;
  MemTableEh1.Append;
  MemTableEh1MenuName.AsString := 'MemTableEh';
  TRefObjectField(MemTableEh1.FieldByName('RefFrameClass')).Value := nil; //MemTableEh
  MemTableEh1.Post;

  MemTableEh1.Next;
  MemTableEh1.Append;
  MemTableEh1MenuName.AsString := '  MemTableSaveLoad';
  TRefObjectField(MemTableEh1.FieldByName('RefFrameClass')).Value := TObject(TfrMemTableSaveLoad); //SaveLoadData
  MemTableEh1.Post;

  MemTableEh1.Next;
  MemTableEh1.Append;
  MemTableEh1MenuName.AsString := '  LiveRestructure';
  TRefObjectField(MemTableEh1.FieldByName('RefFrameClass')).Value := TObject(TfrLiveRestructure);
  MemTableEh1.Post;

  MemTableEh1.Next;
  MemTableEh1.Append;
  MemTableEh1MenuName.AsString := 'WorkingWithHugeData';
  TRefObjectField(MemTableEh1.FieldByName('RefFrameClass')).Value := TObject(TfrWorkingWithHugeData);
  MemTableEh1.Post;

  MemTableEh1.Next;
  MemTableEh1.Append;
  MemTableEh1MenuName.AsString := 'PivotGrid';
  TRefObjectField(MemTableEh1.FieldByName('RefFrameClass')).Value := TObject(TfrPivotGrid);
  MemTableEh1.Post;

  MemTableEh1.Next;
  MemTableEh1.Append;
  MemTableEh1MenuName.AsString := 'Planner';
  TRefObjectField(MemTableEh1.FieldByName('RefFrameClass')).Value := TObject(TfrPlanner);
  MemTableEh1.Post;

  MemTableEh1.Next;
  MemTableEh1.Append;
  MemTableEh1MenuName.AsString := 'DataSet';
  TRefObjectField(MemTableEh1.FieldByName('RefFrameClass')).Value := nil; //DataSet
  MemTableEh1.Post;

  MemTableEh1.Next;
  MemTableEh1.Append;
  MemTableEh1MenuName.AsString := '  Export to Text File';
  TRefObjectField(MemTableEh1.FieldByName('RefFrameClass')).Value := TObject(TfrDataSetExporter);
  MemTableEh1.Post;

  MemTableEh1.Next;
  MemTableEh1.Append;
  MemTableEh1MenuName.AsString := '  Import from Text File';
  TRefObjectField(MemTableEh1.FieldByName('RefFrameClass')).Value := TObject(TfrDataSetImporter);
  MemTableEh1.Post;

  MemTableEh1.First;

  DataSource1.OnDataChange := DataSource1DataChange;
end;

procedure TForm1.ADOConnectionProviderEh1InlineConnectionBeforeConnect(Sender: TObject);
begin
end;

procedure TForm1.ADOConnectionProviderEh2InlineConnectionBeforeConnect(
  Sender: TObject);
begin
end;

procedure TForm1.ODBCConnection1BeforeConnect(Sender: TObject);
begin
end;

procedure TForm1.ODBCConnection2BeforeConnect(Sender: TObject);
begin
end;

function TForm1.SQLConnectionProviderEh1ExecuteCommand(
  SQLDataDriver: TCustomSQLDataDriverEh; Command: TCustomSQLCommandEh;
  var Cursor: TDataSet; var FreeOnEof: Boolean; var Processed: Boolean
  ): Integer;
begin
  Result := 0;
end;

function TForm1.SQLConnectionProviderEh2ExecuteCommand(
  SQLDataDriver: TCustomSQLDataDriverEh; Command: TCustomSQLCommandEh;
  var Cursor: TDataSet; var FreeOnEof: Boolean; var Processed: Boolean
  ): Integer;
begin
  SQLQuery2.Close;
  SQLQuery2.SQL.Text := Command.CommandText.Text;
  SQLQuery2.Open;

  Cursor := SQLQuery2;
  FreeOnEof := False;
  Processed := True;
  Result := 0;
end;

procedure TForm1.ToolButton12Click(Sender: TObject);
//var
//  dSum, dEvg: Double;
//  vSum, vEvg: Variant;
//  cnt: Integer;
//  s: String;
begin
  //cnt := 20;
  //dSum := 797.3599999999999;
  //vSum := VarFmtBCDCreate(VarToBCD(dSum));
  //vEvg := vSum / cnt;
  //s := VarToStr(vEvg);
  //dEvg := vEvg;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
//  if Frame = frMasterDetail then Exit;
//  if frMasterDetail = nil then
//    frMasterDetail := TfrMasterDetail.Create(Self);
  Frame.Parent := nil;
//  Frame := frMasterDetail;
  Frame.Parent := Panel1;

//  SpeedBPanel2.Color := CurPan.Color;
  CurPan.Color := clBtnFace;
//  CurPan := SpeedBPanel2;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  if Frame = frMainGrid then Exit;
  if frMainGrid = nil then
    frMainGrid := TfrMainGrid.Create(Self);
  Frame.Parent := nil;
  Frame := frMainGrid;
  Frame.Parent := Panel1;

//  SpeedBPanel1.Color := CurPan.Color;
  CurPan.Color := clBtnFace;
//  CurPan := SpeedBPanel1;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
begin
  ShowMessage('To Do');
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);
begin
  ShowMessage('To Do');
end;

procedure TForm1.SpeedButton5Click(Sender: TObject);
begin
  ShowMessage('To Do');
end;

procedure TForm1.SpeedButton6Click(Sender: TObject);
begin
  ShowMessage('To Do');
end;

procedure TForm1.SpeedButton7Click(Sender: TObject);
begin
  ShowMessage('To Do');
end;

procedure TForm1.SpeedButton8Click(Sender: TObject);
begin
  ShowMessage('To Do');
end;

(*
procedure TForm1.ToolBar1CustomDraw(Sender: TToolBar; const ARect: TRect;
  var DefaultDraw: Boolean);
begin
  GradientFillCanvas(Sender.Canvas, ApproachToColorEh(clWindow, $00D6D6D6, 30),
    ApproachToColorEh($00D6D6D6, clWindow, 30), ARect, gdVertical);
end;
*)

procedure TForm1.DBComboBoxEh1Change(Sender: TObject);
begin
{$IFDEF EH_LIB_16}
  TStyleManager.TrySetStyle(DBComboBoxEh1.Items[DBComboBoxEh1.ItemIndex]);
{$ENDIF}
end;

procedure TForm1.DBGridEh1GetCellParams(Sender: TObject; Column: TColumnEh;
  AFont: TFont; var Background: TColor; State: TGridDrawState);
begin
  if gdSelected in State then
    AFont.Style := [fsBold];
end;

procedure TForm1.DataSource1DataChange(Sender: TObject; Field: TField);
var
  AClass: TWinControlClass;
  AFrame: TWinControl;
  Rec: TMemoryRecordEh;
  OldFrame :TFrame;
begin
  if MemTableEh1RefFrame.Value = nil then
  begin
    if MemTableEh1RefFrameClass.Value = nil then
    begin
      MemTableEh1.InstantReadEnter(MemTableEh1.InstantReadCurRow+1);
      Rec := MemTableEh1.Rec;
      if MemTableEh1RefFrame.Value = nil then
      begin
        AClass := TWinControlClass(MemTableEh1RefFrameClass.Value);
        AFrame := AClass.Create(Self);
      end else
        AFrame := TWinControl(MemTableEh1RefFrame.Value);
      MemTableEh1.InstantReadLeave;
      DataSource1.OnDataChange := nil;
      MemTableEh1.Edit;
      MemTableEh1RefFrame.Value := AFrame;
      MemTableEh1.Post;
      DataSource1.OnDataChange := DataSource1DataChange;
      if VarIsNull(Rec.DataValues['RefFrame', dvvValueEh]) then
        Rec.DataValues['RefFrame', dvvValueEh] := RefObjectToVariant(MemTableEh1RefFrame.Value);

//      ShowMessage('To Do')
    end else
    begin
      DataSource1.OnDataChange := nil;
      MemTableEh1.Edit;
      try
         MemTableEh1RefFrame.Value := TWinControlClass(MemTableEh1RefFrameClass.Value).Create(Self);

      finally
        MemTableEh1.Post;
        DataSource1.OnDataChange := DataSource1DataChange;
      end;
    end;
  end;

  OldFrame := Frame;
//  Frame.Parent := nil;
  if (MemTableEh1RefFrame.Value <> nil) and (MemTableEh1RefFrame.Value <> Frame) then
  begin
    Frame := TFrame(MemTableEh1RefFrame.Value);
    Frame.Parent := Panel1;
//???    if Frame is TfrEditControls then TfrEditControls(Frame).UpdateLabels;
    OldFrame.Parent := nil;
    Frame.Align := alClient;
    Panel1.Update;
  end;
end;

procedure TForm1.About1Click(Sender: TObject);
begin
  ShowAboutForm;
end;

procedure TForm1.ToolButton2Click(Sender: TObject);
begin
  Height := Height - 1;
end;

procedure TForm1.ToolButton3Click(Sender: TObject);
begin
  Height := Height + 1;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  FillFrameTopPanel(PaintBox1.Canvas, Rect(0, 0, PaintBox1.Width, PaintBox1.Height));
end;

procedure TForm1.actPreviewExecute(Sender: TObject);
begin
  if ActiveControl is TCustomDBGridEh then
  begin
//???    PrintDBGridEh1.DBGridEh := TDBGridEh(ActiveControl);
//???    PrintDBGridEh1.Preview;
  end;
end;

procedure TForm1.MemTableEh1AfterScroll(DataSet: TDataSet);
begin
//  if @DataSource1.OnDataChange <> nil then
//    if MemTableEh1.FieldByName('MenuName').AsString = 'MemTableEh' then
//      MemTableEh1.Next;
end;

procedure TForm1.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TForm1.actAboutExecute(Sender: TObject);
begin
  ShowAboutForm;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F11 then
    actShowObjInspectorExecute(Sender);
end;

procedure TForm1.actShowObjInspectorExecute(Sender: TObject);
var
  FormBounds: TRect;
begin
  FormBounds := Rect(Left+Width+10, Top, Left+Width+10+300, Top+Height);
  if FormBounds.Right > Screen.DesktopRect.Right then
    OffsetRect(FormBounds, Screen.DesktopRect.Right - FormBounds.Right, 0);
  ShowObjectInspectorForm(ActiveControl, FormBounds);
end;

procedure TForm1.InitDataBase;
begin
  InitConnectionProvider1;
  InitConnectionProvider2;
end;

procedure TForm1.InitConnectionProvider1;
var
  FilePath: String;
  ds :String;
  appPath: String;
  findPath: String;
begin
  ds := DirectorySeparator;
  begin
    appPath := ExtractFilePath(ParamStr(0));
    findPath := appPath+'table_Vendors.dfm';
    if FileExists(findPath) then
      FilePath := findPath
    else
    begin
      findPath := appPath+'..'+ds+'DfmData'+ds+'table_Vendors.dfm';
      if FileExists(findPath) then
        FilePath := findPath
      else
      begin
        findPath := appPath+'..'+ds+'..'+ds+'..'+ds+'DfmData'+ds+'table_Vendors.dfm';
        if FileExists(findPath) then
          FilePath := findPath;
      end;
    end;

    if FilePath <> '' then
    begin
      DfmDataTablesPath := ExtractFilePath(FilePath);
      UseDfmDataTablesForConnectionProvider := True;
    end else
      raise Exception.Create('Can''t find data Files');
  end;
end;

procedure TForm1.InitConnectionProvider2;
begin
end;

function TForm1.SQLDBConnectionProviderEh1ExecuteCommand(
  SQLDataDriver: TCustomSQLDataDriverEh; Command: TCustomSQLCommandEh;
  var Cursor: TDataSet; var FreeOnEof: Boolean; var Processed: Boolean
  ): Integer;
var
  TableName: String;
  mteReaded: TMemTableEh;
begin
  Result := 0;
  if UseDfmDataTablesForConnectionProvider and (SQLDataDriver.SelectCommand = Command) then
  begin
    TableName := SQLDataDriver.Name;
    TableName := IntToStr(SQLDataDriver.SpecParams.Count);
    TableName := SQLDataDriver.SpecParams.Values['table'];
    if TableName <> '' then
    begin
      mteReaded := TMemTableEh.Create(nil);
      mteReaded.LoadFromFile(DfmDataTablesPath + TableName + '.dfm');
      mteReaded.Open;
      Cursor := mteReaded;
      FreeOnEof := True;
      Processed := True;
    end;
  end
end;

initialization
  DBGridEhDefaultStyle.IsDrawFocusRect := False;
  DefFontData.Name := 'Tahoma';
//  DefFontData.Name := 'Tahoma';
//  DBGridEhDebugDraw := True;
//  DefFontData.Height := 20;
end.
