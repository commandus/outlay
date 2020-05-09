unit FrameMainGrid;

{$MODE Delphi}

//{$I EhLib.Inc}

interface

uses
  LCLIntf, LCLType, LMessages,
  Messages, SysUtils, Classes, Graphics, Controls, Forms,
{$IFDEF EH_LIB_6} Variants, {$ENDIF}
  Themes, FmtBcd, Variants,
  Dialogs, DBGridEhGrouping, ToolCtrlsEh, DBGridEhToolCtrls, ExtCtrls,
  GridsEh, DBGridsEh, MemTableDataEh, Db, sqldb, DataDriverEh,
  MemTableEh, StdCtrls, DBAxisGridsEh;

type

  { TfrMainGrid }

  TfrMainGrid = class(TFrame)
    Button1: TButton;
    DBGridEh1: TDBGridEh;
    Panel1: TPanel;
    DataSource1: TDataSource;
    mtQuery1: TMemTableEh;
    mtQuery1VNo: TFloatField;
    mtQuery1VName: TStringField;
    mtQuery1PNo: TFloatField;
    mtQuery1PDescription: TStringField;
    mtQuery1PCost: TCurrencyField;
    mtQuery1IQty: TIntegerField;
    mtQuery1VName1: TStringField;
    mtQuery1VPreferred: TBooleanField;
    ddrData1: TSQLDataDriverEh;
    qrVendors: TMemTableEh;
    ddrVendors: TSQLDataDriverEh;
    PaintBox1: TPaintBox;
    FrameTitleLabel: TLabel;
    Image1: TImage;
    RowDetailData: TRowDetailPanelControlEh;
    procedure Button1Click(Sender: TObject);
    procedure DBGridEh1ActiveGroupingStructChanged(Sender: TCustomDBGridEh);
    procedure ddrVendorsAssignCommandParam(DataDriver: TBaseSQLDataDriverEh;
      Command: TBaseSQLCommandEh; MemRecord: TMemoryRecordEh;
      DataValueVersion: TDataValueVersionEh; Param: TParam);
    procedure PaintBox1Paint(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure ReadState(Reader: TReader); override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    { Public declarations }
  end;

implementation

uses Unit1;

{$R *.lfm}

{ TfrMainGrid }

constructor TfrMainGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Panel1.Height := 36;
  DBGridEh1.FooterParams.VertLineColor := DBGridEh1.GridLineParams.DarkColor;
  DBGridEh1.FooterParams.HorzLineColor := DBGridEh1.GridLineParams.DarkColor;
  DBGridEh1.FooterParams.Color := ApproximateColor(DBGridEh1.FixedColor, clWindow, 128);
    DBGridEh1.TitleParams.SecondColor := ApproximateColor(clBtnFace, clWindow, 170);

  DBGridEh1.FooterParams.Font.Style := [fsBold];
  Panel1.DoubleBuffered := True;

  qrVendors.Open;
  mtQuery1.Open;

  Button1.Visible := False;
end;

procedure TfrMainGrid.CreateWnd;
begin
  inherited CreateWnd;
  if (DBGridEh1.HorzScrollBar.ActualSize < 20) then
    DBGridEh1.HorzScrollBar.Size := 20;
end;

procedure TfrMainGrid.PaintBox1Paint(Sender: TObject);
begin
  Form1.FillFrameTopPanel(PaintBox1.Canvas, Rect(0, 0, PaintBox1.Width, PaintBox1.Height));
end;

procedure TfrMainGrid.ddrVendorsAssignCommandParam(
  DataDriver: TBaseSQLDataDriverEh; Command: TBaseSQLCommandEh;
  MemRecord: TMemoryRecordEh; DataValueVersion: TDataValueVersionEh;
  Param: TParam);
begin

end;

procedure TfrMainGrid.Button1Click(Sender: TObject);
//var
//  bcdVar: Variant;
//  s: String;
begin
//  VarFmtBCDCreate(bcdVar, VarToBCD(224518.0639999999994919));
//  bcdVar := bcdVar / 6615;
//  s := VarToStr(bcdVar);
////  Button1.SetBounds(Button1.Left+1, Button1.Top, Button1.Width+1, Button1.Height);
end;

procedure TfrMainGrid.DBGridEh1ActiveGroupingStructChanged(
  Sender: TCustomDBGridEh);
begin

end;

procedure TfrMainGrid.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
//  ScalingFlags := [sfLeft, sfTop, sfWidth, sfHeight];
//  if Form1.Canvas.TextHeight('0') <> 13 then
//    ScaleBy(Form1.Canvas.TextHeight('0'), 13);
end;

end.
