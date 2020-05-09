{*******************************************************}
{                                                       }
{                     EhLib 9.4                         }
{                                                       }
{             TMemTableDataForm component               }
{                    Build 9.4.03                       }
{                                                       }
{      Copyright (c) 2003-2019 by EhLib Team and        }
{                Dmitry V. Bolshakov                    }
{                                                       }
{*******************************************************}

unit MemTableEditEh;

{$I EhLib.Inc}

interface

uses SysUtils, Windows, Messages, Classes, Graphics, Controls,
{$IFDEF CIL}
  Borland.Vcl.Design.DesignIntf,
  Borland.Vcl.Design.DsnDBCst,
{$ELSE}
  DsnDBCst,
  DesignIntf,
{$ENDIF}
  Forms, Dialogs, StdCtrls, Buttons, DB, MemTableEh;

type

  TMemTableDataForm = class(TForm)
    GroupBox1: TGroupBox;
    DataSetList: TListBox;
    OkBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure DataSetListDblClick(Sender: TObject);
    procedure DataSetListKeyPress(Sender: TObject; var Key: Char);
  private
    FDataSet: TCustomMemTableEh;
    FDesigner: IDesigner;
    procedure CheckComponent(const Value: string);
    function Edit: Boolean;
  end;

function EditMemTable(ADataSet: TCustomMemTableEh; ADesigner: IDesigner): Boolean;

implementation

uses TypInfo, DBConsts, Consts;

{$R *.dfm}

function EditMemTable(ADataSet: TCustomMemTableEh; ADesigner: IDesigner): Boolean;
var
  f: TMemTableDataForm;
begin
  f := TMemTableDataForm.Create(Application);
  try
    f.Caption := Format(SClientDataSetEditor, [ADataSet.Owner.Name, DotSep, ADataSet.Name]);
    f.FDataSet := ADataSet;
    f.FDesigner := ADesigner;

    Result := f.Edit;
  finally
    f.Free;
  end;
end;

procedure TMemTableDataForm.CheckComponent(const Value: string);
var
  DataSet: TDataSet;
begin
  DataSet := TDataSet(FDesigner.GetComponent(Value));
  if (DataSet.Owner <> FDataSet.Owner) then
    DataSetList.Items.Add(Concat(DataSet.Owner.Name, '.', DataSet.Name))
  else
    if AnsiCompareText(DataSet.Name, FDataSet.Name) <> 0 then
      DataSetList.Items.Add(DataSet.Name);
end;

function TMemTableDataForm.Edit: Boolean;
begin
  DataSetList.Clear;
  FDesigner.GetComponentNames(GetTypeData(TDataSet.ClassInfo), CheckComponent);
  if DataSetList.Items.Count > 0 then
  begin
    DataSetList.Enabled := True;
    DataSetList.ItemIndex := 0;
    OkBtn.Enabled := True;
    ActiveControl := DataSetList;
  end else
    ActiveControl := CancelBtn;
  Result := ShowModal = mrOK;
end;

procedure TMemTableDataForm.OkBtnClick(Sender: TObject);
var
  DataSet: TDataSet;
begin
  try
    if DataSetList.ItemIndex >= 0 then
    begin
      Screen.Cursor := crHourGlass;
      try
        DataSet := FDesigner.GetComponent(
          DataSetList.Items[DataSetList.ItemIndex]
          ) as TDataSet;
        FDataSet.LoadFromDataSet(DataSet, -1, lmCopy, False);
      finally
        Screen.Cursor := crDefault;
      end;
    end
    else
      ;
  except
    ModalResult := mrNone;
    raise;
  end;
end;

procedure TMemTableDataForm.FormCreate(Sender: TObject);
var
  NonClientMetrics: TNonClientMetrics;
begin
  if ParentFont then
  begin
    NonClientMetrics.cbSize := sizeof(NonClientMetrics);
{$IFDEF CIL}
{$ELSE}
    if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
      Font.Name := NonClientMetrics.lfMessageFont.lfFaceName;
{$ENDIF}
  end;
end;

procedure TMemTableDataForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TMemTableDataForm.DataSetListDblClick(Sender: TObject);
begin
  if OkBtn.Enabled then OkBtn.Click;
end;

procedure TMemTableDataForm.DataSetListKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) and OkBtn.Enabled then OkBtn.Click;
end;

end.
