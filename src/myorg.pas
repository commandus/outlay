unit myorg;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Grids, Vcl.DBGrids,
  dm, Vcl.ExtCtrls, Vcl.DBCtrls, Vcl.Menus, GridsEh, DBAxisGridsEh,
  DBGridEh,
  formoutlay, DBGridEhGrouping, ToolCtrlsEh,
  DBGridEhToolCtrls, DynVarsEh, EhLibVCL, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, Vcl.StdCtrls, IBX.IBCustomDataSet,
  IBX.IBQuery, Vcl.Mask, DBCtrlsEh, DBLookupEh;

type
  TFormMyOrg = class(TFormOutlay)
    DBCBEhMyOrg: TDBLookupComboboxEh;
    DBGridOrgList: TDBGridEh;
    dsMyOrg: TDataSource;
    BSetOwner: TButton;
    BSave: TButton;
    Label1: TLabel;
    Label2: TLabel;
    IBOrg: TIBDataSet;
    IBOrgORGNAME: TIBStringField;
    dsOrg: TDataSource;
    IBMyOrg: TIBDataSet;
    IBStringField2: TIBStringField;
    IBOrgOWNER: TIBStringField;
    procedure BSetOwnerClick(Sender: TObject);
    procedure BSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure SetOrgOwner(const g: TDBGridEh; const orgName: String);
    procedure SelectMyOrg();
  public
  end;

var
  FormMyOrg: TFormMyOrg;

implementation

{$R *.dfm}

procedure TFormMyOrg.BSaveClick(Sender: TObject);
begin
  Close();
end;

procedure TFormMyOrg.SetOrgOwner(const g: TDBGridEh; const orgName: String);
var
  i: Integer;
begin
  if g.SelectedRows.Count > 0 then begin
    try
      g.DataSource.DataSet.DisableControls;
      for i:= 0 to g.SelectedRows.Count - 1 do begin
        g.DataSource.DataSet.GotoBookmark(g.SelectedRows.Items[i]);
        g.DataSource.DataSet.Edit;
        g.DataSource.DataSet.Fields[1].AsString:= orgName;
        g.DataSource.DataSet.Post;
      end;
    finally
      g.DataSource.DataSet.EnableControls;
    end;
  end else begin
    g.DataSource.DataSet.Edit;
    g.DataSource.DataSet.Fields[1].AsString:= orgName;
    g.DataSource.DataSet.Post;
  end;
end;

procedure TFormMyOrg.SelectMyOrg();
begin
  if dm.dmOutlay.myorgname.Length > 0 then
  begin
    IBMyOrg.DisableControls;
    IBMyOrg.First;
    while not IBMyOrg.Eof do
    begin
      if IBMyOrg.FieldByName('ORGNAME').AsString = dm.dmOutlay.myorgname then
        Break;
      IBMyOrg.Next;
    end;
    IBMyOrg.EnableControls;
  end;
end;

procedure TFormMyOrg.BSetOwnerClick(Sender: TObject);
begin
  SetOrgOwner(DBGridOrgList, DBCBEhMyOrg.Text);
end;

procedure TFormMyOrg.FormActivate(Sender: TObject);
begin
  SelectMyOrg();
end;

procedure TFormMyOrg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  dm.dmOutlay.myorgname:= IBMyOrg.FieldByName('ORGNAME').AsString;
  dm.dmOutlay.saveSettings();
  IBMyOrg.Close;
  IBOrg.Close;
end;

procedure TFormMyOrg.FormCreate(Sender: TObject);
begin
  dm.dmOutlay.ActivateDbControls(Self);
end;

end.
