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
    DBComboboxEh1: TDBComboBoxEh;
    DBGridProjectList: TDBGridEh;
    IBQMyOrg: TIBQuery;
    dsMyOrg: TDataSource;
    IBQueryOrg: TIBQuery;
    BSetOwner: TButton;
    BSave: TButton;
    Label1: TLabel;
    Label2: TLabel;
    IBQueryOrgORGNAME: TIBStringField;
    IBQueryOrgOWNER: TIBStringField;
    IBQMyOrgORGNAME: TIBStringField;
    IBQueryLookup: TIBQuery;
    IBStringField1: TIBStringField;
    procedure BSetOwnerClick(Sender: TObject);
    procedure BSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetOrgOwner(const g: TDBGridEh; const orgName: String);
  public
    { Public declarations }
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
        g.DataSource.DataSet.Fields[1].AsString:= orgName;
      end;
    finally
      g.DataSource.DataSet.EnableControls;
    end;
  end;
end;

procedure TFormMyOrg.BSetOwnerClick(Sender: TObject);
var
  o: String;
begin
  o:= DBComboboxEh1.Text;
  SetOrgOwner(DBGridProjectList, o);
end;

procedure TFormMyOrg.FormCreate(Sender: TObject);
var
  c: Integer;
begin
  try
    for c:= 0 to ComponentCount - 1 do begin
      if (Components[c] is TIBQuery) then
        TIBQuery(Components[c]).Active:= true;
      if (Components[c] is TIBDataset) then
        TIBDataset(Components[c]).Active:= true;
    end;
  finally
  end;
end;

end.
