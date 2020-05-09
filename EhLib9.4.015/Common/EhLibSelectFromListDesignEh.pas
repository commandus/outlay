{*******************************************************}
{                                                       }
{                       EhLib 9.4                       }
{            TSelectFromListDesignForm form             }
{                      Build 9.4.001                    }
{                                                       }
{    Copyright (c) 2013-2019 by Dmitry V. Bolshakov     }
{                                                       }
{*******************************************************}

unit EhLibSelectFromListDesignEh;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls;

type
  TSelectFromListDesignForm = class(TForm)
    lDescription: TLabel;
    ListBox1: TListBox;
    bOk: TButton;
    bCancel: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SelectFromListDesignForm: TSelectFromListDesignForm;

function SelectFromList(sl: TStringList; var SelectedIndex: Integer; Description: String): Boolean;

implementation

{$R *.dfm}

function SelectFromList(sl: TStringList; var SelectedIndex: Integer;
  Description: String): Boolean;
var
  Form: TSelectFromListDesignForm;
begin
  Form := TSelectFromListDesignForm.Create(Application);
  Result := False;
  Form.ListBox1.Items := sl;
  Form.lDescription.Caption := Description;
  if Form.ShowModal = mrOk then
  begin
    Result := True;
    SelectedIndex := Form.ListBox1.ItemIndex;
  end;
  Form.Free;
end;

end.
