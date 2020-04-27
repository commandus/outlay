unit formoutlay;

interface
uses
  Vcl.Controls,
  Vcl.Forms;
type
 TFormOutlay = class(TForm)
    procedure CreateParams(var Params: TCreateParams); override;
  end;

implementation

procedure TFormOutlay.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.WndParent := 0;
end;

end.
