program outlay;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  main in 'main.pas' {FormMain},
  rpt in 'rpt.pas' {FormReports},
  dm in 'dm.pas' {dmOutlay: TDataModule},
  formoutlay in 'formoutlay.pas',
  settings in 'settings.pas' {FormSettings},
  myorg in 'myorg.pas' {FormMyOrg},
  dict in 'dict.pas' {FormDict};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar:= True;
  Application.Title:= 'Спецификации';

  dm.TdmOutlay.loadLastStyle();

  Application.CreateForm(TdmOutlay, dmOutlay);
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormDict, FormDict);
  Application.Run;
end.
