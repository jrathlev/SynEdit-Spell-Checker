program SpellCheckDemo;

uses
  Vcl.Forms,
  SpellDemoMain in 'SpellDemoMain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
