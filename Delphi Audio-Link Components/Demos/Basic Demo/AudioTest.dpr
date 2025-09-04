program AudioTest;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  MicInput in 'MicInput.pas',
  SpeakerOutput in 'SpeakerOutput.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Carbon');
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.