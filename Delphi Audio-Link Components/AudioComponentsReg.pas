unit AudioComponentsReg;

interface

uses
  Classes, MicInput, SpeakerOutput;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Audio', [TMicInput, TSpeakerOutput]);
end;

end.
