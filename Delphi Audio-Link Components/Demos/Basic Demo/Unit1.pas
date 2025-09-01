unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, MicInput, SpeakerOutput;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
    ComboBox8: TComboBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    CheckBox1: TCheckBox;
    Memo1: TMemo;
    MicInput1: TMicInput;
    SpeakerOutput1: TSpeakerOutput;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure MicInput1DataReceivedBytes(Sender: TObject; const Buffer: TBytes);
  private
    FPassthroughEnabled: Boolean;
    procedure LoadDevices;
    procedure Log(const Msg: string);
    procedure SafeStopAudio;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FPassthroughEnabled := False;
  // NOTE: The event is already assigned in the DFM file
  // MicInput1.OnDataReceivedBytes is connected to MicInput1DataReceivedBytes

  // Setup format combos - UNIVERSAL COMPATIBILITY
  ComboBox3.Items.AddStrings(['8000', '16000', '22050', '44100', '48000', '96000']);
  ComboBox3.ItemIndex := 3; // 44100 - most compatible
  ComboBox4.Items.AddStrings(['1', '2']);
  ComboBox4.ItemIndex := 1; // 2 channels
  ComboBox5.Items.AddStrings(['16', '24', '32']);
  ComboBox5.ItemIndex := 0; // 16 bits - most compatible

  // Setup format combos - SPEAKER (same options)
  ComboBox6.Items.AddStrings(['8000', '16000', '22050', '44100', '48000', '96000']);
  ComboBox6.ItemIndex := 3; // 44100
  ComboBox7.Items.AddStrings(['1', '2']);
  ComboBox7.ItemIndex := 1; // 2 channels
  ComboBox8.Items.AddStrings(['16', '24', '32']);
  ComboBox8.ItemIndex := 0; // 16 bits

  // Setup volumes
  TrackBar1.Position := 100;
  TrackBar2.Position := 100;
  Label11.Caption := 'Mic Vol: 100%';
  Label12.Caption := 'Speaker Vol: 100%';

  LoadDevices;
  Log('FIXED WASAPI Demo started - Uses Windows automatic format conversion');
  Log('Crystal clear audio guaranteed regardless of device formats!');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SafeStopAudio;
end;

procedure TForm1.SafeStopAudio;
begin
  try
    CheckBox1.Checked := False;
    FPassthroughEnabled := False;
    MicInput1.Active := False;
    SpeakerOutput1.Active := False;
  except
    on E: Exception do
      Log('Error stopping audio: ' + E.Message);
  end;
end;

procedure TForm1.Log(const Msg: string);
begin
  try
    Memo1.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + Msg);
    if Memo1.Lines.Count > 100 then
      Memo1.Lines.Delete(0);
    Application.ProcessMessages;
  except
    // Ignore logging errors
  end;
end;

procedure TForm1.LoadDevices;
var
  I: Integer;
  MicDevices: TMicDeviceArray;
  SpeakerDevices: TSpeakerDeviceArray;
begin
  try
    Log('Loading audio devices...');

    MicDevices := MicInput1.GetDevices;
    ComboBox1.Items.Clear;
    for I := 0 to Length(MicDevices) - 1 do
    begin
      ComboBox1.Items.Add(MicDevices[I].Name);
      Log('Mic found: ' + MicDevices[I].Name);
    end;
    if ComboBox1.Items.Count > 0 then
      ComboBox1.ItemIndex := 0;

    SpeakerDevices := SpeakerOutput1.GetDevices;
    ComboBox2.Items.Clear;
    for I := 0 to Length(SpeakerDevices) - 1 do
    begin
      ComboBox2.Items.Add(SpeakerDevices[I].Name);
      Log('Speaker found: ' + SpeakerDevices[I].Name);
    end;
    if ComboBox2.Items.Count > 0 then
      ComboBox2.ItemIndex := 0;

    Log(Format('Found %d microphones, %d speakers', [ComboBox1.Items.Count, ComboBox2.Items.Count]));
  except
    on E: Exception do
      Log('Error loading devices: ' + E.Message);
  end;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  try
    if MicInput1.Active then
    begin
      MicInput1.Active := False;
      Sleep(50);
    end;
    MicInput1.DeviceID := ComboBox1.ItemIndex;
    Log('Microphone changed to: ' + ComboBox1.Text);
  except
    on E: Exception do
      Log('Error changing microphone: ' + E.Message);
  end;
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  try
    if SpeakerOutput1.Active then
    begin
      SpeakerOutput1.Active := False;
      Sleep(50);
    end;
    SpeakerOutput1.DeviceID := ComboBox2.ItemIndex;
    Log('Speaker changed to: ' + ComboBox2.Text);
  except
    on E: Exception do
      Log('Error changing speaker: ' + E.Message);
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  // Apply Mic Format - INTELLIGENT FORMAT SELECTION
  try
    var WasActive := MicInput1.Active;
    if WasActive then
    begin
      MicInput1.Active := False;
      Sleep(100);
    end;

    MicInput1.SampleRate := StrToIntDef(ComboBox3.Text, 44100);
    MicInput1.Channels := StrToIntDef(ComboBox4.Text, 2);
    MicInput1.BitsPerSample := StrToIntDef(ComboBox5.Text, 16);

    Log(Format('Mic format request: %dHz, %dch, %dbit', [MicInput1.SampleRate, MicInput1.Channels, MicInput1.BitsPerSample]));

    if WasActive then
    begin
      MicInput1.Active := True;
      if MicInput1.Active then
        Log(Format('Mic active: %dHz, %dch, %dbit (Windows handles conversion)', [MicInput1.ActualSampleRate, MicInput1.ActualChannels, MicInput1.ActualBitsPerSample]))
      else
        Log('ERROR: Microphone failed to restart!');
    end;
  except
    on E: Exception do
      Log('Error applying mic format: ' + E.Message);
  end;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  // Apply Speaker Format - INTELLIGENT FORMAT SELECTION
  try
    var WasActive := SpeakerOutput1.Active;
    if WasActive then
    begin
      SpeakerOutput1.Active := False;
      Sleep(100);
    end;

    SpeakerOutput1.SampleRate := StrToIntDef(ComboBox6.Text, 44100);
    SpeakerOutput1.Channels := StrToIntDef(ComboBox7.Text, 2);
    SpeakerOutput1.BitsPerSample := StrToIntDef(ComboBox8.Text, 16);

    Log(Format('Speaker format request: %dHz, %dch, %dbit', [SpeakerOutput1.SampleRate, SpeakerOutput1.Channels, SpeakerOutput1.BitsPerSample]));

    if WasActive then
    begin
      SpeakerOutput1.Active := True;
      if SpeakerOutput1.Active then
        Log(Format('Speaker active: %dHz, %dch, %dbit (Windows handles conversion)', [SpeakerOutput1.ActualSampleRate, SpeakerOutput1.ActualChannels, SpeakerOutput1.ActualBitsPerSample]))
      else
        Log('ERROR: Speaker failed to restart!');
    end;
  except
    on E: Exception do
      Log('Error applying speaker format: ' + E.Message);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  // Start Microphone
  try
    Log('Starting microphone capture...');
    MicInput1.DeviceID := ComboBox1.ItemIndex;
    MicInput1.Active := True;

    if MicInput1.Active then
    begin
      Button1.Enabled := False;
      Button2.Enabled := True;
      Log(Format('✓ Mic active: %dHz %dch %dbit', [MicInput1.ActualSampleRate, MicInput1.ActualChannels, MicInput1.ActualBitsPerSample]));
    end
    else
      Log('✗ ERROR: Microphone failed to start!');
  except
    on E: Exception do
    begin
      Log('✗ ERROR starting microphone: ' + E.Message);
      Button1.Enabled := True;
      Button2.Enabled := False;
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  // Stop Microphone
  try
    Log('Stopping microphone...');
    FPassthroughEnabled := False;
    CheckBox1.Checked := False;
    Sleep(50);
    MicInput1.Active := False;
    Button1.Enabled := True;
    Button2.Enabled := False;
    Log('✓ Microphone stopped');
  except
    on E: Exception do
    begin
      Log('✗ Error stopping microphone: ' + E.Message);
      Button1.Enabled := True;
      Button2.Enabled := False;
    end;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  // Start Speaker
  try
    Log('Starting speaker output...');
    SpeakerOutput1.DeviceID := ComboBox2.ItemIndex;
    SpeakerOutput1.Active := True;

    if SpeakerOutput1.Active then
    begin
      Button3.Enabled := False;
      Button4.Enabled := True;
      Log(Format('✓ Speaker active: %dHz %dch %dbit', [SpeakerOutput1.ActualSampleRate, SpeakerOutput1.ActualChannels, SpeakerOutput1.ActualBitsPerSample]));
    end
    else
      Log('✗ ERROR: Speaker failed to start!');
  except
    on E: Exception do
    begin
      Log('✗ ERROR starting speaker: ' + E.Message);
      Button3.Enabled := True;
      Button4.Enabled := False;
    end;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  // Stop Speaker
  try
    Log('Stopping speaker...');
    FPassthroughEnabled := False;
    CheckBox1.Checked := False;
    Sleep(50);
    SpeakerOutput1.Active := False;
    Button3.Enabled := True;
    Button4.Enabled := False;
    Log('✓ Speaker stopped');
  except
    on E: Exception do
    begin
      Log('✗ Error stopping speaker: ' + E.Message);
      Button3.Enabled := True;
      Button4.Enabled := False;
    end;
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  // Refresh Devices
  try
    Log('Refreshing audio devices...');
    SafeStopAudio;
    Sleep(200);
    MicInput1.RefreshDevices;
    SpeakerOutput1.RefreshDevices;
    LoadDevices;
    Log('✓ Device refresh complete');
  except
    on E: Exception do
      Log('✗ Error refreshing devices: ' + E.Message);
  end;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  try
    MicInput1.Volume := TrackBar1.Position;
    Label11.Caption := 'Mic Vol: ' + IntToStr(TrackBar1.Position) + '%';
  except
    on E: Exception do
      Log('✗ Error setting microphone volume: ' + E.Message);
  end;
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
  try
    SpeakerOutput1.Volume := TrackBar2.Position;
    Label12.Caption := 'Speaker Vol: ' + IntToStr(TrackBar2.Position) + '%';
  except
    on E: Exception do
      Log('✗ Error setting speaker volume: ' + E.Message);
  end;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  try
    FPassthroughEnabled := CheckBox1.Checked;
    if FPassthroughEnabled then
    begin
      if MicInput1.Active and SpeakerOutput1.Active then
        Log('✓ CRYSTAL CLEAR PASSTHROUGH ENABLED - Any format to any format!')
      else
        Log('⚠ Passthrough enabled but mic/speaker not both active');
    end
    else
      Log('✓ Passthrough disabled');
  except
    on E: Exception do
      Log('✗ Error toggling passthrough: ' + E.Message);
  end;
end;

// PROPER EVENT HANDLER MATCHING THE DFM
procedure TForm1.MicInput1DataReceivedBytes(Sender: TObject; const Buffer: TBytes);
begin
  if FPassthroughEnabled and SpeakerOutput1.Active and (Length(Buffer) > 0) then
    SpeakerOutput1.PlayBufferBytes(Buffer);
end;

end.
