unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SpeakerOutput, MicInput,
  Vcl.StdCtrls, ncSources, ncUDPSockets, ncIPUtils, Vcl.ExtCtrls, Vcl.Buttons, System.UITypes,
  Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    ServerSocket: TncServerSource;
    ClientSocket: TncClientSource;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    MicInput1: TMicInput;
    SpeakerOutput1: TSpeakerOutput;
    UDPServerSocket: TncUDPServer;
    UDPClientSocket: TncUDPClient;
    lblServerStatus: TLabel;
    edtServerPort: TEdit;
    btnStartTCPServer: TButton;
    btnStopTCPServer: TButton;
    btnStartUDPServer: TButton;
    btnStopUDPServer: TButton;
    lblClientStatus: TLabel;
    edtClientIP: TEdit;
    edtClientPort: TEdit;
    btnConnectTCP: TButton;
    btnDisconnectTCP: TButton;
    btnConnectUDP: TButton;
    btnDisconnectUDP: TButton;
    rgProtocol: TRadioGroup;
    btnStartMic: TButton;
    btnStopMic: TButton;
    lblMicStatus: TLabel;
    lblSpeakerStatus: TLabel;
    lblConnectionInfo: TLabel;
    memLog: TMemo;
    lblServerPort: TLabel;
    lblClientIP: TLabel;
    lblClientPort: TLabel;
    btnClearLog: TButton;
    pnlServerControls: TPanel;
    pnlClientControls: TPanel;
    btnStartSpeaker: TButton;
    btnStopSpeaker: TButton;
    // Client/Microphone Controls
    lblMicDevice: TLabel;
    cmbMicDevice: TComboBox;
    lblMicSampleRate: TLabel;
    cmbMicSampleRate: TComboBox;
    lblMicChannels: TLabel;
    cmbMicChannels: TComboBox;
    lblMicBits: TLabel;
    cmbMicBits: TComboBox;
    lblMicVolume: TLabel;
    trkMicVolume: TTrackBar;
    lblMicVolumeValue: TLabel;
    btnRefreshMicDevices: TButton;
    btnApplyMicFormat: TButton;
    // Server/Speaker Controls
    lblSpeakerDevice: TLabel;
    cmbSpeakerDevice: TComboBox;
    lblSpeakerSampleRate: TLabel;
    cmbSpeakerSampleRate: TComboBox;
    lblSpeakerChannels: TLabel;
    cmbSpeakerChannels: TComboBox;
    lblSpeakerBits: TLabel;
    cmbSpeakerBits: TComboBox;
    lblSpeakerVolume: TLabel;
    trkSpeakerVolume: TTrackBar;
    lblSpeakerVolumeValue: TLabel;
    btnRefreshSpeakerDevices: TButton;
    btnApplySpeakerFormat: TButton;
    procedure ServerSocketConnected(Sender: TObject; aLine: TncLine);
    function ServerSocketHandleCommand(Sender: TObject; aLine: TncLine;
      aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean;
      const aSenderComponent, aReceiverComponent: string): TBytes;
    procedure MicInput1DataReceivedBytes(Sender: TObject;
      const Buffer: TBytes);
    procedure ClientSocketConnected(Sender: TObject; aLine: TncLine);
    function ClientSocketHandleCommand(Sender: TObject; aLine: TncLine;
      aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean;
      const aSenderComponent, aReceiverComponent: string): TBytes;
    procedure UDPClientSocketReadDatagram(Sender: TObject; aLine: TncLine;
      const aBuf: TBytes; aBufCount: Integer;
      const SenderAddr: TSockAddrStorage);
    procedure UDPServerSocketReadDatagram(Sender: TObject; aLine: TncLine;
      const aBuf: TBytes; aBufCount: Integer;
      const SenderAddr: TSockAddrStorage);
    procedure btnStartTCPServerClick(Sender: TObject);
    procedure btnStopTCPServerClick(Sender: TObject);
    procedure btnStartUDPServerClick(Sender: TObject);
    procedure btnStopUDPServerClick(Sender: TObject);
    procedure btnConnectTCPClick(Sender: TObject);
    procedure btnDisconnectTCPClick(Sender: TObject);
    procedure btnConnectUDPClick(Sender: TObject);
    procedure btnDisconnectUDPClick(Sender: TObject);
    procedure btnStartMicClick(Sender: TObject);
    procedure btnStopMicClick(Sender: TObject);
    procedure btnStartSpeakerClick(Sender: TObject);
    procedure btnStopSpeakerClick(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ServerSocketDisconnected(Sender: TObject; aLine: TncLine);
    procedure ClientSocketDisconnected(Sender: TObject; aLine: TncLine);
    procedure rgProtocolClick(Sender: TObject);
    // Audio Device and Format Controls
    procedure cmbMicDeviceChange(Sender: TObject);
    procedure trkMicVolumeChange(Sender: TObject);
    procedure btnRefreshMicDevicesClick(Sender: TObject);
    procedure btnApplyMicFormatClick(Sender: TObject);
    procedure cmbSpeakerDeviceChange(Sender: TObject);
    procedure trkSpeakerVolumeChange(Sender: TObject);
    procedure btnRefreshSpeakerDevicesClick(Sender: TObject);
    procedure btnApplySpeakerFormatClick(Sender: TObject);
  private
    FIsServerActive: Boolean;
    FIsClientConnected: Boolean;
    FIsMicActive: Boolean;
    FIsSpeakerActive: Boolean;
    FCurrentProtocol: Integer; // 0 = TCP, 1 = UDP
    FLastClientAddr: TSockAddrStorage;
    procedure LogMessage(const AMessage: string);
    procedure UpdateUI;
    procedure SendAudioData(const AData: TBytes);
    procedure ProcessReceivedAudio(const AData: TBytes);
    procedure SetControlStates;
    procedure LoadMicrophoneDevices;
    procedure LoadSpeakerDevices;
    procedure SetupAudioFormatCombos;
    procedure SafeStopAudio;
  public
    { Public declarations }
  end;

const
  AUDIO_COMMAND = 1001;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FIsServerActive := False;
  FIsClientConnected := False;
  FIsMicActive := False;
  FIsSpeakerActive := False;
  FCurrentProtocol := 0; // Default to TCP

  // Clear UDP client address
  FillChar(FLastClientAddr, SizeOf(FLastClientAddr), 0);

  // Set default values
  edtServerPort.Text := '8888';
  edtClientIP.Text := '127.0.0.1';
  edtClientPort.Text := '8888';
  rgProtocol.ItemIndex := 0;

  // Setup audio format combinations
  SetupAudioFormatCombos;

  // Load audio devices
  LoadMicrophoneDevices;
  LoadSpeakerDevices;

  // Initialize UI
  UpdateUI;
  LogMessage('Network Audio Streaming Demo initialized');
  LogMessage('Protocol: TCP=Reliable transmission, UDP=Fast/minimal latency');
  LogMessage('Configure audio devices and formats before starting streaming');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SafeStopAudio;
end;

procedure TForm1.SafeStopAudio;
begin
  try
    // Stop audio components properly
    if FIsMicActive then
    begin
      MicInput1.Active := False;
      FIsMicActive := False;
    end;
    if FIsSpeakerActive then
    begin
      SpeakerOutput1.Active := False;
      FIsSpeakerActive := False;
    end;

    // Stop network sockets
    if FIsServerActive then
    begin
      ServerSocket.Active := False;
      UDPServerSocket.Active := False;
    end;
    if FIsClientConnected then
    begin
      ClientSocket.Active := False;
      UDPClientSocket.Active := False;
    end;
  except
    // Ignore errors during cleanup
  end;
end;

procedure TForm1.SetupAudioFormatCombos;
begin
  // Setup sample rate combos - UNIVERSAL COMPATIBILITY
  cmbMicSampleRate.Items.AddStrings(['8000', '16000', '22050', '44100', '48000', '96000']);
  cmbMicSampleRate.ItemIndex := 3; // 44100 - most compatible
  cmbSpeakerSampleRate.Items.AddStrings(['8000', '16000', '22050', '44100', '48000', '96000']);
  cmbSpeakerSampleRate.ItemIndex := 3; // 44100

  // Setup channel combos
  cmbMicChannels.Items.AddStrings(['1', '2']);
  cmbMicChannels.ItemIndex := 1; // 2 channels (stereo)
  cmbSpeakerChannels.Items.AddStrings(['1', '2']);
  cmbSpeakerChannels.ItemIndex := 1; // 2 channels

  // Setup bit depth combos
  cmbMicBits.Items.AddStrings(['16', '24', '32']);
  cmbMicBits.ItemIndex := 0; // 16 bits - most compatible
  cmbSpeakerBits.Items.AddStrings(['16', '24', '32']);
  cmbSpeakerBits.ItemIndex := 0; // 16 bits

  // Setup volume controls
  trkMicVolume.Min := 0;
  trkMicVolume.Max := 100;
  trkMicVolume.Position := 100;
  lblMicVolumeValue.Caption := '100%';

  trkSpeakerVolume.Min := 0;
  trkSpeakerVolume.Max := 100;
  trkSpeakerVolume.Position := 100;
  lblSpeakerVolumeValue.Caption := '100%';
end;

procedure TForm1.LoadMicrophoneDevices;
var
  I: Integer;
  MicDevices: TMicDeviceArray;
begin
  try
    LogMessage('Loading microphone devices...');
    MicDevices := MicInput1.GetDevices;
    cmbMicDevice.Items.Clear;
    for I := 0 to Length(MicDevices) - 1 do
    begin
      cmbMicDevice.Items.Add(MicDevices[I].Name);
      LogMessage('Mic found: ' + MicDevices[I].Name);
    end;
    if cmbMicDevice.Items.Count > 0 then
      cmbMicDevice.ItemIndex := 0;

    LogMessage(Format('Found %d microphones', [cmbMicDevice.Items.Count]));
  except
    on E: Exception do
      LogMessage('Error loading microphone devices: ' + E.Message);
  end;
end;

procedure TForm1.LoadSpeakerDevices;
var
  I: Integer;
  SpeakerDevices: TSpeakerDeviceArray;
begin
  try
    LogMessage('Loading speaker devices...');
    SpeakerDevices := SpeakerOutput1.GetDevices;
    cmbSpeakerDevice.Items.Clear;
    for I := 0 to Length(SpeakerDevices) - 1 do
    begin
      cmbSpeakerDevice.Items.Add(SpeakerDevices[I].Name);
      LogMessage('Speaker found: ' + SpeakerDevices[I].Name);
    end;
    if cmbSpeakerDevice.Items.Count > 0 then
      cmbSpeakerDevice.ItemIndex := 0;

    LogMessage(Format('Found %d speakers', [cmbSpeakerDevice.Items.Count]));
  except
    on E: Exception do
      LogMessage('Error loading speaker devices: ' + E.Message);
  end;
end;

procedure TForm1.LogMessage(const AMessage: string);
begin
  try
    memLog.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + AMessage);
    if memLog.Lines.Count > 500 then
      memLog.Lines.Delete(0);
    memLog.Perform(WM_VSCROLL, SB_BOTTOM, 0);
    Application.ProcessMessages;
  except
    // Ignore logging errors
  end;
end;

procedure TForm1.UpdateUI;
begin
  FCurrentProtocol := rgProtocol.ItemIndex;

  // Update status labels
  if FIsServerActive then
  begin
    if FCurrentProtocol = 0 then
      lblServerStatus.Caption := 'TCP Server: Running on port ' + edtServerPort.Text
    else
      lblServerStatus.Caption := 'UDP Server: Listening on port ' + edtServerPort.Text;
    lblServerStatus.Font.Color := clGreen;
  end
  else
  begin
    lblServerStatus.Caption := 'Server: Stopped';
    lblServerStatus.Font.Color := clRed;
  end;

  if FIsClientConnected then
  begin
    if FCurrentProtocol = 0 then
      lblClientStatus.Caption := 'TCP Client: Connected to ' + edtClientIP.Text + ':' + edtClientPort.Text
    else
      lblClientStatus.Caption := 'UDP Client: Ready to send to ' + edtClientIP.Text + ':' + edtClientPort.Text;
    lblClientStatus.Font.Color := clGreen;
  end
  else
  begin
    lblClientStatus.Caption := 'Client: Disconnected';
    lblClientStatus.Font.Color := clRed;
  end;

  if FIsMicActive then
  begin
    lblMicStatus.Caption := 'Microphone: Recording';
    lblMicStatus.Font.Color := clGreen;
  end
  else
  begin
    lblMicStatus.Caption := 'Microphone: Stopped';
    lblMicStatus.Font.Color := clRed;
  end;

  if FIsSpeakerActive then
  begin
    lblSpeakerStatus.Caption := 'Speaker: Playing';
    lblSpeakerStatus.Font.Color := clGreen;
  end
  else
  begin
    lblSpeakerStatus.Caption := 'Speaker: Stopped';
    lblSpeakerStatus.Font.Color := clRed;
  end;

  SetControlStates;
end;

procedure TForm1.SetControlStates;
begin
  // Server controls
  btnStartTCPServer.Enabled := not FIsServerActive and (FCurrentProtocol = 0);
  btnStopTCPServer.Enabled := FIsServerActive and (FCurrentProtocol = 0);
  btnStartUDPServer.Enabled := not FIsServerActive and (FCurrentProtocol = 1);
  btnStopUDPServer.Enabled := FIsServerActive and (FCurrentProtocol = 1);

  // Client controls
  btnConnectTCP.Enabled := not FIsClientConnected and (FCurrentProtocol = 0);
  btnDisconnectTCP.Enabled := FIsClientConnected and (FCurrentProtocol = 0);
  btnConnectUDP.Enabled := not FIsClientConnected and (FCurrentProtocol = 1);
  btnDisconnectUDP.Enabled := FIsClientConnected and (FCurrentProtocol = 1);

  // Audio controls
  btnStartMic.Enabled := not FIsMicActive and FIsClientConnected;
  btnStopMic.Enabled := FIsMicActive;
  btnStartSpeaker.Enabled := not FIsSpeakerActive and FIsServerActive;
  btnStopSpeaker.Enabled := FIsSpeakerActive;

  // Disable protocol switching when active
  rgProtocol.Enabled := not (FIsServerActive or FIsClientConnected);

  // Disable port/IP editing when active
  edtServerPort.Enabled := not FIsServerActive;
  edtClientIP.Enabled := not FIsClientConnected;
  edtClientPort.Enabled := not FIsClientConnected;

  // Audio device controls
  cmbMicDevice.Enabled := not FIsMicActive;
  cmbMicSampleRate.Enabled := not FIsMicActive;
  cmbMicChannels.Enabled := not FIsMicActive;
  cmbMicBits.Enabled := not FIsMicActive;
  btnApplyMicFormat.Enabled := not FIsMicActive;
  btnRefreshMicDevices.Enabled := not FIsMicActive;

  cmbSpeakerDevice.Enabled := not FIsSpeakerActive;
  cmbSpeakerSampleRate.Enabled := not FIsSpeakerActive;
  cmbSpeakerChannels.Enabled := not FIsSpeakerActive;
  cmbSpeakerBits.Enabled := not FIsSpeakerActive;
  btnApplySpeakerFormat.Enabled := not FIsSpeakerActive;
  btnRefreshSpeakerDevices.Enabled := not FIsSpeakerActive;
end;

// Audio Device Control Events
procedure TForm1.cmbMicDeviceChange(Sender: TObject);
begin
  try
    if FIsMicActive then
    begin
      MicInput1.Active := False;
      Sleep(50);
    end;
    MicInput1.DeviceID := cmbMicDevice.ItemIndex;
    LogMessage('Microphone changed to: ' + cmbMicDevice.Text);
  except
    on E: Exception do
      LogMessage('Error changing microphone: ' + E.Message);
  end;
end;

procedure TForm1.trkMicVolumeChange(Sender: TObject);
begin
  try
    MicInput1.Volume := trkMicVolume.Position;
    lblMicVolumeValue.Caption := IntToStr(trkMicVolume.Position) + '%';
  except
    on E: Exception do
      LogMessage('Error setting microphone volume: ' + E.Message);
  end;
end;

procedure TForm1.btnRefreshMicDevicesClick(Sender: TObject);
begin
  try
    LogMessage('Refreshing microphone devices...');
    if FIsMicActive then
    begin
      MicInput1.Active := False;
      FIsMicActive := False;
      Sleep(100);
    end;
    MicInput1.RefreshDevices;
    LoadMicrophoneDevices;
    LogMessage('Microphone device refresh complete');
    UpdateUI;
  except
    on E: Exception do
      LogMessage('Error refreshing microphone devices: ' + E.Message);
  end;
end;

procedure TForm1.btnApplyMicFormatClick(Sender: TObject);
begin
  try
    var WasActive := FIsMicActive;
    if WasActive then
    begin
      MicInput1.Active := False;
      Sleep(100);
    end;

    MicInput1.SampleRate := StrToIntDef(cmbMicSampleRate.Text, 44100);
    MicInput1.Channels := StrToIntDef(cmbMicChannels.Text, 2);
    MicInput1.BitsPerSample := StrToIntDef(cmbMicBits.Text, 16);

    LogMessage(Format('Mic format request: %dHz, %dch, %dbit',
      [MicInput1.SampleRate, MicInput1.Channels, MicInput1.BitsPerSample]));

    if WasActive then
    begin
      MicInput1.Active := True;
      if MicInput1.Active then
      begin
        FIsMicActive := True;
        LogMessage(Format('Mic active: %dHz, %dch, %dbit (Windows handles conversion)',
          [MicInput1.ActualSampleRate, MicInput1.ActualChannels, MicInput1.ActualBitsPerSample]));
      end
      else
        LogMessage('ERROR: Microphone failed to restart!');
    end;
    UpdateUI;
  except
    on E: Exception do
      LogMessage('Error applying mic format: ' + E.Message);
  end;
end;

procedure TForm1.cmbSpeakerDeviceChange(Sender: TObject);
begin
  try
    if FIsSpeakerActive then
    begin
      SpeakerOutput1.Active := False;
      Sleep(50);
    end;
    SpeakerOutput1.DeviceID := cmbSpeakerDevice.ItemIndex;
    LogMessage('Speaker changed to: ' + cmbSpeakerDevice.Text);
  except
    on E: Exception do
      LogMessage('Error changing speaker: ' + E.Message);
  end;
end;

procedure TForm1.trkSpeakerVolumeChange(Sender: TObject);
begin
  try
    SpeakerOutput1.Volume := trkSpeakerVolume.Position;
    lblSpeakerVolumeValue.Caption := IntToStr(trkSpeakerVolume.Position) + '%';
  except
    on E: Exception do
      LogMessage('Error setting speaker volume: ' + E.Message);
  end;
end;

procedure TForm1.btnRefreshSpeakerDevicesClick(Sender: TObject);
begin
  try
    LogMessage('Refreshing speaker devices...');
    if FIsSpeakerActive then
    begin
      SpeakerOutput1.Active := False;
      FIsSpeakerActive := False;
      Sleep(100);
    end;
    SpeakerOutput1.RefreshDevices;
    LoadSpeakerDevices;
    LogMessage('Speaker device refresh complete');
    UpdateUI;
  except
    on E: Exception do
      LogMessage('Error refreshing speaker devices: ' + E.Message);
  end;
end;

procedure TForm1.btnApplySpeakerFormatClick(Sender: TObject);
begin
  try
    var WasActive := FIsSpeakerActive;
    if WasActive then
    begin
      SpeakerOutput1.Active := False;
      Sleep(100);
    end;

    SpeakerOutput1.SampleRate := StrToIntDef(cmbSpeakerSampleRate.Text, 44100);
    SpeakerOutput1.Channels := StrToIntDef(cmbSpeakerChannels.Text, 2);
    SpeakerOutput1.BitsPerSample := StrToIntDef(cmbSpeakerBits.Text, 16);

    LogMessage(Format('Speaker format request: %dHz, %dch, %dbit',
      [SpeakerOutput1.SampleRate, SpeakerOutput1.Channels, SpeakerOutput1.BitsPerSample]));

    if WasActive then
    begin
      SpeakerOutput1.Active := True;
      if SpeakerOutput1.Active then
      begin
        FIsSpeakerActive := True;
        LogMessage(Format('Speaker active: %dHz, %dch, %dbit (Windows handles conversion)',
          [SpeakerOutput1.ActualSampleRate, SpeakerOutput1.ActualChannels, SpeakerOutput1.ActualBitsPerSample]));
      end
      else
        LogMessage('ERROR: Speaker failed to restart!');
    end;
    UpdateUI;
  except
    on E: Exception do
      LogMessage('Error applying speaker format: ' + E.Message);
  end;
end;

// Server Events
procedure TForm1.btnStartTCPServerClick(Sender: TObject);
begin
  try
    ServerSocket.Port := StrToInt(edtServerPort.Text);
    ServerSocket.Active := True;
    FIsServerActive := True;
    LogMessage('TCP Server started on port ' + edtServerPort.Text);
    UpdateUI;
  except
    on E: Exception do
    begin
      ShowMessage('Failed to start TCP server: ' + E.Message);
      LogMessage('Error starting TCP server: ' + E.Message);
    end;
  end;
end;

procedure TForm1.btnStopTCPServerClick(Sender: TObject);
begin
  try
    ServerSocket.Active := False;
    FIsServerActive := False;
    if FIsSpeakerActive then
      btnStopSpeakerClick(Self);
    LogMessage('TCP Server stopped');
    UpdateUI;
  except
    on E: Exception do
      LogMessage('Error stopping TCP server: ' + E.Message);
  end;
end;

procedure TForm1.btnStartUDPServerClick(Sender: TObject);
begin
  try
    UDPServerSocket.Port := StrToInt(edtServerPort.Text);
    UDPServerSocket.Active := True;
    FIsServerActive := True;
    LogMessage('UDP Server started on port ' + edtServerPort.Text);
    UpdateUI;
  except
    on E: Exception do
    begin
      ShowMessage('Failed to start UDP server: ' + E.Message);
      LogMessage('Error starting UDP server: ' + E.Message);
    end;
  end;
end;

procedure TForm1.btnStopUDPServerClick(Sender: TObject);
begin
  try
    UDPServerSocket.Active := False;
    FIsServerActive := False;
    if FIsSpeakerActive then
      btnStopSpeakerClick(Self);
    LogMessage('UDP Server stopped');
    UpdateUI;
  except
    on E: Exception do
      LogMessage('Error stopping UDP server: ' + E.Message);
  end;
end;

// Client Events
procedure TForm1.btnConnectTCPClick(Sender: TObject);
begin
  try
    ClientSocket.Host := edtClientIP.Text;
    ClientSocket.Port := StrToInt(edtClientPort.Text);
    ClientSocket.Active := True;
    LogMessage('Attempting TCP connection to ' + edtClientIP.Text + ':' + edtClientPort.Text);
    UpdateUI;
  except
    on E: Exception do
    begin
      ShowMessage('Failed to connect TCP client: ' + E.Message);
      LogMessage('Error connecting TCP client: ' + E.Message);
    end;
  end;
end;

procedure TForm1.btnDisconnectTCPClick(Sender: TObject);
begin
  try
    ClientSocket.Active := False;
    FIsClientConnected := False;
    if FIsMicActive then
      btnStopMicClick(Self);
    LogMessage('TCP Client disconnected');
    UpdateUI;
  except
    on E: Exception do
      LogMessage('Error disconnecting TCP client: ' + E.Message);
  end;
end;

procedure TForm1.btnConnectUDPClick(Sender: TObject);
begin
  try
    UDPClientSocket.Host := edtClientIP.Text;
    UDPClientSocket.Port := StrToInt(edtClientPort.Text);
    UDPClientSocket.Active := True;
    FIsClientConnected := True;
    LogMessage('UDP Client ready to send to ' + edtClientIP.Text + ':' + edtClientPort.Text);
    UpdateUI;
  except
    on E: Exception do
    begin
      ShowMessage('Failed to setup UDP client: ' + E.Message);
      LogMessage('Error setting up UDP client: ' + E.Message);
    end;
  end;
end;

procedure TForm1.btnDisconnectUDPClick(Sender: TObject);
begin
  try
    UDPClientSocket.Active := False;
    FIsClientConnected := False;
    if FIsMicActive then
      btnStopMicClick(Self);
    LogMessage('UDP Client disconnected');
    UpdateUI;
  except
    on E: Exception do
      LogMessage('Error disconnecting UDP client: ' + E.Message);
  end;
end;

// Audio Controls
procedure TForm1.btnStartMicClick(Sender: TObject);
begin
  try
    MicInput1.Active := True;
    FIsMicActive := True;
    LogMessage('Microphone started - audio streaming active');
    LogMessage('Mic format: ' + IntToStr(MicInput1.ActualSampleRate) + 'Hz, ' +
               IntToStr(MicInput1.ActualChannels) + 'ch, ' +
               IntToStr(MicInput1.ActualBitsPerSample) + 'bit');
    UpdateUI;
  except
    on E: Exception do
    begin
      ShowMessage('Failed to start microphone: ' + E.Message);
      LogMessage('Error starting microphone: ' + E.Message);
      FIsMicActive := False;
    end;
  end;
end;

procedure TForm1.btnStopMicClick(Sender: TObject);
begin
  try
    MicInput1.Active := False;
    FIsMicActive := False;
    LogMessage('Microphone stopped');
    UpdateUI;
  except
    on E: Exception do
      LogMessage('Error stopping microphone: ' + E.Message);
  end;
end;

procedure TForm1.btnStartSpeakerClick(Sender: TObject);
begin
  try
    SpeakerOutput1.Active := True;
    FIsSpeakerActive := True;
    LogMessage('Speaker started - ready to receive audio');
    LogMessage('Speaker format: ' + IntToStr(SpeakerOutput1.ActualSampleRate) + 'Hz, ' +
               IntToStr(SpeakerOutput1.ActualChannels) + 'ch, ' +
               IntToStr(SpeakerOutput1.ActualBitsPerSample) + 'bit');
    UpdateUI;
  except
    on E: Exception do
    begin
      ShowMessage('Failed to start speaker: ' + E.Message);
      LogMessage('Error starting speaker: ' + E.Message);
      FIsSpeakerActive := False;
    end;
  end;
end;

procedure TForm1.btnStopSpeakerClick(Sender: TObject);
begin
  try
    SpeakerOutput1.Active := False;
    FIsSpeakerActive := False;
    LogMessage('Speaker stopped');
    UpdateUI;
  except
    on E: Exception do
      LogMessage('Error stopping speaker: ' + E.Message);
  end;
end;

procedure TForm1.btnClearLogClick(Sender: TObject);
begin
  memLog.Clear;
  LogMessage('Log cleared');
end;

procedure TForm1.rgProtocolClick(Sender: TObject);
begin
  UpdateUI;
end;

// Network Events
procedure TForm1.ServerSocketConnected(Sender: TObject; aLine: TncLine);
begin
  LogMessage('TCP Client connected from: ' + aLine.PeerIP);
  lblConnectionInfo.Caption := 'Connected: ' + aLine.PeerIP;
end;

procedure TForm1.ServerSocketDisconnected(Sender: TObject; aLine: TncLine);
begin
  LogMessage('TCP Client disconnected: ' + aLine.PeerIP);
  lblConnectionInfo.Caption := 'No connections';
end;

procedure TForm1.ClientSocketConnected(Sender: TObject; aLine: TncLine);
begin
  FIsClientConnected := True;
  LogMessage('TCP Connected to server successfully');
  UpdateUI;
end;

procedure TForm1.ClientSocketDisconnected(Sender: TObject; aLine: TncLine);
begin
  FIsClientConnected := False;
  if FIsMicActive then
    btnStopMicClick(Self);
  LogMessage('TCP Disconnected from server');
  UpdateUI;
end;

function TForm1.ServerSocketHandleCommand(Sender: TObject; aLine: TncLine;
  aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean;
  const aSenderComponent, aReceiverComponent: string): TBytes;
begin
  case aCmd of
    AUDIO_COMMAND:
    begin
      ProcessReceivedAudio(aData);
    end;
  end;
  Result := nil;
end;

function TForm1.ClientSocketHandleCommand(Sender: TObject; aLine: TncLine;
  aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean;
  const aSenderComponent, aReceiverComponent: string): TBytes;
begin
  // Not used for client
  Result := nil;
end;

procedure TForm1.UDPServerSocketReadDatagram(Sender: TObject; aLine: TncLine;
  const aBuf: TBytes; aBufCount: Integer;
  const SenderAddr: TSockAddrStorage);
var
  AudioData: TBytes;
begin
  if aBufCount > 0 then
  begin
    // Store the client address for potential response
    FLastClientAddr := SenderAddr;

    // Copy the received audio data
    SetLength(AudioData, aBufCount);
    Move(aBuf[0], AudioData[0], aBufCount);
    ProcessReceivedAudio(AudioData);
  end;
end;

procedure TForm1.UDPClientSocketReadDatagram(Sender: TObject; aLine: TncLine;
  const aBuf: TBytes; aBufCount: Integer;
  const SenderAddr: TSockAddrStorage);
begin
  // Not typically used for UDP client sending
end;

// Audio Processing
procedure TForm1.MicInput1DataReceivedBytes(Sender: TObject;
  const Buffer: TBytes);
begin
  if FIsClientConnected and (Length(Buffer) > 0) then
  begin
    SendAudioData(Buffer);
  end;
end;

procedure TForm1.SendAudioData(const AData: TBytes);
begin
  try
    if FCurrentProtocol = 0 then // TCP
    begin
      if ClientSocket.Active and FIsClientConnected then
      begin
        ClientSocket.ExecCommand(AUDIO_COMMAND, AData, False); // No response required for streaming
      end;
    end
    else // UDP
    begin
      if UDPClientSocket.Active then
      begin
        UDPClientSocket.Send(AData);
      end;
    end;
  except
    on E: Exception do
      LogMessage('Error sending audio: ' + E.Message);
  end;
end;

procedure TForm1.ProcessReceivedAudio(const AData: TBytes);
begin
  try
    if FIsSpeakerActive and (Length(AData) > 0) then
    begin
      SpeakerOutput1.PlayBufferBytes(AData);
    end;
  except
    on E: Exception do
      LogMessage('Error processing received audio: ' + E.Message);
  end;
end;

end.
