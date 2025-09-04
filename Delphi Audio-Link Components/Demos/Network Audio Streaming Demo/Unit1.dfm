object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Network Audio Streaming Demo By: BitmasterXor'
  ClientHeight = 640
  ClientWidth = 992
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 485
    Height = 369
    Caption = ' Audio Server (Receiver/Speaker) '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object lblServerStatus: TLabel
      Left = 16
      Top = 28
      Width = 91
      Height = 15
      Caption = 'Server: Stopped'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblServerPort: TLabel
      Left = 16
      Top = 56
      Width = 59
      Height = 15
      Caption = 'Listen Port:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblSpeakerStatus: TLabel
      Left = 16
      Top = 151
      Width = 99
      Height = 15
      Caption = 'Speaker: Stopped'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblConnectionInfo: TLabel
      Left = 16
      Top = 340
      Width = 84
      Height = 15
      Caption = 'No connections'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblSpeakerDevice: TLabel
      Left = 16
      Top = 201
      Width = 82
      Height = 15
      Caption = 'Speaker Device:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblSpeakerSampleRate: TLabel
      Left = 16
      Top = 251
      Width = 68
      Height = 15
      Caption = 'Sample Rate:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblSpeakerChannels: TLabel
      Left = 150
      Top = 251
      Width = 52
      Height = 15
      Caption = 'Channels:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblSpeakerBits: TLabel
      Left = 250
      Top = 251
      Width = 22
      Height = 15
      Caption = 'Bits:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblSpeakerVolume: TLabel
      Left = 16
      Top = 301
      Width = 43
      Height = 15
      Caption = 'Volume:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblSpeakerVolumeValue: TLabel
      Left = 420
      Top = 301
      Width = 28
      Height = 15
      Caption = '100%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object pnlServerControls: TPanel
      Left = 16
      Top = 80
      Width = 450
      Height = 65
      BevelOuter = bvLowered
      TabOrder = 0
      object btnStartTCPServer: TButton
        Left = 8
        Top = 8
        Width = 100
        Height = 25
        Caption = 'Start TCP'
        TabOrder = 0
        OnClick = btnStartTCPServerClick
      end
      object btnStopTCPServer: TButton
        Left = 116
        Top = 8
        Width = 100
        Height = 25
        Caption = 'Stop TCP'
        Enabled = False
        TabOrder = 1
        OnClick = btnStopTCPServerClick
      end
      object btnStartUDPServer: TButton
        Left = 8
        Top = 36
        Width = 100
        Height = 25
        Caption = 'Start UDP'
        TabOrder = 2
        OnClick = btnStartUDPServerClick
      end
      object btnStopUDPServer: TButton
        Left = 116
        Top = 36
        Width = 100
        Height = 25
        Caption = 'Stop UDP'
        Enabled = False
        TabOrder = 3
        OnClick = btnStopUDPServerClick
      end
    end
    object edtServerPort: TEdit
      Left = 88
      Top = 53
      Width = 80
      Height = 23
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      Text = '8888'
    end
    object btnStartSpeaker: TButton
      Left = 16
      Top = 175
      Width = 100
      Height = 25
      Caption = 'Start Speaker'
      TabOrder = 2
      OnClick = btnStartSpeakerClick
    end
    object btnStopSpeaker: TButton
      Left = 124
      Top = 175
      Width = 100
      Height = 25
      Caption = 'Stop Speaker'
      Enabled = False
      TabOrder = 3
      OnClick = btnStopSpeakerClick
    end
    object cmbSpeakerDevice: TComboBox
      Left = 16
      Top = 222
      Width = 300
      Height = 23
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnChange = cmbSpeakerDeviceChange
    end
    object btnRefreshSpeakerDevices: TButton
      Left = 322
      Top = 222
      Width = 70
      Height = 23
      Caption = 'Refresh'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      OnClick = btnRefreshSpeakerDevicesClick
    end
    object cmbSpeakerSampleRate: TComboBox
      Left = 16
      Top = 272
      Width = 80
      Height = 23
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
    end
    object cmbSpeakerChannels: TComboBox
      Left = 150
      Top = 272
      Width = 60
      Height = 23
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 7
    end
    object cmbSpeakerBits: TComboBox
      Left = 250
      Top = 272
      Width = 60
      Height = 23
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 8
    end
    object btnApplySpeakerFormat: TButton
      Left = 330
      Top = 272
      Width = 60
      Height = 23
      Caption = 'Apply'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 9
      OnClick = btnApplySpeakerFormatClick
    end
    object trkSpeakerVolume: TTrackBar
      Left = 80
      Top = 301
      Width = 330
      Height = 33
      Max = 100
      Position = 100
      TabOrder = 10
      OnChange = trkSpeakerVolumeChange
    end
  end
  object GroupBox2: TGroupBox
    Left = 507
    Top = 8
    Width = 485
    Height = 369
    Caption = ' Audio Client (Sender/Microphone) '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object lblClientStatus: TLabel
      Left = 16
      Top = 28
      Width = 114
      Height = 15
      Caption = 'Client: Disconnected'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblClientIP: TLabel
      Left = 16
      Top = 56
      Width = 48
      Height = 15
      Caption = 'Server IP:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblClientPort: TLabel
      Left = 200
      Top = 56
      Width = 60
      Height = 15
      Caption = 'Server Port:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblMicStatus: TLabel
      Left = 16
      Top = 151
      Width = 120
      Height = 15
      Caption = 'Microphone: Stopped'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblMicDevice: TLabel
      Left = 16
      Top = 201
      Width = 106
      Height = 15
      Caption = 'Microphone Device:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblMicSampleRate: TLabel
      Left = 16
      Top = 251
      Width = 68
      Height = 15
      Caption = 'Sample Rate:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblMicChannels: TLabel
      Left = 150
      Top = 251
      Width = 52
      Height = 15
      Caption = 'Channels:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblMicBits: TLabel
      Left = 250
      Top = 251
      Width = 22
      Height = 15
      Caption = 'Bits:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblMicVolume: TLabel
      Left = 16
      Top = 301
      Width = 43
      Height = 15
      Caption = 'Volume:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblMicVolumeValue: TLabel
      Left = 420
      Top = 301
      Width = 28
      Height = 15
      Caption = '100%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object pnlClientControls: TPanel
      Left = 16
      Top = 80
      Width = 450
      Height = 65
      BevelOuter = bvLowered
      TabOrder = 0
      object btnConnectTCP: TButton
        Left = 8
        Top = 8
        Width = 100
        Height = 25
        Caption = 'Connect TCP'
        TabOrder = 0
        OnClick = btnConnectTCPClick
      end
      object btnDisconnectTCP: TButton
        Left = 116
        Top = 8
        Width = 100
        Height = 25
        Caption = 'Disconnect TCP'
        Enabled = False
        TabOrder = 1
        OnClick = btnDisconnectTCPClick
      end
      object btnConnectUDP: TButton
        Left = 8
        Top = 36
        Width = 100
        Height = 25
        Caption = 'Connect UDP'
        TabOrder = 2
        OnClick = btnConnectUDPClick
      end
      object btnDisconnectUDP: TButton
        Left = 116
        Top = 36
        Width = 100
        Height = 25
        Caption = 'Disconnect UDP'
        Enabled = False
        TabOrder = 3
        OnClick = btnDisconnectUDPClick
      end
    end
    object edtClientIP: TEdit
      Left = 83
      Top = 53
      Width = 100
      Height = 23
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      Text = '127.0.0.1'
    end
    object edtClientPort: TEdit
      Left = 277
      Top = 53
      Width = 80
      Height = 23
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      Text = '8888'
    end
    object btnStartMic: TButton
      Left = 16
      Top = 175
      Width = 110
      Height = 25
      Caption = 'Start Microphone'
      TabOrder = 3
      OnClick = btnStartMicClick
    end
    object btnStopMic: TButton
      Left = 134
      Top = 175
      Width = 110
      Height = 25
      Caption = 'Stop Microphone'
      Enabled = False
      TabOrder = 4
      OnClick = btnStopMicClick
    end
    object cmbMicDevice: TComboBox
      Left = 16
      Top = 222
      Width = 300
      Height = 23
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      OnChange = cmbMicDeviceChange
    end
    object btnRefreshMicDevices: TButton
      Left = 322
      Top = 222
      Width = 70
      Height = 23
      Caption = 'Refresh'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      OnClick = btnRefreshMicDevicesClick
    end
    object cmbMicSampleRate: TComboBox
      Left = 16
      Top = 272
      Width = 80
      Height = 23
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 7
    end
    object cmbMicChannels: TComboBox
      Left = 150
      Top = 272
      Width = 60
      Height = 23
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 8
    end
    object cmbMicBits: TComboBox
      Left = 250
      Top = 272
      Width = 60
      Height = 23
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 9
    end
    object btnApplyMicFormat: TButton
      Left = 330
      Top = 272
      Width = 60
      Height = 23
      Caption = 'Apply'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 10
      OnClick = btnApplyMicFormatClick
    end
    object trkMicVolume: TTrackBar
      Left = 80
      Top = 301
      Width = 330
      Height = 33
      Max = 100
      Position = 100
      TabOrder = 11
      OnChange = trkMicVolumeChange
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 383
    Width = 984
    Height = 120
    Caption = ' Audio Injection System '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    object lblInjectedVolume: TLabel
      Left = 16
      Top = 85
      Width = 68
      Height = 15
      Caption = 'Injection Vol:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblInjectedVolumeValue: TLabel
      Left = 320
      Top = 85
      Width = 28
      Height = 15
      Caption = '100%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblBeepFreq: TLabel
      Left = 400
      Top = 25
      Width = 58
      Height = 15
      Caption = 'Frequency:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblBeepDuration: TLabel
      Left = 520
      Top = 25
      Width = 76
      Height = 15
      Caption = 'Duration (ms):'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object chkMixInjectedAudio: TCheckBox
      Left = 18
      Top = 26
      Width = 150
      Height = 17
      Caption = 'Mix Injected Audio'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 0
      OnClick = chkMixInjectedAudioClick
    end
    object trkInjectedVolume: TTrackBar
      Left = 120
      Top = 85
      Width = 190
      Height = 33
      Max = 100
      Position = 100
      TabOrder = 1
      OnChange = trkInjectedVolumeChange
    end
    object edtBeepFreq: TEdit
      Left = 400
      Top = 46
      Width = 60
      Height = 23
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      Text = '800'
    end
    object edtBeepDuration: TEdit
      Left = 520
      Top = 46
      Width = 60
      Height = 23
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      Text = '100'
    end
    object btnInjectBeep: TButton
      Left = 620
      Top = 25
      Width = 100
      Height = 25
      Caption = 'Inject Beep'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnClick = btnInjectBeepClick
    end
    object btnInjectAudioFile: TButton
      Left = 620
      Top = 55
      Width = 100
      Height = 25
      Caption = 'Inject Audio File'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      OnClick = btnInjectAudioFileClick
    end
    object btnStopInjection: TButton
      Left = 740
      Top = 25
      Width = 100
      Height = 25
      Caption = 'Stop Injection'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      OnClick = btnStopInjectionClick
    end
    object btnTestInjection: TButton
      Left = 740
      Top = 55
      Width = 100
      Height = 25
      Caption = 'Test Injection'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 7
      OnClick = btnTestInjectionClick
    end
    object chkLoopAudio: TCheckBox
      Left = 860
      Top = 25
      Width = 121
      Height = 17
      Caption = 'Loop Audio Files'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 8
    end
    object chkVirtualMicMode: TCheckBox
      Left = 18
      Top = 49
      Width = 186
      Height = 17
      Caption = 'Virtual Microphone Mode'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 9
      OnClick = chkVirtualMicModeClick
    end
  end
  object rgProtocol: TRadioGroup
    Left = 8
    Top = 507
    Width = 200
    Height = 65
    Caption = ' Protocol Selection '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    Items.Strings = (
      'TCP (Reliable)'
      'UDP (Fast)')
    ParentFont = False
    TabOrder = 3
    OnClick = rgProtocolClick
  end
  object memLog: TMemo
    Left = 214
    Top = 507
    Width = 778
    Height = 132
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object btnClearLog: TButton
    Left = 8
    Top = 579
    Width = 200
    Height = 60
    Caption = 'Clear Log'
    TabOrder = 5
    OnClick = btnClearLogClick
  end
  object ServerSocket: TncServerSource
    EncryptionKey = 'SetEncryptionKey'
    OnConnected = ServerSocketConnected
    OnDisconnected = ServerSocketDisconnected
    OnHandleCommand = ServerSocketHandleCommand
    Left = 360
    Top = 552
  end
  object ClientSocket: TncClientSource
    EncryptionKey = 'SetEncryptionKey'
    OnConnected = ClientSocketConnected
    OnDisconnected = ClientSocketDisconnected
    OnHandleCommand = ClientSocketHandleCommand
    Left = 712
    Top = 552
  end
  object MicInput1: TMicInput
    OnDataReceivedBytes = MicInput1DataReceivedBytes
    Left = 792
    Top = 552
  end
  object SpeakerOutput1: TSpeakerOutput
    Left = 448
    Top = 552
  end
  object UDPServerSocket: TncUDPServer
    OnReadDatagram = UDPServerSocketReadDatagram
    Left = 272
    Top = 552
  end
  object UDPClientSocket: TncUDPClient
    OnReadDatagram = UDPClientSocketReadDatagram
    Left = 624
    Top = 552
  end
  object OpenDialog1: TOpenDialog
    Filter = 'WAV Files (*.wav)|*.wav|All Files (*.*)|*.*'
    Title = 'Select Audio File for Injection'
    Left = 536
    Top = 552
  end
end
