object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Clean WASAPI Demo - MIC Injection Only - By: BitmasterXor'
  ClientHeight = 580
  ClientWidth = 760
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    760
    580)
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 360
    Height = 255
    Caption = 'Microphone Input'
    TabOrder = 0
    object Label1: TLabel
      Left = 10
      Top = 25
      Width = 36
      Height = 13
      Caption = 'Device:'
    end
    object Label3: TLabel
      Left = 10
      Top = 75
      Width = 64
      Height = 13
      Caption = 'Sample Rate:'
    end
    object Label4: TLabel
      Left = 10
      Top = 105
      Width = 48
      Height = 13
      Caption = 'Channels:'
    end
    object Label5: TLabel
      Left = 10
      Top = 135
      Width = 77
      Height = 13
      Caption = 'Bits per Sample:'
    end
    object Label11: TLabel
      Left = 10
      Top = 200
      Width = 68
      Height = 13
      Caption = 'Mic Vol: 100%'
    end
    object ComboBox1: TComboBox
      Left = 10
      Top = 45
      Width = 280
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = ComboBox1Change
    end
    object ComboBox3: TComboBox
      Left = 90
      Top = 72
      Width = 80
      Height = 21
      Style = csDropDownList
      TabOrder = 1
    end
    object ComboBox4: TComboBox
      Left = 90
      Top = 102
      Width = 80
      Height = 21
      Style = csDropDownList
      TabOrder = 2
    end
    object ComboBox5: TComboBox
      Left = 90
      Top = 132
      Width = 80
      Height = 21
      Style = csDropDownList
      TabOrder = 3
    end
    object Button6: TButton
      Left = 182
      Top = 75
      Width = 158
      Height = 78
      Caption = 'Apply Format'
      TabOrder = 4
      OnClick = Button6Click
    end
    object Button1: TButton
      Left = 21
      Top = 165
      Width = 75
      Height = 25
      Caption = 'Start Mic'
      TabOrder = 5
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 102
      Top = 165
      Width = 75
      Height = 25
      Caption = 'Stop Mic'
      Enabled = False
      TabOrder = 6
      OnClick = Button2Click
    end
    object TrackBar1: TTrackBar
      Left = 10
      Top = 220
      Width = 347
      Height = 25
      Max = 100
      Position = 100
      TabOrder = 7
      OnChange = TrackBar1Change
    end
    object CheckBox1: TCheckBox
      Left = 190
      Top = 191
      Width = 150
      Height = 17
      Caption = 'Enable Mic Passthrough'
      TabOrder = 8
      OnClick = CheckBox1Click
    end
    object CheckBox2: TCheckBox
      Left = 190
      Top = 168
      Width = 150
      Height = 17
      Caption = 'Mix Injected Audio'
      Checked = True
      State = cbChecked
      TabOrder = 9
      OnClick = CheckBox2Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 376
    Top = 8
    Width = 360
    Height = 255
    Caption = 'Speaker Output'
    TabOrder = 1
    object Label2: TLabel
      Left = 10
      Top = 25
      Width = 36
      Height = 13
      Caption = 'Device:'
    end
    object Label6: TLabel
      Left = 10
      Top = 75
      Width = 64
      Height = 13
      Caption = 'Sample Rate:'
    end
    object Label7: TLabel
      Left = 10
      Top = 105
      Width = 48
      Height = 13
      Caption = 'Channels:'
    end
    object Label8: TLabel
      Left = 10
      Top = 135
      Width = 77
      Height = 13
      Caption = 'Bits per Sample:'
    end
    object Label12: TLabel
      Left = 18
      Top = 200
      Width = 92
      Height = 13
      Caption = 'Speaker Vol: 100%'
    end
    object ComboBox2: TComboBox
      Left = 10
      Top = 45
      Width = 280
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = ComboBox2Change
    end
    object ComboBox6: TComboBox
      Left = 90
      Top = 72
      Width = 80
      Height = 21
      Style = csDropDownList
      TabOrder = 1
    end
    object ComboBox7: TComboBox
      Left = 90
      Top = 102
      Width = 80
      Height = 21
      Style = csDropDownList
      TabOrder = 2
    end
    object ComboBox8: TComboBox
      Left = 90
      Top = 132
      Width = 80
      Height = 21
      Style = csDropDownList
      TabOrder = 3
    end
    object Button7: TButton
      Left = 176
      Top = 72
      Width = 158
      Height = 81
      Caption = 'Apply Format'
      TabOrder = 4
      OnClick = Button7Click
    end
    object Button3: TButton
      Left = 12
      Top = 165
      Width = 75
      Height = 25
      Caption = 'Start Speaker'
      TabOrder = 5
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 93
      Top = 165
      Width = 75
      Height = 25
      Caption = 'Stop Speaker'
      Enabled = False
      TabOrder = 6
      OnClick = Button4Click
    end
    object TrackBar2: TTrackBar
      Left = 3
      Top = 220
      Width = 354
      Height = 25
      Max = 100
      Position = 100
      TabOrder = 7
      OnChange = TrackBar2Change
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 272
    Width = 728
    Height = 120
    Caption = 'Microphone Audio Injection'
    TabOrder = 2
    object Label13: TLabel
      Left = 10
      Top = 25
      Width = 55
      Height = 13
      Caption = 'Frequency:'
    end
    object Label14: TLabel
      Left = 130
      Top = 25
      Width = 69
      Height = 13
      Caption = 'Duration (ms):'
    end
    object Label17: TLabel
      Left = 10
      Top = 85
      Width = 81
      Height = 13
      Caption = 'Inject Vol: 100%'
    end
    object Edit1: TEdit
      Left = 10
      Top = 44
      Width = 60
      Height = 21
      TabOrder = 0
      Text = '800'
    end
    object Edit2: TEdit
      Left = 130
      Top = 44
      Width = 60
      Height = 21
      TabOrder = 1
      Text = '1000'
    end
    object Button8: TButton
      Left = 220
      Top = 25
      Width = 120
      Height = 25
      Caption = 'Inject Audio File'
      TabOrder = 2
      OnClick = Button8Click
    end
    object Button9: TButton
      Left = 220
      Top = 55
      Width = 120
      Height = 25
      Caption = 'Inject Beep'
      TabOrder = 3
      OnClick = Button9Click
    end
    object Button10: TButton
      Left = 350
      Top = 25
      Width = 120
      Height = 25
      Caption = 'Stop Injection'
      TabOrder = 4
      OnClick = Button10Click
    end
    object Button11: TButton
      Left = 350
      Top = 55
      Width = 120
      Height = 25
      Caption = 'Test Injection'
      TabOrder = 5
      OnClick = Button11Click
    end
    object CheckBox4: TCheckBox
      Left = 480
      Top = 25
      Width = 120
      Height = 17
      Caption = 'Loop Audio Files'
      TabOrder = 6
    end
    object TrackBar3: TTrackBar
      Left = 100
      Top = 85
      Width = 320
      Height = 25
      Max = 100
      Position = 100
      TabOrder = 7
      OnChange = TrackBar3Change
    end
  end
  object Button5: TButton
    Left = 8
    Top = 400
    Width = 728
    Height = 30
    Caption = 'Refresh All Devices'
    TabOrder = 3
    OnClick = Button5Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 440
    Width = 728
    Height = 130
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object MicInput1: TMicInput
    OnDataReceivedBytes = MicInput1DataReceivedBytes
    Left = 656
    Top = 280
  end
  object SpeakerOutput1: TSpeakerOutput
    Left = 656
    Top = 344
  end
  object OpenDialog1: TOpenDialog
    Filter = 'WAV Files (*.wav)|*.wav|All Files (*.*)|*.*'
    Title = 'Select Audio File'
    Left = 656
    Top = 464
  end
end
