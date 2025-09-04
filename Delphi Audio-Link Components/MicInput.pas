unit MicInput;

interface

uses
  Windows, Classes, SysUtils, Controls, ComObj, ActiveX, Math, MMSystem;

const
  CLSID_MMDeviceEnumerator: TGUID = '{BCDE0395-E52F-467C-8E3D-C4579291692E}';
  IID_IMMDeviceEnumerator: TGUID = '{A95664D2-9614-4F35-A746-DE8DB63617E6}';
  IID_IAudioClient: TGUID = '{1CB9AD4C-DBFA-4c32-B178-C2F568A703B2}';
  IID_IAudioCaptureClient: TGUID = '{C8ADBD64-E71E-48a0-A4DE-185C395CD317}';
  IID_IPropertyStore: TGUID = '{886d8eeb-8cf2-4446-8d02-cdba1dbdcf99}';

  AUDCLNT_SHAREMODE_SHARED = 0;
  AUDCLNT_STREAMFLAGS_EVENTCALLBACK = $00040000;
  AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM = $80000000;
  AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY = $04000000;
  REFTIMES_PER_SEC = 10000000;

  eCapture = 1;
  DEVICE_STATE_ACTIVE = $00000001;
  STGM_READ = $00000000;

type
  REFERENCE_TIME = Int64;

  PWaveFormatEx = ^TWaveFormatEx;
  TWaveFormatEx = record
    wFormatTag: Word;
    nChannels: Word;
    nSamplesPerSec: DWORD;
    nAvgBytesPerSec: DWORD;
    nBlockAlign: Word;
    wBitsPerSample: Word;
    cbSize: Word;
  end;

  PROPERTYKEY = record
    fmtid: TGUID;
    pid: DWORD;
  end;

  IPropertyStore = interface(IUnknown)
    ['{886d8eeb-8cf2-4446-8d02-cdba1dbdcf99}']
    function GetCount(out cProps: DWORD): HRESULT; stdcall;
    function GetAt(iProp: DWORD; out pkey: PROPERTYKEY): HRESULT; stdcall;
    function GetValue(const key: PROPERTYKEY; out pv: TPropVariant): HRESULT; stdcall;
    function SetValue(const key: PROPERTYKEY; const propvar: TPropVariant): HRESULT; stdcall;
    function Commit: HRESULT; stdcall;
  end;

  IMMDevice = interface(IUnknown)
    ['{D666063F-1587-4E43-81F1-B948E807363F}']
    function Activate(const iid: TGUID; dwClsCtx: DWORD; pActivationParams: Pointer; out ppInterface): HRESULT; stdcall;
    function OpenPropertyStore(stgmAccess: DWORD; out ppProperties: IPropertyStore): HRESULT; stdcall;
    function GetId(out ppstrId: PWideChar): HRESULT; stdcall;
    function GetState(out pdwState: DWORD): HRESULT; stdcall;
  end;

  IMMDeviceCollection = interface(IUnknown)
    ['{0BD7A1BE-7A1A-44DB-8397-CC5392387B5E}']
    function GetCount(out pcDevices: UINT): HRESULT; stdcall;
    function Item(nDevice: UINT; out ppDevice: IMMDevice): HRESULT; stdcall;
  end;

  IMMDeviceEnumerator = interface(IUnknown)
    ['{A95664D2-9614-4F35-A746-DE8DB63617E6}']
    function EnumAudioEndpoints(dataFlow: Integer; dwStateMask: DWORD; out ppDevices: IMMDeviceCollection): HRESULT; stdcall;
    function GetDefaultAudioEndpoint(dataFlow: Integer; role: Integer; out ppEndpoint: IMMDevice): HRESULT; stdcall;
  end;

  IAudioClient = interface(IUnknown)
    ['{1CB9AD4C-DBFA-4c32-B178-C2F568A703B2}']
    function Initialize(ShareMode: Integer; StreamFlags: DWORD; hnsBufferDuration: REFERENCE_TIME; hnsPeriodicity: REFERENCE_TIME; pFormat: PWaveFormatEx; AudioSessionGuid: PGuid): HRESULT; stdcall;
    function GetBufferSize(out pNumBufferFrames: UINT32): HRESULT; stdcall;
    function GetStreamLatency(out phnsLatency: REFERENCE_TIME): HRESULT; stdcall;
    function GetCurrentPadding(out pNumPaddingFrames: UINT32): HRESULT; stdcall;
    function IsFormatSupported(ShareMode: Integer; pFormat: PWaveFormatEx; ppClosestMatch: PWaveFormatEx): HRESULT; stdcall;
    function GetMixFormat(out ppDeviceFormat: PWaveFormatEx): HRESULT; stdcall;
    function GetDevicePeriod(out phnsDefaultDevicePeriod, phnsMinimumDevicePeriod: REFERENCE_TIME): HRESULT; stdcall;
    function Start: HRESULT; stdcall;
    function Stop: HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function SetEventHandle(eventHandle: THandle): HRESULT; stdcall;
    function GetService(const riid: TGUID; out ppv): HRESULT; stdcall;
  end;

  IAudioCaptureClient = interface(IUnknown)
    ['{C8ADBD64-E71E-48a0-A4DE-185C395CD317}']
    function GetBuffer(out ppData: PByte; out pNumFramesToRead: UINT32; out pdwFlags: DWORD; out pu64DevicePosition, pu64QPCPosition: UINT64): HRESULT; stdcall;
    function ReleaseBuffer(NumFramesRead: UINT32): HRESULT; stdcall;
    function GetNextPacketSize(out pNumFramesInNextPacket: UINT32): HRESULT; stdcall;
  end;

  TMicDevice = record
    ID: Integer;
    Name: string;
    DeviceInterface: IMMDevice;
  end;

  TMicDeviceArray = array of TMicDevice;
  TOnDataReceivedBytes = procedure(Sender: TObject; const Buffer: TBytes) of object;

  // NEW: Audio injection system for virtual microphone
  TAudioInjectionBuffer = class
  private
    FBuffer: TBytes;
    FPosition: Integer;
    FActive: Boolean;
    FLoop: Boolean;
  public
    constructor Create;
    procedure LoadFromBytes(const Data: TBytes; Loop: Boolean = False);
    procedure LoadFromFile(const FileName: string; Loop: Boolean = False);
    procedure GenerateBeep(Frequency: Integer; Duration: Integer; SampleRate: Integer; Channels: Integer);
    function GetNextChunk(ChunkSize: Integer): TBytes;
    procedure Reset;
    procedure Stop;
    property Active: Boolean read FActive;
    property Loop: Boolean read FLoop write FLoop;
  end;

  TMicInput = class(TComponent)
  private
    FActive: Boolean;
    FDeviceID: Integer;
    FVolume: Integer;
    FSampleRate: Integer;
    FChannels: Integer;
    FBitsPerSample: Integer;
    FOnDataReceivedBytes: TOnDataReceivedBytes;

    // NEW: Virtual microphone mode
    FVirtualMicMode: Boolean;
    FMixInjectedAudio: Boolean;
    FInjectedVolume: Integer;

    FDeviceEnumerator: IMMDeviceEnumerator;
    FDevice: IMMDevice;
    FAudioClient: IAudioClient;
    FCaptureClient: IAudioCaptureClient;
    FWaveFormat: PWaveFormatEx;
    FDesiredFormat: PWaveFormatEx;
    FEventHandle: THandle;
    FCaptureThread: TThread;
    FDevices: TMicDeviceArray;
    FVolumeFactor: Single;
    FTerminateEvent: THandle;

    // NEW: Audio injection
    FInjectionBuffer: TAudioInjectionBuffer;
    FInjectedVolumeFactor: Single;

    procedure SetActive(Value: Boolean);
    procedure SetDeviceID(Value: Integer);
    procedure SetVolume(Value: Integer);
    procedure SetSampleRate(Value: Integer);
    procedure SetChannels(Value: Integer);
    procedure SetBitsPerSample(Value: Integer);
    procedure SetVirtualMicMode(Value: Boolean);
    procedure SetMixInjectedAudio(Value: Boolean);
    procedure SetInjectedVolume(Value: Integer);
    function GetDeviceName(Device: IMMDevice): string;
    function GetActualSampleRate: Integer;
    function GetActualChannels: Integer;
    function GetActualBitsPerSample: Integer;
    function CreateDesiredFormat: PWaveFormatEx;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetDevices: TMicDeviceArray;
    procedure RefreshDevices;

    // NEW: Audio injection methods for virtual microphone
    procedure InjectAudioFile(const FileName: string; Loop: Boolean = False);
    procedure InjectAudioBytes(const Data: TBytes; Loop: Boolean = False);
    procedure InjectBeep(Frequency: Integer; Duration: Integer);
    procedure StopInjection;

    property ActualSampleRate: Integer read GetActualSampleRate;
    property ActualChannels: Integer read GetActualChannels;
    property ActualBitsPerSample: Integer read GetActualBitsPerSample;

  published
    property Active: Boolean read FActive write SetActive default False;
    property DeviceID: Integer read FDeviceID write SetDeviceID default 0;
    property Volume: Integer read FVolume write SetVolume default 100;
    property SampleRate: Integer read FSampleRate write SetSampleRate default 44100;
    property Channels: Integer read FChannels write SetChannels default 2;
    property BitsPerSample: Integer read FBitsPerSample write SetBitsPerSample default 16;

    // NEW: Virtual microphone properties
    property VirtualMicMode: Boolean read FVirtualMicMode write SetVirtualMicMode default False;
    property MixInjectedAudio: Boolean read FMixInjectedAudio write SetMixInjectedAudio default True;
    property InjectedVolume: Integer read FInjectedVolume write SetInjectedVolume default 100;

    property OnDataReceivedBytes: TOnDataReceivedBytes read FOnDataReceivedBytes write FOnDataReceivedBytes;
  end;

  TCaptureThread = class(TThread)
  private
    FMicInput: TMicInput;
    FEvents: array[0..1] of THandle;
  public
    constructor Create(AMicInput: TMicInput);
    destructor Destroy; override;
    procedure Execute; override;
  end;

var
  PKEY_Device_FriendlyName: PROPERTYKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 14);

implementation

// TAudioInjectionBuffer Implementation
constructor TAudioInjectionBuffer.Create;
begin
  inherited Create;
  FActive := False;
  FPosition := 0;
  FLoop := False;
  SetLength(FBuffer, 0);
end;

procedure TAudioInjectionBuffer.LoadFromBytes(const Data: TBytes; Loop: Boolean = False);
begin
  FBuffer := Copy(Data);
  FPosition := 0;
  FLoop := Loop;
  FActive := Length(FBuffer) > 0;
end;

procedure TAudioInjectionBuffer.LoadFromFile(const FileName: string; Loop: Boolean = False);
var
  FileStream: TFileStream;
  WaveHeader: array[0..43] of Byte;
  DataSize: Integer;
  AudioData: TBytes;
begin
  if not FileExists(FileName) then Exit;

  try
    FileStream := TFileStream.Create(FileName, fmOpenRead);
    try
      // Simple WAV file reader - reads header and PCM data
      if FileStream.Size < 44 then Exit;

      FileStream.ReadBuffer(WaveHeader, 44);

      // Basic WAV validation
      if not ((WaveHeader[0] = Ord('R')) and (WaveHeader[1] = Ord('I')) and
              (WaveHeader[2] = Ord('F')) and (WaveHeader[3] = Ord('F'))) then Exit;

      if not ((WaveHeader[8] = Ord('W')) and (WaveHeader[9] = Ord('A')) and
              (WaveHeader[10] = Ord('V')) and (WaveHeader[11] = Ord('E'))) then Exit;

      // Get data size from header
      DataSize := PInteger(@WaveHeader[40])^;
      if DataSize <= 0 then Exit;

      // Read audio data
      SetLength(AudioData, DataSize);
      FileStream.ReadBuffer(AudioData[0], DataSize);

      LoadFromBytes(AudioData, Loop);
    finally
      FileStream.Free;
    end;
  except
    // Ignore file errors
  end;
end;

procedure TAudioInjectionBuffer.GenerateBeep(Frequency: Integer; Duration: Integer; SampleRate: Integer; Channels: Integer);
var
  SampleCount: Integer;
  I, J: Integer;
  Sample: SmallInt;
  Angle: Double;
begin
  SampleCount := (SampleRate * Duration * Channels) div 1000;
  SetLength(FBuffer, SampleCount * 2); // 16-bit samples

  for I := 0 to (SampleCount div Channels) - 1 do
  begin
    Angle := 2.0 * Pi * Frequency * I / SampleRate;
    Sample := Round(Sin(Angle) * 16000);

    for J := 0 to Channels - 1 do
    begin
      PSmallInt(@FBuffer[I * Channels * 2 + J * 2])^ := Sample;
    end;
  end;

  FPosition := 0;
  FLoop := False;
  FActive := True;
end;

function TAudioInjectionBuffer.GetNextChunk(ChunkSize: Integer): TBytes;
var
  AvailableBytes: Integer;
  ChunkBytes: Integer;
begin
  SetLength(Result, 0);

  if not FActive or (Length(FBuffer) = 0) then Exit;

  AvailableBytes := Length(FBuffer) - FPosition;

  if AvailableBytes <= 0 then
  begin
    if FLoop then
    begin
      FPosition := 0;
      AvailableBytes := Length(FBuffer);
    end
    else
    begin
      FActive := False;
      Exit;
    end;
  end;

  ChunkBytes := Min(ChunkSize, AvailableBytes);
  SetLength(Result, ChunkBytes);
  Move(FBuffer[FPosition], Result[0], ChunkBytes);

  Inc(FPosition, ChunkBytes);
end;

procedure TAudioInjectionBuffer.Reset;
begin
  FPosition := 0;
  if Length(FBuffer) > 0 then
    FActive := True;
end;

procedure TAudioInjectionBuffer.Stop;
begin
  FActive := False;
  FPosition := 0;
end;

// TMicInput Implementation
constructor TMicInput.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := False;
  FDeviceID := 0;
  FVolume := 100;
  FVolumeFactor := 1.0;
  FSampleRate := 44100;
  FChannels := 2;
  FBitsPerSample := 16;
  FVirtualMicMode := False;
  FMixInjectedAudio := True;
  FInjectedVolume := 100;
  FInjectedVolumeFactor := 1.0;
  FTerminateEvent := CreateEvent(nil, True, False, nil);
  FInjectionBuffer := TAudioInjectionBuffer.Create;
  CoInitialize(nil);
  // Device enumeration removed from constructor - now lazy loaded in GetDevices
end;

destructor TMicInput.Destroy;
begin
  Active := False;
  if FTerminateEvent <> 0 then
    CloseHandle(FTerminateEvent);
  if FDesiredFormat <> nil then
    CoTaskMemFree(FDesiredFormat);
  FInjectionBuffer.Free;
  CoUninitialize;
  inherited Destroy;
end;

function TMicInput.CreateDesiredFormat: PWaveFormatEx;
begin
  Result := CoTaskMemAlloc(SizeOf(TWaveFormatEx));
  if Result <> nil then
  begin
    ZeroMemory(Result, SizeOf(TWaveFormatEx));
    Result^.wFormatTag := 1; // PCM
    Result^.nChannels := FChannels;
    Result^.nSamplesPerSec := FSampleRate;
    Result^.wBitsPerSample := FBitsPerSample;
    Result^.nBlockAlign := (FChannels * FBitsPerSample) div 8;
    Result^.nAvgBytesPerSec := Result^.nSamplesPerSec * Result^.nBlockAlign;
    Result^.cbSize := 0;
  end;
end;

function TMicInput.GetActualSampleRate: Integer;
begin
  if Assigned(FWaveFormat) then
    Result := FWaveFormat^.nSamplesPerSec
  else
    Result := FSampleRate;
end;

function TMicInput.GetActualChannels: Integer;
begin
  if Assigned(FWaveFormat) then
    Result := FWaveFormat^.nChannels
  else
    Result := FChannels;
end;

function TMicInput.GetActualBitsPerSample: Integer;
begin
  if Assigned(FWaveFormat) then
    Result := FWaveFormat^.wBitsPerSample
  else
    Result := FBitsPerSample;
end;

procedure TMicInput.SetVolume(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if Value > 100 then Value := 100;
  FVolume := Value;
  FVolumeFactor := Value / 100.0;
end;

procedure TMicInput.SetInjectedVolume(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if Value > 100 then Value := 100;
  FInjectedVolume := Value;
  FInjectedVolumeFactor := Value / 100.0;
end;

procedure TMicInput.SetVirtualMicMode(Value: Boolean);
begin
  if FVirtualMicMode <> Value then
  begin
    FVirtualMicMode := Value;
    if FActive then
    begin
      Active := False;
      Active := True;
    end;
  end;
end;

procedure TMicInput.SetMixInjectedAudio(Value: Boolean);
begin
  FMixInjectedAudio := Value;
end;

procedure TMicInput.SetSampleRate(Value: Integer);
begin
  if FSampleRate <> Value then
  begin
    FSampleRate := Value;
    if FActive then
    begin
      Active := False;
      Active := True;
    end;
  end;
end;

procedure TMicInput.SetChannels(Value: Integer);
begin
  if FChannels <> Value then
  begin
    FChannels := Value;
    if FActive then
    begin
      Active := False;
      Active := True;
    end;
  end;
end;

procedure TMicInput.SetBitsPerSample(Value: Integer);
begin
  if FBitsPerSample <> Value then
  begin
    FBitsPerSample := Value;
    if FActive then
    begin
      Active := False;
      Active := True;
    end;
  end;
end;

procedure TMicInput.SetActive(Value: Boolean);
var
  hr: HRESULT;
  StreamFlags: DWORD;
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if FActive then
    begin
      // Start capture
      if FVirtualMicMode or ((FDeviceID >= 0) and (FDeviceID < Length(FDevices))) then
      begin
        ResetEvent(FTerminateEvent);

        if not FVirtualMicMode then
          FDevice := FDevices[FDeviceID].DeviceInterface;

        if FVirtualMicMode or SUCCEEDED(FDevice.Activate(IID_IAudioClient, CLSCTX_ALL, nil, FAudioClient)) then
        begin
          FDesiredFormat := CreateDesiredFormat;
          if FDesiredFormat <> nil then
          begin
            if not FVirtualMicMode then
            begin
              FEventHandle := CreateEvent(nil, False, False, nil);

              StreamFlags := AUDCLNT_STREAMFLAGS_EVENTCALLBACK or
                            AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM or
                            AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY;

              hr := FAudioClient.Initialize(AUDCLNT_SHAREMODE_SHARED, StreamFlags,
                                           REFTIMES_PER_SEC div 10, 0, FDesiredFormat, nil);

              if SUCCEEDED(hr) then
              begin
                FWaveFormat := FDesiredFormat;
                FDesiredFormat := nil;
              end
              else
              begin
                if SUCCEEDED(FAudioClient.GetMixFormat(FWaveFormat)) then
                begin
                  hr := FAudioClient.Initialize(AUDCLNT_SHAREMODE_SHARED, StreamFlags,
                                               REFTIMES_PER_SEC div 10, 0, FWaveFormat, nil);
                end;
              end;

              if SUCCEEDED(hr) then
              begin
                if SUCCEEDED(FAudioClient.SetEventHandle(FEventHandle)) then
                begin
                  if SUCCEEDED(FAudioClient.GetService(IID_IAudioCaptureClient, FCaptureClient)) then
                  begin
                    if SUCCEEDED(FAudioClient.Start) then
                    begin
                      FCaptureThread := TCaptureThread.Create(Self);
                    end;
                  end;
                end;
              end;
            end
            else
            begin
              // Virtual microphone mode - just use desired format
              FWaveFormat := FDesiredFormat;
              FDesiredFormat := nil;
              FCaptureThread := TCaptureThread.Create(Self);
            end;
          end;
        end;
      end;
    end
    else
    begin
      SetEvent(FTerminateEvent);

      if Assigned(FCaptureThread) then
      begin
        FCaptureThread.Terminate;
        FCaptureThread := nil;
      end;

      if Assigned(FAudioClient) then
        FAudioClient.Stop;

      FCaptureClient := nil;
      FAudioClient := nil;

      if FWaveFormat <> nil then
      begin
        CoTaskMemFree(FWaveFormat);
        FWaveFormat := nil;
      end;

      if FDesiredFormat <> nil then
      begin
        CoTaskMemFree(FDesiredFormat);
        FDesiredFormat := nil;
      end;

      if FEventHandle <> 0 then
      begin
        CloseHandle(FEventHandle);
        FEventHandle := 0;
      end;

      FDevice := nil;
    end;
  end;
end;

procedure TMicInput.SetDeviceID(Value: Integer);
begin
  if FDeviceID <> Value then
  begin
    FDeviceID := Value;
    if FActive and not FVirtualMicMode then
    begin
      Active := False;
      Sleep(100);
      Active := True;
    end;
  end;
end;

function TMicInput.GetDeviceName(Device: IMMDevice): string;
var
  PropertyStore: IPropertyStore;
  PropVariant: TPropVariant;
begin
  Result := 'Unknown Device';

  if SUCCEEDED(Device.OpenPropertyStore(STGM_READ, PropertyStore)) then
  begin
    PropVariantInit(PropVariant);
    try
      if SUCCEEDED(PropertyStore.GetValue(PKEY_Device_FriendlyName, PropVariant)) then
      begin
        if PropVariant.vt = VT_LPWSTR then
          Result := WideCharToString(PropVariant.pwszVal);
      end;
    finally
      PropVariantClear(PropVariant);
    end;
  end;
end;

function TMicInput.GetDevices: TMicDeviceArray;
var
  DeviceCollection: IMMDeviceCollection;
  Device: IMMDevice;
  Count, I: UINT;
begin
  SetLength(Result, 0);

  // Lazy load IMMDeviceEnumerator creation
  if not Assigned(FDeviceEnumerator) then
  begin
    if not SUCCEEDED(CoCreateInstance(CLSID_MMDeviceEnumerator, nil, CLSCTX_ALL, IID_IMMDeviceEnumerator, FDeviceEnumerator)) then
      Exit;
  end;

  if SUCCEEDED(FDeviceEnumerator.EnumAudioEndpoints(eCapture, DEVICE_STATE_ACTIVE, DeviceCollection)) then
  begin
    if SUCCEEDED(DeviceCollection.GetCount(Count)) then
    begin
      SetLength(Result, Count);

      for I := 0 to Count - 1 do
      begin
        if SUCCEEDED(DeviceCollection.Item(I, Device)) then
        begin
          Result[I].ID := I;
          Result[I].Name := GetDeviceName(Device);
          Result[I].DeviceInterface := Device;
        end;
      end;
    end;
  end;

  FDevices := Result;
end;

procedure TMicInput.RefreshDevices;
begin
  GetDevices;
end;

// NEW: Audio injection methods
procedure TMicInput.InjectAudioFile(const FileName: string; Loop: Boolean = False);
begin
  FInjectionBuffer.LoadFromFile(FileName, Loop);
end;

procedure TMicInput.InjectAudioBytes(const Data: TBytes; Loop: Boolean = False);
begin
  FInjectionBuffer.LoadFromBytes(Data, Loop);
end;

procedure TMicInput.InjectBeep(Frequency: Integer; Duration: Integer);
begin
  FInjectionBuffer.GenerateBeep(Frequency, Duration, GetActualSampleRate, GetActualChannels);
end;

procedure TMicInput.StopInjection;
begin
  FInjectionBuffer.Stop;
end;

// TCaptureThread - Enhanced with COM initialization and auto-cleanup
constructor TCaptureThread.Create(AMicInput: TMicInput);
begin
  FMicInput := AMicInput;
  if not FMicInput.FVirtualMicMode then
  begin
    FEvents[0] := FMicInput.FEventHandle;
    FEvents[1] := FMicInput.FTerminateEvent;
  end
  else
  begin
    FEvents[0] := 0;
    FEvents[1] := FMicInput.FTerminateEvent;
  end;
  FreeOnTerminate := True;
  inherited Create(False);
end;

destructor TCaptureThread.Destroy;
begin
  CoUninitialize;
  inherited Destroy;
end;

procedure TCaptureThread.Execute;
var
  PacketLength: UINT32;
  Buffer: PByte;
  NumFramesAvailable: UINT32;
  Flags: DWORD;
  DevicePosition, QPCPosition: UINT64;
  BytesToRead: Integer;
  WaitResult: DWORD;
  Sample16: PSmallInt;
  Sample32: PSingle;
  I: Integer;
  TempSample: Integer;
  AudioBytes: TBytes;
  InjectedChunk: TBytes;
  ChunkSize: Integer;
  MicSample16, InjSample16: PSmallInt;
  MixedSample: Integer;
begin
  CoInitialize(nil);

  ChunkSize := (FMicInput.GetActualSampleRate * FMicInput.GetActualChannels * 2) div 20; // 50ms chunks

  while not Terminated do
  begin
    if FMicInput.FVirtualMicMode then
    begin
      // Virtual microphone mode - generate from injection buffer only
      WaitResult := WaitForSingleObject(FEvents[1], 50);
      if WaitResult = WAIT_OBJECT_0 then Break;

      InjectedChunk := FMicInput.FInjectionBuffer.GetNextChunk(ChunkSize);

      if Length(InjectedChunk) > 0 then
      begin
        // Apply injected volume
        if FMicInput.FInjectedVolume <> 100 then
        begin
          InjSample16 := PSmallInt(@InjectedChunk[0]);
          for I := 0 to (Length(InjectedChunk) div 2) - 1 do
          begin
            TempSample := Round(InjSample16^ * FMicInput.FInjectedVolumeFactor);
            if TempSample > 32767 then TempSample := 32767
            else if TempSample < -32768 then TempSample := -32768;
            InjSample16^ := TempSample;
            Inc(InjSample16);
          end;
        end;

        if Assigned(FMicInput.FOnDataReceivedBytes) then
          FMicInput.FOnDataReceivedBytes(FMicInput, InjectedChunk);
      end
      else
      begin
        // No injected audio, send silence
        SetLength(AudioBytes, ChunkSize);
        FillChar(AudioBytes[0], Length(AudioBytes), 0);
        if Assigned(FMicInput.FOnDataReceivedBytes) then
          FMicInput.FOnDataReceivedBytes(FMicInput, AudioBytes);
      end;
    end
    else
    begin
      // Normal microphone mode with optional audio injection mixing
      WaitResult := WaitForMultipleObjects(2, @FEvents[0], False, 50);

      case WaitResult of
        WAIT_OBJECT_0: // Audio ready
        begin
          if SUCCEEDED(FMicInput.FCaptureClient.GetNextPacketSize(PacketLength)) then
          begin
            while (PacketLength <> 0) and not Terminated do
            begin
              if SUCCEEDED(FMicInput.FCaptureClient.GetBuffer(Buffer, NumFramesAvailable, Flags, DevicePosition, QPCPosition)) then
              begin
                BytesToRead := NumFramesAvailable * FMicInput.FWaveFormat^.nBlockAlign;

                if (Buffer <> nil) and (BytesToRead > 0) and Assigned(FMicInput.FOnDataReceivedBytes) then
                begin
                  SetLength(AudioBytes, BytesToRead);
                  Move(Buffer^, AudioBytes[0], BytesToRead);

                  // Apply microphone volume
                  if FMicInput.FVolume <> 100 then
                  begin
                    if FMicInput.FWaveFormat^.wBitsPerSample = 16 then
                    begin
                      Sample16 := PSmallInt(@AudioBytes[0]);
                      for I := 0 to (BytesToRead div 2) - 1 do
                      begin
                        TempSample := Round(Sample16^ * FMicInput.FVolumeFactor);
                        if TempSample > 32767 then TempSample := 32767
                        else if TempSample < -32768 then TempSample := -32768;
                        Sample16^ := TempSample;
                        Inc(Sample16);
                      end;
                    end
                    else if FMicInput.FWaveFormat^.wBitsPerSample = 32 then
                    begin
                      Sample32 := PSingle(@AudioBytes[0]);
                      for I := 0 to (BytesToRead div 4) - 1 do
                      begin
                        Sample32^ := Sample32^ * FMicInput.FVolumeFactor;
                        Inc(Sample32);
                      end;
                    end;
                  end;

                  // Mix with injected audio if enabled
                  if FMicInput.FMixInjectedAudio and FMicInput.FInjectionBuffer.Active then
                  begin
                    InjectedChunk := FMicInput.FInjectionBuffer.GetNextChunk(BytesToRead);

                    if Length(InjectedChunk) > 0 then
                    begin
                      // Mix 16-bit samples
                      if FMicInput.FWaveFormat^.wBitsPerSample = 16 then
                      begin
                        MicSample16 := PSmallInt(@AudioBytes[0]);
                        InjSample16 := PSmallInt(@InjectedChunk[0]);

                        for I := 0 to Min(BytesToRead div 2, Length(InjectedChunk) div 2) - 1 do
                        begin
                          MixedSample := MicSample16^ + Round(InjSample16^ * FMicInput.FInjectedVolumeFactor);
                          if MixedSample > 32767 then MixedSample := 32767
                          else if MixedSample < -32768 then MixedSample := -32768;
                          MicSample16^ := MixedSample;
                          Inc(MicSample16);
                          Inc(InjSample16);
                        end;
                      end;
                    end;
                  end;

                  FMicInput.FOnDataReceivedBytes(FMicInput, AudioBytes);
                end;

                FMicInput.FCaptureClient.ReleaseBuffer(NumFramesAvailable);
              end;

              if FAILED(FMicInput.FCaptureClient.GetNextPacketSize(PacketLength)) then
                Break;
            end;
          end;
        end;

        WAIT_OBJECT_0 + 1: // Terminate signal
          Break;

        WAIT_TIMEOUT:
          Continue;

      else
        Break;
      end;
    end;
  end;
end;

end.
