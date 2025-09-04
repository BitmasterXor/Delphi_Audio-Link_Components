unit SpeakerOutput;

interface

uses
  Windows, Classes, SysUtils, Controls, ComObj, ActiveX, Math;

const
  CLSID_MMDeviceEnumerator: TGUID = '{BCDE0395-E52F-467C-8E3D-C4579291692E}';
  IID_IMMDeviceEnumerator: TGUID = '{A95664D2-9614-4F35-A746-DE8DB63617E6}';
  IID_IAudioClient: TGUID = '{1CB9AD4C-DBFA-4c32-B178-C2F568A703B2}';
  IID_IAudioRenderClient: TGUID = '{F294ACFC-3146-4483-A7BF-ADDCA7C260E2}';
  IID_IPropertyStore: TGUID = '{886d8eeb-8cf2-4446-8d02-cdba1dbdcf99}';

  AUDCLNT_SHAREMODE_SHARED = 0;
  AUDCLNT_STREAMFLAGS_EVENTCALLBACK = $00040000;
  AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM = $80000000;
  AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY = $04000000;
  REFTIMES_PER_SEC = 10000000;

  eRender = 0;
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

  IAudioRenderClient = interface(IUnknown)
    ['{F294ACFC-3146-4483-A7BF-ADDCA7C260E2}']
    function GetBuffer(NumFramesRequested: UINT32; out ppData: PByte): HRESULT; stdcall;
    function ReleaseBuffer(NumFramesWritten: UINT32; dwFlags: DWORD): HRESULT; stdcall;
  end;

  TSpeakerDevice = record
    ID: Integer;
    Name: string;
    DeviceInterface: IMMDevice;
  end;

  TSpeakerDeviceArray = array of TSpeakerDevice;

  TSpeakerOutput = class(TComponent)
  private
    FActive: Boolean;
    FDeviceID: Integer;
    FVolume: Integer;
    FSampleRate: Integer;
    FChannels: Integer;
    FBitsPerSample: Integer;

    FDeviceEnumerator: IMMDeviceEnumerator;
    FDevice: IMMDevice;
    FAudioClient: IAudioClient;
    FRenderClient: IAudioRenderClient;
    FWaveFormat: PWaveFormatEx;
    FDesiredFormat: PWaveFormatEx;
    FEventHandle: THandle;
    FIsPlaying: Boolean;
    FDevices: TSpeakerDeviceArray;
    FBufferFrameCount: UINT32;
    FVolumeFactor: Single;

    procedure SetActive(Value: Boolean);
    procedure SetDeviceID(Value: Integer);
    procedure SetVolume(Value: Integer);
    procedure SetSampleRate(Value: Integer);
    procedure SetChannels(Value: Integer);
    procedure SetBitsPerSample(Value: Integer);
    function GetDeviceName(Device: IMMDevice): string;
    function GetActualSampleRate: Integer;
    function GetActualChannels: Integer;
    function GetActualBitsPerSample: Integer;
    function CreateDesiredFormat: PWaveFormatEx;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetDevices: TSpeakerDeviceArray;
    procedure RefreshDevices;

    // Core playback methods for real-time streaming
    procedure PlayBufferBytes(const Buffer: TBytes);
    procedure PlayBuffer(Buffer: PByte; Size: Integer);

    property IsPlaying: Boolean read FIsPlaying;
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
  end;

var
  PKEY_Device_FriendlyName: PROPERTYKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 14);

implementation

constructor TSpeakerOutput.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := False;
  FDeviceID := 0;
  FVolume := 100;
  FVolumeFactor := 1.0;
  FSampleRate := 44100;
  FChannels := 2;
  FBitsPerSample := 16;
  FIsPlaying := False;
  CoInitialize(nil);
  RefreshDevices;
end;

destructor TSpeakerOutput.Destroy;
begin
  Active := False;
  if FDesiredFormat <> nil then
    CoTaskMemFree(FDesiredFormat);
  CoUninitialize;
  inherited Destroy;
end;

function TSpeakerOutput.CreateDesiredFormat: PWaveFormatEx;
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

procedure TSpeakerOutput.SetActive(Value: Boolean);
var
  hr: HRESULT;
  StreamFlags: DWORD;
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if FActive then
    begin
      // Start playback
      if (FDeviceID >= 0) and (FDeviceID < Length(FDevices)) then
      begin
        FDevice := FDevices[FDeviceID].DeviceInterface;

        if SUCCEEDED(FDevice.Activate(IID_IAudioClient, CLSCTX_ALL, nil, FAudioClient)) then
        begin
          // Create our desired format
          FDesiredFormat := CreateDesiredFormat;
          if FDesiredFormat <> nil then
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
                if SUCCEEDED(FAudioClient.GetBufferSize(FBufferFrameCount)) then
                begin
                  if SUCCEEDED(FAudioClient.GetService(IID_IAudioRenderClient, FRenderClient)) then
                  begin
                    if SUCCEEDED(FAudioClient.Start) then
                    begin
                      FIsPlaying := True;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end
    else
    begin
      FIsPlaying := False;

      if Assigned(FAudioClient) then
        FAudioClient.Stop;

      FRenderClient := nil;
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

procedure TSpeakerOutput.SetDeviceID(Value: Integer);
begin
  if FDeviceID <> Value then
  begin
    FDeviceID := Value;
    if FActive then
    begin
      Active := False;
      Sleep(100);
      Active := True;
    end;
  end;
end;

procedure TSpeakerOutput.SetVolume(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if Value > 100 then Value := 100;
  FVolume := Value;
  FVolumeFactor := Value / 100.0;
end;

procedure TSpeakerOutput.SetSampleRate(Value: Integer);
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

procedure TSpeakerOutput.SetChannels(Value: Integer);
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

procedure TSpeakerOutput.SetBitsPerSample(Value: Integer);
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

function TSpeakerOutput.GetActualSampleRate: Integer;
begin
  if Assigned(FWaveFormat) then
    Result := FWaveFormat^.nSamplesPerSec
  else
    Result := FSampleRate;
end;

function TSpeakerOutput.GetActualChannels: Integer;
begin
  if Assigned(FWaveFormat) then
    Result := FWaveFormat^.nChannels
  else
    Result := FChannels;
end;

function TSpeakerOutput.GetActualBitsPerSample: Integer;
begin
  if Assigned(FWaveFormat) then
    Result := FWaveFormat^.wBitsPerSample
  else
    Result := FBitsPerSample;
end;

function TSpeakerOutput.GetDeviceName(Device: IMMDevice): string;
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

function TSpeakerOutput.GetDevices: TSpeakerDeviceArray;
var
  DeviceCollection: IMMDeviceCollection;
  Device: IMMDevice;
  Count, I: UINT;
begin
  SetLength(Result, 0);

  if not Assigned(FDeviceEnumerator) then
    Exit;

  if SUCCEEDED(FDeviceEnumerator.EnumAudioEndpoints(eRender, DEVICE_STATE_ACTIVE, DeviceCollection)) then
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

procedure TSpeakerOutput.RefreshDevices;
begin
  if SUCCEEDED(CoCreateInstance(CLSID_MMDeviceEnumerator, nil, CLSCTX_ALL, IID_IMMDeviceEnumerator, FDeviceEnumerator)) then
    GetDevices;
end;

// Core real-time audio playback methods
procedure TSpeakerOutput.PlayBufferBytes(const Buffer: TBytes);
var
  RenderBuffer: PByte;
  FramesToWrite: UINT32;
  BytesPerFrame: Integer;
  Padding: UINT32;
  AvailableFrames: UINT32;
  Sample16: PSmallInt;
  Sample32: PSingle;
  I: Integer;
  TempSample: Integer;
  BufferSize: Integer;
begin
  if not FIsPlaying or not Assigned(FRenderClient) or not Assigned(FWaveFormat) or (Length(Buffer) = 0) then
    Exit;

  BytesPerFrame := FWaveFormat^.nBlockAlign;
  if BytesPerFrame = 0 then Exit;

  BufferSize := Length(Buffer);
  FramesToWrite := BufferSize div BytesPerFrame;

  if FramesToWrite > FBufferFrameCount then
    FramesToWrite := FBufferFrameCount;

  if SUCCEEDED(FAudioClient.GetCurrentPadding(Padding)) then
  begin
    AvailableFrames := FBufferFrameCount - Padding;

    if AvailableFrames >= FramesToWrite then
    begin
      if SUCCEEDED(FRenderClient.GetBuffer(FramesToWrite, RenderBuffer)) then
      begin
        var BytesToWrite := FramesToWrite * BytesPerFrame;
        if BytesToWrite > BufferSize then
          BytesToWrite := BufferSize;

        Move(Buffer[0], RenderBuffer^, BytesToWrite);

        // Apply volume control if needed
        if FVolume <> 100 then
        begin
          if FWaveFormat^.wBitsPerSample = 16 then
          begin
            Sample16 := PSmallInt(RenderBuffer);
            for I := 0 to (BytesToWrite div 2) - 1 do
            begin
              TempSample := Round(Sample16^ * FVolumeFactor);
              if TempSample > 32767 then TempSample := 32767
              else if TempSample < -32768 then TempSample := -32768;
              Sample16^ := TempSample;
              Inc(Sample16);
            end;
          end
          else if FWaveFormat^.wBitsPerSample = 32 then
          begin
            Sample32 := PSingle(RenderBuffer);
            for I := 0 to (BytesToWrite div 4) - 1 do
            begin
              Sample32^ := Sample32^ * FVolumeFactor;
              Inc(Sample32);
            end;
          end;
        end;

        FRenderClient.ReleaseBuffer(FramesToWrite, 0);
      end;
    end;
  end;
end;

procedure TSpeakerOutput.PlayBuffer(Buffer: PByte; Size: Integer);
var
  AudioBytes: TBytes;
begin
  if (Buffer = nil) or (Size <= 0) then Exit;

  // Convert PByte to TBytes and use the new method
  SetLength(AudioBytes, Size);
  Move(Buffer^, AudioBytes[0], Size);
  PlayBufferBytes(AudioBytes);
end;

end.
