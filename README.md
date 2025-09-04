# 🎵 Delphi_AudioLink
**Professional Audio Input/Output Components for Delphi with Network Streaming & Audio Injection**

<div align="center">

![Version](https://img.shields.io/badge/Version-2.0-blue?style=for-the-badge)
![Delphi](https://img.shields.io/badge/Delphi-XE2%2B-red?style=for-the-badge)
![Platform](https://img.shields.io/badge/Platform-Windows-green?style=for-the-badge)
![License](https://img.shields.io/badge/License-MIT-orange?style=for-the-badge)

*High-performance WASAPI-based audio components with intelligent format conversion, network streaming capabilities, and advanced audio injection system*

</div>

---

## 🚀 Overview
![Basic Preview](Preview2.png)
![Network Preview](Preview1.png)

Delphi_AudioLink provides two powerful, professional-grade audio components built on Windows Audio Session API (WASAPI) with automatic format conversion, intelligent volume control, network-ready TBytes streaming, and a revolutionary audio injection system. Perfect for VoIP applications, audio streaming, recording software, virtual microphones, and real-time audio processing.

### 🎯 What's Included

- **🎤 TMicInput Component** - Professional microphone capture with virtual microphone mode and audio injection
- **🔊 TSpeakerOutput Component** - High-quality audio playback with intelligent buffering
- **🎛️ Audio Injection System** - Inject WAV files, generated tones, or custom audio into microphone stream
- **🤖 Virtual Microphone Mode** - Pure injection mode bypassing hardware microphone
- **📦 Ready-to-Install Package** - Complete component package for Delphi IDE
- **🌐 Network-Ready Design** - TBytes-based callbacks for seamless network integration

---

## 🏗️ Architecture

```mermaid
graph TD
    A[Microphone Device] --> B[TMicInput Component]
    M[Audio Injection System] --> B
    N[WAV Files] --> M
    O[Tone Generator] --> M
    P[Custom Audio] --> M
    B --> C{Format Conversion}
    C --> D[Volume Processing]
    D --> Q{Mix/Replace Mode}
    Q --> E[TBytes Callback]
    E --> F[Network Stream / Local Processing]
    
    G[Network Stream / Audio Data] --> H[TSpeakerOutput Component]
    H --> I{Format Matching}
    I --> J[Volume Control]
    J --> K[WASAPI Rendering]
    K --> L[Speaker Device]
    
    R[Virtual Mic Mode] -.-> B
    R -.-> S[Hardware Mic Bypass]
```

---

## ⭐ Key Features

### 🎤 **TMicInput Component - Enhanced**
- **WASAPI Integration** - Direct Windows Audio Session API access for minimal latency
- **Audio Injection System** - Inject WAV files, tones, or custom audio into microphone stream
- **Virtual Microphone Mode** - Pure injection mode without hardware microphone
- **Mix & Replace Modes** - Mix injected audio with microphone or replace completely
- **Independent Volume Controls** - Separate volume control for microphone and injected audio
- **Automatic Format Conversion** - Intelligent PCM format handling with AUTOCONVERTPCM flags
- **Multi-Device Support** - Enumerate and select from all available microphone devices
- **Network-Ready Output** - TBytes-based callback system for network streaming
- **Thread-Safe Operation** - Non-blocking audio capture with proper synchronization

### 🎛️ **Audio Injection System - NEW**
- **WAV File Injection** - Load and inject WAV files with optional looping
- **Tone Generation** - Generate sine wave tones at any frequency and duration
- **Custom Audio Injection** - Inject any TBytes audio data programmatically
- **Volume Control** - Independent volume control for injected audio (0-100%)
- **Loop Support** - Continuous playback of injected audio files
- **Format Matching** - Automatic format conversion to match microphone settings
- **Real-Time Control** - Start, stop, and switch injected audio on-the-fly

### 🤖 **Virtual Microphone Mode - NEW**
- **Hardware Bypass** - Complete bypass of physical microphone
- **Pure Injection** - Stream only injected audio content
- **Network Streaming** - Stream injected content over network connections
- **Format Flexibility** - Define custom audio formats for virtual microphone
- **Perfect for Testing** - Ideal for automated testing and audio simulation

### 🔊 **TSpeakerOutput Component**
- **High-Quality Playback** - WASAPI render client with automatic format matching
- **Intelligent Buffering** - Smart buffer management prevents audio dropouts
- **Real-Time Processing** - Non-blocking audio playback with minimal latency
- **Volume Control** - Precise volume adjustment at correct bit depth
- **Network Integration** - Direct TBytes buffer playback for network audio
- **Device Selection** - Full speaker device enumeration and selection

### 🛠️ **Technical Excellence**
- **Format Flexibility** - Automatic conversion between 16/32-bit PCM formats
- **Memory Efficient** - Smart buffer allocation and proper cleanup
- **Error Resilient** - Comprehensive error handling and graceful degradation
- **Performance Optimized** - Direct memory operations for maximum throughput
- **Thread-Safe Design** - Proper synchronization for multi-threaded applications

---

## 📦 Installation

### Prerequisites
- **Delphi XE2 or later**
- **Windows Vista+** (WASAPI requirement Vista SP1 I believe shipped with it)
- **NetCom7 components** (optional) - For network streaming examples

---

## 🚀 Usage Examples

### Basic Microphone Capture with Audio Injection
```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configure microphone
  MicInput1.SampleRate := 44100;
  MicInput1.Channels := 2;
  MicInput1.BitsPerSample := 16;
  MicInput1.Volume := 80;
  MicInput1.InjectedVolume := 70;
  MicInput1.MixInjectedAudio := True;  // Mix with microphone
  MicInput1.OnDataReceivedBytes := OnMicData;
  MicInput1.Active := True;
  
  // Inject a beep tone
  MicInput1.InjectBeep(800, 1000);  // 800Hz for 1000ms
end;

procedure TForm1.OnMicData(Sender: TObject; const Buffer: TBytes);
begin
  // Buffer contains microphone + injected audio mixed together
  ProcessAudioData(Buffer);
end;
```

### Virtual Microphone Mode
```pascal
procedure TForm1.SetupVirtualMicrophone;
begin
  // Enable virtual microphone mode (no hardware microphone)
  MicInput1.VirtualMicMode := True;
  MicInput1.MixInjectedAudio := False;  // Not needed in virtual mode
  MicInput1.SampleRate := 44100;
  MicInput1.Channels := 2;
  MicInput1.BitsPerSample := 16;
  MicInput1.Active := True;
  
  // Inject audio file that will loop continuously
  MicInput1.InjectAudioFile('C:\Audio\background_music.wav', True);
end;
```

### Audio File Injection
```pascal
procedure TForm1.InjectAudioFile;
begin
  // Inject a WAV file with looping
  MicInput1.InjectAudioFile('C:\Sounds\notification.wav', True);
  
  // Or inject without looping
  MicInput1.InjectAudioFile('C:\Sounds\alert.wav', False);
  
  // Stop injection
  MicInput1.StopInjection;
end;
```

### Network Audio Streaming with Injection
```pascal
// Sender Side - Stream microphone + injected audio
procedure TForm1.StreamMicrophoneToNetwork;
begin
  // Configure for network streaming
  MicInput1.VirtualMicMode := False;    // Use hardware mic + injection
  MicInput1.MixInjectedAudio := True;   // Mix injected with microphone
  MicInput1.InjectedVolume := 50;       // Lower injection volume
  
  MicInput1.OnDataReceivedBytes := procedure(Sender: TObject; const Buffer: TBytes)
  begin
    // Send mixed audio data over network
    NetComClient.SendBytes(Buffer);
  end;
  MicInput1.Active := True;
  
  // Inject background music
  MicInput1.InjectAudioFile('C:\Music\background.wav', True);
end;

// Receiver Side - Play network audio
procedure TForm1.OnNetworkAudioReceived(const AudioData: TBytes);
begin
  SpeakerOutput1.PlayBufferBytes(AudioData);
end;
```

### Advanced Injection Control
```pascal
procedure TForm1.AdvancedInjectionExample;
begin
  // Test injection system
  MicInput1.Active := True;
  
  // Inject different tones in sequence
  MicInput1.InjectBeep(400, 500);   // Low tone
  Sleep(600);
  MicInput1.InjectBeep(800, 500);   // Medium tone  
  Sleep(600);
  MicInput1.InjectBeep(1200, 500);  // High tone
  
  // Inject custom audio data
  var CustomAudio: TBytes;
  GenerateCustomAudio(CustomAudio);  // Your custom audio generation
  MicInput1.InjectAudioBytes(CustomAudio, False);
end;
```

### Device Enumeration and Selection
```pascal
procedure TForm1.RefreshAudioDevices;
var
  MicDevices: TMicDeviceArray;
  SpeakerDevices: TSpeakerDeviceArray;
  I: Integer;
begin
  // Get microphone devices
  MicDevices := MicInput1.GetDevices;
  ComboBoxMics.Clear;
  for I := 0 to Length(MicDevices) - 1 do
    ComboBoxMics.Items.Add(MicDevices[I].Name);

  // Get speaker devices  
  SpeakerDevices := SpeakerOutput1.GetDevices;
  ComboBoxSpeakers.Clear;
  for I := 0 to Length(SpeakerDevices) - 1 do
    ComboBoxSpeakers.Items.Add(SpeakerDevices[I].Name);
end;
```

### Real-Time Audio Processing with Injection
```pascal
procedure TForm1.SetupRealTimeAudioWithInjection;
begin
  // Configure for low-latency real-time processing
  MicInput1.SampleRate := 48000;
  MicInput1.Channels := 1;
  MicInput1.BitsPerSample := 16;
  MicInput1.MixInjectedAudio := True;  // Mix injection with microphone
  MicInput1.InjectedVolume := 30;      // Lower injection volume
  
  SpeakerOutput1.SampleRate := 48000;
  SpeakerOutput1.Channels := 1;
  SpeakerOutput1.BitsPerSample := 16;
  
  // Enable real-time audio loop with injection
  MicInput1.OnDataReceivedBytes := procedure(Sender: TObject; const Buffer: TBytes)
  begin
    // Apply audio processing (filters, effects, etc.)
    var ProcessedBuffer := ProcessAudio(Buffer);
    
    // Play immediately for real-time monitoring
    SpeakerOutput1.PlayBufferBytes(ProcessedBuffer);
  end;
  
  MicInput1.Active := True;
  SpeakerOutput1.Active := True;
  
  // Inject subtle background tone for testing
  MicInput1.InjectBeep(220, 5000);  // 220Hz for 5 seconds
end;
```

---

## 🎛️ Component Properties

### TMicInput Properties - Enhanced
| Property | Type | Default | Description |
|----------|------|---------|-------------|
| **Active** | Boolean | False | Start/stop audio capture |
| **DeviceID** | Integer | 0 | Selected microphone device index |
| **Volume** | Integer | 100 | Input volume (0-100%) |
| **SampleRate** | Integer | 44100 | Desired sample rate (Hz) |
| **Channels** | Integer | 2 | Audio channels (1=Mono, 2=Stereo) |
| **BitsPerSample** | Integer | 16 | Bit depth (16 or 32) |
| **VirtualMicMode** | Boolean | False | **NEW:** Enable virtual microphone (no hardware mic) |
| **MixInjectedAudio** | Boolean | True | **NEW:** Mix injected audio with microphone |
| **InjectedVolume** | Integer | 100 | **NEW:** Volume for injected audio (0-100%) |

### TMicInput Events
| Event | Description |
|-------|-------------|
| **OnDataReceivedBytes** | Fired when audio data is captured (TBytes format) |

### TMicInput Methods - NEW Audio Injection
| Method | Description |
|--------|-------------|
| **InjectAudioFile(FileName, Loop)** | Inject WAV file with optional looping |
| **InjectAudioBytes(Data, Loop)** | Inject custom TBytes audio data |
| **InjectBeep(Frequency, Duration)** | Generate and inject sine wave tone |
| **StopInjection** | Stop all audio injection |

### TSpeakerOutput Properties
| Property | Type | Default | Description |
|----------|------|---------|-------------|
| **Active** | Boolean | False | Initialize audio playback system |
| **DeviceID** | Integer | 0 | Selected speaker device index |
| **Volume** | Integer | 100 | Output volume (0-100%) |
| **SampleRate** | Integer | 44100 | Desired sample rate (Hz) |
| **Channels** | Integer | 2 | Audio channels (1=Mono, 2=Stereo) |
| **BitsPerSample** | Integer | 16 | Bit depth (16 or 32) |

### TSpeakerOutput Methods
| Method | Description |
|--------|-------------|
| **PlayBufferBytes(Buffer: TBytes)** | Play audio data from TBytes array |
| **PlayBuffer(Buffer: PByte; Size: Integer)** | Play audio data from pointer |

---

## 🎯 Use Cases - Expanded

### 🏢 **Business & Enterprise**
- **VoIP Applications** - Crystal-clear voice communication with custom audio injection
- **Conference Systems** - Multi-participant audio with background music or alerts
- **Call Recording** - Professional-grade audio capture with notification tones
- **Audio Monitoring** - Real-time audio analysis with test tone injection
- **Virtual Presentations** - Inject background music or sound effects during presentations

### 🎮 **Development & Testing**
- **Audio Streaming Apps** - Network-based audio transmission with custom content
- **Voice Chat Systems** - Real-time voice communication with sound effects
- **Audio Testing Frameworks** - Automated audio testing with controlled audio injection
- **Game Development** - Voice chat with in-game audio effects and notifications
- **Bot Development** - Virtual microphones for automated voice responses

### 🎓 **Educational & Research**
- **Audio Analysis Software** - Acoustic research with controlled test signals
- **Language Learning** - Pronunciation practice with background audio or prompts
- **Music Applications** - Real-time audio effects with backing tracks
- **Accessibility Tools** - Audio-based assistive technologies with custom alerts
- **Audio Simulation** - Simulate various audio environments for research

### 🎵 **Creative & Entertainment**
- **Live Streaming** - Stream with background music, sound effects, or alerts
- **Podcasting** - Inject intro music, transitions, or notification sounds
- **Virtual DJ Applications** - Mix microphone with music tracks
- **Karaoke Systems** - Inject backing tracks while capturing vocals
- **Audio Content Creation** - Create complex audio content with multiple sources

---

## 🔧 Advanced Features

### Audio Injection System Architecture
```pascal
// The injection system uses a sophisticated buffer management system
// that automatically handles format conversion and timing

// Internal structure (simplified):
TAudioInjectionBuffer = class
  - Loads WAV files and converts to PCM
  - Generates sine waves at any frequency
  - Handles looping and position tracking  
  - Provides thread-safe chunk delivery
  - Automatic format matching to microphone
end;
```

### Virtual Microphone Implementation
```pascal
// Virtual microphone mode completely bypasses hardware
if VirtualMicMode then
begin
  // Generate audio chunks from injection system only
  InjectedChunk := InjectionBuffer.GetNextChunk(ChunkSize);
  // Apply volume and send to callback
  SendToCallback(InjectedChunk);
end
else
begin
  // Normal mode: capture from hardware + optional injection
  HardwareAudio := CaptureFromMicrophone();
  if MixInjectedAudio then
    MixedAudio := MixAudio(HardwareAudio, InjectedAudio)
  else
    MixedAudio := HardwareAudio;
  SendToCallback(MixedAudio);
end;
```

### Automatic Format Conversion
The components use Windows' AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM feature plus custom injection format matching:
```pascal
// The component automatically handles format mismatches
MicInput1.SampleRate := 44100;      // Your desired format
MicInput1.Channels := 2;
MicInput1.BitsPerSample := 16;

// Windows automatically converts from device's native format
// Injection system matches the final audio format automatically
// Check actual format with:
ActualRate := MicInput1.ActualSampleRate;
ActualChannels := MicInput1.ActualChannels;
ActualBits := MicInput1.ActualBitsPerSample;
```

### Volume Control Implementation
Enhanced volume control with separate microphone and injection volumes:
```pascal
// Independent volume controls:
// - Microphone volume (0-100%) applied to hardware audio
// - Injection volume (0-100%) applied to injected audio
// - Master mixing with proper clipping prevention
// - 16-bit: Integer multiplication with clipping
// - 32-bit float: Direct float multiplication
// - Prevents distortion and maintains audio quality
```

### Network Integration Pattern with Injection
```pascal
// Enhanced sender with injection
MicInput1.VirtualMicMode := True;  // Virtual microphone mode
MicInput1.OnDataReceivedBytes := procedure(Sender: TObject; const Buffer: TBytes)
begin
  // Add header info for network transmission
  var Header: TAudioHeader;
  Header.SampleRate := MicInput1.ActualSampleRate;
  Header.Channels := MicInput1.ActualChannels;
  Header.BitsPerSample := MicInput1.ActualBitsPerSample;
  Header.DataSize := Length(Buffer);
  Header.IsVirtual := MicInput1.VirtualMicMode;  // NEW: Virtual mic flag
  
  // Send header + audio data (now includes injected content)
  NetworkSend(Header, Buffer);
end;

// Inject content for network streaming
MicInput1.InjectAudioFile('stream_content.wav', True);

// Receiver remains the same
procedure OnNetworkReceive(const Header: TAudioHeader; const Buffer: TBytes);
begin
  SpeakerOutput1.SampleRate := Header.SampleRate;
  SpeakerOutput1.Channels := Header.Channels;
  SpeakerOutput1.BitsPerSample := Header.BitsPerSample;
  SpeakerOutput1.PlayBufferBytes(Buffer);
end;
```

---

## 📈 Performance Specifications

### Supported Audio Formats
- **Sample Rates:** 8kHz - 192kHz (device dependent)
- **Channels:** 1 (Mono) - 8 (7.1 Surround) (device dependent)  
- **Bit Depths:** 16-bit PCM, 32-bit Float PCM
- **Automatic Conversion:** Yes (via Windows WASAPI + custom injection matching)
- **Injection Formats:** WAV files (PCM), Generated tones, Custom TBytes

### Audio Injection Performance
- **Latency:** <10ms injection mixing
- **File Formats:** WAV (PCM 16/32-bit)
- **Tone Generation:** 1Hz - 20kHz frequency range
- **Custom Audio:** Any TBytes PCM data
- **Memory Usage:** Efficient streaming with minimal buffer allocation

---

## 🧪 Testing & Quality

### Automated Tests - Enhanced
- **Component Installation** - Package builds and installs correctly
- **Device Enumeration** - All audio devices detected properly
- **Format Conversion** - Automatic format matching works
- **Audio Injection** - WAV file injection functions correctly
- **Virtual Microphone** - Pure injection mode works without hardware
- **Volume Controls** - Independent volume controls function properly
- **Network Integration** - TBytes callbacks with injection work correctly
- **Memory Management** - No memory leaks during extended operation with injection

### Manual Testing Checklist - Updated
- [ ] Components appear in IDE after installation
- [ ] Microphone devices enumerate correctly
- [ ] Speaker devices enumerate correctly  
- [ ] Audio capture produces valid data
- [ ] Audio playback works without distortion
- [ ] Volume controls function properly
- [ ] **NEW:** Virtual microphone mode works
- [ ] **NEW:** Audio file injection functions correctly
- [ ] **NEW:** Tone generation works at various frequencies
- [ ] **NEW:** Mix mode combines audio properly
- [ ] **NEW:** Independent volume controls work
- [ ] Format conversion handles mismatches
- [ ] Network streaming maintains quality with injection
- [ ] Extended operation remains stable

---

## 🛠️ Compatibility

### Delphi Versions
- **Delphi XE2** - Full compatibility
- **Delphi XE3-XE8** - Full compatibility  
- **Delphi 10.x Seattle+** - Full compatibility
- **Delphi 11.x Alexandria** - Full compatibility
- **Delphi 12.x Athens** - Full compatibility

### Windows Versions
- **Windows Vista** - Minimum requirement (WASAPI)
- **Windows 7/8/8.1** - Full support
- **Windows 10** - Full support, recommended
- **Windows 11** - Full support, latest features

---

## 🤝 Contributing

Contributions welcome! Here's how to help:

1. **Fork** the repository
2. **Create** feature branch (`git checkout -b feature/amazing-audio-feature`)
3. **Test** your changes thoroughly
4. **Commit** with clear messages (`git commit -m 'Add real-time effects processing'`)
5. **Push** to branch (`git push origin feature/amazing-audio-feature`)
6. **Open** Pull Request

### Development Guidelines
- Follow Delphi coding standards and conventions
- Test with multiple audio devices and formats
- Test audio injection with various WAV files and tone frequencies
- Verify virtual microphone mode in different scenarios
- Ensure thread safety in all audio processing
- Add XML documentation for new public methods
- Verify compatibility with target Delphi versions
- Test network scenarios with audio injection

---

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for complete details.

---

## 👨‍💻 Author

**BitmasterXor**
- GitHub: [@BitmasterXor](https://github.com/BitmasterXor)
- Discord: BitmasterXor

---

## 🙏 Acknowledgments

- **Microsoft WASAPI Team** - Excellent low-level audio API
- **Delphi Community** - Continuous support and inspiration  
- **Audio Developers Worldwide** - Best practices and optimization techniques
- **Beta Testers** - Critical feedback during component development
- **NetCom7 Contributors** - Networking components that pair perfectly with these audio components
- **Audio Injection Beta Testers** - Feedback on virtual microphone and injection features

---

## 📚 Additional Resources

### Documentation
- [Windows Audio Session API (WASAPI)](https://docs.microsoft.com/en-us/windows/win32/coreaudio/wasapi)
- [Delphi Component Development Guide](https://docwiki.embarcadero.com/RADStudio/en/Creating_Components)

### Example Projects
- Basic peer-to-peer voice communication (supports TCP and UDP) 
- Professional audio recording / playback application
- **NEW:** Virtual microphone demonstration with audio injection
- **NEW:** Network streaming with background music injection

---

<div align="center">

**⭐ Star this repository if these components help your audio projects!**

**Made with ❤️ By BitmasterXor For the Delphi Community**

</div>
