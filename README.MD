# Greg's Addon Manager for World of Warcraft

A lightweight, powerful addon manager for World of Warcraft built in Delphi. Designed to give you complete control over your addon collection without the bloat of traditional managers.

## Features

### Core Functionality
- **Automatic Addon Detection** - Scans your WoW AddOns directory and imports addon information from TOC files
- **File Watching** - Monitors your addon directory for changes in real-time
- **Backup Engine** - Threaded backup system to preserve your addon configurations
- **Restore System** - Dedicated restore threading (TRestoreThread) for quick addon recovery
- **Multi-Version Support** - Handles Retail, Classic, TBC, and Wrath installations

### Technical Highlights
- Built in Delphi 11.0 with native Windows performance
- Threaded operations (BackupEngineThreaded) for responsive UI
- Smart TOC file parsing for addon metadata
- Efficient file system monitoring with FileWatcher implementation
- Clean form-based architecture (TForm1/TForm2/TForm3 modular design)

## Why Greg's Addon Manager?

Most addon managers are bloated, slow, or try to do too much. Greg's Addon Manager focuses on what matters:
- **Fast** - Native Windows application, no web framework overhead
- **Reliable** - Direct file system operations, no cloud dependencies
- **Transparent** - You control where your addons live and how they're managed
- **Lightweight** - Minimal resource footprint

## Installation
- I'm waiting on Code Signing Cert from SSL.COM in order to code sign the exe file via OV code signing process.

1. Download the latest release from the [Releases](../../releases) page
2. Extract to your preferred location
3. Run `GregsAddonManager.exe`
4. Point it to your World of Warcraft installation directory
5. Let it scan and import your addons

## System Requirements

- Windows 10 or later
- World of Warcraft version: Retail
- ~10MB disk space

## Configuration

The manager automatically detects your WoW installation in standard locations:
```
  WOW_PATH_PATTERNS: array[0..3] of string = (
    'Program Files (x86)\World of Warcraft\_retail_\Interface\AddOns\',
    'Program Files\World of Warcraft\_retail_\Interface\AddOns\',
    'Games\World of Warcraft\_retail_\Interface\AddOns\',
    'Users\Public\Games\World of Warcraft\_retail_\Interface\AddOns\'
  );
```

Custom paths can be configured through the settings interface.

## Building from Source
- Was built using Delphi 11.0 Enterprise

### Prerequisites
- Delphi 11.0 (Community, Professional, or Enterprise)
- Windows SDK

### Compilation
1. Clone this repository
2. Open `GregsAddonManager.dproj` in Delphi
3. Ensure the following units are in your uses clause:
   - `System.Generics.Defaults` (for TComparer support)
   - Standard VCL components
4. Build -> Compile
5. Build -> Build GregsAddonManager

### Known Build Issues
- **VMware Users**: If forms appear tiny in the designer at 1920x1080, adjust DPI settings or design at 100% scaling
- **Unit Conflicts**: Ensure TForm naming doesn't collide between units
- **Missing TComparer**: Add `System.Generics.Defaults` to uses if you encounter compilation errors

## Architecture

```
Greg's Addon Manager
├── FileWatcher Module (real-time directory monitoring)
├── TOC Parser (addon metadata extraction)
├── Backup Engine (threaded backup operations)
├── Restore Engine (TRestoreThread for recovery)
└── UI Layer (responsive form-based interface)
```

## Troubleshooting

### EInvalidPointer / Double-Free Errors
Early versions had FileWatcher cleanup issues. Fixed in v1.1+. If you encounter this:
- Update to latest release
- Ensure FileWatcher is properly destroyed in form cleanup

### Addons Not Detected
- Verify WoW path is correct
- Check that TOC files are present and valid
- Ensure you have read permissions on the AddOns directory

## Roadmap

- [ ] Cloud backup integration (optional)
- [ ] Addon update checking from CurseForge/WoWInterface
- [ ] Profile management (multiple addon sets)
- [ ] Dependency resolution and conflict detection
- [ ] Cross-platform support (FMX version for Mac/Linux)

## Contributing

Contributions welcome! This is a solo project by Greg Timmons, but pull requests for bug fixes and features are appreciated.

### Guidelines
- Keep it lightweight - no unnecessary dependencies
- Test on multiple WoW versions
- Follow existing code style
- Document threading implications

## Credits

**Author**: Greg Timmons (git-timmons)  
**Built with**: Delphi 11.0, blood, sweat, and designer-form-scaling tears

Inspired by the need for a simple, fast, no-nonsense addon manager that doesn't require 500MB of Electron framework.

## License

See [LICENSE](LICENSE) for details.

## Support

- **Issues**: Use the GitHub issue tracker
- **Discussions**: Check the Discussions tab for questions

---

*Because managing addons shouldn't require an addon manager that needs managing.*
