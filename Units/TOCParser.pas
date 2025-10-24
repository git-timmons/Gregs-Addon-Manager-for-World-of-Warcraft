unit TOCParser;

// ============================================================================
// TOC PARSER - WoW Addon Metadata Extraction Engine
// ============================================================================
//
// Purpose: Parse .toc files to extract addon metadata and construct URLs
//
// TOC File Format (Example):
//   ## Interface: 110200
//   ## Title: WeakAuras
//   ## Author: The WeakAuras Team
//   ## Version: 5.20.1
//   ## Notes: A powerful addon...
//   ## X-Curse-Project-ID: 65387
//   ## X-WoWI-ID: 24910
//   ## X-Wago-ID: VBNBxKx5
//   ## X-Website: https://www.curseforge.com/wow/addons/weakauras
//
// URL Construction Strategies:
//   1. X-Website (direct) - use as-is
//   2. X-Curse-Project-ID → CurseForge URL (requires slug or API)
//   3. X-WoWI-ID → WoWInterface URL (simple pattern)
//   4. X-Wago-ID → Wago.io URL (simple pattern)
//   5. No metadata → user must manually add URL
//
// ============================================================================

interface

uses
  System.SysUtils, System.Classes, System.IOUtils;

type
  TTOCInfo = record
    // Basic metadata
    Title: string;
    Author: string;
    Version: string;
    Interface: string;
    Notes: string;
    
    // URL discovery fields
    CurseProjectID: string;
    WoWInterfaceID: string;
    WagoID: string;
    CustomWebsite: string;
    
    // Constructed URL (best available)
    DiscoveredURL: string;
    URLSource: string;  // 'direct', 'curse', 'wowi', 'wago', 'none'
    
    // Parsing metadata
    TOCFilePath: string;
    FolderName: string;
    IsValid: Boolean;
    ParseError: string;
  end;

  TTOCParser = class
  private
    class function ExtractFieldValue(const Line: string): string;
    class function BuildCurseForgeURL(const ProjectID: string): string;
    class function BuildWoWInterfaceURL(const WoWI_ID: string): string;
    class function BuildWagoURL(const WagoID: string): string;
    class function DetermineURLSource(const Info: TTOCInfo): string;
  public
    // Parse a single TOC file
    class function ParseTOCFile(const FilePath: string): TTOCInfo;
    
    // Scan entire AddOns directory
    class function ScanAddOnsFolder(const AddOnsPath: string): TArray<TTOCInfo>;
    
    // URL construction utilities
    class function BuildBestURL(const Info: TTOCInfo): string;
  end;

implementation

{ TTOCParser }

class function TTOCParser.ExtractFieldValue(const Line: string): string;
var
  ColonPos: Integer;
begin
  // Line format: "## FieldName: Value"
  ColonPos := Pos(':', Line);
  if ColonPos > 0 then
  begin
    Result := Trim(Copy(Line, ColonPos + 1, MaxInt));
    // Remove trailing \r if present (Windows line endings)
    if (Length(Result) > 0) and (Result[Length(Result)] = #13) then
      Delete(Result, Length(Result), 1);
  end
  else
    Result := '';
end;

class function TTOCParser.BuildCurseForgeURL(const ProjectID: string): string;
begin
  // CurseForge URLs require a slug (addon name in URL format)
  // We can't derive the slug from just the ID without an API call
  // So we return a base URL that redirects properly
  Result := 'https://www.curseforge.com/wow/addons/' + ProjectID;
  
  // Note: CurseForge actually redirects from project ID:
  // https://www.curseforge.com/projects/65387 → proper addon page
  // But the /wow/addons/ path is more stable long-term
end;

class function TTOCParser.BuildWoWInterfaceURL(const WoWI_ID: string): string;
begin
  // WoWInterface uses simple numeric IDs
  Result := 'https://www.wowinterface.com/downloads/info' + WoWI_ID;
end;

class function TTOCParser.BuildWagoURL(const WagoID: string): string;
begin
  // Wago uses alphanumeric IDs
  Result := 'https://addons.wago.io/addons/' + WagoID;
end;

class function TTOCParser.DetermineURLSource(const Info: TTOCInfo): string;
begin
  // Priority: Direct > CurseForge > WoWInterface > Wago
  if Info.CustomWebsite <> '' then
    Result := 'direct'
  else if Info.CurseProjectID <> '' then
    Result := 'curse'
  else if Info.WoWInterfaceID <> '' then
    Result := 'wowi'
  else if Info.WagoID <> '' then
    Result := 'wago'
  else
    Result := 'none';
end;

class function TTOCParser.BuildBestURL(const Info: TTOCInfo): string;
begin
  // Try each URL source in priority order
  if Info.CustomWebsite <> '' then
    Result := Info.CustomWebsite
  else if Info.CurseProjectID <> '' then
    Result := BuildCurseForgeURL(Info.CurseProjectID)
  else if Info.WoWInterfaceID <> '' then
    Result := BuildWoWInterfaceURL(Info.WoWInterfaceID)
  else if Info.WagoID <> '' then
    Result := BuildWagoURL(Info.WagoID)
  else
    Result := '';  // No URL available - user must add manually
end;

class function TTOCParser.ParseTOCFile(const FilePath: string): TTOCInfo;
var
  Lines: TStringList;
  Line: string;
  FieldName: string;
begin
  // Initialize result
  FillChar(Result, SizeOf(Result), 0);
  Result.TOCFilePath := FilePath;
  Result.FolderName := TPath.GetFileNameWithoutExtension(FilePath);
  Result.IsValid := False;
  
  if not TFile.Exists(FilePath) then
  begin
    Result.ParseError := 'File not found';
    Exit;
  end;
  
  Lines := TStringList.Create;
  try
    try
      Lines.LoadFromFile(FilePath);
      
      // Parse each line
      for Line in Lines do
      begin
        // Skip non-metadata lines
        if not Line.StartsWith('##') then
          Continue;
        
        // Extract field name (between ## and :)
        if Pos(':', Line) > 0 then
        begin
          FieldName := Trim(Copy(Line, 3, Pos(':', Line) - 3));
          
          // Parse known fields
          if SameText(FieldName, 'Title') then
            Result.Title := ExtractFieldValue(Line)
          else if SameText(FieldName, 'Author') then
            Result.Author := ExtractFieldValue(Line)
          else if SameText(FieldName, 'Version') then
            Result.Version := ExtractFieldValue(Line)
          else if SameText(FieldName, 'Interface') then
            Result.Interface := ExtractFieldValue(Line)
          else if SameText(FieldName, 'Notes') then
          begin
            // Only capture first Notes line (English default)
            if Result.Notes = '' then
              Result.Notes := ExtractFieldValue(Line);
          end
          else if SameText(FieldName, 'X-Curse-Project-ID') then
            Result.CurseProjectID := ExtractFieldValue(Line)
          else if SameText(FieldName, 'X-WoWI-ID') then
            Result.WoWInterfaceID := ExtractFieldValue(Line)
          else if SameText(FieldName, 'X-Wago-ID') then
            Result.WagoID := ExtractFieldValue(Line)
          else if SameText(FieldName, 'X-Website') then
            Result.CustomWebsite := ExtractFieldValue(Line);
        end;
      end;
      
      // Consider valid if we at least got a title
      Result.IsValid := (Result.Title <> '');
      
      // Build URL
      Result.DiscoveredURL := BuildBestURL(Result);
      Result.URLSource := DetermineURLSource(Result);
      
    except
      on E: Exception do
      begin
        Result.ParseError := E.Message;
        Result.IsValid := False;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

class function TTOCParser.ScanAddOnsFolder(const AddOnsPath: string): TArray<TTOCInfo>;
var
  Folders: TStringDynArray;
  Folder: string;
  TOCFiles: TStringDynArray;
  TOCFile: string;
  List: TList<TTOCInfo>;
  Info: TTOCInfo;
  FolderName: string;
begin
  SetLength(Result, 0);
  
  if not TDirectory.Exists(AddOnsPath) then
    Exit;
  
  List := TList<TTOCInfo>.Create;
  try
    // Get all addon folders
    Folders := TDirectory.GetDirectories(AddOnsPath);
    
    for Folder in Folders do
    begin
      FolderName := TPath.GetFileName(Folder);
      
      // Skip Blizzard addons (start with Blizzard_)
      if FolderName.StartsWith('Blizzard_', True) then
        Continue;
      
      // Look for TOC files in this folder
      TOCFiles := TDirectory.GetFiles(Folder, '*.toc', TSearchOption.soTopDirectoryOnly);
      
      // Primary TOC: folder name matches TOC name
      // Example: WeakAuras folder → WeakAuras.toc
      for TOCFile in TOCFiles do
      begin
        if SameText(
          TPath.GetFileNameWithoutExtension(TOCFile),
          FolderName
        ) then
        begin
          // Found primary TOC
          Info := ParseTOCFile(TOCFile);
          if Info.IsValid then
          begin
            Info.FolderName := FolderName;
            List.Add(Info);
            Break;  // Only add primary TOC
          end;
        end;
      end;
      
      // If no matching TOC found, try first TOC in folder
      if (List.Count = 0) or (List[List.Count - 1].FolderName <> FolderName) then
      begin
        if Length(TOCFiles) > 0 then
        begin
          Info := ParseTOCFile(TOCFiles[0]);
          if Info.IsValid then
          begin
            Info.FolderName := FolderName;
            List.Add(Info);
          end;
        end;
      end;
    end;
    
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

end.
