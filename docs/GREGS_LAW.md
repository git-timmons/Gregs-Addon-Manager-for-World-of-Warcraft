# Greg’s Law — A Personal Delphi Naming Convention Codebook  
*Created by Gregory Allen Timmons*  

---

### 🧩 Introduction
Greg’s Law is a comprehensive Delphi naming convention system designed to bring **clarity**, **consistency**, and **machine-interpretability** to human codebases.  
This codebook redefines Hungarian notation with modern precision—every variable, field, parameter, constant, event, and component follows a deterministic pattern that makes both human reading and AI-based code generation seamless.  

No single-letter variables.  
No implicit context.  
Every name declares its **scope**, **type**, and **intent**.

---

## SECTION 1 — Base Variable Prefix Standards

| Type | Prefix | Example | Notes |
|------|---------|----------|-------|
| String | `s` | `sFileName` | Text values |
| Integer | `i` | `iIndex` | Numeric counters |
| Boolean | `b` | `bIsValid` | True/False |
| DateTime | `dt` | `dtCreatedOn` | Temporal values |
| Float | `flt` | `fltRatio` | Floating-point |
| Currency | `c` | `cTotalCost` | Monetary |
| File Objects | `fil` | `filIniFile` | TFileStream, TZipFile |
| Pointer | `p` | `pBuffer` | Typed or untyped |
| Object | `o` | `oStream` | Class instance |
| Interface | `intf` | `intfLogger` | COM or Delphi interfaces |
| Variant | `v` | `vResult` | Variant container |
| Record | `rec` | `recCoords` | Record values |
| GUID | `guid` | `guidSessionID` | Unique identifiers |
| Char | `ch` | `chDelimiter` | Single character |
| Byte | `by` | `byFlag` | 0–255 values |
| Word | `w` | `wCount` | 16-bit integers |
| Cardinal | `dw` | `dwThreadID` | 32-bit unsigned |
| Enum Variable | `e` | `eState` | Enum value |
| Set of Enum | `set` | `setFlags` | Enum set |

**Visual Clarity Rule:**  
Interfaces must never use capital `I` as prefix. Always use `intf`.

---

## SECTION 2 — Complex Types & Collections

| Type | Prefix | Example |
|------|---------|----------|
| TArray<T> | `arr` | `arrNames` |
| TList<T> | `lst` | `lstUsers` |
| TObjectList<T> | `objlst` | `objlstAddons` |
| TStringList | `sl` | `slFiles` |
| TDictionary<K,V> | `dict` | `dictSettings` |
| TQueue<T> | `que` | `queTasks` |
| TStack<T> | `stk` | `stkUndo` |
| TCollection | `col` | `colItems` |
| TDataSet | `ds` | `dsUsers` |
| TDataSource | `src` | `srcOrders` |

Rules:
- Collections are **always pluralized** (`lstUsers`, not `lstUser`).
- Never omit prefixes even in private scope.

---

## SECTION 3 — Private Fields & Properties

All fields use a **dual prefix**: `F` + type prefix.

| Type | Example |
|------|----------|
| String | `FsFileName` |
| Integer | `FiCount` |
| Boolean | `FbIsLoaded` |
| DateTime | `FdtCreatedOn` |

Property mirroring:
```pascal
property FileName: string read FsFileName write FsFileName;
```

Use `Self.` in all shadowed contexts for clarity:
```pascal
Self.FsFileName := s_FileName;
```

---

## SECTION 4 — Constants, Enumerations & Result Variables

**Constants:** `ALL_UPPER_CASE_WITH_UNDERSCORES`

```pascal
const
  MAX_RETRIES = 3;
  DEFAULT_PATH = 'Config\settings.ini';
```

**Enumerations:** Prefixed by their type abbreviation  
```pascal
TAddonState = (asNone, asLoading, asLoaded, asFailed);
```

**Result variables:**  
Intermediate results use `res` prefix.  
Final function result = `Result`.

---

## SECTION 5 — Event Handlers & Callback Conventions

| Category | Prefix | Example |
|-----------|---------|----------|
| Event property | `On` | `OnProgress`, `OnError` |
| Field | `FOn` | `FOnProgress` |
| Internal handler | `Do` | `DoProgress` |
| External handler | `Object_OnEvent` | `Manager_OnProgress` |
| Anonymous callback | `Proc` / `Evt` | `ProcFinish`, `EvtError` |
| Thread-safe call | `QueueDo` / `SyncDo` | `QueueDoProgress` |

Example:
```pascal
procedure TBackupEngine.DoProgress(Sender: TObject; const i_Percent: Integer);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, i_Percent);
end;
```

---

## SECTION 6 — Parameters

Parameters use **type prefix + underscore**:

| Type | Example |
|------|----------|
| String | `s_FileName` |
| Integer | `i_Index` |
| Boolean | `b_Enabled` |
| Object | `o_Addon` |
| Array | `arr_Values` |

Example:
```pascal
procedure TAddonManager.LoadAddon(const s_FileName: string; const b_ForceReload: Boolean);
begin
  if b_ForceReload then
    Self.FbLoaded := DoLoad(s_FileName);
end;
```

---

## SECTION 7 — Streams, Threads & Specialized Objects

| Domain | Prefix | Example |
|---------|---------|----------|
| Stream | `stm` | `stmFileConfig` |
| FileStream | `stmFile` | `stmFileInput` |
| MemoryStream | `stmMem` | `stmMemBuffer` |
| Thread | `thr` | `thrBackup` |
| Task | `tsk` | `tskLoadData` |
| CriticalSection | `cs` | `csLock` |
| Event | `evt` | `evtStop` |
| Engine | `eng` | `engBackup` |
| Manager | `mgr` | `mgrAddon` |
| Service | `svc` | `svcNetwork` |
| Utility | `utl` | `utlFiles` |

---

## SECTION 8 — Visual & Component Naming

| Component | Prefix | Example |
|------------|---------|----------|
| TButton | `btn` | `btnSave` |
| TLabel | `lbl` | `lblStatus` |
| TEdit | `edt` | `edtUser` |
| TComboBox | `cmb` | `cmbOptions` |
| TCheckBox | `chk` | `chkEnabled` |
| TMemo | `mem` | `memLog` |
| TPanel | `pnl` | `pnlMain` |
| TGroupBox | `grp` | `grpSettings` |
| TImage | `img` | `imgLogo` |
| TProgressBar | `prg` | `prgBackup` |
| TListView | `lvw` | `lvwAddons` |
| TGrid | `grd` | `grdData` |
| TForm | `frm` | `frmMain`, `frmSettings` |
| TFrame | `frmFrame` | `frmFrameOptions` |

---

### ✅ Summary
Greg’s Law enforces type-driven, scope-aware, and self-descriptive naming across all Delphi code.  
It is designed not only for human readability, but also for **AI consistency** — ensuring that code generation tools can infer structure, hierarchy, and type intent from identifiers alone.

> *Greg’s Law: if you can’t tell what it is by its name, it doesn’t belong in your code.*
