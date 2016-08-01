unit encinifile;

interface

uses Classes, slmd5, SyncObjs, StrUtils;

type
  TEncStringlist = class(TStringList)
  private
    fPassHash: TslMD5Data;
  public
    constructor Create(pass: string); overload;
    constructor Create(pass: TslMD5Data); overload;
    procedure LoadFromFile(const FileName: string); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override; 
  end;

  { TStringHash - used internally by TMemIniFile to optimize searches. }

  PPHashItem = ^PHashItem;
  PHashItem = ^THashItem;
  THashItem = record
    Next: PHashItem;
    Key: string;
    Value: Integer;
  end;

  TStringHash = class
  private
    Buckets: array of PHashItem;
  protected
    function Find(const Key: string): PPHashItem;
    function HashOf(const Key: string): Cardinal; virtual;
  public
    constructor Create(Size: Cardinal = 256);
    destructor Destroy; override;
    procedure Add(const Key: string; Value: Integer);
    procedure Clear;
    procedure Remove(const Key: string);
    function Modify(const Key: string; Value: Integer): Boolean;
    function ValueOf(const Key: string): Integer;
  end;

  TMyCustomIniFile = class(TObject)
  private
    FFileName: string;
  public
    constructor Create(const FileName: string);
    function SectionExists(const Section: string): Boolean;
    function ReadString(const Section, Ident, Default: string): string; virtual; abstract;
    procedure WriteString(const Section, Ident, Value: String); virtual; abstract;
    function ReadInteger(const Section, Ident: string; Default: Longint): Longint; virtual;
    procedure WriteInteger(const Section, Ident: string; Value: Longint); virtual;
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean; virtual;
    procedure WriteBool(const Section, Ident: string; Value: Boolean); virtual;
    function ReadBinaryStream(const Section, Name: string; Value: TStream): Integer; virtual;
    function ReadDate(const Section, Name: string; Default: TDateTime): TDateTime; virtual;
    function ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime; virtual;
    function ReadFloat(const Section, Name: string; Default: Double): Double; virtual;
    function ReadTime(const Section, Name: string; Default: TDateTime): TDateTime; virtual;
    procedure WriteBinaryStream(const Section, Name: string; Value: TStream); virtual;
    procedure WriteDate(const Section, Name: string; Value: TDateTime); virtual;
    procedure WriteDateTime(const Section, Name: string; Value: TDateTime); virtual;
    procedure WriteFloat(const Section, Name: string; Value: Double); virtual;
    procedure WriteTime(const Section, Name: string; Value: TDateTime); virtual;
    procedure ReadSection(const Section: string; Strings: TStrings); virtual; abstract;
    procedure ReadSections(Strings: TStrings); virtual; abstract;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); virtual; abstract;
    procedure EraseSection(const Section: string); virtual; abstract;
    procedure DeleteKey(const Section, Ident: String); virtual; abstract;
    procedure UpdateFile; virtual; abstract;
    function ValueExists(const Section, Ident: string): Boolean;
    property FileName: string read FFileName;
  end;

  { THashedStringList - A TStringList that uses TStringHash to improve the
    speed of Find }
  TMyHashedStringList = class(TStringList)
  private
    FValueHash: TStringHash;
    FNameHash: TStringHash;
    FValueHashValid: Boolean;
    FNameHashValid: Boolean;
    procedure UpdateValueHash;
    procedure UpdateNameHash;
  protected
    procedure Changed; override;
  public
    destructor Destroy; override;
    function IndexOf(const S: string): Integer; override;
    function IndexOfName(const Name: string): Integer; override;
  end;


  // threadsafe TIniFile with encryption support
  TEncIniFile = class(TMyCustomIniFile)
  private
    il: TCriticalSection;
    fSima: Boolean;
    FFilename: string;
    FPassHash: TslMD5Data;
    FSections: TStringList;
    fCompression: Boolean;
    function AddSection(const Section: string): TStrings;
    function GetCaseSensitive: Boolean;
    procedure LoadValues;
    procedure SetCaseSensitive(Value: Boolean);
  public
    AutoUpdate: Boolean;
    constructor Create(const FileName, Passphrase: string; autoupdate: Boolean = False; compression: Boolean = True); overload;
    constructor Create(const FileName: string; Passphrase: TslMD5Data; autoupdate: Boolean = False; compression: Boolean = True); overload;
    destructor Destroy; override;
    procedure LoadUnencrypted(filename: string);
    procedure SaveUnencrypted(filename: string);
    procedure Clear;
    procedure DeleteKey(const Section, Ident: String); override;
    procedure EraseSection(const Section: string); override;
    procedure GetStrings(List: TStrings);
    procedure ReadSection(const Section: string; Strings: TStrings); override;
    procedure ReadSections(Strings: TStrings); override;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
    function ReadString(const Section, Ident, Default: string): string; override;
    procedure SetStrings(List: TStrings);
    procedure UpdateFile; override;
    procedure Rename(const FileName: string; Reload: Boolean);
    procedure WriteString(const Section, Ident, Value: String); override;
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
  end;

  
implementation

uses SysUtils, slblowfish, configunit;

{ TStringHash }

procedure TStringHash.Add(const Key: string; Value: Integer);
var
  Hash: Integer;
  Bucket: PHashItem;
begin
  Hash := HashOf(Key) mod Cardinal(Length(Buckets));
  New(Bucket);
  Bucket^.Key := Key;
  Bucket^.Value := Value;
  Bucket^.Next := Buckets[Hash];
  Buckets[Hash] := Bucket;
end;

procedure TStringHash.Clear;
var
  I: Integer;
  P, N: PHashItem;
begin
  for I := 0 to Length(Buckets) - 1 do
  begin
    P := Buckets[I];
    while P <> nil do
    begin
      N := P^.Next;
      Dispose(P);
      P := N;
    end;
    Buckets[I] := nil;
  end;
end;

constructor TStringHash.Create(Size: Cardinal);
begin
  inherited Create;
  SetLength(Buckets, Size);
end;

destructor TStringHash.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TStringHash.Find(const Key: string): PPHashItem;
var
  Hash: Integer;
begin
  Hash := HashOf(Key) mod Cardinal(Length(Buckets));
  Result := @Buckets[Hash];
  while Result^ <> nil do
  begin
    if Result^.Key = Key then
      Exit
    else
      Result := @Result^.Next;
  end;
end;

function TStringHash.HashOf(const Key: string): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(Key) do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor
      Ord(Key[I]);
end;

function TStringHash.Modify(const Key: string; Value: Integer): Boolean;
var
  P: PHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
  begin
    Result := True;
    P^.Value := Value;
  end
  else
    Result := False;
end;

procedure TStringHash.Remove(const Key: string);
var
  P: PHashItem;
  Prev: PPHashItem;
begin
  Prev := Find(Key);
  P := Prev^;
  if P <> nil then
  begin
    Prev^ := P^.Next;
    Dispose(P);
  end;
end;

function TStringHash.ValueOf(const Key: string): Integer;
var
  P: PHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
    Result := P^.Value
  else
    Result := -1;
end;


{ TMyCustomIniFile }

constructor TMyCustomIniFile.Create(const FileName: string);
begin
  FFileName := FileName;
end;

function TMyCustomIniFile.SectionExists(const Section: string): Boolean;
var
  S: TStrings;
begin
  S := TStringList.Create;
  try
    ReadSection(Section, S);
    Result := S.Count > 0;
  finally
    S.Free;
  end;
end;

function TMyCustomIniFile.ReadInteger(const Section, Ident: string;
  Default: Longint): Longint;
var
  IntStr: string;
begin
  IntStr := ReadString(Section, Ident, '');
  if (Length(IntStr) > 2) and (IntStr[1] = '0') and
     ((IntStr[2] = 'X') or (IntStr[2] = 'x')) then
    IntStr := '$' + Copy(IntStr, 3, Maxint);
  Result := StrToIntDef(IntStr, Default);
end;

procedure TMyCustomIniFile.WriteInteger(const Section, Ident: string; Value: Longint);
begin
  WriteString(Section, Ident, IntToStr(Value));
end;

function TMyCustomIniFile.ReadBool(const Section, Ident: string;
  Default: Boolean): Boolean;
begin
  Result := ReadInteger(Section, Ident, Ord(Default)) <> 0;
end;

function TMyCustomIniFile.ReadDate(const Section, Name: string; Default: TDateTime): TDateTime;
var
  DateStr: string;
begin
  DateStr := ReadString(Section, Name, '');
  Result := Default;
  if DateStr <> '' then
  try
    Result := StrToDate(DateStr);
  except
    on EConvertError do
      // Ignore EConvertError exceptions
    else
      raise;
  end;
end;

function TMyCustomIniFile.ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime;
var
  DateStr: string;
begin
  DateStr := ReadString(Section, Name, '');
  Result := Default;
  if DateStr <> '' then
  try
    Result := StrToDateTime(DateStr);
  except
    on EConvertError do
      // Ignore EConvertError exceptions
    else
      raise;
  end;
end;

function TMyCustomIniFile.ReadFloat(const Section, Name: string; Default: Double): Double;
var
  FloatStr: string;
begin
  FloatStr := ReadString(Section, Name, '');
  Result := Default;
  if FloatStr <> '' then
  try
    Result := StrToFloat(FloatStr);
  except
    on EConvertError do
      // Ignore EConvertError exceptions
    else
      raise;
  end;
end;

function TMyCustomIniFile.ReadTime(const Section, Name: string; Default: TDateTime): TDateTime;
var
  TimeStr: string;
begin
  TimeStr := ReadString(Section, Name, '');
  Result := Default;
  if TimeStr <> '' then
  try
    Result := StrToTime(TimeStr);
  except
    on EConvertError do
      // Ignore EConvertError exceptions
    else
      raise;
  end;
end;

procedure TMyCustomIniFile.WriteDate(const Section, Name: string; Value: TDateTime);
begin
  WriteString(Section, Name, DateToStr(Value));
end;

procedure TMyCustomIniFile.WriteDateTime(const Section, Name: string; Value: TDateTime);
begin
  WriteString(Section, Name, DateTimeToStr(Value));
end;

procedure TMyCustomIniFile.WriteFloat(const Section, Name: string; Value: Double);
begin
  WriteString(Section, Name, FloatToStr(Value));
end;

procedure TMyCustomIniFile.WriteTime(const Section, Name: string; Value: TDateTime);
begin
  WriteString(Section, Name, TimeToStr(Value));
end;

procedure TMyCustomIniFile.WriteBool(const Section, Ident: string; Value: Boolean);
const
  Values: array[Boolean] of string = ('0', '1');
begin
  WriteString(Section, Ident, Values[Value]);
end;

function TMyCustomIniFile.ValueExists(const Section, Ident: string): Boolean;
var
  S: TStrings;
begin
  S := TStringList.Create;
  try
    ReadSection(Section, S);
    Result := S.IndexOf(Ident) > -1;
  finally
    S.Free;
  end;
end;

function TMyCustomIniFile.ReadBinaryStream(const Section, Name: string;
  Value: TStream): Integer;
var
  Text: string;
  Stream: TMemoryStream;
  Pos: Integer;
begin
  Text := ReadString(Section, Name, '');
  if Text <> '' then
  begin
    if Value is TMemoryStream then
      Stream := TMemoryStream(Value)
    else
      Stream := TMemoryStream.Create;

    try
      Pos := Stream.Position;
      Stream.SetSize(Stream.Size + Length(Text) div 2);
      HexToBin(PChar(Text), PChar(Integer(Stream.Memory) + Stream.Position), Length(Text) div 2);
      Stream.Position := Pos;
      if Value <> Stream then
        Value.CopyFrom(Stream, Length(Text) div 2);
      Result := Stream.Size - Pos;
    finally
      if Value <> Stream then
        Stream.Free;
    end;
  end
  else
    Result := 0;
end;

procedure TMyCustomIniFile.WriteBinaryStream(const Section, Name: string;
  Value: TStream);
var
  Text: string;
  Stream: TMemoryStream;
begin
  SetLength(Text, (Value.Size - Value.Position) * 2);
  if Length(Text) > 0 then
  begin
    if Value is TMemoryStream then
      Stream := TMemoryStream(Value)
    else
      Stream := TMemoryStream.Create;

    try
      if Stream <> Value then
      begin
        Stream.CopyFrom(Value, Value.Size - Value.Position);
        Stream.Position := 0;
      end;
      BinToHex(PChar(Integer(Stream.Memory) + Stream.Position), PChar(Text),
        Stream.Size - Stream.Position);
    finally
      if Value <> Stream then
        Stream.Free;
    end;
  end;
  WriteString(Section, Name, Text);
end;



{ THashedStringList }

procedure TMyHashedStringList.Changed;
begin
  inherited Changed;
  FValueHashValid := False;
  FNameHashValid := False;
end;

destructor TMyHashedStringList.Destroy;
begin
  FValueHash.Free;
  FNameHash.Free;
  inherited Destroy;
end;

function TMyHashedStringList.IndexOf(const S: string): Integer;
begin
  UpdateValueHash;
  if not CaseSensitive then
    Result :=  FValueHash.ValueOf(AnsiUpperCase(S))
  else
    Result :=  FValueHash.ValueOf(S);
end;

function TMyHashedStringList.IndexOfName(const Name: string): Integer;
begin
  UpdateNameHash;
  if not CaseSensitive then
    Result := FNameHash.ValueOf(AnsiUpperCase(Name))
  else
    Result := FNameHash.ValueOf(Name);
end;

procedure TMyHashedStringList.UpdateNameHash;
var
  I: Integer;
  P: Integer;
  Key: string;
begin
  if FNameHashValid then Exit;
  
  if FNameHash = nil then
    FNameHash := TStringHash.Create
  else
    FNameHash.Clear;
  for I := 0 to Count - 1 do
  begin
    Key := Get(I);
    P := AnsiPos('=', Key);
    if P <> 0 then
    begin
      if not CaseSensitive then
        Key := AnsiUpperCase(Copy(Key, 1, P - 1))
      else
        Key := Copy(Key, 1, P - 1);
      FNameHash.Add(Key, I);
    end;
  end;
  FNameHashValid := True;
end;

procedure TMyHashedStringList.UpdateValueHash;
var
  I: Integer;
begin
  if FValueHashValid then Exit;
  
  if FValueHash = nil then
    FValueHash := TStringHash.Create
  else
    FValueHash.Clear;
  for I := 0 to Count - 1 do
    if not CaseSensitive then
      FValueHash.Add(AnsiUpperCase(Self[I]), I)
    else
      FValueHash.Add(Self[I], I);
  FValueHashValid := True;
end;

constructor TEncIniFile.Create(const FileName: string; Passphrase: TslMD5Data; autoupdate: Boolean = False; compression: Boolean = True);
begin
  inherited Create(FileName);
  il:= TCriticalSection.Create;
  fPassHash:= Passphrase;
  self.AutoUpdate:= autoupdate;
  FFilename:= FileName;
  fCompression:= compression;
  FSections := TMyHashedStringList.Create;
{$IFDEF LINUX}
  FSections.CaseSensitive := True;
{$ENDIF}
  LoadValues;
end;

constructor TEncIniFile.Create(const FileName, Passphrase: string; autoupdate: Boolean = False; compression: Boolean = True);
begin
  if passPhrase = '' then
    fSima:= True;
  Create(FileName, slMD5String(Passphrase), autoupdate, compression);
end;

destructor TEncIniFile.Destroy;
begin
  if AutoUpdate then
    UpdateFile;
    
  if FSections <> nil then
    Clear;
  FSections.Free;
  il.Free;
  inherited Destroy;
end;

function TEncIniFile.AddSection(const Section: string): TStrings;
begin
  Result := TMyHashedStringList.Create;
  try
    TMyHashedStringList(Result).CaseSensitive := CaseSensitive;
    FSections.AddObject(Section, Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TEncIniFile.Clear;
var
  I: Integer;
begin
  il.Enter;
  for I := 0 to FSections.Count - 1 do
    TObject(FSections.Objects[I]).Free;
  FSections.Clear;
  il.Leave;
end;

procedure TEncIniFile.DeleteKey(const Section, Ident: String);
var
  I, J: Integer;
  Strings: TStrings;
begin
  il.Enter;
  I := FSections.IndexOf(Section);
  if I >= 0 then
  begin
    Strings := TStrings(FSections.Objects[I]);
    J := Strings.IndexOfName(Ident);
    if J >= 0 then
      Strings.Delete(J);
  end;

  if self.AutoUpdate then
    UpdateFile;
  
  il.Leave;  
end;

procedure TEncIniFile.EraseSection(const Section: string);
var
  I: Integer;
begin
  il.Enter;
  I := FSections.IndexOf(Section);
  if I >= 0 then
  begin
    TStrings(FSections.Objects[I]).Free;
    FSections.Delete(I);
  end;

  if self.AutoUpdate then
    UpdateFile;
  
  il.Leave;
end;

function TEncIniFile.GetCaseSensitive: Boolean;
begin
  il.Enter;
  Result := FSections.CaseSensitive;
  il.Leave;  
end;

procedure TEncIniFile.GetStrings(List: TStrings);
var
  I, J: Integer;
  Strings: TStrings;
  ListSplitFile: TStringList;
  K: Integer;
  split_site_data: Boolean;
  Found: Boolean;
  S: string;
  const splitredirectkeys : array [1..8] of string = ( 'username', 'password', 'max_dn',
  'max_up', 'slots', 'ProxyName', 'NoLoginMSG', 'IRCNick' );
begin
  split_site_data := config.ReadBool('sites', 'split_site_data', False);
  List.BeginUpdate;
  try
    for I := 0 to FSections.Count - 1 do
    begin
      List.Add('[' + FSections[I] + ']');
      Strings := TStrings(FSections.Objects[I]);

      if (split_site_data) then begin
  		  if AnsiEndsText('sites.dat', FFilename) and (1 = Pos('site-', FSections[I])) then
                  begin
  			  ListSplitFile := TStringList.Create;
    			for J := 0 to Strings.Count - 1 do begin
    			  S := Strings.Names[J];
	    		  Found := False;
		    	  for K := 1 to Length(splitredirectkeys) do
                          begin
    		  		if S = splitredirectkeys[K] then
                                begin
    			  	  Found := True;
		    		  break;
  				end;
     			  end;
	    		  if not Found then
		      		if (1 = Pos('rank-', S)) or (1 = Pos('bnc_', S)) then
				        Found := True;
    			  if Found then
	      			List.Add(Strings[J])
		  	    else
			    	  ListSplitFile.Add(Strings[J])
  			  end;
  	  		S := FSections[I];
	  	  	S := Copy(S, 6, Length(S)-5);
  	  		S := ExtractFilePath(ParamStr(0))+'rtpl'+PathDelim+S+'.settings';
  		  	ListSplitFile.SaveToFile(S);
  			  ListSplitFile.Free;
  		  end else begin
  			  for J := 0 to Strings.Count - 1 do List.Add(Strings[J]);
  		  end;
	    end else begin
      for J := 0 to Strings.Count - 1 do List.Add(Strings[J]);
      List.Add('');
    end;
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TEncIniFile.LoadValues;
var
  List: TStringList;
  myS: TMemoryStream;
begin
  if (FileName <> '') and FileExists(FileName) then
  begin
    myS:= TMemoryStream.Create;
    List := TStringList.Create;
    if not fSima then
    begin
      DecryptFileToStream(fFilename, myS, fPassHash, fCompression);
      List.LoadFromStream(myS);
    end else
    begin
      List.LoadFromFile(fFilename);
    end;
    try
      SetStrings(List);
    finally
      List.Free;
      myS.Free;
    end;
  end
  else
    Clear;
end;

procedure TEncIniFile.ReadSection(const Section: string;
  Strings: TStrings);
var
  I, J: Integer;
  SectionStrings: TStrings;
begin
  il.Enter;
  Strings.BeginUpdate;
  try
    Strings.Clear;
    I := FSections.IndexOf(Section);
    if I >= 0 then
    begin
      SectionStrings := TStrings(FSections.Objects[I]);
      for J := 0 to SectionStrings.Count - 1 do
        Strings.Add(SectionStrings.Names[J]);
    end;
  finally
    Strings.EndUpdate;
    il.Leave;
  end;
end;

procedure TEncIniFile.ReadSections(Strings: TStrings);
begin
  il.Enter;
  Strings.Assign(FSections);
  il.Leave;
end;

procedure TEncIniFile.ReadSectionValues(const Section: string;
  Strings: TStrings);
var
  I: Integer;
begin
  il.Enter;
  Strings.BeginUpdate;
  try
    Strings.Clear;
    I := FSections.IndexOf(Section);
    if I >= 0 then
      Strings.Assign(TStrings(FSections.Objects[I]));
  finally
    Strings.EndUpdate;
    il.Leave;
  end;
end;

function TEncIniFile.ReadString(const Section, Ident,
  Default: string): string;
var
  I: Integer;
  Strings: TStrings;
begin
  Result := Default;
  il.Enter;
  I := FSections.IndexOf(Section);
  if I >= 0 then
  begin
    Strings := TStrings(FSections.Objects[I]);
    I := Strings.IndexOfName(Ident);
    if I >= 0 then
      Result := Copy(Strings[I], Length(Ident) + 2, Maxint);
  end;
  il.Leave;
end;


procedure TEncIniFile.SetCaseSensitive(Value: Boolean);
var
  I: Integer;
begin
  il.Enter;
  if Value <> FSections.CaseSensitive then
  begin
    FSections.CaseSensitive := Value;
    for I := 0 to FSections.Count - 1 do
      with TMyHashedStringList(FSections.Objects[I]) do
      begin
        CaseSensitive := Value;
        Changed;
      end;
    TMyHashedStringList(FSections).Changed;
  end;
  il.Leave;
end;

procedure TEncIniFile.SetStrings(List: TStrings);
var
  I, J: Integer;
  S: string;
  Strings: TStrings;
  ListSplitFile: TStringList;
  split_site_data: Boolean;
begin
  Clear;
  il.Enter;
  Strings := nil;

  if config <> nil then begin
    split_site_data := config.ReadBool('sites', 'split_site_data', False);
  end else begin
    split_site_data := False;
  end;

  for I := 0 to List.Count - 1 do
  begin
    S := Trim(List[I]);
    if (S <> '') and (S[1] <> ';') then
      if (S[1] = '[') and (S[Length(S)] = ']') then
      begin
        Delete(S, 1, 1);
        SetLength(S, Length(S)-1);
        Strings := AddSection(Trim(S));

        if (split_site_data) then begin
          if AnsiEndsText('sites.dat', FFilename) then
          begin
            S := Trim(S);
            if 1 = Pos('site-', S) then
            begin
              S := Copy(S, 6, Length(S)-5);
              S := ExtractFilePath(ParamStr(0))+'rtpl'+PathDelim+S+'.settings';
              if FileExists(S) then
              begin
                ListSplitFile := TStringList.Create;
                try
                  ListSplitFile.LoadFromFile(S);
                  for J := 0 to ListSplitFile.Count - 1 do
                    Strings.Add(ListSplitFile[J]);
                finally
                  ListSplitFile.Free;
                end;
              end;
            end;
          end;
        end;
      end
      else
        if Strings <> nil then
        begin
          J := Pos('=', S);
          if J > 0 then // remove spaces before and after '='
            Strings.Add(Trim(Copy(S, 1, J-1)) + '=' + Trim(Copy(S, J+1, MaxInt)) )
          else
            Strings.Add(S);
        end;
  end;
  il.Leave;
end;

procedure TEncIniFile.UpdateFile;
var
  List: TStringList;
  myS: TMemoryStream;
begin
  myS:= TMemoryStream.Create;
  List := TStringList.Create;
  try
    GetStrings(List);

    if not fSima then
    begin
      List.SaveToStream(myS);
      EncryptStreamToFile(myS, fFilename, fPassHash, fCompression);
    end else
      list.SaveToFile(fFilename);
  finally
    List.Free;
    myS.Free;
  end;
end;


procedure TEncIniFile.Rename(const FileName: string; Reload: Boolean);
begin
  FFileName := FileName;
  if Reload then
    LoadValues;
end;

procedure TEncIniFile.WriteString(const Section, Ident, Value: String);
var
  I: Integer;
  S: string;
  Strings: TStrings;
begin
  il.Enter;
  I := FSections.IndexOf(Section);
  if I >= 0 then
    Strings := TStrings(FSections.Objects[I])
  else
    Strings := AddSection(Section);
  S := Ident + '=' + Value;
  I := Strings.IndexOfName(Ident);
  if I >= 0 then
    Strings[I] := S
  else
    Strings.Add(S);

  if self.AutoUpdate then
    UpdateFile;

  il.Leave;
end;

procedure TEncIniFile.SaveUnencrypted(filename: string);
var
  List: TStringList;
begin
  il.Enter;
  List := TStringList.Create;
  try
    GetStrings(List);

    List.SaveToFile(filename);
  finally
    List.Free;
    il.Leave;
  end;
end;


procedure TEncIniFile.LoadUnencrypted(filename: string);
var
  List: TStringList;
begin
  il.Enter;
  if (FileName <> '') and FileExists(FileName) then
  begin
    List := TStringList.Create;
    try
      List.LoadFromFile(filename);
      SetStrings(List);
    finally
      List.Free;
    end;
  end
  else
    Clear;
  il.Leave;
end;


{ TEncStringlist }

constructor TEncStringlist.Create(pass: string);
begin
  fPassHash:= slMD5String(pass);
  inherited Create;
end;

constructor TEncStringlist.Create(pass: TslMD5Data);
begin
  fPassHash:= pass;
  inherited Create;
end;

procedure TEncStringlist.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  if FileExists(FileName) then
  begin
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end;
end;

procedure TEncStringlist.LoadFromStream(Stream: TStream);
var s: TStringStream;
begin
  s:= TStringStream.Create( '' );
  BeginUpdate;
  try
    DecryptStreamToStream(Stream, s, fPassHash, True);
    SetTextStr(s.DataString);
  finally
    s.Free;
    EndUpdate;
  end;
end;

procedure TEncStringlist.SaveToStream(Stream: TStream);
var s: TStringStream;
begin
  s:= TStringStream.Create( GetTextStr );
  s.Position:= 0;
  try
    EncryptStreamToStream(s, Stream, FPassHash, True);
  finally
    s.Free;
  end;
end;

end.
