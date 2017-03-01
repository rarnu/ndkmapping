unit codeTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  TParamMap = specialize TFPGMap<String, String>;

type

  { TTypeConvert }

  TTypeConvert = class
  public
    class function KTypeIsArray(AType: string): Boolean;
    class function KTypeIsList(AType: string): Boolean;
    class function KTypeIsMap(AType: string): Boolean;
    class function KTypeIsSet(AType: string): Boolean;
    class function KTypeToCType(AType: string): string;
    class function KTypeToCMapType(AType: string): string;
    class function KTypeToPType(AType: string): string;
    class function KTypeToCallMethod(AType: string): string;
    class function KTypeToGetSig(AType: string; AFullFields: TParamMap): string;
    class function KTypeToSetSig(AType: string; AFullFields: TParamMap): string;
    class function KTypeToSig(AType: string; AFullFields: TParamMap): string;
    class function KFieldToGetName(AField: string): string;
    class function KFieldToSetName(AField: string): string;
  end;

implementation

{ TTypeConvert }

class function TTypeConvert.KTypeIsArray(AType: string): Boolean;
begin
  Result := AType.StartsWith('Array<');
end;

class function TTypeConvert.KTypeIsList(AType: string): Boolean;
begin
  Result := AType.Contains('List<');
end;

class function TTypeConvert.KTypeIsMap(AType: string): Boolean;
begin
  Result := AType.Contains('Map<');
end;

class function TTypeConvert.KTypeIsSet(AType: string): Boolean;
begin
  Result := AType.Contains('Set<');
end;

class function TTypeConvert.KTypeToCType(AType: string): string;
const
  HEAD_ARRAY = 'Array<';
  HEAD_LIST = 'List<';
  HEAD_MAP = 'Map<';
  HEAD_SET = 'Set<';
var
  r: string = '';
begin
  if (AType.StartsWith(HEAD_ARRAY)) then AType:= AType.Replace(HEAD_ARRAY, '', [rfIgnoreCase, rfReplaceAll]).Trim.Trim(['>']);
  if (AType.Contains(HEAD_LIST)) then AType:= AType.Substring(AType.IndexOf(HEAD_LIST) + HEAD_LIST.Length).Trim.Trim(['>']);
  if (AType.Contains(HEAD_MAP)) then AType:= AType.Substring(AType.IndexOf(HEAD_MAP) + HEAD_MAP.Length).Trim.Trim(['>']);
  if (AType.Contains(HEAD_SET)) then AType:= AType.Substring(AType.IndexOf(HEAD_SET) + HEAD_SET.Length).Trim.Trim(['>']);
  if (AType = 'Int') then r := 'int';
  if (AType = 'Double') then r := 'double';
  if (AType = 'Boolean') then r := 'bool';
  if (AType = 'Byte') then r := 'unsigned char';
  if (AType = 'Char') then r := 'char';
  if (AType = 'Short') then r := 'short';
  if (AType = 'Long') then r := 'long long';
  if (AType = 'Float') then r := 'float';
  if (AType = 'String') then r := 'string';
  if (r = '') then r := AType + '*';
  Exit(r);
end;

class function TTypeConvert.KTypeToCMapType(AType: string): string;
var
  arr: TStringArray;
  s: string;
  p1: string;
  p2: string;
begin
  AType:= AType.Substring(AType.IndexOf('<')).Trim.Trim(['<', '>']);
  arr := AType.Split([',']);
  p1 := KTypeToCType(arr[0].Trim);
  p2 := KTypeToCType(arr[1].Trim);
  Exit(Format('%s, %s', [p1, p2]));
end;

class function TTypeConvert.KTypeToPType(AType: string): string;
var
  r: string = '';
begin
  if (AType = 'Int') then r := 'Integer';
  if (AType = 'Double') then r := 'Double';
  if (AType = 'Boolean') then r := 'Boolean';
  if (AType = 'Byte') then r := 'Byte';
  if (AType = 'Char') then r := 'Char';
  if (AType = 'Short') then r := 'ShortInt';
  if (AType = 'Long') then r := 'Int64';
  if (AType = 'Float') then r := 'Extended';
  if (AType = 'String') then r := 'String';
  if (r = '') then r := AType;
  Exit(r);
end;

class function TTypeConvert.KTypeToCallMethod(AType: string): string;
var
  r: string = '';
begin
  if (AType = 'Int') then r := 'CallIntMethod';
  if (AType = 'Double') then r := 'CallDoubleMethod';
  if (AType = 'Boolean') then r := 'CallBooleanMethod';
  if (AType = 'Byte') then r := 'CallByteMethod';
  if (AType = 'Char') then r := 'CallCharMethod';
  if (AType = 'Short') then r := 'CallShortMethod';
  if (AType = 'Long') then r := 'CallLongMethod';
  if (AType = 'Float') then r := 'CallFloatMethod';
  if (r = '') then r := 'CallObjectMethod';
  Exit(r);
end;

class function TTypeConvert.KTypeToGetSig(AType: string; AFullFields: TParamMap
  ): string;
const
  HEAD_ARRAY = 'Array<';
  HEAD_LIST = 'List<';
  HEAD_MAP = 'Map<';
  HEAD_SET = 'Set<';
var
  r: string = '';
  isArray: Boolean = False;
  isList: Boolean = False;
  isMap: Boolean = False;
  isSet: Boolean = False;
begin
  if (AType.StartsWith(HEAD_ARRAY)) then begin
    isArray:= True;
    AType:= AType.Replace(HEAD_ARRAY, '', [rfIgnoreCase, rfReplaceAll]).Trim.Trim(['>']);
  end;
  if (AType.Contains(HEAD_LIST)) then begin
    isList:= True;
    AType:= AType.Substring(AType.IndexOf(HEAD_LIST) + HEAD_LIST.Length).Trim.Trim(['>']);
  end;
  if (AType.Contains(HEAD_MAP)) then begin
    isMap:= True;
    AType:= AType.Substring(AType.IndexOf(HEAD_MAP) + HEAD_MAP.Length).Trim.Trim(['>']);
  end;
  if (AType.Contains(HEAD_SET)) then begin
    isSet:= True;
    AType:= AType.Substring(AType.IndexOf(HEAD_SET) + HEAD_SET.Length).Trim.Trim(['>']);
  end;

  if (isArray) then begin
    if (AType = 'Int') then r := '()[I';
    if (AType = 'Double') then r := '()[D';
    if (AType = 'Boolean') then r := '()[Z';
    if (AType = 'Byte') then r := '()[B';
    if (AType = 'Char') then r := '()[C';
    if (AType = 'Short') then r := '()[S';
    if (AType = 'Long') then r := '()[J';
    if (AType = 'Float') then r := '()[F';
    if (r = '') then
      if (AFullFields.IndexOf(AType) <> -1) then
        r := '()[L' + AFullFields.KeyData[AType].replace('.', '/', [rfReplaceAll, rfIgnoreCase]) + ';'
      else
        r := '()[Ljava/lang/' + AType + ';';
  end else if (isList) then begin
    // list
    r := '()Ljava/util/ArrayList;';
  end else if (isMap) then begin
    // map
    r := '()Ljava/util/HashMap;';
  end else if (isSet) then begin
    // set
    r := '()Ljava/util/HashSet;';
  end else begin
    if (AType = 'Int') then r := '()I';
    if (AType = 'Double') then r := '()D';
    if (AType = 'Boolean') then r := '()Z';
    if (AType = 'Byte') then r := '()B';
    if (AType = 'Char') then r := '()C';
    if (AType = 'Short') then r := '()S';
    if (AType = 'Long') then r := '()J';
    if (AType = 'Float') then r := '()F';
    if (r = '' ) then
      if (AFullFields.IndexOf(AType) <> -1) then
        r := '()L' + AFullFields.KeyData[AType].replace('.', '/', [rfIgnoreCase, rfReplaceAll]) + ';'
      else
        r := '()Ljava/lang/' + AType + ';';
  end;
  Exit(r);
end;

class function TTypeConvert.KTypeToSetSig(AType: string; AFullFields: TParamMap
  ): string;
const
  HEAD_ARRAY = 'Array<';
  HEAD_LIST = 'List<';
  HEAD_MAP = 'Map<';
  HEAD_SET = 'Set<';
var
  r: string;
  isArray: Boolean = False;
  isList: Boolean = False;
  isMap: Boolean = False;
  isSet: Boolean = False;
begin
  if (AType.StartsWith(HEAD_ARRAY)) then begin
    isArray:= True;
    AType:= AType.Replace(HEAD_ARRAY, '', [rfIgnoreCase, rfReplaceAll]).Trim.Trim(['>']);
  end;
  if (AType.Contains(HEAD_LIST)) then begin
    isList:= True;
    AType:= AType.Substring(AType.IndexOf(HEAD_LIST) + HEAD_LIST.Length).Trim.Trim(['>']);
  end;
  if (AType.Contains(HEAD_MAP)) then begin
    isMap:= True;
    AType:= AType.Substring(AType.IndexOf(HEAD_MAP) + HEAD_MAP.Length).Trim.Trim(['>']);
  end;
  if (AType.Contains(HEAD_SET)) then begin
    isSet:= True;
    AType:= AType.Substring(AType.IndexOf(HEAD_SET) + HEAD_SET.Length).Trim.Trim(['>']);
  end;

  if (isArray) then begin
    if (AType = 'Int') then r := '([I)V';
    if (AType = 'Double') then r := '([D)V';
    if (AType = 'Boolean') then r := '([Z)V';
    if (AType = 'Byte') then r := '([B)V';
    if (AType = 'Char') then r := '([C)V';
    if (AType = 'Short') then r := '([S]V';
    if (AType = 'Long') then r := '([J)V';
    if (AType = 'Float') then r := '([F)V';
    if (r = '') then
      if (AFullFields.IndexOf(AType) <> -1) then
        r := '([L' + AFullFields.KeyData[AType].replace('.', '/', [rfReplaceAll, rfIgnoreCase]) + ';)V'
      else
        r := '([Ljava/lang/' + AType + ';)V';
  end  else if (isList) then begin
    // list
    r := '(Ljava/util/ArrayList;)V'
  end else if (isMap) then begin
    // map
    r := '(Ljava/util/HashMap;)V'
  end else if (isSet) then begin
    // set
    r := '(Ljava/util/HashSet;)V'
  end else begin
    if (AType = 'Int') then r := '(I)V';
    if (AType = 'Double') then r := '(D)V';
    if (AType = 'Boolean') then r := '(Z)V';
    if (AType = 'Byte') then r := '(B)V';
    if (AType = 'Char') then r := '(C)V';
    if (AType = 'Short') then r := '(S)V';
    if (AType = 'Long') then r := '(J)V';
    if (AType = 'Float') then r := '(F)V';
    if (r = '') then
      if (AFullFields.IndexOf(AType) <> -1) then
        r := '(L' + AFullFields.KeyData[AType].replace('.', '/', [rfIgnoreCase, rfReplaceAll]) + ';)V'
      else
        r := '(Ljava/lang/' + AType + ';)V';
  end;
  Exit(r);
end;

class function TTypeConvert.KTypeToSig(AType: string; AFullFields: TParamMap
  ): string;
var
  r: string;
begin
  if (AType = 'Int') then r := 'I';
  if (AType = 'Double') then r := 'D';
  if (AType = 'Boolean') then r := 'Z';
  if (AType = 'Byte') then r := 'B';
  if (AType = 'Char') then r := 'C';
  if (AType = 'Short') then r := 'S';
  if (AType = 'Long') then r := 'J';
  if (AType = 'Float') then r := 'F';
  if (r = '') then
    if (AFullFields.IndexOf(AType) <> -1) then
      r := 'L' + AFullFields.KeyData[AType].replace('.', '/', [rfIgnoreCase, rfReplaceAll]) + ';'
    else
      r := 'Ljava/lang/' + AType + ';';
  Exit(r);
end;

class function TTypeConvert.KFieldToGetName(AField: string): string;
begin
  Result := 'get' + UpperCase(AField[1]) + AField.Substring(1);
end;

class function TTypeConvert.KFieldToSetName(AField: string): string;
begin
  Result := 'set' + UpperCase(AField[1]) + AField.Substring(1);
end;

end.
