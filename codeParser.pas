unit codeParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, codeTypes;

type

  { TCodeParser }

  TCodeParser = class
  public
    class function getClassName(ktClassCode: string): string;
    class function getPackageName(ktClassCode: string): string;
    class function getFields(ktClassCode: string): TParamMap;
    class function getFieldsFullPath(ktClassCode: string; oriMap: TParamMap; oriPkg: string; oriPath: string): TParamMap;
    class function getConstructoreSig(ktClassCode: string; fullMap: TParamMap): string;
    class function getConstructoreParams(ktClassCode: string; fullMap: TParamMap): string;
  end;

implementation

{ TCodeParser }

class function TCodeParser.getClassName(ktClassCode: string): string;
const
  HEAD = 'data class';
var
  r: string;
begin
  r := ktClassCode.Substring(ktClassCode.IndexOf(HEAD) + HEAD.Length);
  r := r.Substring(0, r.IndexOf('('));
  r := r.Trim;
  Exit(r);
end;

class function TCodeParser.getPackageName(ktClassCode: string): string;
const
  HEAD = 'package';
var
  r: string;
begin
  r := ktClassCode.Substring(ktClassCode.IndexOf(HEAD) + HEAD.Length);
  r := r.Substring(0, r.IndexOf(#10));
  r := r.Trim;
  Exit(r);
end;

class function TCodeParser.getFields(ktClassCode: string): TParamMap;
var
  r: string;
  arr: TStringArray;
  sfield: string;
  fieldarr: TStringArray;
begin
  r := ktClassCode.Substring(ktClassCode.IndexOf('('));
  r := r.Trim;
  r := r.Trim(['(', ')']);
  arr := r.Split(['var', 'val']);
  Result := TParamMap.Create;
  for sfield in arr do begin
    if (sfield.Trim <> '') then begin
      fieldarr := sfield.Trim.Split(':');
      Result.Add(fieldarr[0].Trim, fieldarr[1].Replace('?', '', [rfIgnoreCase, rfReplaceAll]).Trim.Trim([',']));
    end;
  end;
end;

class function TCodeParser.getFieldsFullPath(ktClassCode: string;
  oriMap: TParamMap; oriPkg: string; oriPath: string): TParamMap;
var
  importList: TStringList;
  codeList: TStringList;
  i, j: Integer;
  src: TSearchRec;
begin
  Result := TParamMap.Create;
  codeList := TStringList.Create;
  codeList.Text:= ktClassCode;

  importList := TStringList.Create;
  for i := 0 to codeList.Count - 1 do if (codeList[i].Trim.StartsWith('import ')) then importList.Add(codeList[i].Trim);
  if (not oriPath.EndsWith('/')) then oriPath += '/';
  if (oriPath = '/') then oriPath:= './';
  if (FindFirst(oriPath + '*.kt', faAnyFile, src) = 0) then begin
    repeat
      if (src.Name = '.') or (src.Name = '..') then Continue;
      importList.Add(Format('import %s.%s', [oriPkg, string(src.Name).Replace('.kt', '', [rfIgnoreCase, rfReplaceAll])]));
    until FindNext(src) <> 0;
    FindClose(src);
  end;

  for i := 0 to oriMap.Count - 1 do
    for j := 0 to importList.Count - 1 do
      if (importList[j].Trim.EndsWith('.' + oriMap.Data[i])) then
        Result.Add(oriMap.Data[i], importList[j].Trim.Replace('import', '', [rfIgnoreCase, rfReplaceAll]).Trim);

  importList.Free;
  codeList.Free;
end;

class function TCodeParser.getConstructoreSig(ktClassCode: string; fullMap: TParamMap): string;
var
  sig: string = '';
  r: string;
  arr: TStringArray;
  sfield: string;
  sx: string;
  ret: string = '';
begin
  // array, list, map, set
  r := ktClassCode.Substring(ktClassCode.IndexOf('('));
  r := r.Trim;
  r := r.Trim(['(', ')']);
  arr := r.Split(['var', 'val']);

  for sfield in arr do begin
    if (sfield.Trim <> '') then begin
      sx := sfield.Split(':')[1].Replace('?', '', [rfIgnoreCase, rfReplaceAll]).Trim.Trim([',']);
      sig := TTypeConvert.KTypeToSig(sx, fullMap);
      ret += sig;
    end;
  end;
  ret := '(' + ret + ')V';
  Exit(ret);
end;

class function TCodeParser.getConstructoreParams(ktClassCode: string;
  fullMap: TParamMap): string;
var
  sig: string = '';
  r: string;
  arr: TStringArray;
  sfield: string;
  sx: string;
  sf: string;
  ret: string = '';
begin
  r := ktClassCode.Substring(ktClassCode.IndexOf('('));
  r := r.Trim;
  r := r.Trim(['(', ')']);
  arr := r.Split(['var', 'val']);

  for sfield in arr do begin
    if (sfield.Trim <> '') then begin
      sf := sfield.Split(':')[0].Trim;
      sx := sfield.Split(':')[1].Replace('?', '', [rfIgnoreCase, rfReplaceAll]).Trim.Trim([',']);
      if (TTypeConvert.KTypeIsArray(sx)) then begin
        // array
        sig := TTypeConvert.KTypeToCType(sx);
        if (sig = 'string') then
          ret += Format('JNIUtils::arrayToStringArray(env, %s), ', [sf])
        else if (sig.Contains('*')) then
          ret += Format('JNIUtils::arrayToObjectArray(env, %s), ', [sf])
        else
          ret += Format('JNIUtils::arrayTo%sArray(env, %s), ', [TTypeConvert.KFieldToFirstUpper(sig) ,sf]);
      end else if (TTypeConvert.KTypeIsList(sx)) then begin
        sig := TTypeConvert.KTypeToCType(sx);
        // list
        ret += Format('JNIExt::%s_listToJArrayList(env, %s), ', [sf, sf]);
      end else if (TTypeConvert.KTypeIsMap(sx)) then begin
        // map
        sig := TTypeConvert.KTypeToCMapType(sx);
        ret += Format('JNIExt::%s_mapToJHashMap(env, %s), ', [sf, sf]);
      end else if (TTypeConvert.KTypeIsSet(sx)) then begin
        sig := TTypeConvert.KTypeToCType(sx);
        // set
        ret += Format('JNIExt::%s_setToJHashSet(env, %s), ', [sf, sf]);
      end else begin
        sig := TTypeConvert.KTypeToCType(sx);
        if (sig = 'string') then
          ret += Format('env->NewStringUTF(%s.data()), ', [sf])
        else if (sig.Contains('*')) then
          ret += Format('%s->toJObject(env), ', [sf])
        else
          ret += sf + ', ';
      end;
    end;
  end;
  ret := ret.Trim.Trim([',']);
  Exit(ret);
end;

end.


