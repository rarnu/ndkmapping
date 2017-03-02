unit codeParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, codeTypes;

type

  { TCodeParser }

  TCodeParser = class
  private
    class function getClassName(ktClassCode: string): string;
    class function getPackageName(ktClassCode: string): string;
    class function getFields(APkgName: string; ktClassCode: string; ABasePath: string): TFieldList;
    class function getFieldInfo(APkgName: string; AFieldName: string; AFieldType: string; ABasePath: string; ktClassCode: string): TFieldInfo;
    class function getConstructorSig(AFieldList: TFieldList): string;
    class function typeToSig(AFullPackageName: string): string;
    class function typeToFullPackageName(AType: string; ABasePath: string; ktClassCode: string; APkgName: string): string;
    class function typeExtractPackageName(AFullPackageName: string): string;
    class function typeToCategory(AType: string): TFieldTypeCategory;
  public
    class function getClassInfo(ktClassCode: string; ABasePath: string): TClassInfo;

  end;

implementation

{ TCodeParser }

class function TCodeParser.getClassInfo(ktClassCode: string; ABasePath: string
  ): TClassInfo;
begin
  Result := TClassInfo.Create;
  with Result do begin
    ktClassName:= getClassName(ktClassCode);
    packageName:= getPackageName(ktClassCode);
    fullPackageName := Format('%s.%s', [packageName, ktClassName]);
    fieldList := getFields(packageName, ktClassCode, ABasePath);
    constructorSig:= getConstructorSig(fieldList);
  end;
end;

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

class function TCodeParser.getFields(APkgName: string; ktClassCode: string;
  ABasePath: string): TFieldList;

var
  r: string;
  arr: TStringArray;
  sfield: string;
  fieldarr: TStringArray;
  info: TFieldInfo;
begin
  Result := TFieldList.Create;
  r := ktClassCode.Substring(ktClassCode.IndexOf('('));
  r := r.Trim;
  r := r.Trim(['(', ')']);
  arr := r.Split(['var', 'val']);
  for sfield in arr do begin
    if (sfield.Trim <> '') then begin
      fieldarr := sfield.Trim.Split(':');
      info := getFieldInfo(APkgName, fieldarr[0].Trim, fieldarr[1].Replace('?', '', [rfIgnoreCase, rfReplaceAll]).Trim.Trim([',']), ABasePath, ktClassCode);
      if (info <> nil) then Result.Add(info);
    end;
  end;
end;

class function TCodeParser.getFieldInfo(APkgName: string; AFieldName: string;
  AFieldType: string; ABasePath: string; ktClassCode: string): TFieldInfo;
var
  mp1, mp2: string;
begin
  Result := TFieldInfo.Create;
  with Result do begin
    fieldName:= AFieldName;
    isArray:= TTypeConvert.KTypeIsArray(AFieldType);
    isList:= TTypeConvert.KTypeIsList(AFieldType);
    isMap:= TTypeConvert.KTypeIsMap(AFieldType);
    isSet:= TTypeConvert.KTypeIsSet(AFieldType);
    if (isList) then begin
      // list
      baseType := TFieldType.Create;
      baseType.fieldType:= 'ArrayList';
      baseType.fieldFullPackageName:= 'java.util.ArrayList';
      baseType.fieldPackageName:= 'java.util';
      baseType.fieldSignature:= 'Ljava/util/ArrayList;';
      baseType.fieldCategory:= ftcObject;
      // generic
      genericType1 := TFieldType.Create;
      genericType1.fieldType:= TTypeConvert.KTypeToSimpleKType(AFieldType);
      genericType1.fieldFullPackageName:= typeToFullPackageName(TTypeConvert.KTypeToSimpleKType(AFieldType), ABasePath, ktClassCode, APkgName);
      genericType1.fieldPackageName:= typeExtractPackageName(genericType1.fieldFullPackageName);
      genericType1.fieldSignature:= typeToSig(genericType1.fieldFullPackageName);
      genericType1.fieldCategory:= typeToCategory(TTypeConvert.KTypeToSimpleKType(AFieldType));
    end else if (isMap) then begin
      // map
      baseType := TFieldType.Create;
      baseType.fieldType:= 'HashMap';
      baseType.fieldFullPackageName:= 'java.util.HashMap';
      baseType.fieldPackageName:= 'java.util';
      baseType.fieldSignature:= 'Ljava/util/HashMap;';
      baseType.fieldCategory:= ftcObject;

      TTypeConvert.KTypeExtractMapKTypes(AFieldType, mp1, mp2);
      genericType1 := TFieldType.Create;
      genericType1.fieldType:= TTypeConvert.KTypeToSimpleKType(mp1);
      genericType1.fieldFullPackageName:= typeToFullPackageName(TTypeConvert.KTypeToSimpleKType(mp1), ABasePath, ktClassCode, APkgName);
      genericType1.fieldPackageName:= typeExtractPackageName(genericType1.fieldFullPackageName);
      genericType1.fieldSignature:= typeToSig(genericType1.fieldFullPackageName);
      genericType1.fieldCategory:= typeToCategory(TTypeConvert.KTypeToSimpleKType(mp1));

      genericType2 := TFieldType.Create;
      genericType2.fieldType:= TTypeConvert.KTypeToSimpleKType(mp2);
      genericType2.fieldFullPackageName:= typeToFullPackageName(TTypeConvert.KTypeToSimpleKType(mp2), ABasePath, ktClassCode, APkgName);
      genericType2.fieldPackageName:= typeExtractPackageName(genericType2.fieldFullPackageName);
      genericType2.fieldSignature:= typeToSig(genericType2.fieldFullPackageName);
      genericType2.fieldCategory:= typeToCategory(TTypeConvert.KTypeToSimpleKType(mp2));


    end else if (isSet) then begin
      // set
      baseType := TFieldType.Create;
      baseType.fieldType:= 'HashSet';
      baseType.fieldFullPackageName:= 'java.util.HashSet';
      baseType.fieldPackageName:= 'java.util';
      baseType.fieldSignature:= 'Ljava/util/HashSet;';
      baseType.fieldCategory:= ftcObject;
      // generic
      genericType1 := TFieldType.Create;
      genericType1.fieldType:= TTypeConvert.KTypeToSimpleKType(AFieldType);
      genericType1.fieldFullPackageName:= typeToFullPackageName(TTypeConvert.KTypeToSimpleKType(AFieldType), ABasePath, ktClassCode, APkgName);
      genericType1.fieldPackageName:= typeExtractPackageName(genericType1.fieldFullPackageName);
      genericType1.fieldSignature:= typeToSig(genericType1.fieldFullPackageName);
      genericType1.fieldCategory:= typeToCategory(TTypeConvert.KTypeToSimpleKType(AFieldType));
    end else if (isArray) then begin
      // array
      baseType := TFieldType.Create;
      baseType.fieldType:= TTypeConvert.KTypeToSimpleKType(AFieldType);
      baseType.fieldFullPackageName:= typeToFullPackageName(TTypeConvert.KTypeToSimpleKType(AFieldType), ABasePath, ktClassCode, APkgName);
      baseType.fieldPackageName:= typeExtractPackageName(baseType.fieldFullPackageName);
      baseType.fieldSignature:= '[' + typeToSig(baseType.fieldFullPackageName);
      baseType.fieldCategory:= typeToCategory(TTypeConvert.KTypeToSimpleKType(AFieldType));
    end else begin
      baseType := TFieldType.Create;
      baseType.fieldType:= TTypeConvert.KTypeToSimpleKType(AFieldType);
      baseType.fieldFullPackageName:= typeToFullPackageName(TTypeConvert.KTypeToSimpleKType(AFieldType), ABasePath, ktClassCode, APkgName);
      baseType.fieldPackageName:= typeExtractPackageName(baseType.fieldFullPackageName);
      baseType.fieldSignature:= typeToSig(baseType.fieldFullPackageName);
      baseType.fieldCategory:= typeToCategory(AFieldType);
    end;
  end;
end;

class function TCodeParser.getConstructorSig(AFieldList: TFieldList): string;
var
  r: string = '';
  i: Integer;
begin
  for i := 0 to AFieldList.Count - 1 do begin
    r += AFieldList[i].baseType.fieldSignature;
  end;
  Exit(Format('(%s)V', [r]));
end;

class function TCodeParser.typeToSig(AFullPackageName: string): string;
const
  SIMPLE_TYPE: array[0..7] of string = ('Int', 'Double', 'Float', 'Short', 'Long', 'Boolean', 'Byte', 'Char');
  SIMPLE_SIG: array[0..7] of string = ('I', 'D', 'F', 'S', 'J', 'Z', 'B', 'C');
var
  i: Integer;
begin
  for i := 0 to Length(SIMPLE_TYPE) - 1 do begin
    if (SIMPLE_TYPE[i] = AFullPackageName) then Exit(SIMPLE_SIG[i]);
  end;
  Exit(Format('L%s;', [AFullPackageName.Replace('.', '/', [rfIgnoreCase, rfReplaceAll])]));
end;

class function TCodeParser.typeToFullPackageName(AType: string;
  ABasePath: string; ktClassCode: string; APkgName: string): string;
const
  SIMPLE_TYPE: array[0..7] of string = ('Int', 'Double', 'Float', 'Short', 'Long', 'Boolean', 'Byte', 'Char');
var
  src: TSearchRec;
  r: string;
  st: string;
  isSimple: Boolean = False;
  isImport: Boolean = False;
  isPath: Boolean = False;
  codeList: TStringList;
  i: Integer;
  simport: string;
begin
  for st in SIMPLE_TYPE do begin
    if (st = AType) then begin
      isSimple:= True;
      Break;
    end;
  end;
  if (isSimple) then Exit(AType);

  codeList := TStringList.Create;
  codeList.Text:= ktClassCode;
  for i := 0 to codeList.Count - 1 do begin
    if (codeList[i].Trim.StartsWith('import ')) then begin
      simport:= codeList[i].Trim;
      if (simport.EndsWith('.' + AType)) then begin
        isImport:= True;
        r := simport.Replace('import', '', []).Trim;
        Break;
      end;

    end;
  end;
  codeList.Free;
  if (isImport) then Exit(r);

  if (FindFirst(ABasePath + '*.kt', faAnyFile, src) = 0) then begin
    repeat
      if (src.Name = '.') or (src.Name = '..') then Continue;
      if (src.Name = AType + '.kt') then begin
        isPath:= True;
        r := APkgName + '.' + src.Name;
        if (r.EndsWith('.kt')) then r := r.Substring(0, r.Length - 3);
        Break;
      end;
    until FindNext(src) <> 0;
    FindClose(src);
  end;

  if (isPath) then Exit(r);

  r := 'java.lang.' + AType;
  Exit(r);

end;

class function TCodeParser.typeExtractPackageName(AFullPackageName: string
  ): string;
var
  r: string;
begin
  if (AFullPackageName.Contains('.')) then begin
    r := AFullPackageName.Substring(0, AFullPackageName.LastIndexOf('.'));
  end else begin
    r := AFullPackageName;
  end;
  Exit(r);
end;

class function TCodeParser.typeToCategory(AType: string): TFieldTypeCategory;
const
  SIMPLE_TYPE: array[0..7] of string = ('Int', 'Double', 'Float', 'Short', 'Long', 'Boolean', 'Byte', 'Char');
var
  st: string;
  isSimple: Boolean = False;
begin
  for st in SIMPLE_TYPE do begin
    if (st = AType) then begin
      isSimple:= True;
      Break;
    end;
  end;
  if (isSimple) then Exit(ftcSimple);
  if (AType = 'String') then Exit(ftcString);
  Exit(ftcObject);
end;

end.


