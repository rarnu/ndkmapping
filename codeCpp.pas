unit codeCpp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, codeTypes;

type

  { TCodeCpp }

  TCodeCpp = class
  private
    class procedure head(codeList: TStringList; AClassInfo: TClassInfo);
    class procedure code(AOutPath: string; AClassInfo: TClassInfo; AMaxArraySize: Integer);
  public
    class procedure generate(AOutPath: string; AKtFile: string; AMaxArraySize: Integer);
  end;

implementation

uses
  codeParser;

{ TCodeCpp }

class procedure TCodeCpp.head(codeList: TStringList; AClassInfo: TClassInfo);
var
  i: Integer;
  tInc: string;
begin
  // head
  with codeList do begin
    Add('#include <jni.h>');
    Add('#include <stdint.h>');
    Add('#include <stddef.h>');
    Add('#include <stdlib.h>');
    Add('#include <string>');
    Add('#include <list>');
    Add('#include <map>');
    Add('#include <set>');
    for i := 0 to AClassInfo.fieldList.Count - 1 do begin
      if (AClassInfo.fieldList[i].baseType.fieldCategory = ftcObject) then begin
        if (not AClassInfo.fieldList[i].isList) and (not AClassInfo.fieldList[i].isMap) and (not AClassInfo.fieldList[i].isSet) then begin
          tInc:= Format('#include "%s.h"', [AClassInfo.fieldList[i].baseType.fieldType]);
          WriteLn('1: ' + tInc);
          if (Text.IndexOf(tInc) = -1) then Add(tInc);
        end else begin
          if (AClassInfo.fieldList[i].genericType1 <> nil) then begin
            if (AClassInfo.fieldList[i].genericType1.fieldCategory = ftcObject) then begin
              tInc:= Format('#include "%s.h"', [AClassInfo.fieldList[i].genericType1.fieldType]);
              WriteLn('2: ' + tInc);
              if (Text.IndexOf(tInc) = -1) then Add(tInc);
            end;
          end;
          if (AClassInfo.fieldList[i].genericType2 <> nil) then begin
            if (AClassInfo.fieldList[i].genericType2.fieldCategory = ftcObject) then begin
              tInc:= Format('#include "%s.h"', [AClassInfo.fieldList[i].genericType2.fieldType]);
              WriteLn('3: ' + tInc);
              if (Text.IndexOf(tInc) = -1) then Add(tInc);
            end;
          end;

        end;
      end;
    end;
    Add('using namespace std;');
  end;
end;

class procedure TCodeCpp.code(AOutPath: string; AClassInfo: TClassInfo;
  AMaxArraySize: Integer);
var
  codeList: TStringList;
  i: Integer;
  c: string;
begin
  // generate head code
  codeList := TStringList.Create;
  head(codeList, AClassInfo);
  with codeList do begin
    Add(Format('class %s {', [AClassInfo.ktClassName]));
    Add('public:');
    for i := 0 to AClassInfo.fieldList.Count - 1 do begin
      if (AClassInfo.fieldList[i].isArray) then begin
        Add(Format('    %s %s[%d];', [TTypeConvert.KTypeToCType(AClassInfo.fieldList[i].baseType.fieldType), AClassInfo.fieldList[i].fieldName, AMaxArraySize]));
      end else if (AClassInfo.fieldList[i].isList) then begin
        Add(Format('    list<%s> %s;', [TTypeConvert.KTypeToCType(AClassInfo.fieldList[i].genericType1.fieldType), AClassInfo.fieldList[i].fieldName]));
      end else if (AClassInfo.fieldList[i].isMap) then begin
        Add(Format('    map<%s, %s> %s;', [TTypeConvert.KTypeToCType(AClassInfo.fieldList[i].genericType1.fieldType), TTypeConvert.KTypeToCType(AClassInfo.fieldList[i].genericType2.fieldType), AClassInfo.fieldList[i].fieldName]));
      end else if (AClassInfo.fieldList[i].isSet) then begin
        Add(Format('    set<%s> %s;', [TTypeConvert.KTypeToCType(AClassInfo.fieldList[i].genericType1.fieldType), AClassInfo.fieldList[i].fieldName]));
      end else begin
        Add(Format('    %s %s;', [TTypeConvert.KTypeToCType(AClassInfo.fieldList[i].baseType.fieldType), AClassInfo.fieldList[i].fieldName]));
      end;
    end;
    Add('    static ' + AClassInfo.ktClassName + '* fromJObject(JNIEnv *env, jobject obj);');
    Add('    jobject toJObject(JNIEnv *env);');
    Add('};');
    SaveToFile(AOutPath + AClassInfo.ktClassName + '.h');
    Free;
  end;

  codeList := TStringList.Create;
  with codeList do begin
    Add(Format('#include "%s.h"', [AClassInfo.ktClassName]));
    Add('#include "JNIUtils.h"');
    // TODO: generate cpp
    SaveToFile(AOutPath + AClassInfo.ktClassName + '.cpp');
    Free;
  end;


end;

class procedure TCodeCpp.generate(AOutPath: string; AKtFile: string;
  AMaxArraySize: Integer);
var
  codeText: string = '';
  clsInfo: TClassInfo;
begin
  with TStringList.Create do begin
    LoadFromFile(AKtFile);
    codeText:= Text;
    Free;
  end;
  clsInfo := TCodeParser.getClassInfo(codeText, ExtractFilePath(AKtFile));
  // WriteLn(clsInfo.ToString);
  code(AOutPath, clsInfo, AMaxArraySize);
  if (clsInfo <> nil) then clsInfo.Free;
end;

end.

