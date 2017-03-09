unit testCode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, codeTypes;

type

  { TTestJava }

  { TTestCode }

  TTestCode = class
  private
    class procedure headCpp(APkg: string; codeList: TStringList);
    class procedure headCppImpl(codeList: TStringList);
    class procedure headPas(Apkg: string; codeList: TStringList);
    class procedure headJava(APkg: string; codeList: TStringList);
    class procedure headKotlin(APkg: string; codeList: TStringList);
  public
    class procedure generateJava(APkg: string; AOutPath: string; AClassInfo: TClassInfo);
    class procedure generateKotlin(APkg: string; AOutPath: string; AClassInfo: TClassInfo);
    class procedure generateCppJNI(APkg: string; AOutPath: string; AClassInfo: TClassInfo);
    class procedure generatePasJNI(Apkg: string; AOutPath: string; AClassInfo: TClassInfo);
  end;

implementation

{ TTestCode }

class procedure TTestCode.headCpp(APkg: string; codeList: TStringList);
begin
  with codeList do begin
    Add('#include <jni.h>');
    Add('');
    Add('#ifndef JNI_TEST_H');
    Add('#define JNI_TEST_H');
    Add('#ifdef __cplusplus');
    Add('extern "C" {');
    Add('#endif // __cplusplus');
    Add('');
    Add('');
    Add('#ifdef __cplusplus');
    Add('}');
    Add('#endif // __cplusplus');
    Add('#endif // SAMPLE_JNI_H');
  end;
end;

class procedure TTestCode.headCppImpl(codeList: TStringList);
begin
  with codeList do begin
    Add('#include "JNITest.h"');
    Add('');
  end;
end;

class procedure TTestCode.headPas(Apkg: string; codeList: TStringList);
begin
  with codeList do begin
    Add('library JNITest;');
    Add('');
    Add('{$mode objfpc}{$H+}');
    Add('');
    Add('uses');
    Add('  cthreads, Classes, SysUtils, JNI2;');
    Add('');
    Add('');
    Add('exports');
    Add('');
    Add('');
    Add('begin');
    Add('end.');
  end;
end;

class procedure TTestCode.headJava(APkg: string; codeList: TStringList);
begin
  with codeList do begin
    Add(Format('package %s;', [APkg]));
    Add('');
    Add('');
    Add('public class JNITest {');
    Add('');
    Add('    static {');
    Add('        System.loadLibrary("ndkmapping_generated");');
    Add('    }');
    Add('');
    Add('');
    Add('}');
  end;
end;

class procedure TTestCode.headKotlin(APkg: string; codeList: TStringList);
begin
  with codeList do begin
    Add(Format('package %s', [APkg]));
    Add('');
    Add('');
    Add('object JNITest {');
    Add('');
    Add('    init {');
    Add('        System.loadLibrary("ndkmapping_generated")');
    Add('    }');
    Add('');
    Add('');
    Add('}');
  end;
end;

class procedure TTestCode.generateJava(APkg: string; AOutPath: string;
  AClassInfo: TClassInfo);
var
  codeList: TStringList;
  path: string;
  idx: Integer = 0;
begin
  path := AOutPath + 'JNITest.java';
  codeList := TStringList.Create;
  with codeList do begin
    if (FileExists(path)) then
      LoadFromFile(path)
    else
      headJava(APkg, codeList);
    while True do begin
      if (Strings[idx].StartsWith('package ')) then begin
        idx += 2;
        Insert(idx, Format('import %s;', [AClassInfo.fullPackageName]));
        idx := Count - 2;
        Insert(idx, Format('    public static native %s test%s(%s c);', [
          AClassInfo.ktClassName, AClassInfo.ktClassName, AClassInfo.ktClassName
        ]));
        Break;
      end;
      Inc(idx);
    end;
    SaveToFile(path);
    Free;
  end;

end;

class procedure TTestCode.generateKotlin(APkg: string; AOutPath: string;
  AClassInfo: TClassInfo);
var
  codeList: TStringList;
  path: string;
  idx: Integer = 0;
begin
  path := AOutPath + 'JNITest.kt';
  codeList := TStringList.Create;
  with codeList do begin
    if (FileExists(path)) then
      LoadFromFile(path)
    else
      headKotlin(APkg, codeList);
    while True do begin
      if (Strings[idx].StartsWith('package ')) then begin
        idx += 2;
        Insert(idx, Format('import %s', [AClassInfo.fullPackageName]));
        idx := Count - 2;
        Insert(idx, Format('    external fun test%s(c: %s?): %s?', [
          AClassInfo.ktClassName, AClassInfo.ktClassName, AClassInfo.ktClassName
        ]));
        Break;
      end;
      Inc(idx);
    end;
    SaveToFile(path);
    Free;
  end;
end;

class procedure TTestCode.generateCppJNI(APkg: string; AOutPath: string;
  AClassInfo: TClassInfo);
var
  codeList: TStringList;
  path: string;
  idx: Integer = 0;
begin
  // generate cpp jni
  path := AOutPath + 'JNITest.h';
  codeList := TStringList.Create;
  with codeList do begin
    if (FileExists(path)) then
      LoadFromFile(path)
    else
      headCpp(APkg, codeList);
    while True do begin
      if (Strings[idx].StartsWith('#endif // __cplusplus')) then begin
        idx += 2;
        Insert(idx, Format('JNIEXPORT jobject JNICALL Java_%s_JNITest_test%s(JNIEnv *, jclass, jobject);', [
          AClassInfo.packageName.Replace('.', '_', [rfIgnoreCase, rfReplaceAll]), AClassInfo.ktClassName]));
        Break;
      end;
      Inc(idx);
    end;
    SaveToFile(path);
    Free;
  end;

  path := AOutPath + 'JNITest.cpp';
  codeList := TStringList.Create;
  idx := 0;
  with codeList do begin
    if (FileExists(path)) then
      LoadFromFile(path)
    else
      headCppImpl(codeList);

    while True do begin
      if (Strings[idx].StartsWith('#include "JNITest.h"')) then begin
        idx += 1;
        Insert(idx, Format('#include "%s.h"', [AClassInfo.ktClassName]));
        Add(Format('JNIEXPORT jobject JNICALL Java_%s_JNITest_test%s(JNIEnv *env, jclass obj, jobject o) {', [
          AClassInfo.packageName.Replace('.', '_', [rfIgnoreCase, rfReplaceAll]),
          AClassInfo.ktClassName]));
        Add(Format('    %s* nativeObj = %s::fromJObject(env, o);', [AClassInfo.ktClassName, AClassInfo.ktClassName]));
        Add('    return nativeObj->toJObject(env);');
        Add('}');
        Add('');
        Break;
      end;
      Inc(idx);
    end;
    SaveToFile(path);
    Free;
  end;
end;

class procedure TTestCode.generatePasJNI(Apkg: string; AOutPath: string;
  AClassInfo: TClassInfo);
var
  codeList: TStringList;
  path: string;
  oidx: Integer = 0;
  idx: Integer = 0;
  strTmp: string;
begin
  // generate pas jni
  path := AOutPath + 'JNITest.pas';
  codeList := TStringList.Create;
  with codeList do begin
    if (FileExists(path)) then
      LoadFromFile(path)
    else
      headPas(Apkg, codeList);
    while True do begin
      if (Strings[idx].StartsWith('uses')) then begin
        idx += 1;
        strTmp:= Strings[idx];
        strTmp := strTmp.Trim.TrimRight([';']);
        strTmp += ', unt' + AClassInfo.ktClassName + ';';
        Strings[idx] := '  ' + strTmp;
      end else if (Strings[idx].StartsWith('exports')) then begin
        oidx:= idx;
        idx -= 1;
        Insert(idx, Format('function Java_%s_JNITest_test%s(env: PJNIEnv; obj: jclass; o: jobject): jobject; stdcall;'#13#10'var'#13#10'  nativeObj: T%s;'#13#10'begin'#13#10'  nativeObj := T%s.fromJObject(env, o);'#13#10'  Exit(nativeObj.toJObject(env));'#13#10'end;'#13#10, [
          AClassInfo.packageName.Replace('.', '_', [rfIgnoreCase, rfReplaceAll]),
          AClassInfo.ktClassName, AClassInfo.ktClassName, AClassInfo.ktClassName]));
        idx := oidx + 2;
        strTmp:= Strings[idx];
        strTmp:= strTmp.Trim.TrimRight([';']);
        if (strTmp <> '') then strTmp += ', ';
        strTmp += Format('Java_%s_JNITest_test%s,', [
          AClassInfo.packageName.Replace('.', '_', [rfReplaceAll, rfIgnoreCase]),
          AClassInfo.ktClassName]);
        strTmp:= strTmp.Trim.TrimRight([',']);
        strTmp += ';';
        Strings[idx] := '  ' + strTmp;
        Break;
      end;
      Inc(idx);
    end;
    SaveToFile(path);
    Free;
  end;
end;

end.

