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
    class procedure headJava(APkg: string; codeList: TStringList);
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

class procedure TTestCode.headJava(APkg: string; codeList: TStringList);
begin
  with codeList do begin
    Add(Format('package %s', [APkg]));
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
begin
  // TODO: generate kotlin
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
begin
  // TODO: generate pas jni
end;

end.

