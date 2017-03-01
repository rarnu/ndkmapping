unit codeGenerater;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, codeParser, codeTypes;

type

  { TCodeGenerator }

  TCodeGenerator = class
  private
    // cpp
    class procedure generateCppHeader(AOutPath: string; AClassName: string; AFields: TParamMap);
    class procedure generateCpp(AOutPath: string; AClassName: string; APackageName: string; AConstructor: string; AConstructorParam: string; AFields: TParamMap; AFieldsFull: TParamMap);
    // pas
  public
    class procedure generateDir(ALanguage: string; AOutPath: string; AMainDir: string; AFileList: TStringList);
    class procedure generate(ALanguage: string; AOutPath: string; AMainFile: string; AFileList: TStringList);
    class procedure generateMakefile(ALanguage: string; AOutPath: string; AFileList: TStringList);
    class procedure genetateShellfile(ALanguage: string; AOutPath: string);
  end;

implementation

{ TCodeGenerator }

class procedure TCodeGenerator.generateCppHeader(AOutPath: string;
  AClassName: string; AFields: TParamMap);
var
  sl: TStringList;
  i: Integer;
begin
  sl := TStringList.Create;

  // include
  sl.Add('#include <jni.h>');
  sl.Add('#include <stdint.h>');
  sl.Add('#include <stddef.h>');
  sl.Add('#include <stdlib.h>');
  sl.Add('#include <string>');
  sl.Add('#include <list>');
  sl.Add('#include <map>');

  for i := 0 to AFields.Count - 1 do
    if (TTypeConvert.KTypeToCType(AFields.Data[i]).Contains('*')) then
      sl.Add(Format('#include "%s.h"', [AFields.Data[i]]));

  sl.Add('using namespace std;');

  // class
  sl.Add('class ' + AClassName + ' {');
  sl.Add('public:');
  for i := 0 to AFields.Count - 1 do begin
    sl.Add(Format('    %s %s;', [TTypeConvert.KTypeToCType(AFields.Data[i]), AFields.Keys[i]]));
  end;
  sl.Add('    static ' + AClassName + '* fromJObject(JNIEnv *env, jobject obj);');
  sl.Add('    jobject toJObject(JNIEnv *env);');
  sl.Add('};');
  sl.SaveToFile(AOutPath + AClassName + '.h');
  sl.Free;
end;

class procedure TCodeGenerator.generateCpp(AOutPath: string;
  AClassName: string; APackageName: string; AConstructor: string;
  AConstructorParam: string; AFields: TParamMap; AFieldsFull: TParamMap);
var
  sl: TStringList;
  i: Integer;
  callType: string;
begin
  sl := TStringList.Create;
  // include
  sl.Add('#include "' + AClassName + '.h"');
  // from jobject
  sl.Add(Format('%s* %s::fromJObject(JNIEnv *env, jobject obj) {', [AClassName, AClassName]));
  sl.Add(Format('    %s *ret = NULL;', [AClassName]));
  sl.Add('    if (env && obj) {');
  sl.Add(Format('        ret = new %s();', [AClassName]));
  sl.Add(Format('        jclass cls = env->FindClass("%s/%s");', [APackageName.Replace('.', '/', [rfIgnoreCase, rfReplaceAll]), AClassName]));
  for i := 0 to AFields.Count - 1 do begin
    if (i = 0) then
      sl.Add(Format('        jmethodID m = env->GetMethodID(cls, "%s", "%s");', [TTypeConvert.KFieldToGetName(AFields.Keys[i]), TTypeConvert.KTypeToGetSig(AFields.Data[i], AFieldsFull)]))
    else
      sl.Add(Format('        m = env->GetMethodID(cls, "%s", "%s");', [TTypeConvert.KFieldToGetName(AFields.Keys[i]), TTypeConvert.KTypeToGetSig(AFields.Data[i], AFieldsFull)]));
    callType:= TTypeConvert.KTypeToCallMethod(AFields.Data[i]);
    if (callType.Contains('Object')) then begin
      if (TTypeConvert.KTypeToCType(AFields.Data[i]) = 'string') then begin
        sl.Add(Format('        ret->%s = string(env->GetStringUTFChars((jstring)env->CallObjectMethod(obj, m), NULL));', [AFields.Keys[i]]));
      end else begin
        sl.Add(Format('        ret->%s = %s::fromJObject(env, env->CallObjectMethod(obj, m));', [AFields.Keys[i], AFields.Data[i]]));
      end;
    end else begin
      sl.Add(Format('        ret->%s = env->%s(obj, m);', [AFields.Keys[i], callType]));
    end;
  end;
  sl.Add('    }');
  sl.Add('    return ret;');
  sl.Add('}');
  // to jobject
  sl.Add(Format('jobject %s::toJObject(JNIEnv *env) {', [AClassName]));
  sl.Add('    jobject ret = NULL;');
  sl.Add('    if (env) {');
  sl.Add(Format('        jclass cls = env->FindClass("%s/%s");', [APackageName.Replace('.', '/', [rfIgnoreCase, rfReplaceAll]), AClassName]));
  sl.Add(Format('        jmethodID m = env->GetMethodID(cls, "<init>", "%s");', [AConstructor]));
  sl.Add(Format('        ret = env->NewObject(cls, m, %s);', [AConstructorParam]));
  sl.Add('    }');
  sl.Add('    return ret;');
  sl.Add('}');

  sl.SaveToFile(AOutPath + AClassName + '.cpp');
  sl.Free;
end;

class procedure TCodeGenerator.generateDir(ALanguage: string; AOutPath: string;
  AMainDir: string; AFileList: TStringList);
var
  src: TSearchRec;
  p: string;
begin
  if (not AMainDir.EndsWith('/')) then AMainDir += '/';
  if (FindFirst(AMainDir + '*.kt', faAnyFile, src) = 0) then begin
    repeat
      if (src.Name = '.') or (src.Name = '..') then Continue;
      p := AMainDir + src.Name;
      if (DirectoryExists(p)) then
        generateDir(ALanguage, AOutPath, p, AFileList)
      else
        generate(ALanguage, AOutPath, p, AFileList);
    until FindNext(src) <> 0;
    FindClose(src);
  end;
end;

class procedure TCodeGenerator.generate(ALanguage: string; AOutPath: string;
  AMainFile: string; AFileList: TStringList);
var
  codeText: string = '';
  clsName: string = '';
  clsPackage: string = '';
  clsFields: TParamMap = nil;
  clsFieldFull: TParamMap = nil;
  clsConstructor: string = '';
  clsConstructorParam: string = '';
begin
  with TStringList.Create do begin
    LoadFromFile(AMainFile);
    codeText:= Text;
    Free;
  end;
  clsName := TCodeParser.getClassName(codeText);
  clsPackage:= TCodeParser.getPackageName(codeText);
  clsFields := TCodeParser.getFields(codeText);
  clsFieldFull := TCodeParser.getFieldsFullPath(codeText, clsFields, clsPackage, ExtractFilePath(AMainFile));
  clsConstructor:= TCodeParser.getConstructoreSig(codeText, clsFieldFull);
  clsConstructorParam:= TCodeParser.getConstructoreParams(codeText, clsFieldFull);

  if (ALanguage = 'cpp') then begin
    generateCppHeader(AOutPath, clsName, clsFields);
    generateCpp(AOutPath, clsName, clsPackage, clsConstructor, clsConstructorParam, clsFields, clsFieldFull);
    AFileList.Add(clsName + '.cpp');
  end else if (ALanguage = 'pas') then begin
    // TODO:

    AFileList.Add(clsName + '.pas');
  end;

  if (clsFields <> nil) then clsFields.Free;
  if (clsFieldFull <> nil) then clsFieldFull.Free;
end;

class procedure TCodeGenerator.generateMakefile(ALanguage: string;
  AOutPath: string; AFileList: TStringList);
var
  sl: TStringList;
  i: Integer;
begin
  if (ALanguage = 'cpp') then begin

    // generate make file
    sl := TStringList.Create;
    sl.Add('LOCAL_PATH := $(call my-dir)');
    sl.Add('include $(CLEAR_VARS)');
    sl.Add('LOCAL_CPPFLAGS := -std=c++11 -fexceptions');
    sl.Add('LOCAL_MODULE    := ndkmapping_generated');
    sl.Add('LOCAL_SRC_FILES := \');
    for i := 0 to AFileList.Count - 1 do sl.Add(Format('        %s \', [AFileList[i]]));
    sl.Add('');
    sl.Add('LOCAL_C_INCLUDES := \');
    sl.Add('        $(LOCAL_PATH) \');
    sl.Add('        $(LOCAL_PATH)/classes \');
    sl.Add('LOCAL_CFLAGS += -D_PLATFORM_ANDROID');
    sl.Add('LOCAL_LDLIBS := -llog -landroid');
    sl.Add('include $(BUILD_SHARED_LIBRARY)');
    sl.SaveToFile(AOutPath + 'Android.mk');
    sl.Free;

    sl := TStringList.Create;
    sl.Add('APP_ABI := armeabi armeabi-v7a x86');
    sl.Add('APP_PLATFORM := android-16');
    sl.Add('APP_STL := gnustl_static');
    sl.SaveToFile(AOutPath + 'Application.mk');
    sl.Free;

  end;
end;

class procedure TCodeGenerator.genetateShellfile(ALanguage: string;
  AOutPath: string);
var
  sl: TStringList;
begin
  // generate shell file
  if (ALanguage = 'cpp') then begin
    sl := TStringList.Create;
    sl.Add('#!/bin/sh');
    sl.Add('CLEAN=$1');
    sl.Add('ndk-build ${CLEAN} NDK_PROJECT_PATH=. APP_BUILD_SCRIPT=Android.mk NDK_APPLICATION_MK=Application.mk');
    sl.Add('if [[ $CLEAN && $CLEAN == "clean" ]]; then');
    sl.Add('    rm -fr ./obj');
    sl.Add('    rm -fr ./libs');
    sl.Add('fi');
    sl.SaveToFile(AOutPath + 'build.sh');
    sl.Free;
  end;
end;

end.

