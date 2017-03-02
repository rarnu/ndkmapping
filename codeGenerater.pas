unit codeGenerater;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TCodeGenerator }

  TCodeGenerator = class
  public
    class procedure generateDir(ALanguage: string; AMaxArraySize: Integer; AOutPath: string; AMainDir: string);
    class procedure generate(ALanguage: string; AMaxArraySize: Integer; AOutPath: string; AMainFile: string);
    class procedure generateMakefile(ALanguage: string; AOutPath: string);
    class procedure genetateShellfile(ALanguage: string; AOutPath: string);
  end;

implementation

uses
  codeCpp, codePas;

{ TCodeGenerator }

(*

class procedure TCodeGenerator.generateCpp(AOutPath: string;
  AClassName: string; APackageName: string; AConstructor: string;
  AConstructorParam: string; AFields: TParamMap; AFieldsFull: TParamMap;
  AMaxArraySize: Integer);
var
  sl: TStringList;
  i: Integer;
  callType: string;
  sig: string;
  mp1, mp2: string;
begin

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
      if (TTypeConvert.KTypeIsArray(AFields.Data[i])) then begin
        sig := TTypeConvert.KTypeToCType(AFields.Data[i]);
        sl.Add(Format('        JNIExt::%s_jarrayToObjectArray(env, (jobjectArray)env->CallObjectMethod(obj, m), %s);', [AFields.Keys[i], AFields.Keys[i]]));
        TJNIExt.AddJArrayToObjectArray(AOutPath, AClassName, AFields.Keys[i], AFields.Data[i], AMaxArraySize);
      end else if (TTypeConvert.KTypeIsList(AFields.Data[i])) then begin
        sig := TTypeConvert.KTypeToCType(AFields.Data[i]);
        sl.Add(Format('        JNIExt::%s_jArrayListToList(env, (jobjectArray)env->CallObjectMethod(obj, m), %s);', [AFields.Keys[i], AFields.Keys[i]]));
        TJNIExt.AddJArrayListToList(AOutPath, AClassName, AFields.Keys[i], AFields.Data[i]);
      end else if (TTypeConvert.KTypeIsMap(AFields.Data[i])) then begin
        // map
        TTypeConvert.KTYpeExtractMapTypes(AFields.Data[i], mp1, mp2);
        sl.Add(Format('        JNIExt::%s_jHashMapToMap(env, (jobjectArray)env->CallObjectMethod(obj, m) %s);', [AFields.Keys[i], AFields.Keys[i]]));
        TJNIExt.AddJHashMapToMap(AOutPath, AClassName, AFields.Keys[i], AFields.Data[i]);
      end else if (TTypeConvert.KTypeIsSet(AFields.Data[i])) then begin
        // set
        sig := TTypeConvert.KTypeToCType(AFields.Data[i]);
        sl.Add(Format('        JNIExt::%s_jHashSetToSet(env, (jobjectArray)env->CallObjectMethod(obj, m), %s);', [AFields.Keys[i], AFields.Keys[i]]));
        TJNIExt.AddJHashsetToSet(AOutPath, AClassName, AFields.Keys[i], AFields.Data[i]);
      end else begin
        if (TTypeConvert.KTypeToCType(AFields.Data[i]) = 'string') then begin
          sl.Add(Format('        ret->%s = string(env->GetStringUTFChars((jstring)env->CallObjectMethod(obj, m), NULL));', [AFields.Keys[i]]));
        end else begin
          sl.Add(Format('        ret->%s = %s::fromJObject(env, env->CallObjectMethod(obj, m));', [AFields.Keys[i], AFields.Data[i]]));
        end;
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
*)

class procedure TCodeGenerator.generateDir(ALanguage: string;
  AMaxArraySize: Integer; AOutPath: string; AMainDir: string);
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
        generateDir(ALanguage, AMaxArraySize, AOutPath, p)
      else
        generate(ALanguage, AMaxArraySize, AOutPath, p);
    until FindNext(src) <> 0;
    FindClose(src);
  end;
end;

class procedure TCodeGenerator.generate(ALanguage: string;
  AMaxArraySize: Integer; AOutPath: string; AMainFile: string);
begin
  if (ALanguage = 'cpp') then begin
    TCodeCpp.generate(AOutPath, AMainFile, AMaxArraySize);
  end else if (ALanguage = 'pas') then begin
    TCodePas.generate(AOutPath, AMainFile, AMaxArraySize);
  end;
end;

class procedure TCodeGenerator.generateMakefile(ALanguage: string;
  AOutPath: string);
var
  sl: TStringList;
  src: TSearchRec;
begin
  if (ALanguage = 'cpp') then begin

    // generate make file
    sl := TStringList.Create;
    sl.Add('LOCAL_PATH := $(call my-dir)');
    sl.Add('include $(CLEAR_VARS)');
    sl.Add('');
    sl.Add('LOCAL_CPPFLAGS := -std=c++11 -fexceptions');
    sl.Add('LOCAL_MODULE    := ndkmapping_generated');
    sl.Add('');
    sl.Add('LOCAL_SRC_FILES := \');

    if (FindFirst(AOutPath + '*.cpp', faAnyFile, src) = 0) then begin
      repeat
        if (src.Name = '.') or (src.Name = '..') then Continue;
        sl.Add(Format('        %s \', [src.Name]));
      until FindNext(src) <> 0;
      FindClose(src);
    end;
    sl.Add('');
    sl.Add('LOCAL_C_INCLUDES := \');
    sl.Add('        $(LOCAL_PATH) \');
    sl.Add('');
    sl.Add('LOCAL_CFLAGS += -D_PLATFORM_ANDROID');
    sl.Add('LOCAL_LDLIBS := -llog -landroid');
    sl.Add('');
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


