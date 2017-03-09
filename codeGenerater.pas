unit codeGenerater;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, FileUtil;

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
  tmp: string = '';
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
  end else if (ALanguage = 'pas') then begin
    CopyFile(ExtractFilePath(ParamStr(0)) + 'pas/JNI2.pas', AOutPath + 'JNI2.pas');
    if (FindFirst(AOutPath + '*.pas', faAnyFile, src) = 0) then begin
      repeat
        if (src.Name = '.') or (src.Name = '..') then Continue;
        tmp += src.Name + ' ';
      until FindNext(src) <> 0;
      FindClose(src);
    end;
    tmp := tmp.Trim;
    sl := TStringList.Create;
    sl.Add(Format('fpc64 %s', [tmp]));
    sl.SaveToFile(AOutPath + 'compile.sh');
    sl.Free;
  end;
end;

class procedure TCodeGenerator.genetateShellfile(ALanguage: string;
  AOutPath: string);
var
  sl: TStringList;
  src: TSearchRec;
  tmp: string = '';
  srcName: string;
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
  end else if (ALanguage = 'pas') then begin
    sl := TStringList.Create;
    sl.Add('library ndkmapping_generated;');
    sl.Add('');
    sl.Add('{$mode objfpc}{$H+}');
    sl.Add('');
    if (FindFirst(AOutPath + '*.pas', faAnyFile, src) = 0) then begin
      repeat
        if (src.Name = '.') or (src.Name = '..') then Continue;
        srcName:= src.Name;
        srcName:= srcName.Substring(0, srcName.LastIndexOf('.'));
        tmp += ', ' + srcName;
      until FindNext(src) <> 0;
      FindClose(src);
    end;
    tmp := tmp.Trim;
    sl.Add(Format('uses cthreads, Classes, Sysutils%s;', [tmp]));
    sl.Add('');
    sl.Add('begin');
    sl.Add('');
    sl.Add('end.');
    sl.SaveToFile(AOutPath + 'ndkmapping_generated.lpr');
    sl.Free;
    // pas shell
    sl := TStringList.Create;
    sl.Add('#!/bin/sh');
    sl.Add('ROOT_PATH=/usr/local/codetyphon');
    sl.Add('TYPHON_PATH=${ROOT_PATH}/typhon');
    sl.Add('TYPHON_BIN_LIB=${ROOT_PATH}/binLibraries');
    sl.Add('ANDROID_API=android-5.0-api21');
    sl.Add('FPC=/usr/local/codetyphon/fpc/fpc64/bin/x86_64-linux/fpc');
    sl.Add('if [ ! -d "lib" ]; then');
    sl.Add('    mkdir lib');
    sl.Add('fi');
    sl.Add('if [ ! -d "out" ]; then');
    sl.Add('    mkdir out');
    sl.Add('fi');
    sl.Add('__compile() {');
    sl.Add('    CPU=$1');
    sl.Add('    LIB=$2');
    sl.Add('    rm -fr lib/${LIB}-android/*');
    sl.Add('    mkdir lib/${LIB}-android/');
    sl.Add('    if [ ! -d "out/${LIB}" ]; then');
    sl.Add('        mkdir out/${LIB}');
    sl.Add('    fi');
    sl.Add('    ${FPC} -B -Tandroid -P${CPU} -MObjFPC -Scghi -Cg -O1 -l -vewnhibq \');
    sl.Add('        -Filib/${LIB}-android -FUlib/${LIB}-android \');
    sl.Add('        -Fl${TYPHON_BIN_LIB}/${ANDROID_API}-${LIB} \');
    sl.Add('        -Fu. \');
    sl.Add('        -oout/${LIB}/libndkmapping_generated.so \');
    sl.Add('        ndkmapping_generated.lpr');
    sl.Add('}');
    sl.Add('__compile "arm" "arm"');
    sl.Add('__compile "i386" "i386"');
    sl.Add('');
    sl.SaveToFile(AOutPath + 'build.sh');
    sl.Free;
  end;
end;

end.


