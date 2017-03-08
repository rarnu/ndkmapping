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

  end else if (ALanguage = 'pas') then begin
    // TODO: pas make
    sl := TStringList.Create;
    sl.Add('#!/bin/sh');
    sl.Add('ROOT_PATH=/usr/local/codetyphon');
    sl.Add('TYPHON_PATH=${ROOT_PATH}/typhon');
    sl.Add('TYPHON_BIN_LIB=${ROOT_PATH}/binLibraries');
    sl.Add('FPC=/usr/local/codetyphon/fpc/fpc64/bin/x86_64-linux/fpc');
    sl.Add('');
    sl.Add('if [ ! -d "lib" ]; then');
    sl.Add('    mkdir lib');
    sl.Add('fi');
    sl.Add('if [ ! -d "out" ]; then');
    sl.Add('    mkdir out');
    sl.Add('fi');
    sl.Add('');
    sl.Add('__compile() {');

    sl.Add('}');

    (*

        CPU=$1
        LIB=$2
        PROJ=$3
        rm -fr lib/${LIB}-android/*
        mkdir lib/${LIB}-android/
        if [ ! -d "out/${LIB}" ]; then
                mkdir out/${LIB}
        fi
        ${FPC} -B -Tandroid -P${CPU} \
        -MObjFPC -Scghi -Cg -O1 -l -vewnhibq \
        -Filib/${LIB}-android \
        -Fl${TYPHON_BIN_LIB}/android-5.0-api21-${LIB} \
        -Fu. -FUlib/${LIB}-android \
        -oout/${LIB}/librarnu${PROJ}.so \
        ${PROJ}.lpr


__compile "arm" "arm" "cmd"
__compile "i386" "i386" "cmd"
__compile "mipsel" "mips" "cmd"
    *)

    sl.SaveToFile(AOutPath + 'build.sh');
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
  end else if (ALanguage = 'pas') then begin
    // TODO: pas shell
  end;
end;

end.


