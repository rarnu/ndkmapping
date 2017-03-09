unit testGenerater;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, codeTypes;

type

  { TTestGenerator }

  TTestGenerator = class
  public
    class procedure generateDir(ALanguage: string; ALngX: string; APkg:string; AOutPath: string; AMainDir: string);
    class procedure generate(ALanguage: string; ALngX: string; APkg: string; AOutPath: string; AMainFile: string);
    class procedure injectMakefile(ALanguage: string; AOutPath: string);
    class procedure injectShellCopy(ALanguage: string; AOutPath: string; ACpPath: string);
  end;

implementation

uses
  testCode, codeParser;

{ TTestGenerator }

class procedure TTestGenerator.generateDir(ALanguage: string; ALngX: string;
  APkg: string; AOutPath: string; AMainDir: string);
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
        generateDir(ALanguage, ALngX, APkg, AOutPath, p)
      else
        generate(ALanguage, ALngX, APkg, AOutPath, p);
    until FindNext(src) <> 0;
    FindClose(src);
  end;
end;

class procedure TTestGenerator.generate(ALanguage: string; ALngX: string;
  APkg: string; AOutPath: string; AMainFile: string);

var
  codeText: string = '';
  clsInfo: TClassInfo;

begin
  with TStringList.Create do begin
    LoadFromFile(AMainFile);
    codeText:= Text;
    Free;
  end;
  clsInfo := TCodeParser.getClassInfo(codeText, ExtractFilePath(AMainFile));

  if (ALanguage = 'java') then begin
    TTestCode.generateJava(APkg, AOutPath, clsInfo);
  end else if (ALanguage = 'kotlin') then begin
    TTestCode.generateKotlin(APkg, AOutPath, clsInfo);
  end;

  if (ALngX = 'cpp') then begin
    TTestCode.generateCppJNI(APkg, AOutPath, clsInfo);
  end else if (ALngX = 'pas') then begin
    TTestCode.generatePasJNI(APkg, AOutPath, clsInfo);
  end;

  if (clsInfo <> nil) then clsInfo.Free;
end;

class procedure TTestGenerator.injectMakefile(ALanguage: string;
  AOutPath: string);
var
  path: string;
  src: TSearchRec;
  idx: Integer = 0;
begin
  if (ALanguage = 'cpp') then begin
    path := AOutPath + 'Android.mk';
    if (FileExists(path)) then begin
      with TStringList.Create do begin
        LoadFromFile(path);
        if (Text.IndexOf('JNITest.cpp') = -1) then begin
          while True do begin
            if (Strings[idx].StartsWith('LOCAL_SRC_FILES')) then begin
              idx += 1;
              Insert(idx, '        JNITest.cpp \');
              Break;
            end;
            Inc(idx);
          end;
        end;
        SaveToFile(path);
        Free;
      end;
    end;
  end else if (ALanguage = 'pas') then begin
    // inject pas makefile
    path := AOutPath + 'build_test.sh';
    with TStringList.Create do begin
      Add('#!/bin/sh');
      Add('ROOT_PATH=/usr/local/codetyphon');
      Add('TYPHON_PATH=${ROOT_PATH}/typhon');
      Add('TYPHON_BIN_LIB=${ROOT_PATH}/binLibraries');
      Add('ANDROID_API=android-5.0-api21');
      Add('FPC=/usr/local/codetyphon/fpc/fpc64/bin/x86_64-linux/fpc');
      Add('if [ ! -d "lib" ]; then');
      Add('    mkdir lib');
      Add('fi');
      Add('if [ ! -d "out" ]; then');
      Add('    mkdir out');
      Add('fi');
      Add('__compile() {');
      Add('    CPU=$1');
      Add('    LIB=$2');
      Add('    rm -fr lib/${LIB}-android/*');
      Add('    mkdir lib/${LIB}-android/');
      Add('    if [ ! -d "out/${LIB}" ]; then');
      Add('        mkdir out/${LIB}');
      Add('    fi');
      Add('    ${FPC} -B -Tandroid -P${CPU} -MObjFPC -Scghi -Cg -O1 -l -vewnhibq \');
      Add('        -Filib/${LIB}-android -FUlib/${LIB}-android \');
      Add('        -Fl${TYPHON_BIN_LIB}/${ANDROID_API}-${LIB} \');
      Add('        -Fu. \');
      Add('        -oout/${LIB}/libjnitest.so \');
      Add('        JNITest.pas');
      Add('}');
      Add('__compile "arm" "arm"');
      Add('__compile "i386" "i386"');
      Add('');
      SaveToFile(path);
      Free;
    end;
  end;
end;

class procedure TTestGenerator.injectShellCopy(ALanguage: string;
  AOutPath: string; ACpPath: string);
var
  path: string;
  cpCmd: string;
begin
  if (ALanguage = 'cpp') then begin
    path := AOutPath + 'build.sh';
    if (FileExists(path)) then begin
      with TStringList.Create do begin
        LoadFromFile(path);
        cpCmd:= Format('cp -fr libs/* %s', [ACpPath]);
        if (Text.IndexOf(cpCmd) = -1) then Add(cpCmd);
        SaveToFile(path);
        Free;
      end;
    end;
  end else if (ALanguage = 'pas') then begin
    // inject shell script
    path := AOutPath + 'build_test.sh';
    if (FileExists(path)) then begin
      with TStringList.Create do begin
        LoadFromFile(path);
        Add('mv out/arm out/armeabi');
        Add('mv out/i386 out/x86');
        Add('cp -fr out/armeabi out/armeabi-v7a');
        cpCmd:= Format('cp -fr out/* %s', [ACpPath]);
        Add(cpCmd);
        SaveToFile(path);
        Free;
      end;
    end;
  end;
end;

end.

