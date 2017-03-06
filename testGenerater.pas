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
    // TODO: inject pas makefile
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
    // TODO: inject shell script
  end;
end;

end.

