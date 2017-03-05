unit testGenerater;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TTestGenerator }

  TTestGenerator = class
  public
    class procedure generateDir(ALanguage: string; AOutPath: string; AMainDir: string);
    class procedure generate(ALanguage: string; AOutPath: string; AMainFile: string);
    class procedure injectMakefile(ALanguage: string; AOutPath: string);
    class procedure injectShellCopy(ALanguage: string; AOutPath: string; ACpPath: string);
  end;

implementation

uses
  testJava, testKotlin;

{ TTestGenerator }

class procedure TTestGenerator.generateDir(ALanguage: string; AOutPath: string;
  AMainDir: string);
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
        generateDir(ALanguage, AOutPath, p)
      else
        generate(ALanguage, AOutPath, p);
    until FindNext(src) <> 0;
    FindClose(src);
  end;
end;

class procedure TTestGenerator.generate(ALanguage: string; AOutPath: string;
  AMainFile: string);
begin
  if (ALanguage = 'java') then begin
    TTestJava.generate(AOutPath, AMainFile);
  end else if (ALanguage = 'kotlin') then begin
    TTestKotlin.generate(AOutPath, AMainFile);
  end;
end;

class procedure TTestGenerator.injectMakefile(ALanguage: string;
  AOutPath: string);
begin
  // TODO: inject makefile
end;

class procedure TTestGenerator.injectShellCopy(ALanguage: string;
  AOutPath: string; ACpPath: string);
begin
  // TODO: inject shell script
end;

end.

