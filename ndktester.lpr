program ndktester;

{$mode objfpc}{$H+}

uses
  cthreads, Classes, SysUtils, CustApp, testGenerater;

type

  { TNDKTester }

  TNDKTester = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TNDKTester }

procedure TNDKTester.DoRun;
var
  lng: string = ''; // language
  lngx: string = ''; // language x
  bld: string = ''; // build options
  pkg: string = ''; // pacakge name
  cpPath: string = ''; // copy path
  outPath: string = ''; // output path
  mainDir: string = '';  // kotlin file dir
begin
  if (HasOption('l')) then lng := GetOptionValue('l');
  if (HasOption('x')) then lngx:= GetOptionValue('x');
  if (HasOption('b')) then bld := GetOptionValue('b');
  if (HasOption('p')) then pkg:= GetOptionValue('p');
  if (HasOption('c')) then cpPath:= GetOptionValue('c');
  if (HasOption('o')) then outPath:= GetOptionValue('o');
  mainDir:= ParamStr(ParamCount);

  if (lng = '') or (lngx = '') or (pkg = '') or (outPath = '') or (mainDir = '') or (not DirectoryExists(mainDir)) then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if (not mainDir.EndsWith('/')) then mainDir += '/';

  if (lng <> 'java') and (lng <> 'kotlin')  then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if (lngx <> 'cpp') and (lngx <> 'pas') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if (bld <> '') and (bld <> 'mk') and (bld <> 'mkshcp') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if (not DirectoryExists(outPath)) then ForceDirectories(outPath);
  if (not DirectoryExists(outPath)) then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if (bld = 'mkshcp') then begin
    if (cpPath = '') then begin
      WriteHelp;
      Terminate;
      Exit;
    end;
    if (not DirectoryExists(cpPath)) then ForceDirectories(cpPath);
    if (not DirectoryExists(cpPath)) then begin
      WriteHelp;
      Terminate;
      Exit;
    end;
  end;

  // generate test api
  if (not outPath.EndsWith('/')) then outPath += '/';
  if (not cpPath.EndsWith('/')) then cpPath += '/';

  if (lng = 'java') then DeleteFile(outPath + 'JNITest.java');
  if (lng = 'kotlin') then DeleteFile(outPath + 'JNITest.kt');
  if (lngx = 'cpp') then begin
    DeleteFile(outPath + 'JNITest.h');
    DeleteFile(outPath + 'JNITest.cpp');
  end;
  if (lngx = 'pas') then DeleteFile(outPath + 'JNITest.pas');
  TTestGenerator.generateDir(lng, lngx, pkg, outPath, mainDir);
  if (bld.Contains('mk')) then TTestGenerator.injectMakefile(lngx, outPath);
  if (bld = 'mkshcp') then TTestGenerator.injectShellCopy(lngx, outPath, cpPath);

  Terminate;
end;

constructor TNDKTester.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:= False;
end;

destructor TNDKTester.Destroy;
begin
  inherited Destroy;
end;

procedure TNDKTester.WriteHelp;
begin
  WriteLn('NDK Tester v0.0');
  WriteLn('');
  WriteLn('usage:');
  WriteLn('');
  WriteLn('  ndktester <options> <Kotlin Class File Path>');
  WriteLn('');
  WriteLn('  options:');
  WriteLn('    -l language (java, kotlin)');
  WriteLn('    -x exported JNI code language (cpp, pas)');
  WriteLn('    -b build option (mk, mkshcp)');
  WriteLn('    -p base package name');
  WriteLn('    -c copy path');
  WriteLn('    -o output path');
  WriteLn('sample:');
  WriteLn('');
  WriteLn('    ndktester -l kotlin -x cpp -b mkshcp -p com.sample.ndk -c ./jniLibs/ -o ./out/ ./kotlin/');
  WriteLn('');
end;

{$IFNDEF WINDOWS}
var
  Application: TNDKTester;
{$ENDIF}
begin
  {$IFNDEF WINDOWS}
  Application:=TNDKTester.Create(nil);
  Application.Run;
  Application.Free;
  {$ELSE}
  WriteLn('NDK Tester does NOT support Windows now.');
  {$ENDIF}
end.

