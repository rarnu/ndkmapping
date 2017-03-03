program ndkmapping;

{$mode objfpc}{$H+}

uses
  cthreads, Classes, sysutils, CustApp, codeGenerater, codeParser, codeTypes,
  codeCpp, codePas;

type

  { TNDKMappingApp }

  TNDKMappingApp = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TNDKMappingApp }

procedure TNDKMappingApp.DoRun;
var
  lng: string = '';  // language options
  bld: string = '';  // build options
  op: string = '';   // output path
  mx: Integer = 0;   // max array size
  alng: string = ''; // api language
  mainDir: string = '';  // main kotlin file path
  fileList: TStringList;
begin
  if (HasOption('l')) then lng := GetOptionValue('l');
  if (HasOption('b')) then bld := GetOptionValue('b');
  if (HasOption('o')) then op := GetOptionValue('o');
  if (HasOption('m')) then mx := StrToIntDef(GetOptionValue('m'), 0);
  if (HasOption('a')) then alng:= GetOptionValue('a');
  mainDir:= ParamStr(ParamCount);
  if (lng = '') or (op = '') or (mainDir = '') or (not DirectoryExists(mainDir)) then begin
    WriteHelp;
    Terminate;
    Exit;
  end;
  if (not mainDir.EndsWith('/')) then mainDir += '/';

  // check options
  if (lng <> 'cpp') and (lng <> 'pas') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if (bld <> '') and (bld <> 'mk') and (bld <> 'mksh') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if (alng <> '') and (alng <> 'kotlin') and (alng <> 'java') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if (not DirectoryExists(op)) then ForceDirectories(op);
  if (not DirectoryExists(op)) then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if (not op.EndsWith('/')) then op += '/';

  fileList := TStringList.Create;
  TCodeGenerator.generateDir(lng, mx, op, mainDir);

  if (bld.Contains('mk')) then TCodeGenerator.generateMakefile(lng, op);
  if (bld.Contains('sh')) then TCodeGenerator.genetateShellfile(lng, op);
  if (alng = 'kotlin') then begin
    // TODO: generate kotlin api sample
  end;

  if (alng = 'java') then begin
    // TODO: generate java api sample
  end;

  fileList.Free;
  Terminate;
end;

constructor TNDKMappingApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:= False;
end;

destructor TNDKMappingApp.Destroy;
begin
  inherited Destroy;
end;

procedure TNDKMappingApp.WriteHelp;
begin
  WriteLn('NDK Mapping v0.0');
  WriteLn('');
  WriteLn('usage:');
  WriteLn('');
  WriteLn('    ndkmapping <options> <Kotlin Class File Path>');
  WriteLn('');
  WriteLn('    options:');
  WriteLn('        -l language (cpp, pas)');
  WriteLn('        -b build option (mk, mksh)');
  WriteLn('        -m max array size (must >= 0)');
  WriteLn('        -a generate test api file with language (kotlin, java)');
  WriteLn('        -o output path');
  WriteLn('');
  WriteLn('sample:');
  WriteLn('');
  WriteLn('    ndkmapping -l cpp -b mksh -m 100 -a kotlin -o ./out/ ./classes/');
  WriteLn('');
end;

{$IFNDEF WINDOWS}
var
  Application: TNDKMappingApp;
{$ENDIF}
begin
  {$IFNDEF WINDOWS}
  Application:=TNDKMappingApp.Create(nil);
  Application.Run;
  Application.Free;
  {$ELSE}
  WriteLn('NDK Mapping does NOT support Windows now.');
  {$ENDIF}
end.


