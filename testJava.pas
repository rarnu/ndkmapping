unit testJava;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, codeTypes;

type

  { TTestJava }

  TTestJava = class
  public
    class procedure generate(AOutPath: string; AKtFile: string);
  end;

implementation

uses
  codeParser;

{ TTestJava }

class procedure TTestJava.generate(AOutPath: string; AKtFile: string);
var
  codeText: string = '';
  clsInfo: TClassInfo;
begin
  with TStringList.Create do begin
    LoadFromFile(AKtFile);
    codeText:= Text;
    Free;
  end;
  clsInfo := TCodeParser.getClassInfo(codeText, ExtractFilePath(AKtFile));

  // TODO: generate code

  if (clsInfo <> nil) then clsInfo.Free;
end;

end.

