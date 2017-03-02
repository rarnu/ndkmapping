unit codePas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TCodePas }

  TCodePas = class
  public
    class procedure generate(AOutPath: string; AKtFile: string; AMaxArraySize: Integer);
  end;

implementation

{ TCodePas }

class procedure TCodePas.generate(AOutPath: string; AKtFile: string;
  AMaxArraySize: Integer);
begin
  // TODO: parse pascal
end;

end.

