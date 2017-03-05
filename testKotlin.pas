unit testKotlin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TTestKotlin }

  TTestKotlin = class
  public
    class procedure generate(AOutPath: string; AKtFile: string);
  end;

implementation

{ TTestKotlin }

class procedure TTestKotlin.generate(AOutPath: string; AKtFile: string);
begin
  // TODO: generate
end;

end.

