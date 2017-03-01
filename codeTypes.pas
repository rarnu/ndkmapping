unit codeTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  TParamMap = specialize TFPGMap<String, String>;

type

  { TTypeConvert }

  TTypeConvert = class
  public
    class function KTypeToCType(AType: string): string;
    class function KTypeToPType(AType: string): string;
    class function KTypeToCallMethod(AType: string): string;
    class function KTypeToGetSig(AType: string; AFullFields: TParamMap): string;
    class function KTypeToSetSig(AType: string; AFullFields: TParamMap): string;
    class function KTypeToSig(AType: string; AFullFields: TParamMap): string;
    class function KFieldToGetName(AField: string): string;
    class function KFieldToSetName(AField: string): string;
  end;

implementation

{ TTypeConvert }

class function TTypeConvert.KTypeToCType(AType: string): string;
var
  r: string = '';
begin
  if (AType = 'Int') then r := 'int';
  if (AType = 'Double') then r := 'double';
  if (AType = 'Boolean') then r := 'bool';
  if (AType = 'Byte') then r := 'unsigned char';
  if (AType = 'Char') then r := 'char';
  if (AType = 'Short') then r := 'short';
  if (AType = 'Long') then r := 'long long';
  if (AType = 'Float') then r := 'float';
  if (AType = 'String') then r := 'string';
  if (r = '') then r := AType + '*';
  Exit(r);
end;

class function TTypeConvert.KTypeToPType(AType: string): string;
var
  r: string = '';
begin
  if (AType = 'Int') then r := 'Integer';
  if (AType = 'Double') then r := 'Double';
  if (AType = 'Boolean') then r := 'Boolean';
  if (AType = 'Byte') then r := 'Byte';
  if (AType = 'Char') then r := 'Char';
  if (AType = 'Short') then r := 'ShortInt';
  if (AType = 'Long') then r := 'Int64';
  if (AType = 'Float') then r := 'Extended';
  if (AType = 'String') then r := 'String';
  if (r = '') then r := AType;
  Exit(r);
end;

class function TTypeConvert.KTypeToCallMethod(AType: string): string;
var
  r: string = '';
begin
  if (AType = 'Int') then r := 'CallIntMethod';
  if (AType = 'Double') then r := 'CallDoubleMethod';
  if (AType = 'Boolean') then r := 'CallBooleanMethod';
  if (AType = 'Byte') then r := 'CallByteMethod';
  if (AType = 'Char') then r := 'CallCharMethod';
  if (AType = 'Short') then r := 'CallShortMethod';
  if (AType = 'Long') then r := 'CallLongMethod';
  if (AType = 'Float') then r := 'CallFloatMethod';
  if (r = '') then r := 'CallObjectMethod';
  Exit(r);
end;

class function TTypeConvert.KTypeToGetSig(AType: string; AFullFields: TParamMap
  ): string;
var
  r: string = '';
begin
  if (AType = 'Int') then r := '()I';
  if (AType = 'Double') then r := '()D';
  if (AType = 'Boolean') then r := '()Z';
  if (AType = 'Byte') then r := '()B';
  if (AType = 'Char') then r := '()C';
  if (AType = 'Short') then r := '()S';
  if (AType = 'Long') then r := '()J';
  if (AType = 'Float') then r := '()F';
  if (r = '' ) then
    if (AFullFields.IndexOf(AType) <> -1) then
      r := '()L' + AFullFields.KeyData[AType].replace('.', '/', [rfIgnoreCase, rfReplaceAll]) + ';'
    else
      r := '()Ljava/lang/' + AType + ';';
  Exit(r);
end;

class function TTypeConvert.KTypeToSetSig(AType: string; AFullFields: TParamMap
  ): string;
var
  r: string;
begin
  if (AType = 'Int') then r := '(I)V';
  if (AType = 'Double') then r := '(D)V';
  if (AType = 'Boolean') then r := '(Z)V';
  if (AType = 'Byte') then r := '(B)V';
  if (AType = 'Char') then r := '(C)V';
  if (AType = 'Short') then r := '(S)V';
  if (AType = 'Long') then r := '(J)V';
  if (AType = 'Float') then r := '(F)V';
  if (r = '') then
    if (AFullFields.IndexOf(AType) <> -1) then
      r := '(L' + AFullFields.KeyData[AType].replace('.', '/', [rfIgnoreCase, rfReplaceAll]) + ';)V'
    else
      r := '(Ljava/lang/' + AType + ';)V';
  Exit(r);
end;

class function TTypeConvert.KTypeToSig(AType: string; AFullFields: TParamMap
  ): string;
var
  r: string;
begin
  if (AType = 'Int') then r := 'I';
  if (AType = 'Double') then r := 'D';
  if (AType = 'Boolean') then r := 'Z';
  if (AType = 'Byte') then r := 'B';
  if (AType = 'Char') then r := 'C';
  if (AType = 'Short') then r := 'S';
  if (AType = 'Long') then r := 'J';
  if (AType = 'Float') then r := 'F';
  if (r = '') then
    if (AFullFields.IndexOf(AType) <> -1) then
      r := 'L' + AFullFields.KeyData[AType].replace('.', '/', [rfIgnoreCase, rfReplaceAll]) + ';'
    else
      r := 'Ljava/lang/' + AType + ';';
  Exit(r);
end;

class function TTypeConvert.KFieldToGetName(AField: string): string;
begin
  Result := 'get' + UpperCase(AField[1]) + AField.Substring(1);
end;

class function TTypeConvert.KFieldToSetName(AField: string): string;
begin
  Result := 'set' + UpperCase(AField[1]) + AField.Substring(1);
end;

end.
