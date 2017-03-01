unit codeJNIExt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TJNIExt }

  TJNIExt = class
  public
    class procedure AddJArrayToObjectArray(AOutPath: string; AClassName: string; AField: string; AFieldType: string; AMaxArraySize: Integer);
  end;

implementation

uses
  codeTypes;

{ TJNIExt }

class procedure TJNIExt.AddJArrayToObjectArray(AOutPath: string;
  AClassName: string; AField: string; AFieldType: string; AMaxArraySize: Integer
  );
var
  sl: TStringList;
  slCpp: TStringList;
  p: string;
  pCpp: string;
  mname: string;
  lastLineRemoved: Boolean = False;
  idx: Integer;
begin
  WriteLn(Format('f = %s, ft = %s', [AField, AFieldType]));
  sl := TStringList.Create;
  p := AOutPath + 'JNIExt.h';
  if (FileExists(p)) then sl.LoadFromFile(p)
  else begin
    sl.Add('#include <jni.h>');
    sl.Add('#include <stdint.h>');
    sl.Add('#include <stddef.h>');
    sl.Add('#include <stdlib.h>');
    sl.Add('#include <string>');
    sl.Add('#include <list>');
    sl.Add('#include <map>');
    sl.Add('#include <set>');
    sl.Add('#include "JNIUtils.h"');
    sl.Add('using namespace std;');
    sl.Add('class JNIExt {');
    sl.Add('public:');
    sl.Add('};');
  end;
  mname:= Format('void %s_jarrayToObjectArray(JNIEnv *env, jobjectArray arr, string type, %s (&dest)[%d]);', [AField, TTypeConvert.KTypeToCType(AFieldType), AMaxArraySize]);
  if (sl.Text.IndexOf(mname) = -1) then begin
    idx := sl.Count - 1;
    while not lastLineRemoved do begin
      if (sl[idx].Trim = '};') then begin
        sl.Delete(idx);
        lastLineRemoved:= True;
      end;
      Dec(idx);
    end;
    sl.Add('    ' + mname);
    sl.Add('};');
  end;

  sl.SaveToFile(p);
  sl.Free;

  pCpp:= AOutPath + 'JNIExt.cpp';
  slCpp := TStringList.Create;
  if (FileExists(pCpp)) then slCpp.LoadFromFile(pCpp)
  else begin
    slCpp.Add('#include "JNIExt.h"');
  end;

  mname:= Format('void JNIExt::%s_jarrayToObjectArray(JNIEnv *env, jobjectArray arr, string type, %s (&dest)[%d]) {', [AField, TTypeConvert.KTypeToCType(AFieldType), AMaxArraySize]);
  if (sl.Text.IndexOf(mname) = -1) then begin
    sl.Add(mname);
    // TODO: jarray to object array
    sl.Add('}');
  end;
  slCpp.SaveToFile(pCpp);
  slCpp.Free;
end;

end.

