unit codePas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, codeTypes;

type

  { TCodePas }

  TCodePas = class
  private
    class procedure headUses(codeList: TStringList; AClassInfo: TClassInfo);
    class procedure addMethod(codeList: TStringList; ktClassName: string; methodList: TFieldMap);
    class procedure addMethodImpl(codeList: TStringList; ktClassName: string; methodList: TFieldMap; AMaxArraySize: Integer);
    class function constructorParams(AFieldList: TFieldList; methodList: TFieldMap): string;
    class procedure code(AOutPath: string; AClassInfo: TClassInfo; AMaxArraySize: Integer);
  public
    class procedure generate(AOutPath: string; AKtFile: string; AMaxArraySize: Integer);
  end;

implementation

uses
  codeParser;

{ TCodePas }

class procedure TCodePas.headUses(codeList: TStringList; AClassInfo: TClassInfo
  );
var
  tInc: string;
  usesList: string = '';
  i: Integer;
begin
  with codeList do begin
    Add(Format('unit unt%s;', [AClassInfo.ktClassName]));
    Add('');
    Add('{$mode objfpc}{$H+}');
    Add('');
    Add('interface');
    Add('');
    Add('uses Classes, SysUtils, FGL, JNI2{| uses |};');
    Add('');
    Add('type');
    Add('{| generic |}');
  end;

  for i := 0 to AClassInfo.fieldList.Count - 1 do begin
    if (AClassInfo.fieldList[i].baseType.fieldCategory = ftcObject) then begin
      if (not AClassInfo.fieldList[i].isList) and (not AClassInfo.fieldList[i].isMap) and (not AClassInfo.fieldList[i].isSet) then begin
        tInc:= Format('unt%s', [AClassInfo.fieldList[i].baseType.fieldType]);
        if (usesList.IndexOf(tInc) = -1) then usesList += tInc + ', ';
      end else begin
        if (AClassInfo.fieldList[i].genericType1 <> nil) then begin
          if (AClassInfo.fieldList[i].genericType1.fieldCategory = ftcObject) then begin
            tInc:= Format('unt%s', [AClassInfo.fieldList[i].genericType1.fieldType]);
            if (usesList.IndexOf(tInc) = -1) then usesList += tInc + ', ';
          end;
        end;
        if (AClassInfo.fieldList[i].genericType2 <> nil) then begin
          if (AClassInfo.fieldList[i].genericType2.fieldCategory = ftcObject) then begin
            tInc:= Format('unt%s', [AClassInfo.fieldList[i].genericType2.fieldType]);
            if (usesList.IndexOf(tInc) = -1) then usesList += tInc + ', ';
          end;
        end;
      end;
    end;
  end;
  usesList:= usesList.Trim.TrimRight([',']);
  if (usesList <> '') then usesList:= ', ' + usesList;
  codeList.Text:= codeList.Text.Replace('{| uses |}', usesList, [rfIgnoreCase, rfReplaceAll]);
end;

class procedure TCodePas.addMethod(codeList: TStringList; ktClassName: string;
  methodList: TFieldMap);
var
  strTmp: string = '';
  i: Integer;
begin
  for i := 0 to methodList.Count - 1 do begin
    strTmp += Format('    class %s'#13#10, [methodList.Keys[i]]);
  end;
  codeList.Text:= codeList.Text.Replace('{| method |}', strTmp, [rfReplaceAll, rfIgnoreCase]);
end;

class procedure TCodePas.addMethodImpl(codeList: TStringList;
  ktClassName: string; methodList: TFieldMap; AMaxArraySize: Integer);
var
  i: Integer;
  strTmp: string;
begin
  with codeList do begin
    for i := 0 to methodList.Count - 1 do begin
      with methodList.Data[i] do begin
        strTmp:= methodList.Keys[i];
        strTmp:= Format('class %s T%s.%s', [strTmp.Substring(0, strTmp.IndexOf(' ')), ktClassName, strTmp.Substring(strTmp.IndexOf(' ') + 1)]);
        Add(strTmp);
        if (strTmp.Contains('jarrayToObjectArray')) then begin
          Add('var');
          Add('  jarr: jobjectArray;');
          Add('  count: Integer;');
          Add('  i: Integer;');
          Add('  tmpStr: jstring;');
          Add('  tmpObj: jobject;');
          Add('begin');
          Add('  jarr := jobjectArray(arr);');
          Add('  count := env^^.GetArrayLength(env, jarr);');
          Add('  for i := 0 to count - 1 do begin');
          if (TTypeConvert.KTypeToPType(baseType.fieldType) = 'String') then begin
            Add('    tmpStr := env^^.GetObjectArrayElement(env, jarr, i);');
            Add('    dest[i] := TJNIEnv.JStringToString(env, tmpStr);');
          end else begin
            Add('    tmpObj := env^^.GetObjectArrayElement(env, jarr, i);');
            Add(Format('    dest[i] := T%s.fromJObject(env, tmpObj);', [baseType.fieldType]));
          end;
          Add('  end;');
        end;
        if (strTmp.Contains('jarrayToArray')) then begin
          Add('var');
          Add('  jTCls: jclass;');
          Add('  mValue: jmethodID;');
          Add('  jarr: jobjectArray;');
          Add('  count: Integer;');
          Add('  i: Integer;');
          Add('  tmpObj: jobject;');
          Add('begin');
          Add(Format('  jTCls := env^^.FindClass(env, ''%s'');', [baseType.fieldFullPackageName.Replace('.', '/', [rfIgnoreCase, rfReplaceAll])]));
          Add(Format('  mValue := env^^.GetMethodID(env, jTCls, ''%s'', ''%s'');', [
            TTypeConvert.KTypeToJObjectGetName(baseType.fieldType), TTypeConvert.KTypeToJObjectGetSig(baseType.fieldType)]));
          Add('  jarr := jobjectArray(arr);');
          Add('  count := env^^.GetArrayLength(env, jarr);');
          Add('  for i := 0 to count - 1 do begin');
          Add('    tmpObj := env^^.GetObjectArrayElement(env, jarr, i);');
          Add(Format('    dest[i] := %s(env^^.%s(env, tmpObj, mValue));', [
            TTypeConvert.KTypeToPType(baseType.fieldType), TTypeConvert.KTypeToCallMethod(baseType.fieldType)]));
          Add('  end;');
        end;
        if (strTmp.Contains('jArrayListToList')) then begin
          Add('var');
          Add('  clsList: jclass;');
          Add('  mGet: jmethodID;');
          Add('  mSize: jmethodID;');
          Add('  jTCls: jclass;');
          Add('  mValue: jmethodID;');
          Add('  count: Integer;');
          Add('  i: Integer;');
          Add('  jo: jobject;');
          if (TTypeConvert.KTypeToCType(genericType1.fieldType).Contains('*')) then
            Add(Format('  item: %s;', [TTypeConvert.KTypeToPType(genericType1.fieldType)]))
          else  if (TTypeConvert.KTypeToPType(genericType1.fieldType) = 'String') then
            Add('  item: String;')
          else
            Add(format('  item: %s;', [TTypeConvert.KTypeToPType(genericType1.fieldType)]));
          Add('begin');
          Add('  lst.Clear;');
          Add('  clsList := env^^.FindClass(env, ''java/util/List'');');
          Add('  mGet := env^^.GetMethodID(env, clsList, ''get'', ''(I)Ljava/lang/Object;'');');
          Add('  mSize := env^^.GetMethodID(env, clsList, ''size'', ''()I'');');
          if (TTypeConvert.KTypeIsBasicType(genericType1.fieldType)) then begin
            Add(Format('  jTCls := env^^.FindClass(env, ''%s'');', [
              genericType1.fieldFullPackageName.Replace('.', '/', [rfReplaceAll, rfIgnoreCase])]));
            Add(Format('  mValue := env^^.GetMethodID(env, jTCls, ''%s'', ''%s'');', [
              TTypeConvert.KTypeToJObjectGetName(genericType1.fieldType), TTypeConvert.KTypeToJObjectGetSig(genericType1.fieldType)]));
          end;
          Add('  count := env^^.CallIntMethod(env, obj, mSize);');
          Add('  for i := 0 to count - 1 do begin');
          Add('    jo := env^^.CallObjectMethodA(env, obj, mGet, TJNIEnv.ArgsToJValues(env, [i]));');
          if (TTypeConvert.KTypeToCType(genericType1.fieldType).Contains('*')) then
            Add(Format('    item := %s.fromJObject(env, jo);', [TTypeConvert.KTypeToPType(genericType1.fieldType)]))
          else if (TTypeConvert.KTypeToPType(genericType1.fieldType) = 'String') then
            Add('    item := TJNIEnv.JStringToString(env, jstring(jo));')
          else
            Add(Format('    item := %s(env^^.%s(env, jo, mValue));', [TTypeConvert.KTypeToPType(genericType1.fieldType), TTypeConvert.KTypeToCallMethod(genericType1.fieldType)]));
          Add('    lst.Add(item);');
          Add('  end;');
        end;
        if (strTmp.Contains('jHashMapToMap')) then begin
          Add('var');
          Add('  clsMap: jclass;');
          Add('  clsSet: jclass;');
          Add('  clsIter: jclass;');
          Add('  mKeySet: jmethodID;');
          Add('  mGet: jmethodID;');
          Add('  mIter: jmethodID;');
          Add('  mNext: jmethodID;');
          Add('  mHasNext: jmethodID;');
          Add('  objSet: jobject;');
          Add('  objIter: jobject;');
          Add('  jT1Cls: jclass;');
          Add('  jT1Value: jmethodID;');
          Add('  jT2Cls: jclass;');
          Add('  jT2Value: jmethodID;');
          Add('  hasNext: jboolean;');
          Add('  key: jobject;');
          Add('  val: jobject;');
          if (TTypeConvert.KTypeToCType(genericType1.fieldType).Contains('*')) then
            Add(Format('  mKey: %s;', [TTypeConvert.KTypeToPType(genericType1.fieldType)]))
          else if (TTypeConvert.KTypeToPType(genericType1.fieldType) = 'String') then
            Add('  mKey: String;')
          else
            Add(Format('  mKey: %s;', [TTypeConvert.KTypeToPType(genericType1.fieldType)]));
          if (TTypeConvert.KTypeToCType(genericType2.fieldType).Contains('*')) then
            Add(Format('  mVal: %s;', [TTypeConvert.KTypeToPType(genericType2.fieldType)]))
          else if (TTypeConvert.KTypeToPType(genericType2.fieldType) = 'String') then
            Add('  mVal: String;')
          else
            Add(Format('  mVal: %s;', [TTypeConvert.KTypeToPType(genericType2.fieldType)]));
          Add('begin');
          Add('  mp.Clear;');
          Add('  clsMap := env^^.FindClass(env, ''java/util/Map'');');
          Add('  clsSet := env^^.FindClass(env, ''java/util/Set'');');
          Add('  clsIter := env^^.FindClass(env, ''java/util/Iterator'');');
          Add('  mKeySet := env^^.GetMethodID(env, clsMap, ''keySet'', ''()Ljava/util/Set;'');');
          Add('  mGet := env^^.GetMethodID(env, clsMap, ''get'', ''(Ljava/lang/Object;)Ljava/lang/Object;'');');
          Add('  mIter := env^^.GetMethodID(env, clsSet, ''iterator'', ''()Ljava/util/Iterator;'');');
          Add('  mNext := env^^.GetMethodID(env, clsIter, ''next'', ''()Ljava/lang/Object;'');');
          Add('  mHasNext := env^^.GetMethodID(env, clsIter, ''hasNext'', ''()Z'');');
          Add('  objSet := env^^.CallObjectMethod(env, obj, mKeySet);');
          Add('  objIter := env^^.CallObjectMethod(env, objSet, mIter);');
          if (TTypeConvert.KTypeIsBasicType(genericType1.fieldType)) then begin
            Add(Format('  jT1Cls := env^^.FindClass(env, ''%s'');', [
              genericType1.fieldFullPackageName.Replace('.', '/', [rfIgnoreCase, rfReplaceAll])]));
            Add(Format('  jT1Value := env^^.GetMethodID(env, jT1Cls, ''%s'', ''%s'');', [
              TTypeConvert.KTypeToJObjectGetName(genericType1.fieldType), TTypeConvert.KTypeToJObjectGetSig(genericType1.fieldType)]));
          end;
          if (TTypeConvert.KTypeIsBasicType(genericType2.fieldType)) then begin
            Add(Format('  jT2Cls := env^^.FindClass(env, ''%s'');', [
              genericType2.fieldFullPackageName.Replace('.', '/', [rfIgnoreCase, rfReplaceAll])]));
            Add(Format('  jT2Value := env^^.GetMethodID(env, jT2Cls, ''%s'', ''%s'');', [
              TTypeConvert.KTypeToJObjectGetName(genericType2.fieldType), TTypeConvert.KTypeToJObjectGetSig(genericType2.fieldType)]));
          end;
          Add('  while (True) do begin');
          Add('    hasNext := env^^.CallBooleanMethod(env, objIter, mHasNext);');
          Add('    if (hasNext = JNI_FALSE) then Break;');
          Add('    key := env^^.CallObjectMethod(env, objIter, mNext);');
          Add('    val := env^^.CallObjectMethodA(env, obj, mGet, TJNIEnv.ArgsToJValues(env, [key]));');
          if (TTypeConvert.KTypeToCType(genericType1.fieldType).Contains('*')) then
            Add(Format('    mKey := %s.fromJObject(env, val);',[TTypeConvert.KTypeToPType(genericType1.fieldType)]))
          else if (TTypeConvert.KTypeToPType(genericType1.fieldType) = 'String') then
            Add('    mKey := TJNIEnv.JStringToString(env, jstring(key));')
          else
            Add(Format('    mKey := %s(env^^.%s(env, key, jT1Value));', [TTypeConvert.KTypeToPType(genericType1.fieldType), TTypeConvert.KTypeToCallMethod(genericType1.fieldType)]));
          if (TTypeConvert.KTypeToCType(genericType2.fieldType).Contains('*')) then
            Add(Format('    mVal := %s.fromJObject(env, val);',[TTypeConvert.KTypeToPType(genericType2.fieldType)]))
          else if (TTypeConvert.KTypeToPType(genericType2.fieldType) = 'String') then
            Add('    mVal := TJNIEnv.JStringToString(env, jstring(val));')
          else
            Add(Format('    mVal := %s(env^^.%s(env, key, jT2Value));', [TTypeConvert.KTypeToPType(genericType2.fieldType), TTypeConvert.KTypeToCallMethod(genericType2.fieldType)]));
          Add('    mp.Add(mKey, mVal);');
          Add('  end;');
        end;
        if (strTmp.Contains('jHashSetToSet')) then begin
          Add('var');
          Add('  clsSet: jclass;');
          Add('  clsIter: jclass;');
          Add('  mIter: jmethodID;');
          Add('  mNext: jmethodID;');
          Add('  mHasNext: jmethodID;');
          Add('  objIter: jobject;');
          Add('  jTCls: jclass;');
          Add('  mValue: jmethodID;');
          Add('  hasNext: jboolean;');
          Add('  key: jobject;');
          if (TTypeConvert.KTypeToCType(genericType1.fieldType).Contains('*')) then
            Add(Format('  mKey: %s;', [TTypeConvert.KTypeToPType(genericType1.fieldType)]))
          else if (TTypeConvert.KTypeToPType(genericType1.fieldType) = 'String') then
            Add('  mKey: String;')
          else
            Add(Format('  mKey: %s;', [TTypeConvert.KTypeToPType(genericType1.fieldType)]));
          Add('begin');
          Add('  st.Clear;');
          Add('  clsSet := env^^.FindClass(env, ''java/util/Set'');');
          Add('  clsIter := env^^.FindClass(env, ''java/util/Iterator'');');
          Add('  mIter := env^^.GetMethodID(env, clsSet, ''iterator'', ''()Ljava/util/Iterator;'');');
          Add('  mNext := env^^.GetMethodID(env, clsIter, ''next'', ''()Ljava/lang/Object;'');');
          Add('  mHasNext := env^^.GetMethodID(env, clsIter, ''hasNext'', ''()Z'');');
          Add('  objIter := env^^.CallObjectMethod(env, obj, mIter);');
          if (TTypeConvert.KTypeIsBasicType(genericType1.fieldType)) then begin
            Add(Format('  jTCls := env^^.FindClass(env, ''%s'');', [
              genericType1.fieldFullPackageName.Replace('.', '/', [rfReplaceAll, rfIgnoreCase])]));
            Add(Format('  mValue := env^^.GetMethodID(env, jTCls, ''%s'', ''%s'');', [
              TTypeConvert.KTypeToJObjectGetName(genericType1.fieldType), TTypeConvert.KTypeToJObjectGetSig(genericType1.fieldType)]));
          end;
          Add('  while (True) do begin');
          Add('    hasNext := env^^.CallBooleanMethod(env, objIter, mHasNext);');
          Add('    if (hasNext = JNI_FALSE) then Break;');
          Add('    key := env^^.CallObjectMethod(env, objIter, mNext);');
          if (TTypeConvert.KTypeToCType(genericType1.fieldType).Contains('*')) then
            Add(Format('    mKey := %s.fromJObject(env, key);', [TTypeConvert.KTypeToPType(genericType1.fieldType)]))
          else if (TTypeConvert.KTypeToPType(genericType1.fieldType) = 'String') then
            Add('    mKey := TJNIEnv.JStringToString(env, jstring(key));')
          else
            Add(Format('    mKey := %s(env^^.%s(env, key, mValue));', [
              TTypeConvert.KTypeToPType(genericType1.fieldType), TTypeConvert.KTypeToCallMethod(genericType1.fieldType)]));
          Add('    st.Add(mKey);');
          Add('  end;');
        end;
        if (strTmp.Contains('arrayToJArray')) then begin
          Add('var');
          Add('  clsType: jclass;');
          Add('  mConstructor: jmethodID;');
          Add('  i: Integer;');
          Add('  item: jobject;');
          Add('begin');
          Add(Format('  clsType := env^^.FindClass(env, ''%s'');', [baseType.fieldFullPackageName.Replace('.', '/', [rfIgnoreCase, rfReplaceAll])]));
          if (TTypeConvert.KTypeIsBasicType(baseType.fieldType)) then begin
            Add(Format('  mConstructor := env^^.GetMethodID(env, clsType, ''<init>'', ''%s'');', [TTypeConvert.KTypeToJObjectConstructorSig(baseType.fieldType)]));
          end;
          Add(Format('  Result := env^^.NewObjectArray(env, %d, clsType, nil);', [AMaxArraySize]));
          Add(Format('  for i := 0 to %d - 1 do begin', [AMaxArraySize]));
          if (TTypeConvert.KTypeIsBasicType(baseType.fieldType)) then begin
            Add('    item := env^^.NewObjectA(env, clsType, mConstructor, TJNIEnv.ArgsToJValues(env, [dest[i]]));');
            Add('    env^^.SetObjectArrayElement(env, Result, i, item);');
          end else if (TTypeConvert.KTypeToPType(baseType.fieldType) = 'String') then
            Add('    env^^.SetObjectArrayElement(env, Result, i, TJNIEnv.StringToJString(env, dest[i]));')
          else begin
            Add('    if (dest[i] <> nil) then');
            Add('      env^^.SetObjectArrayElement(env, Result, i, dest[i].toJObject(env));');
          end;
          Add('  end;');
        end;
        if (strTmp.Contains('listToJArrayList')) then begin
          Add('var');
          Add('  clsList: jclass;');
          Add('  mInitList: jmethodID;');
          Add('  mAdd: jmethodID;');
          Add('  clsType: jclass;');
          Add('  mConstructor: jmethodID;');
          Add('  i: Integer;');
          Add('  tmp: jobject;');
          Add('begin');
          Add('  clsList := env^^.FindClass(env, ''java/util/ArrayList'');');
          Add('  mInitList := env^^.GetMethodID(env, clsList, ''<init>'', ''()V'');');
          Add('  mAdd := env^^.GetMethodID(env, clsList, ''add'', ''(Ljava/lang/Object;)Z'');');
          Add('  Result := env^^.NewObject(env, clsList, mInitList);');
          if (TTypeConvert.KTypeIsBasicType(genericType1.fieldType)) then begin
            Add(Format('  clsType := env^^.FindClass(env, ''%s'');', [genericType1.fieldFullPackageName.Replace('.', '/', [rfIgnoreCase, rfReplaceAll])]));
            Add(Format('  mConstructor := env^^.GetMethodID(env, clsType, ''<init>'', ''%s'');', [TTypeConvert.KTypeToJObjectConstructorSig(genericType1.fieldType)]));
          end;
          Add('  for i := 0 to lst.Count - 1 do begin');
          if (TTypeConvert.KTypeToCType(genericType1.fieldType).Contains('*')) then
            Add('    tmp := lst[i].toJObject(env);')
          else if (TTypeConvert.KTypeToPType(genericType1.fieldType) = 'String') then
            Add('    tmp := TJNIEnv.StringToJString(env, lst[i]);')
          else
            Add('    tmp := env^^.NewObjectA(env, clsType, mConstructor, TJNIEnv.ArgsToJValues(env, [lst[i]]));');
          Add('    env^^.CallBooleanMethodA(env, Result, mAdd, TJNIEnv.ArgsToJValues(env, [tmp]))');
          Add('  end;');
        end;

        if (strTmp.Contains('mapToJHashMap')) then begin
          Add('var');
          Add('  clsMap: jclass;');
          Add('  mInitMap: jmethodID;');
          Add('  mPut: jmethodID;');
          Add('  clsType1: jclass;');
          Add('  mmConstructor1: jmethodID;');
          Add('  clsType2: jclass;');
          Add('  mmConstructor2: jmethodID;');
          Add('  i: Integer;');
          Add('  key: jobject;');
          Add('  val: jobject;');
          Add('begin');
          Add('  clsMap := env^^.FindClass(env, ''java/util/HashMap'');');
          Add('  mInitMap := env^^.GetMethodID(env, clsMap, ''<init>'', ''()V'');');
          Add('  mPut := env^^.GetMethodID(env, clsMap, ''put'', ''(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;'');');
          Add('  Result := env^^.NewObject(env, clsMap, mInitMap);');
          if (TTypeConvert.KTypeIsBasicType(genericType1.fieldType)) then begin
            Add(Format('  clsType1 := env^^.FindClass(env, ''%s'');', [genericType1.fieldFullPackageName.Replace('.', '/', [rfIgnoreCase, rfReplaceAll])]));
            Add(Format('  mmConstructor1 := env^^.GetMethodID(env, clsType1, ''<init>'', ''%s'');', [TTypeConvert.KTypeToJObjectConstructorSig(genericType1.fieldType)]));
          end;
          if (TTypeConvert.KTypeIsBasicType(genericType2.fieldType)) then begin
            Add(Format('  clsType2 := env^^.FindClass(env, ''%s'');', [genericType2.fieldFullPackageName.Replace('.', '/', [rfIgnoreCase, rfReplaceAll])]));
            Add(Format('  mmConstructor2 := env^^.GetMethodID(env, clsType2, ''<init>'', ''%s'');', [TTypeConvert.KTypeToJObjectConstructorSig(genericType2.fieldType)]));
          end;
          Add('  for i := 0 to mp.Count - 1 do begin');
          if (TTypeConvert.KTypeToCType(genericType1.fieldType).Contains('*')) then
            Add('    key := mp.Keys[i].toJObject(env);')
          else if (TTypeConvert.KTypeToPType(genericType1.fieldType) = 'String') then
            Add('    key := TJNIEnv.StringToJString(env, mp.Keys[i]);')
          else begin
            Add('    key := env^^.NewObjectA(env, clsType1, mmConstructor1, TJNIEnv.ArgsToJValues(env, [mp.Keys[i]]));');
          end;
          if (TTypeConvert.KTypeToCType(genericType2.fieldType).Contains('*')) then
            Add('    val := mp.Data[i].toJObject(env);')
          else if (TTypeConvert.KTypeToPType(genericType2.fieldType) = 'String') then
            Add('    val := TJNIEnv.StringToJString(env, mp.Data[i]);')
          else begin
            Add('    val := env^^.NewObjectA(env, clsType2, mmConstructor2, TJNIEnv.ArgsToJValues(env, [mp.Data[i]]));');
          end;
          Add('    env^^.CallObjectMethodA(env, Result, mPut, TJNIEnv.ArgsToJValues(env, [key, val]));');
          Add('  end;');
        end;
        if (strTmp.Contains('setToJHashSet')) then begin
          Add('var');
          Add('  clsSet: jclass;');
          Add('  mInitSet: jmethodID;');
          Add('  mAdd: jmethodID;');
          Add('  clsType: jclass;');
          Add('  mConstructor: jmethodID;');
          Add('  i: Integer;');
          Add('  key: jobject;');
          Add('begin');
          Add('  clsSet := env^^.FindClass(env, ''java/util/HashSet'');');
          Add('  mInitSet := env^^.GetMethodID(env, clsSet, ''<init>'', ''()V'');');
          Add('  mAdd := env^^.GetMethodID(env, clsSet, ''add'', ''(Ljava/lang/Object;)Z'');');
          Add('  Result := env^^.NewObject(env, clsSet, mInitSet);');
          if (TTypeConvert.KTypeIsBasicType(genericType1.fieldType)) then begin
            Add(Format('  clsType := env^^.FindClass(env, ''%s'');', [genericType1.fieldFullPackageName.Replace('.', '/', [rfIgnoreCase, rfReplaceAll])]));
            Add(Format('  mConstructor := env^^.GetMethodID(env, clsType, ''<init>'', ''%s'');', [TTypeConvert.KTypeToJObjectConstructorSig(genericType1.fieldType)]));
          end;
          Add('  for i := 0 to st.Count - 1 do begin');
          if (TTypeConvert.KTypeToCType(genericType1.fieldType).Contains('*')) then
            Add('    key := st[i].toJObject(env);')
          else if (TTypeConvert.KTypeToPType(genericType1.fieldType) = 'String') then
            Add('    key := TJNIEnv.StringToJString(env, st[i]);')
          else
            Add('    key := env^^.NewObjectA(env, clsType, mConstructor, TJNIEnv.ArgsToJValues(env, [st[i]]));');
          Add('    env^^.CallBooleanMethodA(env, Result, mAdd, TJNIEnv.ArgsToJValues(env, [key]));');
          Add('  end;');
        end;
        Add('end;');
        Add('');
      end;
    end;
  end;

end;

class function TCodePas.constructorParams(AFieldList: TFieldList;
  methodList: TFieldMap): string;
var
  r: string = '';
  i: Integer;
begin
  for i := 0 to AFieldList.Count - 1 do begin
    if (AFieldList[i].isArray) then begin
      r += Format('%s_arrayToJArray(env, %s), ', [AFieldList[i].fieldName, AFieldList[i].fieldName]);
      methodList.Add(Format('function %s_arrayToJArray(env: PJNIEnv; var dest: T%sArray): jobjectArray;', [
        AFieldList[i].fieldName, AFieldList[i].fieldName]), AFieldList[i]);
    end else if (AFieldList[i].isList) then begin
      r += Format('%s_listToJArrayList(env, %s), ', [AFieldList[i].fieldName, AFieldList[i].fieldName]);
      methodList.Add(Format('function %s_listToJArrayList(env: PJNIEnv; var lst: T%sList): jobject;', [
        AFieldList[i].fieldName, AFieldList[i].fieldName]), AFieldList[i]);
    end else if (AFieldList[i].isMap) then begin
      r += Format('%s_mapToJHashMap(env, %s), ', [AFieldList[i].fieldName, AFieldList[i].fieldName]);
      methodList.Add(Format('function %s_mapToJHashMap(env: PJNIEnv; var mp: T%sMap): jobject;', [
        AFieldList[i].fieldName, AFieldList[i].fieldName]), AFieldList[i]);
    end else if (AFieldList[i].isSet) then begin
      r += Format('%s_setToJHashSet(env, %s), ', [AFieldList[i].fieldName, AFieldList[i].fieldName]);
      methodList.Add(Format('function %s_setToJHashSet(env: PJNIEnv; var st: T%sSet): jobject;', [
        AFieldList[i].fieldName, AFieldList[i].fieldName]), AFieldList[i]);
    end else begin
      case AFieldList[i].baseType.fieldCategory of
      ftcSimple: r += AFieldList[i].fieldName + ', ';
      ftcString: r += Format('TJNIEnv.StringToJString(env, %s), ', [AFieldList[i].fieldName]);
      ftcObject: r += Format('%s.toJObject(env), ', [AFieldList[i].fieldName]);
      end;
    end;
  end;
  r := r.Trim.Trim([',']);
  Exit(r);
end;

class procedure TCodePas.code(AOutPath: string; AClassInfo: TClassInfo;
  AMaxArraySize: Integer);
var
  codeList: TStringList;
  addMethodList: TFieldMap;
  genericList: TStringList;
  i: Integer;
  callType: string;
begin
  addMethodList:= TFieldMap.Create;
  codeList := TStringList.Create;
  genericList := TStringList.Create;
  with codeList do begin
    headUses(codeList, AClassInfo);
    Add(Format('  T%s = class', [AClassInfo.ktClassName]));
    Add('  public');
    // fields
    for i := 0 to AClassInfo.fieldList.Count - 1 do begin
      if (AClassInfo.fieldList[i].isArray) then
        Add(Format('    %s: T%sArray;', [AClassInfo.fieldList[i].fieldName, AClassInfo.fieldList[i].fieldName]))
      else if (AClassInfo.fieldList[i].isList) then
        Add(Format('    %s: T%sList;', [AClassInfo.fieldList[i].fieldName, AClassInfo.fieldList[i].fieldName]))
      else if (AClassInfo.fieldList[i].isMap) then
        Add(Format('    %s: T%sMap;', [AClassInfo.fieldList[i].fieldName, AClassInfo.fieldList[i].fieldName]))
      else if (AClassInfo.fieldList[i].isSet) then
        Add(Format('    %s: T%sSet;', [AClassInfo.fieldList[i].fieldName, AClassInfo.fieldList[i].fieldName]))
      else
        Add(Format('    %s: %s;', [AClassInfo.fieldList[i].fieldName, TTypeConvert.KTypeToPType(AClassInfo.fieldList[i].baseType.fieldType)]));
    end;
    Add(Format('    class function fromJObject(env: PJNIEnv; obj: jobject): T%s;', [AClassInfo.ktClassName]));
    Add('    function toJObject(env: PJNIEnv): jobject;');
    Add('{| method |}');
    Add('  end;');
    Add('');
    Add('implementation');
    // methods
    Add('');
    Add(Format('class function T%s.fromJObject(env: PJNIEnv; obj: jobject): T%s;', [AClassInfo.ktClassName, AClassInfo.ktClassName]));
    Add('var');
    Add('  cls: jclass;');
    Add('  m: jmethodID;');
    Add('begin');
    Add('  Result := nil;');
    Add('  if (env <> nil) and (obj <> nil) then begin');
    Add(Format('    Result := T%s.Create;', [AClassInfo.ktClassName]));
    Add(Format('    cls := env^^.FindClass(env, ''%s'');', [AClassInfo.fullPackageName.Replace('.', '/', [rfIgnoreCase, rfReplaceAll])]));
    for i := 0 to AClassInfo.fieldList.Count - 1 do begin
      if (i = 0) then begin
        Add(Format('    m := env^^.GetMethodID(env, cls, ''%s'', ''()%s'');', [
          TTypeConvert.KFieldToGetName(AClassInfo.fieldList[i].fieldName),
          AClassInfo.fieldList[i].baseType.fieldSignature]));
      end else begin
        Add(Format('    m := env^^.GetMethodID(env, cls, ''%s'', ''()%s'');', [
          TTypeConvert.KFieldToGetName(AClassInfo.fieldList[i].fieldName),
          AClassInfo.fieldList[i].baseType.fieldSignature]));
      end;
      callType:= TTypeConvert.KTypeToCallMethod(AClassInfo.fieldList[i].baseType.fieldType);
      // pas
      if (callType.Contains('Object')) then begin
        if (AClassInfo.fieldList[i].isArray) then begin
          Add(Format('    %s_jarrayToObjectArray(env, env^^.CallObjectMethod(env, obj, m), Result.%s);', [
            AClassInfo.fieldList[i].fieldName, AClassInfo.fieldList[i].fieldName]));
          addMethodList.Add(Format('procedure %s_jarrayToObjectArray(env: PJNIEnv; arr: jobject; var dest: T%sArray);', [
            AClassInfo.fieldList[i].fieldName, AClassInfo.fieldList[i].fieldName]), AClassInfo.fieldList[i]);
          genericList.Add(Format('  T%sArray = array[0..%d] of %s;', [AClassInfo.fieldList[i].fieldName, AMaxArraySize - 1, TTypeConvert.KTypeToPType(AClassInfo.fieldList[i].baseType.fieldType)]));
        end else if (AClassInfo.fieldList[i].isList) then begin
          Add(Format('    %s_jArrayListToList(env, env^^.CallObjectMethod(env, obj, m), Result.%s);', [
            AClassInfo.fieldList[i].fieldName, AClassInfo.fieldList[i].fieldName]));
          addMethodList.Add(Format('procedure %s_jArrayListToList(env: PJNIEnv; obj: jobject; var lst: T%sList);', [
            AClassInfo.fieldList[i].fieldName, AClassInfo.fieldList[i].fieldName]), AClassInfo.fieldList[i]);
          genericList.Add(Format('  T%sList = specialize TFPGList<%s>;', [AClassInfo.fieldList[i].fieldName, TTypeConvert.KTypeToPType(AClassInfo.fieldList[i].genericType1.fieldType)]));
        end else if (AClassInfo.fieldList[i].isMap) then begin
          Add(Format('    %s_jHashMapToMap(env, env^^.CallObjectMethod(env, obj, m), Result.%s);', [
            AClassInfo.fieldList[i].fieldName, AClassInfo.fieldList[i].fieldName]));
          addMethodList.Add(Format('procedure %s_jHashMapToMap(env: PJNIEnv; obj: jobject; var mp: T%sMap);', [
            AClassInfo.fieldList[i].fieldName, AClassInfo.fieldList[i].fieldName]),
            AClassInfo.fieldList[i]);
          genericList.Add(Format('  T%sMap = specialize TFPGMap<%s, %s>;', [
            AClassInfo.fieldList[i].fieldName, TTypeConvert.KTypeToPType(AClassInfo.fieldList[i].genericType1.fieldType), TTypeConvert.KTypeToPType(AClassInfo.fieldList[i].genericType2.fieldType)]));
        end else if (AClassInfo.fieldList[i].isSet) then begin
          Add(Format('  %s_jHashSetToSet(env, env^^.CallObjectMethod(env, obj, m), Result.%s);', [
            AClassInfo.fieldList[i].fieldName, AClassInfo.fieldList[i].fieldName]));
          addMethodList.Add(Format('procedure %s_jHashSetToSet(env: PJNIEnv; obj: jobject; var st: T%sSet);', [
            AClassInfo.fieldList[i].fieldName, AClassInfo.fieldList[i].fieldName]), AClassInfo.fieldList[i]);
          genericList.Add(Format('  T%sSet = specialize TFPGList<%s>;', [AClassInfo.fieldList[i].fieldName, TTypeConvert.KTypeToPType(AClassInfo.fieldList[i].genericType1.fieldType)]));
        end else begin
          if (TTypeConvert.KTypeToPType(AClassInfo.fieldList[i].baseType.fieldType) = 'String') then begin
            Add(Format('    Result.%s := TJniEnv.JStringToString(env, env^^.CallObjectMethod(env, obj, m));', [AClassInfo.fieldList[i].fieldName]));
          end else begin
            Add(Format('    Result.%s := T%s.fromJObject(env, env^^.CallObjectMethod(env, obj, m));', [
              AClassInfo.fieldList[i].fieldName, AClassInfo.fieldList[i].baseType.fieldType]));
          end;
        end;
      end else begin
        if (AClassInfo.fieldList[i].isArray) then begin
          Add(Format('    %s_jarrayToArray(env, env^^.CallObjectMethod(env, obj, m), Result.%s);', [
            AClassInfo.fieldList[i].fieldName, AClassInfo.fieldList[i].fieldName]));
          addMethodList.Add(Format('procedure %s_jarrayToArray(env: PJNIEnv; arr: jobject; var dest: T%sArray);', [
            AClassInfo.fieldList[i].fieldName, AClassInfo.fieldList[i].fieldName]), AClassInfo.fieldList[i]);
          genericList.Add(Format('  T%sArray = array[0..%d] of %s;', [AClassInfo.fieldList[i].fieldName, AMaxArraySize - 1, TTypeConvert.KTypeToPType(AClassInfo.fieldList[i].baseType.fieldType)]));
        end else begin
          Add(Format('    Result.%s := env^^.%s(env, obj, m);', [AClassInfo.fieldList[i].fieldName, callType]));
        end;
      end;
    end;
    Add('  end;');
    Add('end;');
    Add('');
    Add(Format('function T%s.toJObject(env: PJNIEnv): jobject;', [AClassInfo.ktClassName]));
    Add('var');
    Add('  cls: jclass;');
    Add('  m: jmethodID;');
    Add('begin');
    Add('  Result := nil;');
    Add('  if (env <> nil) then begin');
    Add(Format('    cls := env^^.FindClass(env, ''%s'');', [AClassInfo.fullPackageName.Replace('.', '/', [rfIgnoreCase, rfReplaceAll])]));
    Add(Format('    m := env^^.GetMethodID(env, cls, ''<init>'', ''%s'');', [AClassInfo.constructorSig]));
    Add(Format('    Result := env^^.NewObjectA(env, cls, m, TJNIEnv.ArgsToJValues(env, [%s]));', [constructorParams(AClassInfo.fieldList, addMethodList)]));
    Add('  end;');
    Add('');
    Add('end;');
    Add('');
    Text:= Text.Replace('{| generic |}', genericList.Text, [rfIgnoreCase, rfReplaceAll]);
    addMethod(codeList, AClassInfo.ktClassName, addMethodList);
    addMethodImpl(codeList, AClassInfo.ktClassName, addMethodList, AMaxArraySize);
    Add('end.');
    SaveToFile(AOutPath + 'unt' + AClassInfo.ktClassName + '.pas');
    Free;
  end;

  addMethodList.Free;
  genericList.Free;
end;

class procedure TCodePas.generate(AOutPath: string; AKtFile: string;
  AMaxArraySize: Integer);
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
  code(AOutPath, clsInfo, AMaxArraySize);
  if (clsInfo <> nil) then clsInfo.Free;
end;

end.

