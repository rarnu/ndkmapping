unit codeCpp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, codeTypes;

type

  { TCodeCpp }

  TCodeCpp = class
  private
    class procedure head(codeList: TStringList; AClassInfo: TClassInfo);
    class procedure addMethod(codeList: TStringList; ktClassName: string; methodList: TFieldMap; AMaxArraySize: Integer);
    class procedure addImpl(codeList: TStringList; ACat: TImplCategory; AType: TFieldType; AType2: TFieldType; AMaxArraySize: Integer);
    class function constructorParams(AFieldList: TFieldList; methodList: TFieldMap; AMaxArraySize: Integer): string;
    class procedure code(AOutPath: string; AClassInfo: TClassInfo; AMaxArraySize: Integer);
  public
    class procedure generate(AOutPath: string; AKtFile: string; AMaxArraySize: Integer);
  end;

implementation

uses
  codeParser;

{ TCodeCpp }

class procedure TCodeCpp.head(codeList: TStringList; AClassInfo: TClassInfo);
var
  i: Integer;
  tInc: string;
begin
  // head
  with codeList do begin
    Add('#include <jni.h>');
    Add('#include <stdint.h>');
    Add('#include <stddef.h>');
    Add('#include <stdlib.h>');
    Add('#include <string>');
    Add('#include <list>');
    Add('#include <map>');
    Add('#include <set>');
    for i := 0 to AClassInfo.fieldList.Count - 1 do begin
      if (AClassInfo.fieldList[i].baseType.fieldCategory = ftcObject) then begin
        if (not AClassInfo.fieldList[i].isList) and (not AClassInfo.fieldList[i].isMap) and (not AClassInfo.fieldList[i].isSet) then begin
          tInc:= Format('#include "%s.h"', [AClassInfo.fieldList[i].baseType.fieldType]);
          if (Text.IndexOf(tInc) = -1) then Add(tInc);
        end else begin
          if (AClassInfo.fieldList[i].genericType1 <> nil) then begin
            if (AClassInfo.fieldList[i].genericType1.fieldCategory = ftcObject) then begin
              tInc:= Format('#include "%s.h"', [AClassInfo.fieldList[i].genericType1.fieldType]);
              if (Text.IndexOf(tInc) = -1) then Add(tInc);
            end;
          end;
          if (AClassInfo.fieldList[i].genericType2 <> nil) then begin
            if (AClassInfo.fieldList[i].genericType2.fieldCategory = ftcObject) then begin
              tInc:= Format('#include "%s.h"', [AClassInfo.fieldList[i].genericType2.fieldType]);
              if (Text.IndexOf(tInc) = -1) then Add(tInc);
            end;
          end;

        end;
      end;
    end;
    Add('using namespace std;');
  end;
end;

class procedure TCodeCpp.addMethod(codeList: TStringList; ktClassName: string;
  methodList: TFieldMap; AMaxArraySize: Integer);
var
  i: Integer;
  tC: string;
  typ: string;
begin
  for i := 0 to methodList.Count - 1 do begin
    tC:= methodList.Keys[i];
    typ := tC.Substring(0, tC.IndexOf(' ')).Trim;
    tC:= tC.Substring(tc.IndexOf(' ')).Trim;
    tC:= Format('%s %s::%s {', [typ, ktClassName, tC]);
    codeList.Add(tC);
    // add method code
    if (tC.Contains('jarrayToObjectArray')) then addImpl(codeList, icJArrayToObjectArray, methodList.Data[i].baseType, nil, AMaxArraySize);
    if (tC.Contains('jarrayToArray')) then addImpl(codeList, icJArrayToSimpleArray, methodList.Data[i].baseType, nil, AMaxArraySize);
    if (tC.Contains('jArrayListToList')) then addImpl(codeList, icJArrayListToList, methodList.Data[i].genericType1, nil, AMaxArraySize);
    if (tC.Contains('jHashMapToMap')) then addImpl(codeList, icJHashMapToMap, methodList.Data[i].genericType1, methodList.Data[i].genericType2, AMaxArraySize);
    if (tC.Contains('jHashSetToSet')) then addImpl(codeList, icJHashSetToSet, methodList.Data[i].genericType1, nil, AMaxArraySize);
    if (tC.Contains('arrayToJArray')) then addImpl(codeList, icArrayToJArray, methodList.Data[i].baseType, nil, AMaxArraySize);
    if (tC.Contains('listToJArrayList')) then addImpl(codeList, icListToJArrayList, methodList.Data[i].genericType1, nil, AMaxArraySize);
    if (tC.Contains('mapToJHashMap')) then addImpl(codeList, icMapToJHashMap, methodList.Data[i].genericType1, methodList.Data[i].genericType2, AMaxArraySize);
    if (tC.Contains('setToJHashSet')) then addImpl(codeList, icSetToJHashSet, methodList.Data[i].genericType1, nil, AMaxArraySize);
    codeList.Add('}');
    codeList.Add('');
  end;
end;

class procedure TCodeCpp.addImpl(codeList: TStringList; ACat: TImplCategory;
  AType: TFieldType; AType2: TFieldType; AMaxArraySize: Integer);
begin
  with codeList do begin
    case ACat of
    icJArrayToObjectArray:
      begin
        Add('    jobjectArray jarr = (jobjectArray) arr;');
        Add('    int count = env->GetArrayLength(jarr);');
        Add('    for (int i = 0; i < count; i++) {');
        if (TTypeConvert.KTypeToCType(AType.fieldType) = 'string') then begin
          Add('        jstring tmpStr = (jstring) env->GetObjectArrayElement(jarr, i);');
          Add('        dest[i] = string(env->GetStringUTFChars(tmpStr, NULL));');
        end else begin
          Add('        jobject tmpObj = env->GetObjectArrayElement(jarr, i);');
          Add(Format('        dest[i] = %s::fromJObject(env, tmpObj);', [AType.fieldType]));
        end;
        Add('    }');
      end;
    icJArrayToSimpleArray:
      begin
        Add(Format('    jclass jTCls = env->FindClass("%s");', [
          AType.fieldFullPackageName.Replace('.', '/', [rfIgnoreCase, rfReplaceAll])]));
        Add(Format('    jmethodID mValue = env->GetMethodID(jTCls, "%s", "%s");', [
          TTypeConvert.KTypeToJObjectGetName(AType.fieldType), TTypeConvert.KTypeToJObjectGetSig(AType.fieldType)]));
        Add('    jobjectArray jarr = (jobjectArray) arr;');
        Add('    int count = env->GetArrayLength(jarr);');
        Add('    for (int i = 0; i < count; i++) {');
        Add('        jobject tmpObj = env->GetObjectArrayElement(jarr, i);');
        Add(Format('        dest[i] = (%s) env->%s(tmpObj, mValue);', [
          TTypeConvert.KTypeToCType(AType.fieldType), TTypeConvert.KTypeToCallMethod(AType.fieldType)]));
        Add('    }');
      end;
    icJArrayListToList:
      begin
        Add('    lst.clear();');
        Add('    jclass clsList = env->FindClass("java/util/List");');
        Add('    jmethodID mGet = env->GetMethodID(clsList, "get", "(I)Ljava/lang/Object;");');
        Add('    jmethodID mSize = env->GetMethodID(clsList, "size", "()I");');
        if (TTypeConvert.KTypeIsBasicType(AType.fieldType)) then begin
          Add(Format('    jclass jTCls = env->FindClass("%s");', [
            AType.fieldFullPackageName.Replace('.', '/', [rfReplaceAll, rfIgnoreCase])]));
          Add(Format('    jmethodID mValue = env->GetMethodID(jTCls, "%s", "%s");', [
            TTypeConvert.KTypeToJObjectGetName(AType.fieldType), TTypeConvert.KTypeToJObjectGetSig(AType.fieldType)]));
        end;
        Add('    int count = env->CallIntMethod(obj, mSize);');
        Add('    for (int i = 0; i < count; i++) {');
        Add('        jobject jo = env->CallObjectMethod(obj, mGet, i);');
        if (TTypeConvert.KTypeToCType(AType.fieldType).Contains('*')) then
          Add(Format('        %s item = %s::fromJObject(env, jo);', [TTypeConvert.KTypeToCType(AType.fieldType), AType.fieldType]))
        else if (TTypeConvert.KTypeToCType(AType.fieldType) = 'string') then
          Add('        string item = string(env->GetStringUTFChars((jstring) jo, NULL));')
        else begin
          Add(Format('        %s item = (%s) env->%s(jo, mValue);', [
            TTypeConvert.KTypeToCType(AType.fieldType), TTypeConvert.KTypeToCType(AType.fieldType), TTypeConvert.KTypeToCallMethod(AType.fieldType)]));
        end;
        Add('        lst.push_back(item);');
        Add('    }');
      end;
    icJHashMapToMap:
      begin
        Add('    mp.clear();');
        Add('    jclass clsMap = env->FindClass("java/util/Map");');
        Add('    jclass clsSet = env->FindClass("java/util/Set");');
        Add('    jclass clsIter = env->FindClass("java/util/Iterator");');
        Add('    jmethodID mKeySet = env->GetMethodID(clsMap, "keySet", "()Ljava/util/Set;");');
        Add('    jmethodID mGet = env->GetMethodID(clsMap, "get", "(Ljava/lang/Object;)Ljava/lang/Object;");');
        Add('    jmethodID mIter = env->GetMethodID(clsSet, "iterator", "()Ljava/util/Iterator;");');
        Add('    jmethodID mNext = env->GetMethodID(clsIter, "next", "()Ljava/lang/Object;");');
        Add('    jmethodID mHasNext = env->GetMethodID(clsIter, "hasNext", "()Z");');
        Add('    jobject objSet = env->CallObjectMethod(obj, mKeySet);');
        Add('    jobject objIter = env->CallObjectMethod(objSet, mIter);');
        if (TTypeConvert.KTypeIsBasicType(AType.fieldType)) then begin
          Add(Format('    jclass jT1Cls = env->FindClass("%s");', [
            AType.fieldFullPackageName.Replace('.', '/', [rfIgnoreCase, rfReplaceAll])]));
          Add(Format('    jmethodID jT1Value = env->GetMethodID(jT1Cls, "%s", "%s");', [
            TTypeConvert.KTypeToJObjectGetName(AType.fieldType), TTypeConvert.KTypeToJObjectGetSig(AType.fieldType)]));
        end;
        if (TTypeConvert.KTypeIsBasicType(AType2.fieldType)) then begin
          Add(Format('    jclass jT2Cls = env->FindClass("%s");', [
            AType2.fieldFullPackageName.Replace('.', '/', [rfIgnoreCase, rfReplaceAll])]));
          Add(Format('    jmethodID jT2Value = env->GetMethodID(jT2Cls, "%s", "%s");', [
            TTypeConvert.KTypeToJObjectGetName(AType2.fieldType), TTypeConvert.KTypeToJObjectGetSig(AType2.fieldType)]));
        end;
        Add('    while (true) {');
        Add('        jboolean hasNext = env->CallBooleanMethod(objIter, mHasNext);');
        Add('        if (hasNext == JNI_FALSE) {');
        Add('            break;');
        Add('        }');
        Add('        jobject key = env->CallObjectMethod(objIter, mNext);');
        Add('        jobject val = env->CallObjectMethod(obj, mGet, key);');
        if (TTypeConvert.KTypeToCType(AType.fieldType).Contains('*')) then
          Add(Format('        %s mKey = %s::fromJObject(env, val);', [TTypeConvert.KTypeToCType(AType.fieldType), AType.fieldType]))
        else if (TTypeConvert.KTypeToCType(AType.fieldType) = 'string') then
          Add('        string mKey = string(env->GetStringUTFChars((jstring) key, NULL));')
        else begin
          Add(Format('        %s mKey = (%s) env->%s(key, jT1Value);', [
            TTypeConvert.KTypeToCType(AType.fieldType), TTypeConvert.KTypeToCType(AType.fieldType), TTypeConvert.KTypeToCallMethod(AType.fieldType)]));
        end;
        if (TTypeConvert.KTypeToCType(AType2.fieldType).Contains('*')) then
          Add(Format('        %s mVal = %s::fromJObject(env, val);', [TTypeConvert.KTypeToCType(AType2.fieldType), AType2.fieldType]))
        else if (TTypeConvert.KTypeToCType(AType2.fieldType) = 'string') then
          Add('        string mVal = string(env->GetStringUTFChars((jstring) val, NULL));')
        else begin
          Add(Format('        %s mVal = (%s) env->%s(val, jT2Value);', [
            TTypeConvert.KTypeToCType(AType2.fieldType), TTypeConvert.KTypeToCType(AType2.fieldType), TTypeConvert.KTypeToCallMethod(AType2.fieldType)]));
        end;
        Add('        mp[mKey] = mVal;');
        Add('    }');
      end;
    icJHashSetToSet:
      begin
        Add('    st.clear();');
        Add('    jclass clsSet = env->FindClass("java/util/Set");');
        Add('    jclass clsIter = env->FindClass("java/util/Iterator");');
        Add('    jmethodID mIter = env->GetMethodID(clsSet, "iterator", "()Ljava/util/Iterator;");');
        Add('    jmethodID mNext = env->GetMethodID(clsIter, "next", "()Ljava/lang/Object;");');
        Add('    jmethodID mHasNext = env->GetMethodID(clsIter, "hasNext", "()Z");');
        Add('    jobject objIter = env->CallObjectMethod(obj, mIter);');
        if (TTypeConvert.KTypeIsBasicType(AType.fieldType)) then begin
          Add(Format('    jclass jTCls = env->FindClass("%s");', [
            AType.fieldFullPackageName.Replace('.', '/', [rfReplaceAll, rfIgnoreCase])]));
          Add(Format('    jmethodID mValue = env->GetMethodID(jTCls, "%s", "%s");', [
            TTypeConvert.KTypeToJObjectGetName(AType.fieldType), TTypeConvert.KTypeToJObjectGetSig(AType.fieldType)]));
        end;
        Add('    while (true) {');
        Add('        jboolean hasNext = env->CallBooleanMethod(objIter, mHasNext);');
        Add('        if (hasNext == JNI_FALSE) {');
        Add('            break;');
        Add('        }');
        Add('        jobject key = env->CallObjectMethod(objIter, mNext);');
        if (TTypeConvert.KTypeToCType(AType.fieldType).Contains('*')) then
          Add(Format('        %s mKey = %s::fromJObject(env, key);', [TTypeConvert.KTypeToCType(AType.fieldType), AType.fieldType]))
        else if (TTypeConvert.KTypeToCType(AType.fieldType) = 'string') then
          Add('        string mKey = string(env->GetStringUTFChars((jstring) key, NULL));')
        else begin
          Add(Format('        %s mKey = (%s) env->%s(key, mValue);', [
            TTypeConvert.KTypeToCType(AType.fieldType), TTypeConvert.KTypeToCType(AType.fieldType), TTypeConvert.KTypeToCallMethod(AType.fieldType)]));
        end;
        Add('        st.insert(mKey);');
        Add('    }');
      end;
    icArrayToJArray:
      begin
        Add(Format('    jclass clsType = env->FindClass("%s");', [AType.fieldFullPackageName.Replace('.', '/', [rfIgnoreCase, rfReplaceAll])]));
        if (TTypeConvert.KTypeIsBasicType(AType.fieldType)) then begin
          Add(Format('    jmethodID mConstructor = env->GetMethodID(clsType, "<init>", "%s");', [TTypeConvert.KTypeToJObjectConstructorSig(AType.fieldType)]));
        end;
        Add(Format('    jobjectArray ret = env->NewObjectArray(%d, clsType, NULL);', [AMaxArraySize]));
        Add(Format('    for (int i = 0; i < %d; i++) {', [AMaxArraySize]));
        if (TTypeConvert.KTypeIsBasicType(AType.fieldType)) then begin
          Add('        jobject item = env->NewObject(clsType, mConstructor, dest[i]);');
          Add('        env->SetObjectArrayElement(ret, i, item);');
        end else if (TTypeConvert.KTypeToCType(AType.fieldType) = 'string') then
          Add('        env->SetObjectArrayElement(ret, i, env->NewStringUTF(dest[i].data()));')
        else begin
          Add('        if (dest[i] != NULL) {');
          Add('            env->SetObjectArrayElement(ret, i, dest[i]->toJObject(env));');
          Add('        }');
        end;
        Add('    }');
        Add('    return ret;');
      end;
    icListToJArrayList:
      begin
        Add('    jclass clsList = env->FindClass("java/util/ArrayList");');
        Add('    jmethodID mInitList = env->GetMethodID(clsList, "<init>", "()V");');
        Add('    jmethodID mAdd = env->GetMethodID(clsList, "add", "(Ljava/lang/Object;)Z");');
        Add('    jobject ret = env->NewObject(clsList, mInitList);');
        if (TTypeConvert.KTypeIsBasicType(AType.fieldType)) then begin
          Add(Format('    jclass clsType = env->FindClass("%s");', [AType.fieldFullPackageName.Replace('.', '/', [rfIgnoreCase, rfReplaceAll])]));
          Add(Format('    jmethodID mConstructor = env->GetMethodID(clsType, "<init>", "%s");', [TTypeConvert.KTypeToJObjectConstructorSig(AType.fieldType)]));
        end;
        Add('    for (auto iter = lst.begin(); iter != lst.end(); iter++) {');
        if (TTypeConvert.KTypeToCType(AType.fieldType).Contains('*')) then
          Add('        jobject tmp = (*iter)->toJObject(env);')
        else if (TTypeConvert.KTypeToCType(AType.fieldType) = 'string') then
          Add('        jstring tmp = env->NewStringUTF((*iter).data());')
        else begin
          Add(Format('        jobject tmp = env->NewObject(clsType, mConstructor, (j%s) (*iter));', [TTypeConvert.KTypeToJNIType(AType.fieldType)]));
        end;
        Add('        env->CallBooleanMethod(ret, mAdd, tmp);');
        Add('    }');
        Add('    return ret;');
      end;
    icMapToJHashMap:
      begin
        Add('    jclass clsMap = env->FindClass("java/util/HashMap");');
        Add('    jmethodID mInitMap = env->GetMethodID(clsMap, "<init>", "()V");');
        Add('    jmethodID mPut = env->GetMethodID(clsMap, "put", "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");');
        Add('    jobject ret = env->NewObject(clsMap, mInitMap);');
        if (TTypeConvert.KTypeIsBasicType(AType.fieldType)) then begin
          Add(Format('    jclass clsType1 = env->FindClass("%s");', [AType.fieldFullPackageName.Replace('.', '/', [rfIgnoreCase, rfReplaceAll])]));
          Add(Format('    jmethodID mmConstructor1 = env->GetMethodID(clsType1, "<init>", "%s");', [TTypeConvert.KTypeToJObjectConstructorSig(AType.fieldType)]));
        end;
        if (TTypeConvert.KTypeIsBasicType(AType2.fieldType)) then begin
          Add(Format('    jclass clsType2 = env->FindClass("%s");', [AType2.fieldFullPackageName.Replace('.', '/', [rfIgnoreCase, rfReplaceAll])]));
          Add(Format('    jmethodID mmConstructor2 = env->GetMethodID(clsType2, "<init>", "%s");', [TTypeConvert.KTypeToJObjectConstructorSig(AType2.fieldType)]));
        end;
        Add('    for (auto iter = mp.begin(); iter != mp.end(); iter++) {');
        if (TTypeConvert.KTypeToCType(AType.fieldType).Contains('*')) then
          Add('        jobject key = iter->first->toJObject(env);')
        else if (TTypeConvert.KTypeToCType(AType.fieldType) = 'string') then
          Add('        jstring key = env->NewStringUTF(iter->first.data());')
        else begin
          Add('        jobject key = env->NewObject(clsType1, mmConstructor1, (j%s) iter->first);', [TTypeConvert.KTypeToJNIType(AType.fieldType)]);
        end;
        if (TTypeConvert.KTypeToCType(AType2.fieldType).Contains('*')) then
          Add('        jobject val = iter->second->toJObject(env);')
        else if (TTypeConvert.KTypeToCType(AType2.fieldType) = 'string') then
          Add('        jstring val = env->NewStringUTF(iter->second.data());')
        else begin
          Add('        jobject val = env->NewObject(clsType2, mmConstructor2, (j%s) iter->second);', [TTypeConvert.KTypeToJNIType(AType.fieldType)]);
        end;
        Add('        env->CallObjectMethod(ret, mPut, key, val);');
        Add('    }');
        Add('    return ret;');
      end;
    icSetToJHashSet:
      begin
        Add('    jclass clsSet = env->FindClass("java/util/HashSet");');
        Add('    jmethodID mInitSet = env->GetMethodID(clsSet, "<init>", "()V");');
        Add('    jmethodID mAdd = env->GetMethodID(clsSet, "add", "(Ljava/lang/Object;)Z");');
        Add('    jobject ret = env->NewObject(clsSet, mInitSet);');
        if (TTypeConvert.KTypeIsBasicType(AType.fieldType)) then begin
          Add(Format('    jclass clsType = env->FindClass("%s");', [AType.fieldFullPackageName.Replace('.', '/', [rfIgnoreCase, rfReplaceAll])]));
          Add(Format('    jmethodID mConstructor = env->GetMethodID(clsType, "<init>", "%s");', [TTypeConvert.KTypeToJObjectConstructorSig(AType.fieldType)]));
        end;
        Add('    for (auto iter = st.begin(); iter != st.end(); iter++) {');
        if (TTypeConvert.KTypeToCType(AType.fieldType).Contains('*')) then
          Add('        jobject key = (*iter)->toJObject(env);')
        else if (TTypeConvert.KTypeToCType(AType.fieldType) = 'string') then
          Add('        jstring key = env->NewStringUTF((*iter).data());')
        else begin
          Add(Format('        jobject key = env->NewObject(clsType, mConstructor, (j%s) (*iter));', [TTypeConvert.KTypeToJNIType(AType.fieldType)]));
        end;
        Add('        env->CallBooleanMethod(ret, mAdd, key);');
        Add('    }');
        Add('    return ret;');
      end;
    end;
  end;

end;

class function TCodeCpp.constructorParams(AFieldList: TFieldList;
  methodList: TFieldMap; AMaxArraySize: Integer): string;
var
  r: string = '';
  i: Integer;
begin
  for i := 0 to AFieldList.Count - 1 do begin
    if (AFieldList[i].isArray) then begin
      r += Format('%s_arrayToJArray(env, %s), ', [AFieldList[i].fieldName, AFieldList[i].fieldName]);
      methodList.Add(Format('jobjectArray %s_arrayToJArray(JNIEnv* env, %s (&dest)[%d])', [
        AFieldList[i].fieldName, TTypeConvert.KTypeToCType(AFieldList[i].baseType.fieldType), AMaxArraySize]), AFieldList[i]);
    end else if (AFieldList[i].isList) then begin
      r += Format('%s_listToJArrayList(env, %s), ', [AFieldList[i].fieldName, AFieldList[i].fieldName]);
      methodList.Add(Format('jobject %s_listToJArrayList(JNIEnv *env, list<%s> &lst)', [
        AFieldList[i].fieldName, TTypeConvert.KTypeToCType(AFieldList[i].genericType1.fieldType)]), AFieldList[i]);
    end else if (AFieldList[i].isMap) then begin
      r += Format('%s_mapToJHashMap(env, %s), ', [AFieldList[i].fieldName, AFieldList[i].fieldName]);
      methodList.Add(Format('jobject %s_mapToJHashMap(JNIEnv *env, map<%s, %s> &mp)', [
        AFieldList[i].fieldName, TTypeConvert.KTypeToCType(AFieldList[i].genericType1.fieldType), TTypeConvert.KTypeToCType(AFieldList[i].genericType2.fieldType)]), AFieldList[i]);
    end else if (AFieldList[i].isSet) then begin
      r += Format('%s_setToJHashSet(env, %s), ', [AFieldList[i].fieldName, AFieldList[i].fieldName]);
      methodList.Add(Format('jobject %s_setToJHashSet(JNIEnv *env, set<%s> &st)', [
        AFieldList[i].fieldName, TTypeConvert.KTypeToCType(AFieldList[i].genericType1.fieldType)]), AFieldList[i]);
    end else begin
      case AFieldList[i].baseType.fieldCategory of
      ftcSimple: r += AFieldList[i].fieldName + ', ';
      ftcString: r += Format('env->NewStringUTF(%s.data()), ', [AFieldList[i].fieldName]);
      ftcObject: r += Format('%s->toJObject(env), ', [AFieldList[i].fieldName]);
      end;
    end;
  end;
  r := r.Trim.Trim([',']);
  Exit(r);
end;

class procedure TCodeCpp.code(AOutPath: string; AClassInfo: TClassInfo;
  AMaxArraySize: Integer);
var
  codeList: TStringList;
  i: Integer;
  callType: string;
  addMethodList: TFieldMap;
begin
  addMethodList:= TFieldMap.Create;
  codeList := TStringList.Create;
  with codeList do begin
    Add(Format('#include "%s.h"', [AClassInfo.ktClassName]));
    Add('');
    // generate cpp
    Add(Format('%s* %s::fromJObject(JNIEnv *env, jobject obj) {', [AClassInfo.ktClassName, AClassInfo.ktClassName]));
    Add(Format('    %s *ret = NULL;', [AClassInfo.ktClassName]));
    Add('    if (env && obj) {');
    Add(Format('        ret = new %s();', [AClassInfo.ktClassName]));
    Add(Format('        jclass cls = env->FindClass("%s");', [AClassInfo.fullPackageName.Replace('.', '/', [rfIgnoreCase, rfReplaceAll])]));
    for i := 0 to AClassInfo.fieldList.Count - 1 do begin
      if (i = 0) then begin
        Add(Format('        jmethodID m = env->GetMethodID(cls, "%s", "()%s");', [
          TTypeConvert.KFieldToGetName(AClassInfo.fieldList[i].fieldName),
          AClassInfo.fieldList[i].baseType.fieldSignature]));
      end else begin
        Add(Format('        m = env->GetMethodID(cls, "%s", "()%s");', [
          TTypeConvert.KFieldToGetName(AClassInfo.fieldList[i].fieldName),
          AClassInfo.fieldList[i].baseType.fieldSignature]));
        WriteLn('AClassInfo:' + AClassInfo.fieldList[i].baseType.fieldSignature);
      end;
      callType:= TTypeConvert.KTypeToCallMethod(AClassInfo.fieldList[i].baseType.fieldType);
      if (callType.Contains('Object')) then begin
        if (AClassInfo.fieldList[i].isArray) then begin
          Add(Format('        %s_jarrayToObjectArray(env, env->CallObjectMethod(obj, m), ret->%s);', [
            AClassInfo.fieldList[i].fieldName, AClassInfo.fieldList[i].fieldName]));
          addMethodList.Add(Format('void %s_jarrayToObjectArray(JNIEnv *env, jobject arr, %s (&dest)[%d])', [
            AClassInfo.fieldList[i].fieldName, TTypeConvert.KTypeToCType(AClassInfo.fieldList[i].baseType.fieldType), AMaxArraySize]), AClassInfo.fieldList[i]);
        end else if (AClassInfo.fieldList[i].isList) then begin
          Add(Format('        %s_jArrayListToList(env, env->CallObjectMethod(obj, m), ret->%s);', [
            AClassInfo.fieldList[i].fieldName, AClassInfo.fieldList[i].fieldName]));
          addMethodList.Add(Format('void %s_jArrayListToList(JNIEnv *env, jobject obj, list<%s> &lst)', [
            AClassInfo.fieldList[i].fieldName, TTypeConvert.KTypeToCType(AClassInfo.fieldList[i].genericType1.fieldType)]), AClassInfo.fieldList[i]);
        end else if (AClassInfo.fieldList[i].isMap) then begin
          Add(Format('        %s_jHashMapToMap(env, env->CallObjectMethod(obj, m), ret->%s);', [
            AClassInfo.fieldList[i].fieldName, AClassInfo.fieldList[i].fieldName]));
          addMethodList.Add(Format('void %s_jHashMapToMap(JNIEnv *env, jobject obj, map<%s, %s> &mp)', [
            AClassInfo.fieldList[i].fieldName, TTypeConvert.KTypeToCType(AClassInfo.fieldList[i].genericType1.fieldType), TTypeConvert.KTypeToCType(AClassInfo.fieldList[i].genericType2.fieldType)]),
            AClassInfo.fieldList[i]);
        end else if (AClassInfo.fieldList[i].isSet) then begin
          Add(Format('        %s_jHashSetToSet(env, env->CallObjectMethod(obj, m), ret->%s);', [
            AClassInfo.fieldList[i].fieldName, AClassInfo.fieldList[i].fieldName]));
          addMethodList.Add(Format('void %s_jHashSetToSet(JNIEnv *env, jobject obj, set<%s> &st)', [
            AClassInfo.fieldList[i].fieldName, TTypeConvert.KTypeToCType(AClassInfo.fieldList[i].genericType1.fieldType)]), AClassInfo.fieldList[i]);
        end else begin
          if (TTypeConvert.KTypeToCType(AClassInfo.fieldList[i].baseType.fieldType) = 'string') then begin
            Add(Format('        ret->%s = string(env->GetStringUTFChars((jstring)env->CallObjectMethod(obj, m), NULL));', [AClassInfo.fieldList[i].fieldName]));
          end else begin
            Add(Format('        ret->%s = %s::fromJObject(env, env->CallObjectMethod(obj, m));', [AClassInfo.fieldList[i].fieldName, AClassInfo.fieldList[i].baseType.fieldType]));
          end;
        end;
      end else begin
        if (AClassInfo.fieldList[i].isArray) then begin
          Add(Format('        %s_jarrayToArray(env, env->CallObjectMethod(obj, m), ret->%s);', [
            AClassInfo.fieldList[i].fieldName, AClassInfo.fieldList[i].fieldName]));
          addMethodList.Add(Format('void %s_jarrayToArray(JNIEnv *env, jobject arr, %s (&dest)[%d])', [
            AClassInfo.fieldList[i].fieldName, TTypeConvert.KTypeToCType(AClassInfo.fieldList[i].baseType.fieldType), AMaxArraySize]), AClassInfo.fieldList[i]);
        end else begin
          Add(Format('        ret->%s = env->%s(obj, m);', [AClassInfo.fieldList[i].fieldName, callType]));
        end;
      end;

    end;
    Add('    }');
    Add('    return ret;');
    Add('}');
    Add('');
    Add(Format('jobject %s::toJObject(JNIEnv *env) {', [AClassInfo.ktClassName]));
    // to object
    Add('    jobject ret = NULL;');
    Add('    if (env) {');
    Add(Format('        jclass cls = env->FindClass("%s");', [AClassInfo.fullPackageName.Replace('.', '/', [rfIgnoreCase, rfReplaceAll])]));
    Add(Format('        jmethodID m = env->GetMethodID(cls, "<init>", "%s");', [AClassInfo.constructorSig]));
    Add(Format('        ret = env->NewObject(cls, m, %s);', [constructorParams(AClassInfo.fieldList, addMethodList, AMaxArraySize)]));
    Add('    }');
    Add('    return ret;');
    Add('}');
    Add('');
    addMethod(codeList, AClassInfo.ktClassName, addMethodList, AMaxArraySize);
    SaveToFile(AOutPath + AClassInfo.ktClassName + '.cpp');
    Free;
  end;

  // generate head code
  codeList := TStringList.Create;
  with codeList do begin
    Add(Format('#ifndef %s_H', [AClassInfo.ktClassName.ToUpper]));
    Add(Format('#define %s_H', [AClassInfo.ktClassName.ToUpper]));
    Add('');
    head(codeList, AClassInfo);
    Add('');
    Add(Format('class %s {', [AClassInfo.ktClassName]));
    Add('public:');
    for i := 0 to AClassInfo.fieldList.Count - 1 do begin
      if (AClassInfo.fieldList[i].isArray) then begin
        Add(Format('    %s %s[%d];', [TTypeConvert.KTypeToCType(AClassInfo.fieldList[i].baseType.fieldType), AClassInfo.fieldList[i].fieldName, AMaxArraySize]));
      end else if (AClassInfo.fieldList[i].isList) then begin
        Add(Format('    list<%s> %s;', [TTypeConvert.KTypeToCType(AClassInfo.fieldList[i].genericType1.fieldType), AClassInfo.fieldList[i].fieldName]));
      end else if (AClassInfo.fieldList[i].isMap) then begin
        Add(Format('    map<%s, %s> %s;', [TTypeConvert.KTypeToCType(AClassInfo.fieldList[i].genericType1.fieldType), TTypeConvert.KTypeToCType(AClassInfo.fieldList[i].genericType2.fieldType), AClassInfo.fieldList[i].fieldName]));
      end else if (AClassInfo.fieldList[i].isSet) then begin
        Add(Format('    set<%s> %s;', [TTypeConvert.KTypeToCType(AClassInfo.fieldList[i].genericType1.fieldType), AClassInfo.fieldList[i].fieldName]));
      end else begin
        Add(Format('    %s %s;', [TTypeConvert.KTypeToCType(AClassInfo.fieldList[i].baseType.fieldType), AClassInfo.fieldList[i].fieldName]));
      end;
    end;
    Add('    static ' + AClassInfo.ktClassName + '* fromJObject(JNIEnv *env, jobject obj);');
    Add('    jobject toJObject(JNIEnv *env);');
    for i := 0 to addMethodList.Count - 1 do Add(Format('    static %s;', [addMethodList.Keys[i]]));
    Add('};');
    Add('');
    Add(Format('#endif // %s_H', [AClassInfo.ktClassName.ToUpper]));
    SaveToFile(AOutPath + AClassInfo.ktClassName + '.h');
    Free;
  end;
  addMethodList.Free;
end;

class procedure TCodeCpp.generate(AOutPath: string; AKtFile: string;
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

