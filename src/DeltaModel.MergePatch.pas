{
  DeltaModel - RFC 7396 JSON Merge Patch Engine & Dirty Field Tracking

  Implements IETF RFC 7396 ("JSON Merge Patch", application/merge-patch+json):
  - Normative Section 2 MergePatch(Target, Patch) algorithm for arbitrary JSON
    documents (recursive object merge, null-to-delete, atomic array replacement,
    non-object target/patch semantics).
  - Model-level ApplyMergePatch(AModel, APatchJson) with Dirty Field tracking
    for both TDeltaField (Style A) and Fluent Class Schema primitive properties
    (Style B).
  - Protection against primary key (PK), virtual, non-updatable and hidden
    field overwrites.
  - Direct integration with TDMSQLBuilder.CreatePartialUpdate to emit minimal
    UPDATE statements containing exclusively dirty columns.
}

unit DeltaModel.MergePatch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Variants, fpjson, jsonparser,
  DeltaModel, DeltaModel.Fields, DeltaModel.Metadata, DeltaModel.List,
  DeltaModel.ORM.Types, DeltaModel.SQLBuilder;

type
  EDeltaMergePatchException = class(Exception);

  TMergePatchOption = (
    mpoRejectPKOverwrite,     // Raises EDeltaMergePatchException if patch attempts to modify a PK field
    mpoRejectUnknownFields,   // Raises EDeltaMergePatchException if patch contains unknown keys
    mpoClearDirtyBeforeApply  // Clears existing dirty flags before applying the patch
  );
  TMergePatchOptions = set of TMergePatchOption;

  { TDeltaMergePatch - High-level facade for RFC 7396 JSON Merge Patch }
  TDeltaMergePatch = class
  public
    class function Merge(ATarget, APatch: TJSONData): TJSONData; static;
    class function MergeJson(const ATargetJson, APatchJson: RawByteString): RawByteString; static;
    class function Apply(AModel: TDeltaModel; APatchJson: TJSONObject;
      AOptions: TMergePatchOptions = []): Integer; static; overload;
    class function Apply(AModel: TDeltaModel; const APatchJsonStr: RawByteString;
      AOptions: TMergePatchOptions = []): Integer; static; overload;
    class function BuildPartialUpdateSQL(AModel: TDeltaModel;
      ADialect: TDatabaseDialect; const AWhereClause: string = '';
      AUseNamedParams: Boolean = True): string; static;
  end;

{ RFC 7396 Section 2 normative algorithm on arbitrary JSON values.
  Always returns a newly allocated TJSONData owned by the caller;
  ATarget and APatch are never mutated or freed. }
function MergePatch(ATarget, APatch: TJSONData): TJSONData; overload;
function MergePatch(const ATargetJson, APatchJson: RawByteString): RawByteString; overload;

{ Applies an RFC 7396 Merge Patch object to a TDeltaModel instance,
  updating only present keys and marking modified fields as Dirty.
  Returns the number of fields modified by the patch. }
function ApplyMergePatch(AModel: TDeltaModel; APatchJson: TJSONObject;
  AOptions: TMergePatchOptions = []): Integer; overload;
function ApplyMergePatch(AModel: TDeltaModel; const APatchJsonStr: RawByteString;
  AOptions: TMergePatchOptions = []): Integer; overload;

implementation

uses
  DeltaSerialization;

function CreateModelInstance(AClass: TClass): TObject;
begin
  if AClass = nil then
    Exit(nil);
  if AClass.InheritsFrom(TDeltaModel) then
    Result := TDeltaModelClass(AClass).Create
  else
    Result := AClass.Create;
end;

function FindFieldMeta(AMeta: TModelMetadata; const AKey: string): PDeltaFieldMeta;
var
  I: Integer;
begin
  Result := nil;
  if AMeta = nil then Exit;
  for I := 0 to AMeta.FieldCount - 1 do
  begin
    if SameText(AMeta.Fields[I].PropName, AKey) or
       SameText(AMeta.Fields[I].FieldName, AKey) then
      Exit(@AMeta.Fields[I]);
  end;
end;

function MergePatch(ATarget, APatch: TJSONData): TJSONData;
var
  PatchObj, TargetObj, ResultObj: TJSONObject;
  I, ExistingIdx: Integer;
  KeyName: TJSONStringType;
  PatchVal, TargetVal, MergedVal: TJSONData;
begin
  // RFC 7396 Section 2:
  // if Patch is an Object:
  if (APatch <> nil) and (APatch.JSONType = jtObject) then
  begin
    PatchObj := TJSONObject(APatch);

    // if Target is not an Object: Target = {}
    if (ATarget <> nil) and (ATarget.JSONType = jtObject) then
    begin
      TargetObj := TJSONObject(ATarget);
      ResultObj := TJSONObject(TargetObj.Clone);
    end
    else
      ResultObj := TJSONObject.Create;

    try
      // for each Name/Value pair in Patch:
      for I := 0 to PatchObj.Count - 1 do
      begin
        KeyName := PatchObj.Names[I];
        PatchVal := PatchObj.Items[I];

        if (PatchVal = nil) or (PatchVal.JSONType = jtNull) then
        begin
          // if Name exists in Target: remove the Name/Value pair from Target
          ExistingIdx := ResultObj.IndexOfName(KeyName);
          if ExistingIdx >= 0 then
            ResultObj.Delete(ExistingIdx);
        end
        else
        begin
          // Target[Name] = MergePatch(Target[Name], Value)
          ExistingIdx := ResultObj.IndexOfName(KeyName);
          if ExistingIdx >= 0 then
            TargetVal := ResultObj.Items[ExistingIdx]
          else
            TargetVal := nil;

          MergedVal := MergePatch(TargetVal, PatchVal);
          if ExistingIdx >= 0 then
            ResultObj.Items[ExistingIdx] := MergedVal
          else
            ResultObj.Add(KeyName, MergedVal);
        end;
      end;

      Result := ResultObj;
    except
      ResultObj.Free;
      raise;
    end;
  end
  else
  begin
    // else: return Patch
    if APatch = nil then
      Result := TJSONNull.Create
    else
      Result := APatch.Clone;
  end;
end;

function MergePatch(const ATargetJson, APatchJson: RawByteString): RawByteString;
var
  TargetData, PatchData, MergedData: TJSONData;
begin
  TargetData := nil;
  PatchData := nil;
  MergedData := nil;
  try
    if Trim(ATargetJson) <> '' then
      TargetData := GetJSON(ATargetJson, False)
    else
      TargetData := TJSONObject.Create;

    if Trim(APatchJson) <> '' then
      PatchData := GetJSON(APatchJson, False)
    else
      PatchData := TJSONObject.Create;

    MergedData := MergePatch(TargetData, PatchData);
    Result := MergedData.AsJSON;
  finally
    MergedData.Free;
    PatchData.Free;
    TargetData.Free;
  end;
end;

function ApplyDeltaFieldPatch(AModel: TDeltaModel; AFieldMeta: PDeltaFieldMeta;
  ADeltaField: TDeltaField; APatchVal: TJSONData; AOptions: TMergePatchOptions): Boolean;
var
  K: Integer;
  ChildObj: TObject;
  NestedModel: TDeltaModel;
  NestedCount: Integer;
begin
  Result := False;
  if not ADeltaField.Visible then
    Exit(False);

  // Nested HasOne relation
  if ADeltaField is TDFHasOne then
  begin
    if APatchVal.JSONType = jtNull then
    begin
      TDFHasOne(ADeltaField).Clear;
      ADeltaField.Dirty := True;
      AModel.MarkDirty(AFieldMeta^.PropIndex);
      Exit(True);
    end
    else if APatchVal is TJSONObject then
    begin
      if TDFHasOne(ADeltaField).Model = nil then
      begin
        if TDFHasOne(ADeltaField).RelationClass <> nil then
          TDFHasOne(ADeltaField).Model := CreateModelInstance(TDFHasOne(ADeltaField).RelationClass);
      end;
      if Assigned(TDFHasOne(ADeltaField).Model) then
      begin
        if TDFHasOne(ADeltaField).Model is TDeltaModel then
        begin
          NestedModel := TDeltaModel(TDFHasOne(ADeltaField).Model);
          NestedCount := ApplyMergePatch(NestedModel, TJSONObject(APatchVal), AOptions);
          if NestedCount > 0 then
          begin
            ADeltaField.Dirty := True;
            AModel.MarkDirty(AFieldMeta^.PropIndex);
            Exit(True);
          end;
        end
        else
        begin
          DeserializeObj(TDFHasOne(ADeltaField).Model, TJSONObject(APatchVal));
          ADeltaField.Dirty := True;
          AModel.MarkDirty(AFieldMeta^.PropIndex);
          Exit(True);
        end;
      end;
    end;
    Exit(False);
  end;

  // Nested HasMany relation (RFC 7396: arrays are replaced atomically)
  if ADeltaField is TDFHasMany then
  begin
    TDFHasMany(ADeltaField).Clear;
    if (APatchVal is TJSONArray) and (TDFHasMany(ADeltaField).RelationClass <> nil) then
    begin
      for K := 0 to TJSONArray(APatchVal).Count - 1 do
      begin
        ChildObj := CreateModelInstance(TDFHasMany(ADeltaField).RelationClass);
        if TJSONArray(APatchVal)[K] is TJSONObject then
          DeserializeObj(ChildObj, TJSONObject(TJSONArray(APatchVal)[K]));
        TDFHasMany(ADeltaField).Add(ChildObj);
      end;
    end;
    ADeltaField.Dirty := True;
    AModel.MarkDirty(AFieldMeta^.PropIndex);
    Exit(True);
  end;

  // Date / Time / DateTime fields
  if (ADeltaField is TDFDateRequired) or (ADeltaField is TDFDateNull) or
     (ADeltaField is TDFTimeRequired) or (ADeltaField is TDFTimeNull) or
     (ADeltaField is TDFDateTimeRequired) or (ADeltaField is TDFDateTimeNull) then
  begin
    if APatchVal.JSONType = jtNull then
      ADeltaField.Clear
    else
      DateTimeToField(ADeltaField, APatchVal.AsString);
    ADeltaField.Dirty := True;
    AModel.MarkDirty(AFieldMeta^.PropIndex);
    Exit(True);
  end;

  // Scalar TDeltaField types
  case APatchVal.JSONType of
    jtNull:
      ADeltaField.Clear;
    jtNumber:
      begin
        if (ADeltaField is TDFIntNull) or (ADeltaField is TDFIntRequired) or
           (ADeltaField is TDFInt64Null) or (ADeltaField is TDFInt64Required) then
          ADeltaField.Value := APatchVal.AsInt64
        else
          ADeltaField.Value := APatchVal.AsFloat;
      end;
    jtString:
      ADeltaField.Value := APatchVal.AsString;
    jtBoolean:
      ADeltaField.Value := APatchVal.AsBoolean;
  else
    raise EDeltaMergePatchException.CreateFmt(
      'Incompatible JSON value type for field "%s.%s".',
      [AModel.ClassName, AFieldMeta^.PropName]
    );
  end;

  ADeltaField.Dirty := True;
  AModel.MarkDirty(AFieldMeta^.PropIndex);
  Result := True;
end;

function ApplyPrimitivePropPatch(AModel: TDeltaModel; APropInfo: PPropInfo;
  APropIndex: Integer; APatchVal: TJSONData; AOptions: TMergePatchOptions): Boolean;
var
  PropType: PTypeInfo;
  PropObj: TObject;
begin
  Result := False;
  if (APropInfo = nil) or (APropInfo^.SetProc = nil) then
    Exit(False);

  PropType := APropInfo^.PropType;

  if (APatchVal.JSONType = jtNull) and (PropType^.Kind <> tkClass) then
  begin
    AModel.SetNull(APropIndex);
    AModel.MarkDirty(APropIndex);
    Exit(True);
  end;

  if PropType^.Kind <> tkClass then
    AModel.ClearNull(APropIndex);

  case PropType^.Kind of
    tkString, tkWString, tkLString, tkAString, tkChar, tkWChar, tkUnicodeString:
      SetStrProp(AModel, APropInfo, APatchVal.AsString);

    tkInteger, tkInt64:
      SetOrdProp(AModel, APropInfo, APatchVal.AsInt64);

    tkBool:
      SetOrdProp(AModel, APropInfo, Ord(APatchVal.AsBoolean));

    tkEnumeration:
      begin
        if APatchVal.JSONType = jtString then
          SetOrdProp(AModel, APropInfo, GetEnumValue(PropType, APatchVal.AsString))
        else
          SetOrdProp(AModel, APropInfo, APatchVal.AsInteger);
      end;

    tkFloat:
      SetFloatProp(AModel, APropInfo, APatchVal.AsFloat);

    tkVariant:
      SetVariantProp(AModel, APropInfo, APatchVal.Value);

    tkClass:
      begin
        PropObj := GetObjectProp(AModel, APropInfo);
        if APatchVal.JSONType = jtNull then
        begin
          if PropObj is TCustomDeltaModelList then
            TCustomDeltaModelList(PropObj).ClearList
          else
            AModel.SetNull(APropIndex);
        end
        else if PropObj is TCustomDeltaModelList then
          TCustomDeltaModelList(PropObj).FromJson(APatchVal.AsJSON)
        else if (PropObj is TDeltaModel) and (APatchVal is TJSONObject) then
          ApplyMergePatch(TDeltaModel(PropObj), TJSONObject(APatchVal), AOptions)
        else if (PropObj <> nil) and (APatchVal is TJSONObject) then
          DeserializeObj(PropObj, TJSONObject(APatchVal))
        else
          Exit(False);
      end;
  else
    Exit(False);
  end;

  AModel.MarkDirty(APropIndex);
  Result := True;
end;

function ApplyMergePatch(AModel: TDeltaModel; APatchJson: TJSONObject;
  AOptions: TMergePatchOptions): Integer;
var
  Meta: TModelMetadata;
  ClassInfo: TRTTIClassInfo;
  I: Integer;
  KeyName: TJSONStringType;
  PatchVal: TJSONData;
  FieldMeta: PDeltaFieldMeta;
  PropItem: PDeltaPropItem;
  PropObj: TObject;
  IsPK: Boolean;
begin
  Result := 0;
  if (AModel = nil) or (APatchJson = nil) then
    Exit(0);

  if mpoClearDirtyBeforeApply in AOptions then
    AModel.ClearAllDirty;

  if APatchJson.Count = 0 then
    Exit(0);

  Meta := TRTTICache.GetModelMetadata(AModel.ClassType);
  ClassInfo := TRTTICache.GetClassProps(AModel.ClassType);

  for I := 0 to APatchJson.Count - 1 do
  begin
    KeyName := APatchJson.Names[I];
    PatchVal := APatchJson.Items[I];

    FieldMeta := FindFieldMeta(Meta, KeyName);
    if FieldMeta <> nil then
    begin
      IsPK := FieldMeta^.IsPrimaryKey or (dboPrimaryKey in FieldMeta^.DBOptions);
      if IsPK then
      begin
        if mpoRejectPKOverwrite in AOptions then
          raise EDeltaMergePatchException.CreateFmt(
            'Primary key field "%s.%s" cannot be modified by Merge Patch.',
            [AModel.ClassName, FieldMeta^.PropName]
          );
        Continue;
      end;

      if FieldMeta^.IsVirtual then
        Continue;

      if not (dboUpdate in FieldMeta^.DBOptions) then
        Continue;

      if FieldMeta^.IsDeltaField then
      begin
        PropObj := GetObjectProp(AModel, FieldMeta^.PropInfo);
        if (PropObj <> nil) and (PropObj is TDeltaField) then
        begin
          if ApplyDeltaFieldPatch(AModel, FieldMeta, TDeltaField(PropObj), PatchVal, AOptions) then
            Inc(Result);
        end;
      end
      else
      begin
        if ApplyPrimitivePropPatch(AModel, FieldMeta^.PropInfo, FieldMeta^.PropIndex, PatchVal, AOptions) then
          Inc(Result);
      end;
    end
    else
    begin
      // Fallback for published properties not registered in TModelMetadata
      PropItem := ClassInfo.FindProp(KeyName);
      if (PropItem <> nil) and PropItem^.HasSetProc then
      begin
        if SameText(PropItem^.Name, Meta.PKPropName) or SameText(PropItem^.Name, Meta.PKFieldName) then
        begin
          if mpoRejectPKOverwrite in AOptions then
            raise EDeltaMergePatchException.CreateFmt(
              'Primary key field "%s.%s" cannot be modified by Merge Patch.',
              [AModel.ClassName, PropItem^.Name]
            );
          Continue;
        end;

        if ApplyPrimitivePropPatch(AModel, PropItem^.PropInfo, PropItem^.Index, PatchVal, AOptions) then
          Inc(Result);
      end
      else if mpoRejectUnknownFields in AOptions then
        raise EDeltaMergePatchException.CreateFmt(
          'Unknown field "%s" in Merge Patch for "%s".',
          [KeyName, AModel.ClassName]
        );
    end;
  end;
end;

function ApplyMergePatch(AModel: TDeltaModel; const APatchJsonStr: RawByteString;
  AOptions: TMergePatchOptions): Integer;
var
  RawData: TJSONData;
begin
  Result := 0;
  if (AModel = nil) or (Trim(APatchJsonStr) = '') then
    Exit(0);

  RawData := GetJSON(APatchJsonStr, False);
  try
    if not (RawData is TJSONObject) then
      raise EDeltaMergePatchException.Create('RFC 7396 model Merge Patch payload must be a JSON object.');
    Result := ApplyMergePatch(AModel, TJSONObject(RawData), AOptions);
  finally
    RawData.Free;
  end;
end;

{ TDeltaMergePatch }

class function TDeltaMergePatch.Merge(ATarget, APatch: TJSONData): TJSONData;
begin
  Result := MergePatch(ATarget, APatch);
end;

class function TDeltaMergePatch.MergeJson(const ATargetJson,
  APatchJson: RawByteString): RawByteString;
begin
  Result := MergePatch(ATargetJson, APatchJson);
end;

class function TDeltaMergePatch.Apply(AModel: TDeltaModel;
  APatchJson: TJSONObject; AOptions: TMergePatchOptions): Integer;
begin
  Result := ApplyMergePatch(AModel, APatchJson, AOptions);
end;

class function TDeltaMergePatch.Apply(AModel: TDeltaModel;
  const APatchJsonStr: RawByteString; AOptions: TMergePatchOptions): Integer;
begin
  Result := ApplyMergePatch(AModel, APatchJsonStr, AOptions);
end;

class function TDeltaMergePatch.BuildPartialUpdateSQL(AModel: TDeltaModel;
  ADialect: TDatabaseDialect; const AWhereClause: string;
  AUseNamedParams: Boolean): string;
begin
  Result := TDMSQLBuilder.CreatePartialUpdate(AModel, ADialect, AWhereClause, AUseNamedParams);
end;

end.
