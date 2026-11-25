unit DeltaModel.ORM.Persister;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Variants, TypInfo,
  DeltaModel, DeltaModel.Fields, DeltaModel.Metadata,
  DeltaValidator, DeltaModel.ORM.Types, DeltaModel.ORM.Interfaces,
  DeltaModel.SQLBuilder, DeltaModel.ORM.Hydrator;

type
  { TDeltaPersister: Driver-agnostic persistence operations (Insert/Update/Patch/Delete/Save/BulkInsert) }
  TDeltaPersister = class
  private
    class procedure ValidateModel(AModel: TDeltaModel); static;
    class function AutoIncReturningCols(AModel: TDeltaModel): string; static;
    class procedure AssignReturnedAutoInc(AModel: TDeltaModel;
      const ARS: IDeltaResultSet); static;
    class procedure PopulateAutoIncFromLastId(const AConn: IDeltaConnection;
      AModel: TDeltaModel); static;
  public
    class function Insert(const AConn: IDeltaConnection; AModel: TDeltaModel): Boolean; overload; static;
    class function Insert(const AConn: IDeltaConnection; AModel: TDeltaModel;
      AReturnClass: TDeltaModelClass): TDeltaModel; overload; static;

    class function Update(const AConn: IDeltaConnection; AModel: TDeltaModel): Boolean; overload; static;
    class function Update(const AConn: IDeltaConnection; AModel: TDeltaModel;
      AReturnClass: TDeltaModelClass): TDeltaModel; overload; static;

    class function Patch(const AConn: IDeltaConnection; AModel: TDeltaModel): Boolean; overload; static;
    class function Patch(const AConn: IDeltaConnection; AModel: TDeltaModel;
      AReturnClass: TDeltaModelClass): TDeltaModel; overload; static;

    class function Delete(const AConn: IDeltaConnection; AModel: TDeltaModel): Boolean; static;
    class function Save(const AConn: IDeltaConnection; AModel: TDeltaModel): Boolean; static;
    class function HasPKValue(AModel: TDeltaModel): Boolean; static;

    class function BulkInsert(const AConn: IDeltaConnection;
      AModels: array of TDeltaModel; ABatchSize: Integer = 500): Integer; overload; static;
    class function BulkInsert(const AConn: IDeltaConnection;
      AList: TDeltaModelList; ABatchSize: Integer = 500): Integer; overload; static;
  end;

  { TDelete }
  TDelete = class
  public
    class function Exec(const AConn: IDeltaConnection; AModel: TDeltaModel): Boolean; static;
  end;

  { TUpdate }
  TUpdate = class
  public
    class function UpdateObject(const AConn: IDeltaConnection; AModel: TDeltaModel): Boolean; overload; static;
    class function UpdateObject(const AConn: IDeltaConnection; AModel: TDeltaModel;
      Return: TDeltaModelClass): TDeltaModel; overload; static;
    class function PatchObject(const AConn: IDeltaConnection; AModel: TDeltaModel): Boolean; overload; static;
    class function PatchObject(const AConn: IDeltaConnection; AModel: TDeltaModel;
      Return: TDeltaModelClass): TDeltaModel; overload; static;
  end;

  { TInsert }
  TInsert = class
  public
    class function InsertObject(const AConn: IDeltaConnection; AModel: TDeltaModel): Boolean; overload; static;
    class function InsertObject(const AConn: IDeltaConnection; AModel: TDeltaModel;
      Return: TDeltaModelClass): TDeltaModel; overload; static;
    class function BulkInsertObjects(const AConn: IDeltaConnection;
      AModels: array of TDeltaModel; ABatchSize: Integer = 500): Integer; overload; static;
    class function BulkInsertObjects(const AConn: IDeltaConnection;
      AList: TDeltaModelList; ABatchSize: Integer = 500): Integer; overload; static;
  end;

  { TSave }
  TSave = class
  public
    class function HasPKValue(AModel: TDeltaModel): Boolean; static;
    class function SaveObject(const AConn: IDeltaConnection; AModel: TDeltaModel): Boolean; static;
  end;

implementation

{ TDeltaPersister }

class procedure TDeltaPersister.ValidateModel(AModel: TDeltaModel);
begin
  AModel.Validate;
  with AModel.Validator.Validate do
  begin
    if not OK then
      raise EDeltaValidation.Create(Message);
  end;
end;

class function TDeltaPersister.AutoIncReturningCols(AModel: TDeltaModel): string;
var
  Meta: TModelMetadata;
  I: Integer;
  F: TDeltaField;
begin
  Result := '';
  Meta := TRTTICache.GetModelMetadata(AModel.ClassType);
  if Meta.FieldCount > 0 then
  begin
    for I := 0 to Meta.FieldCount - 1 do
    begin
      if Meta.Fields[I].IsVirtual or (not Meta.Fields[I].IsAutoInc) then
        Continue;
      if not Result.IsEmpty then
        Result := Result + ', ';
      Result := Result + Meta.Fields[I].FieldName;
    end;
    Exit;
  end;

  for I := 0 to AModel.FieldList.Count - 1 do
  begin
    F := AModel.FieldList[I];
    if F.IsVirtual or not (dboAutoInc in F.DBOptions) then
      Continue;
    if not Result.IsEmpty then
      Result := Result + ', ';
    Result := Result + F.FieldName;
  end;
end;

class procedure TDeltaPersister.AssignReturnedAutoInc(AModel: TDeltaModel;
  const ARS: IDeltaResultSet);
var
  Meta: TModelMetadata;
  I, ColIdx: Integer;
  PropObj: TObject;
  F: TDeltaField;
begin
  Meta := TRTTICache.GetModelMetadata(AModel.ClassType);
  if Meta.FieldCount > 0 then
  begin
    for I := 0 to Meta.FieldCount - 1 do
    begin
      if Meta.Fields[I].IsVirtual or (not Meta.Fields[I].IsAutoInc) then
        Continue;
      ColIdx := ARS.FindField(Meta.Fields[I].FieldName);
      if ColIdx < 0 then
        Continue;

      if Meta.Fields[I].IsDeltaField then
      begin
        PropObj := GetObjectProp(AModel, Meta.Fields[I].PropInfo);
        if PropObj is TDeltaField then
          TDeltaField(PropObj).Value := ARS.AsVariant(ColIdx);
      end
      else
      begin
        AModel.ClearNull(Meta.Fields[I].PropIndex);
        SetOrdProp(AModel, Meta.Fields[I].PropInfo, ARS.AsLargeInt(ColIdx));
      end;
    end;
    Exit;
  end;

  for I := 0 to AModel.FieldList.Count - 1 do
  begin
    F := AModel.FieldList[I];
    if not F.IsVirtual and (dboAutoInc in F.DBOptions) then
      F.Value := ARS.AsVariant(F.FieldName);
  end;
end;

class procedure TDeltaPersister.PopulateAutoIncFromLastId(
  const AConn: IDeltaConnection; AModel: TDeltaModel);
var
  LastIdSQL: string;
  LastId: Variant;
  Meta: TModelMetadata;
  I: Integer;
  PropObj: TObject;
  F: TDeltaField;
  CurVal: Int64;
begin
  LastIdSQL := AConn.Dialect.SQL.LastInsertIdSQL;
  if LastIdSQL.IsEmpty then
    Exit;

  Meta := TRTTICache.GetModelMetadata(AModel.ClassType);
  for I := 0 to Meta.FieldCount - 1 do
  begin
    if not Meta.Fields[I].IsAutoInc then
      Continue;

    if Meta.Fields[I].IsDeltaField then
    begin
      PropObj := GetObjectProp(AModel, Meta.Fields[I].PropInfo);
      if PropObj is TDeltaField then
      begin
        F := TDeltaField(PropObj);
        if F.IsNull or (F.AsLargeInt = 0) then
        begin
          LastId := AConn.ExecuteScalar(LastIdSQL);
          if not (VarIsNull(LastId) or VarIsEmpty(LastId)) then
            F.AsLargeInt := LastId;
          Break;
        end;
      end;
    end
    else
    begin
      CurVal := GetOrdProp(AModel, Meta.Fields[I].PropInfo);
      if AModel.IsNull(Meta.Fields[I].PropIndex) or (CurVal = 0) then
      begin
        LastId := AConn.ExecuteScalar(LastIdSQL);
        if not (VarIsNull(LastId) or VarIsEmpty(LastId)) then
        begin
          AModel.ClearNull(Meta.Fields[I].PropIndex);
          SetOrdProp(AModel, Meta.Fields[I].PropInfo, Int64(LastId));
        end;
        Break;
      end;
    end;
  end;
end;

class function TDeltaPersister.Insert(const AConn: IDeltaConnection;
  AModel: TDeltaModel): Boolean;
var
  Stmt: IDeltaStatement;
  RS: IDeltaResultSet;
  ReturningCols: string;
begin
  AModel.BeforeInsert;
  ValidateModel(AModel);

  Stmt := AConn.CreateStatement;
  if AConn.Dialect.SQL.UseReturningForAutoInc then
    ReturningCols := AutoIncReturningCols(AModel)
  else
    ReturningCols := '';

  if not ReturningCols.IsEmpty then
  begin
    Stmt.SQL := TDMSQLBuilder.CreateInsertReturning(
      AModel, AConn.Dialect.SQL, True, ReturningCols);
    TDeltaHydrator.BindModelParams(AModel, Stmt.Params);
    RS := Stmt.ExecuteQuery;
    try
      Result := RS.Next;
      if Result then
      begin
        AssignReturnedAutoInc(AModel, RS);
        AModel.AfterInsert;
      end;
    finally
      RS.Close;
    end;
    Exit;
  end;

  Stmt.SQL := TDMSQLBuilder.CreateInsert(AModel, AConn.Dialect.SQL);
  TDeltaHydrator.BindModelParams(AModel, Stmt.Params);
  Result := Stmt.Execute > 0;
  if Result then
  begin
    PopulateAutoIncFromLastId(AConn, AModel);
    AModel.AfterInsert;
  end;
end;

class function TDeltaPersister.Insert(const AConn: IDeltaConnection;
  AModel: TDeltaModel; AReturnClass: TDeltaModelClass): TDeltaModel;
var
  Stmt: IDeltaStatement;
  RS: IDeltaResultSet;
begin
  Result := nil;
  AModel.BeforeInsert;
  ValidateModel(AModel);

  Stmt := AConn.CreateStatement(
    TDMSQLBuilder.CreateInsertReturning(AModel, AConn.Dialect.SQL));
  TDeltaHydrator.BindModelParams(AModel, Stmt.Params);
  RS := Stmt.ExecuteQuery;
  try
    if not RS.Next then
      Exit(nil);
    Result := AReturnClass.Create;
    TDeltaHydrator.FromResultSet(Result, RS);
    AModel.AfterInsert;
  finally
    RS.Close;
  end;
end;

class function TDeltaPersister.Update(const AConn: IDeltaConnection;
  AModel: TDeltaModel): Boolean;
var
  Stmt: IDeltaStatement;
begin
  AModel.BeforeUpdate;
  ValidateModel(AModel);

  Stmt := AConn.CreateStatement(
    TDMSQLBuilder.CreateUpdate(
      AModel, AConn.Dialect.SQL, TDMSQLBuilder.WhereClausePK(AModel, True)));
  TDeltaHydrator.BindModelParams(AModel, Stmt.Params);
  Result := Stmt.Execute > 0;
  if Result then
    AModel.AfterUpdate;
end;

class function TDeltaPersister.Update(const AConn: IDeltaConnection;
  AModel: TDeltaModel; AReturnClass: TDeltaModelClass): TDeltaModel;
var
  Stmt: IDeltaStatement;
  RS: IDeltaResultSet;
begin
  Result := nil;
  AModel.BeforeUpdate;
  ValidateModel(AModel);

  Stmt := AConn.CreateStatement(
    TDMSQLBuilder.CreateUpdateReturning(
      AModel, AConn.Dialect.SQL, TDMSQLBuilder.WhereClausePK(AModel, True)));
  TDeltaHydrator.BindModelParams(AModel, Stmt.Params);
  RS := Stmt.ExecuteQuery;
  try
    if not RS.Next then
      Exit(nil);
    Result := AReturnClass.Create;
    TDeltaHydrator.FromResultSet(Result, RS);
    AModel.AfterUpdate;
  finally
    RS.Close;
  end;
end;

class function TDeltaPersister.Patch(const AConn: IDeltaConnection;
  AModel: TDeltaModel): Boolean;
var
  SQLText: string;
  Stmt: IDeltaStatement;
begin
  if (AModel = nil) or (not AModel.HasDirtyFields) then
    Exit(False);

  SQLText := TDMSQLBuilder.CreatePartialUpdate(
    AModel, AConn.Dialect.SQL, TDMSQLBuilder.WhereClausePK(AModel, True));
  if SQLText.IsEmpty then
    Exit(False);

  AModel.BeforeUpdate;
  ValidateModel(AModel);

  Stmt := AConn.CreateStatement(SQLText);
  TDeltaHydrator.BindModelParams(AModel, Stmt.Params);
  Result := Stmt.Execute > 0;
  if Result then
  begin
    AModel.ClearAllDirty;
    AModel.AfterUpdate;
  end;
end;

class function TDeltaPersister.Patch(const AConn: IDeltaConnection;
  AModel: TDeltaModel; AReturnClass: TDeltaModelClass): TDeltaModel;
var
  SQLText: string;
  Stmt: IDeltaStatement;
  RS: IDeltaResultSet;
begin
  if (AModel = nil) or (not AModel.HasDirtyFields) then
    Exit(nil);

  SQLText := TDMSQLBuilder.CreatePartialUpdateReturning(
    AModel, AConn.Dialect.SQL, TDMSQLBuilder.WhereClausePK(AModel, True));
  if SQLText.IsEmpty then
    Exit(nil);

  AModel.BeforeUpdate;
  ValidateModel(AModel);

  Stmt := AConn.CreateStatement(SQLText);
  TDeltaHydrator.BindModelParams(AModel, Stmt.Params);
  RS := Stmt.ExecuteQuery;
  try
    if not RS.Next then
      Exit(nil);
    Result := AReturnClass.Create;
    TDeltaHydrator.FromResultSet(Result, RS);
    AModel.ClearAllDirty;
    AModel.AfterUpdate;
  finally
    RS.Close;
  end;
end;

class function TDeltaPersister.Delete(const AConn: IDeltaConnection;
  AModel: TDeltaModel): Boolean;
var
  Stmt: IDeltaStatement;
begin
  AModel.BeforeDelete;
  Stmt := AConn.CreateStatement(
    TDMSQLBuilder.CreateDelete(
      AModel, AConn.Dialect.SQL, TDMSQLBuilder.WhereClausePK(AModel, True)));
  TDeltaHydrator.BindModelParams(AModel, Stmt.Params);
  Result := Stmt.Execute > 0;
  if Result then
    AModel.AfterDelete;
end;

class function TDeltaPersister.HasPKValue(AModel: TDeltaModel): Boolean;
var
  Meta: TModelMetadata;
  NestedObj: TObject;
  Obj: TDeltaField;
  Val: Variant;
begin
  Result := False;
  if AModel = nil then
    Exit;

  Meta := TRTTICache.GetModelMetadata(AModel.ClassType);
  if Meta.PKPropInfo = nil then
    Exit;

  if Meta.PKPropInfo^.PropType^.Kind = tkClass then
  begin
    NestedObj := GetObjectProp(AModel, Meta.PKPropInfo);
    if NestedObj is TDeltaField then
    begin
      Obj := TDeltaField(NestedObj);
      if not Obj.IsNull then
      begin
        if VarIsNumeric(Obj.Value) then
          Result := Double(Obj.Value) > 0
        else if VarIsStr(Obj.Value) then
          Result := not VarToStr(Obj.Value).Trim.IsEmpty
        else
          Result := True;
      end;
    end;
    Exit;
  end;

  Val := GetPropValue(AModel, Meta.PKPropName);
  if VarIsNumeric(Val) then
    Result := Double(Val) > 0
  else if VarIsStr(Val) then
    Result := not VarToStr(Val).Trim.IsEmpty;
end;

class function TDeltaPersister.Save(const AConn: IDeltaConnection;
  AModel: TDeltaModel): Boolean;
begin
  AModel.BeforeSave;
  if HasPKValue(AModel) then
    Result := Update(AConn, AModel)
  else
    Result := Insert(AConn, AModel);
  if Result then
    AModel.AfterSave;
end;

class function TDeltaPersister.BulkInsert(const AConn: IDeltaConnection;
  AModels: array of TDeltaModel; ABatchSize: Integer): Integer;
var
  Stmt: IDeltaStatement;
  I, J, TotalCount, BatchEnd, CurrentBatchSize: Integer;
  BatchModels: array of TDeltaModel;
  OwnsTransaction: Boolean;
  SQLText: string;
begin
  Result := 0;
  TotalCount := Length(AModels);
  if TotalCount = 0 then
    Exit;

  if ABatchSize <= 0 then
    ABatchSize := 500;

  for I := 0 to TotalCount - 1 do
  begin
    AModels[I].BeforeInsert;
    ValidateModel(AModels[I]);
  end;

  OwnsTransaction := not AConn.TransactionActive;
  if OwnsTransaction then
    AConn.StartTransaction;

  try
    I := 0;
    while I < TotalCount do
    begin
      BatchEnd := I + ABatchSize - 1;
      if BatchEnd >= TotalCount then
        BatchEnd := TotalCount - 1;
      CurrentBatchSize := BatchEnd - I + 1;

      SetLength(BatchModels, CurrentBatchSize);
      for J := 0 to CurrentBatchSize - 1 do
        BatchModels[J] := AModels[I + J];

      Stmt := AConn.CreateStatement;
      if AConn.Dialect.SQL.SupportsMultiRowBulkInsert then
      begin
        Stmt.SQL := TDMSQLBuilder.CreateBulkInsert(BatchModels, AConn.Dialect.SQL);
        for J := 0 to CurrentBatchSize - 1 do
          TDeltaHydrator.BindModelParams(BatchModels[J], Stmt.Params, '_' + IntToStr(J));
        Stmt.Execute;
      end
      else
      begin
        for J := 0 to CurrentBatchSize - 1 do
        begin
          SQLText := TDMSQLBuilder.CreateBulkInsertRow(BatchModels[J], AConn.Dialect.SQL);
          if Stmt.SQL <> SQLText then
            Stmt.SQL := SQLText;
          Stmt.ClearParams;
          TDeltaHydrator.BindModelParams(BatchModels[J], Stmt.Params);
          Stmt.Execute;
        end;
      end;

      Result := Result + CurrentBatchSize;
      for J := 0 to CurrentBatchSize - 1 do
        BatchModels[J].AfterInsert;

      I := BatchEnd + 1;
    end;

    if OwnsTransaction then
      AConn.Commit;
  except
    if OwnsTransaction then
    begin
      try
        AConn.Rollback;
      except
      end;
    end;
    raise;
  end;
end;

class function TDeltaPersister.BulkInsert(const AConn: IDeltaConnection;
  AList: TDeltaModelList; ABatchSize: Integer): Integer;
var
  Arr: array of TDeltaModel;
  I: Integer;
begin
  if (AList = nil) or (AList.Records = nil) or (AList.Records.Count = 0) then
    Exit(0);

  SetLength(Arr, AList.Records.Count);
  for I := 0 to AList.Records.Count - 1 do
    Arr[I] := AList.Records[I];

  Result := BulkInsert(AConn, Arr, ABatchSize);
end;

{ Facades }

class function TDelete.Exec(const AConn: IDeltaConnection; AModel: TDeltaModel): Boolean;
begin
  Result := TDeltaPersister.Delete(AConn, AModel);
end;

class function TUpdate.UpdateObject(const AConn: IDeltaConnection; AModel: TDeltaModel): Boolean;
begin
  Result := TDeltaPersister.Update(AConn, AModel);
end;

class function TUpdate.UpdateObject(const AConn: IDeltaConnection; AModel: TDeltaModel;
  Return: TDeltaModelClass): TDeltaModel;
begin
  Result := TDeltaPersister.Update(AConn, AModel, Return);
end;

class function TUpdate.PatchObject(const AConn: IDeltaConnection; AModel: TDeltaModel): Boolean;
begin
  Result := TDeltaPersister.Patch(AConn, AModel);
end;

class function TUpdate.PatchObject(const AConn: IDeltaConnection; AModel: TDeltaModel;
  Return: TDeltaModelClass): TDeltaModel;
begin
  Result := TDeltaPersister.Patch(AConn, AModel, Return);
end;

class function TInsert.InsertObject(const AConn: IDeltaConnection; AModel: TDeltaModel): Boolean;
begin
  Result := TDeltaPersister.Insert(AConn, AModel);
end;

class function TInsert.InsertObject(const AConn: IDeltaConnection; AModel: TDeltaModel;
  Return: TDeltaModelClass): TDeltaModel;
begin
  Result := TDeltaPersister.Insert(AConn, AModel, Return);
end;

class function TInsert.BulkInsertObjects(const AConn: IDeltaConnection;
  AModels: array of TDeltaModel; ABatchSize: Integer): Integer;
begin
  Result := TDeltaPersister.BulkInsert(AConn, AModels, ABatchSize);
end;

class function TInsert.BulkInsertObjects(const AConn: IDeltaConnection;
  AList: TDeltaModelList; ABatchSize: Integer): Integer;
begin
  Result := TDeltaPersister.BulkInsert(AConn, AList, ABatchSize);
end;

class function TSave.HasPKValue(AModel: TDeltaModel): Boolean;
begin
  Result := TDeltaPersister.HasPKValue(AModel);
end;

class function TSave.SaveObject(const AConn: IDeltaConnection; AModel: TDeltaModel): Boolean;
begin
  Result := TDeltaPersister.Save(AConn, AModel);
end;

end.
