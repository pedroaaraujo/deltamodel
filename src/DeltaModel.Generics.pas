unit DeltaModel.Generics;

{$IFDEF FPC}
  {$mode Delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB, Variants, TypInfo,
  DeltaModel, DeltaModel.Fields, DeltaModel.Metadata, DeltaModel.JsonStreamer,
  DeltaModel.ORM.Interfaces, DeltaModel.ORM.DML;

type
  { High-performance Typed ORM Query with Generics, Eager Loading and Streaming }
  TDeltaQuery<T: TDeltaModel> = class
  private
    FQuery: TQuery;
    FEngine: IDeltaORMEngine;
    FIncludes: TStringList;
    class function GetModelPropertyValue(AModel: TObject; const APropName: string): Variant; static;
  public
    constructor Create(AEngine: IDeltaORMEngine);
    destructor Destroy; override;

    function Include(const ARelationName: string): TDeltaQuery<T>;
    function Where(const ACondition: string): TDeltaQuery<T>;
    function AndWhere(const ACondition: string): TDeltaQuery<T>; overload;
    function AndWhere(const AField: string; const AValue: Variant): TDeltaQuery<T>; overload;
    function OrWhere(const ACondition: string): TDeltaQuery<T>; overload;
    function OrWhere(const AField: string; const AValue: Variant): TDeltaQuery<T>; overload;
    function OrderBy(const AOrderBy: string): TDeltaQuery<T>;
    function Limit(ALimit: Integer): TDeltaQuery<T>;
    function Offset(AOffset: Integer): TDeltaQuery<T>;
    function Page(APageNumber, APageSize: Integer): TDeltaQuery<T>;
    function Select(const AColumns: string): TDeltaQuery<T>;
    function Param(const AName: string; const AValue: Variant): TDeltaQuery<T>;
    function Clear: TDeltaQuery<T>;

    function First: T;
    function FindById(const AId: Variant): T;
    function All(ALimit: Integer = -1; AOffset: Integer = -1): TGDeltaModelList<T>;
    function Count: Int64;
    function Exists: Boolean;
    function Delete: Boolean;
    function AsJsonString: RawByteString;
    function StreamJsonTo(AStream: TStream; ABatchSize: Integer = 500): Int64;

    function Save(AModel: T): Boolean;
    function Insert(AModel: T): Boolean;
    function Update(AModel: T): Boolean;
    function BulkInsert(AList: TGDeltaModelList<T>; ABatchSize: Integer = 500): Integer;

    property RawQuery: TQuery read FQuery;
    property Includes: TStringList read FIncludes;
  end;

implementation

{ TDeltaQuery<T> }

class function TDeltaQuery<T>.GetModelPropertyValue(AModel: TObject; const APropName: string): Variant;
var
  PropInfo: PPropInfo;
  PropObj: TObject;
begin
  Result := Null;
  if AModel = nil then Exit;
  PropInfo := GetPropInfo(AModel.ClassType, APropName);
  if PropInfo = nil then Exit;

  if PropInfo^.PropType^.Kind = tkClass then
  begin
    PropObj := GetObjectProp(AModel, PropInfo);
    if PropObj is TDeltaField then
      Result := TDeltaField(PropObj).Value;
  end
  else
    Result := GetPropValue(AModel, APropName);
end;

constructor TDeltaQuery<T>.Create(AEngine: IDeltaORMEngine);
begin
  inherited Create;
  FEngine := AEngine;
  FQuery := TQuery.Create(AEngine);
  FQuery.SetModel(T);
  FIncludes := TStringList.Create;
  FIncludes.Duplicates := dupIgnore;
  FIncludes.Sorted := True;
end;

destructor TDeltaQuery<T>.Destroy;
begin
  FIncludes.Free;
  FQuery.Free;
  inherited Destroy;
end;

function TDeltaQuery<T>.Include(const ARelationName: string): TDeltaQuery<T>;
begin
  if FIncludes.IndexOf(ARelationName) < 0 then
    FIncludes.Add(ARelationName);
  Result := Self;
end;

function TDeltaQuery<T>.Where(const ACondition: string): TDeltaQuery<T>;
begin
  FQuery.Where(ACondition);
  Result := Self;
end;

function TDeltaQuery<T>.AndWhere(const ACondition: string): TDeltaQuery<T>;
begin
  FQuery.AndWhere(ACondition);
  Result := Self;
end;

function TDeltaQuery<T>.AndWhere(const AField: string; const AValue: Variant): TDeltaQuery<T>;
begin
  FQuery.AndWhere(AField, AValue);
  Result := Self;
end;

function TDeltaQuery<T>.OrWhere(const ACondition: string): TDeltaQuery<T>;
begin
  FQuery.OrWhere(ACondition);
  Result := Self;
end;

function TDeltaQuery<T>.OrWhere(const AField: string; const AValue: Variant): TDeltaQuery<T>;
begin
  FQuery.OrWhere(AField, AValue);
  Result := Self;
end;

function TDeltaQuery<T>.OrderBy(const AOrderBy: string): TDeltaQuery<T>;
begin
  FQuery.OrderBy(AOrderBy);
  Result := Self;
end;

function TDeltaQuery<T>.Limit(ALimit: Integer): TDeltaQuery<T>;
begin
  FQuery.Limit(ALimit);
  Result := Self;
end;

function TDeltaQuery<T>.Offset(AOffset: Integer): TDeltaQuery<T>;
begin
  FQuery.Offset(AOffset);
  Result := Self;
end;

function TDeltaQuery<T>.Page(APageNumber, APageSize: Integer): TDeltaQuery<T>;
begin
  FQuery.Page(APageNumber, APageSize);
  Result := Self;
end;

function TDeltaQuery<T>.Select(const AColumns: string): TDeltaQuery<T>;
begin
  FQuery.Select(AColumns);
  Result := Self;
end;

function TDeltaQuery<T>.Param(const AName: string; const AValue: Variant): TDeltaQuery<T>;
begin
  FQuery.Param(AName, AValue);
  Result := Self;
end;

function TDeltaQuery<T>.Clear: TDeltaQuery<T>;
begin
  FQuery.Clear;
  FIncludes.Clear;
  Result := Self;
end;

function TDeltaQuery<T>.First: T;
var
  List: TGDeltaModelList<T>;
begin
  if FIncludes.Count > 0 then
  begin
    List := All(1, 0);
    try
      if List.Count > 0 then
      begin
        Result := List.First;
        List.Records.Extract(Result);
      end
      else
        Result := nil;
    finally
      List.Free;
    end;
  end
  else
    Result := T(FQuery.First);
end;

function TDeltaQuery<T>.FindById(const AId: Variant): T;
var
  Meta: TModelMetadata;
  PK: string;
  List: TGDeltaModelList<T>;
begin
  if FIncludes.Count > 0 then
  begin
    Meta := TRTTICache.GetModelMetadata(T);
    PK := Meta.PKFieldName;
    if PK.IsEmpty then PK := 'id';
    FQuery.Clear;
    Where(PK + ' = ' + VarToStr(AId));
    List := All(1, 0);
    try
      if List.Count > 0 then
      begin
        Result := List.First;
        List.Records.Extract(Result);
      end
      else
        Result := nil;
    finally
      List.Free;
    end;
  end
  else
    Result := T(FQuery.FindById(AId));
end;

function TDeltaQuery<T>.All(ALimit: Integer; AOffset: Integer): TGDeltaModelList<T>;
var
  PaginatedList: TDeltaModelPaginatedList;
  I, J, R: Integer;
  ParentModel: T;
  Meta: TModelMetadata;
  Rel: TDeltaModelRelation;
  RelName, FKName, PKProp, IdListStr: string;
  ParentPKVal, ChildFKVal: Variant;
  ParentPropObj, ChildObj: TObject;
  ChildQuery: TQuery;
  ChildList: TDeltaModelPaginatedList;
  ChildMeta: TModelMetadata;
  ChildClass: TDeltaModelClass;
  PropInfo: PPropInfo;
begin
  PaginatedList := FQuery.All(ALimit, AOffset);
  try
    Result := TGDeltaModelList<T>.Create;
    for I := 0 to PaginatedList.Count - 1 do
      Result.Records.Add(T(PaginatedList.Records[I]));
    PaginatedList.Records.FreeObjects := False;
    PaginatedList.Records.Clear;
  finally
    PaginatedList.Free;
  end;

  if (FIncludes.Count > 0) and (Result.Count > 0) then
  begin
    Meta := TRTTICache.GetModelMetadata(T);
    PKProp := Meta.PKPropName;
    if PKProp.IsEmpty then PKProp := 'id';

    for R := 0 to FIncludes.Count - 1 do
    begin
      RelName := FIncludes[R];
      Rel := nil;
      if Meta.Schema <> nil then
        Rel := Meta.Schema.FindRelation(RelName);

      if (Rel <> nil) and (Rel.RelationKind = drkHasMany) and (Rel.RelatedClass <> nil) and
         Rel.RelatedClass.InheritsFrom(TDeltaModel) then
      begin
        ChildClass := TDeltaModelClass(Rel.RelatedClass);
        ChildMeta := TRTTICache.GetModelMetadata(ChildClass);

        FKName := Rel.ForeignKeyField;
        if FKName.IsEmpty then
          FKName := LowerCase(Meta.TableName) + '_id';

        // Coleta os IDs únicos dos pais
        IdListStr := '';
        for I := 0 to Result.Count - 1 do
        begin
          ParentModel := Result[I];
          ParentPKVal := GetModelPropertyValue(ParentModel, PKProp);

          if not VarIsNull(ParentPKVal) and not VarIsEmpty(ParentPKVal) then
          begin
            if not IdListStr.IsEmpty then
              IdListStr := IdListStr + ',';
            if VarIsNumeric(ParentPKVal) then
              IdListStr := IdListStr + VarToStr(ParentPKVal)
            else
              IdListStr := IdListStr + QuotedStr(VarToStr(ParentPKVal));
          end;
        end;

        if not IdListStr.IsEmpty then
        begin
          // Executa query secundária em lote (WHERE fk_field IN (...))
          ChildQuery := TQuery.Create(FEngine);
          try
            ChildQuery.SetModel(ChildClass);
            ChildQuery.Where(FKName + ' IN (' + IdListStr + ')');
            ChildList := ChildQuery.All(-1, -1);
            try
              for J := 0 to ChildList.Count - 1 do
              begin
                ChildObj := ChildList.Records[J];
                ChildFKVal := GetModelPropertyValue(ChildObj, FKName);
                if VarIsNull(ChildFKVal) or VarIsEmpty(ChildFKVal) then
                begin
                  if ChildMeta.FindField(FKName) <> nil then
                    ChildFKVal := GetModelPropertyValue(ChildObj, ChildMeta.FindField(FKName)^.PropName);
                end;

                for I := 0 to Result.Count - 1 do
                begin
                  ParentModel := Result[I];
                  ParentPKVal := GetModelPropertyValue(ParentModel, PKProp);

                  if (not VarIsNull(ParentPKVal)) and (ParentPKVal = ChildFKVal) then
                  begin
                    PropInfo := GetPropInfo(ParentModel.ClassType, RelName);
                    if PropInfo <> nil then
                    begin
                      ParentPropObj := GetObjectProp(ParentModel, PropInfo);
                      if ParentPropObj is TDFHasMany then
                        TDFHasMany(ParentPropObj).Add(ChildObj)
                      else if ParentPropObj is TDeltaModelList then
                        TDeltaModelList(ParentPropObj).Add(TDeltaModel(ChildObj));
                    end;
                    Break;
                  end;
                end;
              end;
              ChildList.Records.FreeObjects := False;
              ChildList.Records.Clear;
            finally
              ChildList.Free;
            end;
          finally
            ChildQuery.Free;
          end;
        end;
      end;
    end;
  end;
end;

function TDeltaQuery<T>.Count: Int64;
begin
  Result := FQuery.Count;
end;

function TDeltaQuery<T>.Exists: Boolean;
begin
  Result := FQuery.Exists;
end;

function TDeltaQuery<T>.Delete: Boolean;
begin
  Result := FQuery.Delete;
end;

function TDeltaQuery<T>.AsJsonString: RawByteString;
begin
  Result := FQuery.AsJsonString;
end;

function TDeltaQuery<T>.StreamJsonTo(AStream: TStream; ABatchSize: Integer): Int64;
var
  CurrentOffset: Integer;
  BatchList: TGDeltaModelList<T>;
  Streamer: TDeltaFastJsonStreamer;
  I: Integer;
  IsFirstRecord: Boolean;
  ChunkBuf: RawByteString;
  Sep: RawByteString;
begin
  Result := 0;
  if AStream = nil then Exit;
  if ABatchSize <= 0 then ABatchSize := 500;

  Streamer := TDeltaFastJsonStreamer.Create;
  try
    ChunkBuf := '[';
    AStream.WriteBuffer(ChunkBuf[1], Length(ChunkBuf));

    CurrentOffset := 0;
    IsFirstRecord := True;

    repeat
      BatchList := All(ABatchSize, CurrentOffset);
      try
        if (BatchList = nil) or (BatchList.Count = 0) then
          Break;

        for I := 0 to BatchList.Count - 1 do
        begin
          if not IsFirstRecord then
          begin
            Sep := ',';
            AStream.WriteBuffer(Sep[1], 1);
          end;
          IsFirstRecord := False;

          Streamer.Clear;
          Streamer.WriteModel(BatchList[I]);
          ChunkBuf := Streamer.AsJSON;
          if Length(ChunkBuf) > 0 then
            AStream.WriteBuffer(ChunkBuf[1], Length(ChunkBuf));

          Inc(Result);
        end;

        Inc(CurrentOffset, BatchList.Count);
        if BatchList.Count < ABatchSize then
          Break;
      finally
        BatchList.Free;
      end;
    until False;

    ChunkBuf := ']';
    AStream.WriteBuffer(ChunkBuf[1], Length(ChunkBuf));
  finally
    Streamer.Free;
  end;
end;

function TDeltaQuery<T>.Save(AModel: T): Boolean;
begin
  Result := TSave.SaveObject(FEngine, AModel);
end;

function TDeltaQuery<T>.Insert(AModel: T): Boolean;
begin
  Result := TInsert.InsertObject(FEngine, AModel);
end;

function TDeltaQuery<T>.Update(AModel: T): Boolean;
begin
  Result := TUpdate.UpdateObject(FEngine, AModel);
end;

function TDeltaQuery<T>.BulkInsert(AList: TGDeltaModelList<T>; ABatchSize: Integer): Integer;
begin
  Result := TInsert.BulkInsertObjects(FEngine, AList, ABatchSize);
end;

end.
