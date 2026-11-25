unit DeltaModel.SQLBuilder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Variants, StrUtils,
  DeltaModel, DeltaModel.Fields, DeltaModel.Metadata,
  DeltaModel.ORM.Types, DeltaModel.ORM.Interfaces,
  DeltaModel.ORM.Criteria, DeltaModel.ORM.Registry;

type
  { TDMSQLBuilder: Dialect-agnostic SQL builder delegating syntax to IDeltaSQLDialect }
  TDMSQLBuilder = class
  private
    FCommand: string;
    FModel: TDeltaModel;
    FFields: TStringList;
    FValues: TStringList;
    FWhereConditions: TStringList;
    FOrderBy: string;
    FLimit: Integer;
    FOffset: Integer;
    FDialect: IDeltaSQLDialect;
    FGroupBy: string;
    FUseQuotes: Boolean;
    FIsCount: Boolean;
    FOnlyDirty: Boolean;
    FTableAlias: string;
    FJoins: TStringList;
    FParams: TDeltaParams;

    function FieldAndValuesToSQL: string;
    function GetTableName: string;
    function GetTableNameWithAlias: string;
    function JoinsToSQL: string;
    function ExtractFieldsAndValues(UseNamedParams: Boolean): Boolean;
    function FieldsToSQL: string;
    function ValuesToSQL: string;
    function WhereToSQL: string;
    function QuoteIdentifier(const AIdentifier: string): string;
  public
    constructor Create(AModel: TDeltaModel; const ADialect: IDeltaSQLDialect); overload;
    constructor Create(AModel: TDeltaModel; const ADialect: IDeltaDialect); overload;
    constructor Create(AModel: TDeltaModel; const ADialectName: string); overload;
    destructor Destroy; override;

    function Select: TDMSQLBuilder; overload;
    function Select(const AColumns: string): TDMSQLBuilder; overload;
    function Select(const AColumns: array of string): TDMSQLBuilder; overload;
    function Count: TDMSQLBuilder;
    function Insert(UseNamedParams: Boolean = True): TDMSQLBuilder;
    function Update(UseNamedParams: Boolean = True): TDMSQLBuilder;
    function UpdateDirty(UseNamedParams: Boolean = True): TDMSQLBuilder;
    function Delete: TDMSQLBuilder;
    function TableAlias(const AAlias: string): TDMSQLBuilder;
    function Join(AJoinType: TJoinType; const ATable, AOnCondition: string;
      const AAlias: string = ''): TDMSQLBuilder; overload;
    function Join(const AJoinClause: string): TDMSQLBuilder; overload;
    function InnerJoin(const ATable, AOnCondition: string;
      const AAlias: string = ''): TDMSQLBuilder;
    function LeftJoin(const ATable, AOnCondition: string;
      const AAlias: string = ''): TDMSQLBuilder;
    function RightJoin(const ATable, AOnCondition: string;
      const AAlias: string = ''): TDMSQLBuilder;
    function FullJoin(const ATable, AOnCondition: string;
      const AAlias: string = ''): TDMSQLBuilder;
    function Where(const ACondition: string): TDMSQLBuilder; overload;
    function Where(const AField: string; const AValue: Variant): TDMSQLBuilder; overload;
    function Where(const AField: string; AOp: TComparisonOp;
      const AValue: Variant): TDMSQLBuilder; overload;
    function Where(const AField, AOpStr: string;
      const AValue: Variant): TDMSQLBuilder; overload;
    function AndWhere(const ACondition: string): TDMSQLBuilder; overload;
    function AndWhere(const AField: string; const AValue: Variant): TDMSQLBuilder; overload;
    function AndWhere(const AField: string; AOp: TComparisonOp;
      const AValue: Variant): TDMSQLBuilder; overload;
    function AndWhere(const AField, AOpStr: string;
      const AValue: Variant): TDMSQLBuilder; overload;
    function OrWhere(const ACondition: string): TDMSQLBuilder; overload;
    function OrWhere(const AField: string; const AValue: Variant): TDMSQLBuilder; overload;
    function OrWhere(const AField: string; AOp: TComparisonOp;
      const AValue: Variant): TDMSQLBuilder; overload;
    function OrWhere(const AField, AOpStr: string;
      const AValue: Variant): TDMSQLBuilder; overload;
    function WhereBetween(const AField: string;
      const AVal1, AVal2: Variant): TDMSQLBuilder;
    function WhereIn(const AField: string;
      const AValues: array of Variant): TDMSQLBuilder;
    function WhereNotIn(const AField: string;
      const AValues: array of Variant): TDMSQLBuilder;
    function WhereNull(const AField: string): TDMSQLBuilder;
    function WhereNotNull(const AField: string): TDMSQLBuilder;
    function OrderBy(const AField: string): TDMSQLBuilder;
    function Limit(const ALimit: Integer): TDMSQLBuilder;
    function Offset(const AOffset: Integer): TDMSQLBuilder;
    function Page(const APageNumber, APageSize: Integer): TDMSQLBuilder;
    function SelectFieldsText: string;
    function GroupBy(const AField: string): TDMSQLBuilder;
    function UseQuotes(AValue: Boolean): TDMSQLBuilder;
    function Clear: TDMSQLBuilder;
    function Build: string;
    procedure BindParams(AParams: TDeltaParams);
    property Params: TDeltaParams read FParams;

    class function WhereClausePK(AModel: TDeltaModel): string; overload; static;
    class function WhereClausePK(AModel: TDeltaModel; UseNamedParams: Boolean): string; overload; static;

    class function CreateInsert(AModel: TDeltaModel; const ADialect: IDeltaSQLDialect; UseNamedParams: Boolean = True): string; overload; static;
    class function CreateInsert(AModel: TDeltaModel; const ADialectName: string; UseNamedParams: Boolean = True): string; overload; static;

    class function CreateInsertReturning(AModel: TDeltaModel; const ADialect: IDeltaSQLDialect; UseNamedParams: Boolean = True; const AReturningCols: string = '*'): string; overload; static;
    class function CreateInsertReturning(AModel: TDeltaModel; const ADialectName: string; UseNamedParams: Boolean = True; const AReturningCols: string = '*'): string; overload; static;

    class function CreateUpdate(AModel: TDeltaModel; const ADialect: IDeltaSQLDialect; const WhereClause: string = ''; UseNamedParams: Boolean = True): string; overload; static;
    class function CreateUpdate(AModel: TDeltaModel; const ADialectName: string; const WhereClause: string = ''; UseNamedParams: Boolean = True): string; overload; static;

    class function CreateUpdateReturning(AModel: TDeltaModel; const ADialect: IDeltaSQLDialect; const WhereClause: string = ''; UseNamedParams: Boolean = True): string; overload; static;
    class function CreateUpdateReturning(AModel: TDeltaModel; const ADialectName: string; const WhereClause: string = ''; UseNamedParams: Boolean = True): string; overload; static;

    class function CreatePartialUpdate(AModel: TDeltaModel; const ADialect: IDeltaSQLDialect; const WhereClause: string = ''; UseNamedParams: Boolean = True): string; overload; static;
    class function CreatePartialUpdate(AModel: TDeltaModel; const ADialectName: string; const WhereClause: string = ''; UseNamedParams: Boolean = True): string; overload; static;

    class function CreatePartialUpdateReturning(AModel: TDeltaModel; const ADialect: IDeltaSQLDialect; const WhereClause: string = ''; UseNamedParams: Boolean = True): string; overload; static;
    class function CreatePartialUpdateReturning(AModel: TDeltaModel; const ADialectName: string; const WhereClause: string = ''; UseNamedParams: Boolean = True): string; overload; static;

    class function CreateDelete(AModel: TDeltaModel; const ADialect: IDeltaSQLDialect; const WhereClause: string = ''): string; overload; static;
    class function CreateDelete(AModel: TDeltaModel; const ADialectName: string; const WhereClause: string = ''): string; overload; static;

    class function CreateSelect(AModel: TDeltaModel; const ADialect: IDeltaSQLDialect; const WhereClause: string = ''): string; overload; static;
    class function CreateSelect(AModel: TDeltaModel; const ADialectName: string; const WhereClause: string = ''): string; overload; static;

    class function CreateCount(AModel: TDeltaModel; const ADialect: IDeltaSQLDialect; const WhereClause: string = ''): string; overload; static;
    class function CreateCount(AModel: TDeltaModel; const ADialectName: string; const WhereClause: string = ''): string; overload; static;

    class function CreateBulkInsert(AModels: array of TDeltaModel; const ADialect: IDeltaSQLDialect): string; overload; static;
    class function CreateBulkInsert(AModels: array of TDeltaModel; const ADialectName: string): string; overload; static;
    class function CreateBulkInsertRow(AModel: TDeltaModel; const ADialect: IDeltaSQLDialect): string; static;

    class function ExtractFieldNames(AModel: TDeltaModel): TStringList; static;
    class function ExtractFieldValues(AModel: TDeltaModel; ASuffix: string = ''): TStringList; static;
  end;

implementation

{ TDMSQLBuilder }

constructor TDMSQLBuilder.Create(AModel: TDeltaModel;
  const ADialect: IDeltaSQLDialect);
begin
  inherited Create;
  FModel := AModel;
  if ADialect <> nil then
    FDialect := ADialect
  else
    FDialect := TDeltaORMRegistry.ResolveDialect('postgresql').SQL;

  FFields := TStringList.Create;
  FFields.Delimiter := ',';
  FFields.StrictDelimiter := True;
  FValues := TStringList.Create;
  FValues.Delimiter := ',';
  FValues.StrictDelimiter := True;
  FWhereConditions := TStringList.Create;
  FJoins := TStringList.Create;
  FTableAlias := '';
  FLimit := -1;
  FOffset := -1;
  FGroupBy := '';
  FOrderBy := '';
  FUseQuotes := False;
  FIsCount := False;
  FOnlyDirty := False;
  FParams := TDeltaParams.Create;
end;

constructor TDMSQLBuilder.Create(AModel: TDeltaModel; const ADialect: IDeltaDialect);
begin
  if ADialect <> nil then
    Create(AModel, ADialect.SQL)
  else
    Create(AModel, IDeltaSQLDialect(nil));
end;

constructor TDMSQLBuilder.Create(AModel: TDeltaModel; const ADialectName: string);
begin
  Create(AModel, TDeltaORMRegistry.ResolveDialect(ADialectName).SQL);
end;

destructor TDMSQLBuilder.Destroy;
begin
  FFields.Free;
  FValues.Free;
  FWhereConditions.Free;
  FJoins.Free;
  FParams.Free;
  inherited Destroy;
end;

function TDMSQLBuilder.UseQuotes(AValue: Boolean): TDMSQLBuilder;
begin
  FUseQuotes := AValue;
  Result := Self;
end;

function TDMSQLBuilder.QuoteIdentifier(const AIdentifier: string): string;
begin
  if FUseQuotes and (FDialect <> nil) then
    Result := FDialect.QuoteIdentifier(AIdentifier)
  else
    Result := AIdentifier;
end;

function TDMSQLBuilder.GetTableName: string;
begin
  if FModel <> nil then
    Result := QuoteIdentifier(FModel.TableName)
  else
    Result := '';
end;

function TDMSQLBuilder.GetTableNameWithAlias: string;
begin
  Result := GetTableName;
  if not FTableAlias.IsEmpty then
    Result := Result + ' ' + QuoteIdentifier(FTableAlias);
end;

function TDMSQLBuilder.JoinsToSQL: string;
begin
  Result := '';
  if FJoins.Count > 0 then
    Result := ' ' + string.Join(' ', FJoins.ToStringArray);
end;

function TDMSQLBuilder.ExtractFieldsAndValues(UseNamedParams: Boolean): Boolean;
var
  Meta: TModelMetadata;
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropCount, I: Integer;
  PropValue: Variant;
  Obj: TDeltaField;
  NestedObj: TObject;
  FieldMeta: PDeltaFieldMeta;
  FS: TFormatSettings;
  IsPK, IsAuto: Boolean;
  ColName: string;
begin
  Result := False;
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  FS.ThousandSeparator := #0;
  if FModel = nil then
    Exit;

  Meta := TRTTICache.GetModelMetadata(FModel.ClassType);
  if Meta.FieldCount > 0 then
  begin
    for I := 0 to Meta.FieldCount - 1 do
    begin
      FieldMeta := @Meta.Fields[I];
      if FieldMeta^.IsVirtual then
        Continue;

      IsPK := FieldMeta^.IsPrimaryKey or (dboPrimaryKey in FieldMeta^.DBOptions);
      IsAuto := FieldMeta^.IsAutoInc or (dboAutoInc in FieldMeta^.DBOptions);

      if FCommand = 'INSERT INTO' then
      begin
        if IsAuto or
           (not (dboInsert in FieldMeta^.DBOptions) and not (dboUpdate in FieldMeta^.DBOptions)) then
          Continue;
      end
      else
      begin
        if not (dboUpdate in FieldMeta^.DBOptions) or IsPK then
          Continue;
      end;

      if FieldMeta^.IsDeltaField then
      begin
        NestedObj := GetObjectProp(FModel, FieldMeta^.PropInfo);
        if (NestedObj = nil) or not (NestedObj is TDeltaField) then
          Continue;
        Obj := TDeltaField(NestedObj);

        if FOnlyDirty and (FCommand = 'UPDATE') then
        begin
          if not (Obj.Dirty or FModel.IsDirty(FieldMeta^.PropIndex)) then
            Continue;
        end
        else if Obj.IsNull then
          Continue;

        FFields.Add(QuoteIdentifier(FieldMeta^.FieldName));
        if UseNamedParams then
          FValues.Add(':' + FieldMeta^.FieldName)
        else if Obj.IsNull then
          FValues.Add('NULL')
        else if VarIsNumeric(Obj.Value) then
          FValues.Add(Obj.AsString.Replace(',', '', [rfReplaceAll]))
        else
          FValues.Add(TSQLCriteriaHelper.QuoteString(Obj.AsString, FDialect));
      end
      else
      begin
        if FOnlyDirty and (FCommand = 'UPDATE') then
        begin
          if not FModel.IsDirty(FieldMeta^.PropIndex) then
            Continue;
        end
        else if FModel.IsNull(FieldMeta^.PropIndex) and (FCommand <> 'INSERT INTO') then
          Continue;

        FFields.Add(QuoteIdentifier(FieldMeta^.FieldName));
        if UseNamedParams then
          FValues.Add(':' + FieldMeta^.FieldName)
        else if FModel.IsNull(FieldMeta^.PropIndex) then
          FValues.Add('NULL')
        else
        begin
          case FieldMeta^.FieldKind of
            dfkInteger:  FValues.Add(IntToStr(GetOrdProp(FModel, FieldMeta^.PropInfo)));
            dfkFloat:    FValues.Add(FloatToStr(GetFloatProp(FModel, FieldMeta^.PropInfo), FS));
            dfkBoolean:  FValues.Add(IntToStr(GetOrdProp(FModel, FieldMeta^.PropInfo)));
            dfkDateTime: FValues.Add(QuotedStr(FormatDateTime('yyyy-mm-dd hh:nn:ss', GetFloatProp(FModel, FieldMeta^.PropInfo))));
          else
            FValues.Add(TSQLCriteriaHelper.QuoteString(GetStrProp(FModel, FieldMeta^.PropInfo), FDialect));
          end;
        end;
      end;
    end;
    Result := FFields.Count > 0;
    Exit;
  end;

  PropCount := GetPropList(FModel.ClassInfo, tkProperties, nil);
  if PropCount = 0 then
    Exit;

  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropList(FModel.ClassInfo, tkProperties, PropList, False);
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];
      PropValue := GetPropValue(FModel, PropInfo^.Name, False);

      if PropInfo^.PropType^.Kind = tkClass then
      begin
        NestedObj := GetObjectProp(FModel, PropInfo^.Name);
        if NestedObj is TDeltaField then
        begin
          Obj := TDeltaField(NestedObj);
          if Obj.IsVirtual then
            Continue;

          if FCommand = 'INSERT INTO' then
          begin
            if Obj.IsNull or (dboAutoInc in Obj.DBOptions) or
               (not (dboInsert in Obj.DBOptions) and not (dboUpdate in Obj.DBOptions)) then
              Continue;
          end
          else if Obj.IsNull or not (dboUpdate in Obj.DBOptions) or (dboPrimaryKey in Obj.DBOptions) then
            Continue;

          if not Obj.FieldName.IsEmpty then
            ColName := Obj.FieldName
          else
            ColName := LowerCase(PropInfo^.Name);

          FFields.Add(QuoteIdentifier(ColName));
          if UseNamedParams then
            FValues.Add(':' + ColName)
          else if Obj.IsNull then
            FValues.Add('NULL')
          else if VarIsNumeric(Obj.Value) then
            FValues.Add(Obj.AsString.Replace(',', '', [rfReplaceAll]))
          else
            FValues.Add(TSQLCriteriaHelper.QuoteString(Obj.AsString, FDialect));
        end;
      end
      else
      begin
        if VarIsNull(PropValue) or (PropInfo^.SetProc = nil) then
          Continue;

        FFields.Add(QuoteIdentifier(PropInfo^.Name));
        if UseNamedParams then
          FValues.Add(':' + PropInfo^.Name)
        else if VarIsNumeric(PropValue) then
          FValues.Add(FloatToStr(Double(PropValue), FS))
        else if VarIsStr(PropValue) then
          FValues.Add(TSQLCriteriaHelper.QuoteString(VarToStr(PropValue), FDialect))
        else
          FValues.Add(VarToStr(PropValue));
      end;
    end;
    Result := FFields.Count > 0;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;
end;

function TDMSQLBuilder.FieldsToSQL: string;
begin
  Result := string.Join(', ', FFields.ToStringArray);
end;

function TDMSQLBuilder.ValuesToSQL: string;
begin
  Result := string.Join(', ', FValues.ToStringArray);
end;

function TDMSQLBuilder.WhereToSQL: string;
begin
  Result := '';
  if FWhereConditions.Count > 0 then
    Result := ' WHERE ' + string.Join(' AND ', FWhereConditions.ToStringArray);
end;

class function TDMSQLBuilder.WhereClausePK(AModel: TDeltaModel): string;
begin
  Result := WhereClausePK(AModel, False);
end;

class function TDMSQLBuilder.WhereClausePK(AModel: TDeltaModel;
  UseNamedParams: Boolean): string;
var
  Meta: TModelMetadata;
  FieldMeta: PDeltaFieldMeta;
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropCount, I: Integer;
  Obj: TDeltaField;
  NestedObj: TObject;
  SL: TStringList;
  PKVal: Variant;
  ColName: string;
begin
  if AModel = nil then
    Exit('');

  Meta := TRTTICache.GetModelMetadata(AModel.ClassType);
  if (Meta <> nil) and (Meta.PKPropInfo <> nil) then
  begin
    FieldMeta := Meta.FindFieldByProp(Meta.PKPropName);
    if (FieldMeta <> nil) and (not FieldMeta^.IsDeltaField) then
    begin
      if UseNamedParams then
        Exit('(' + Meta.PKFieldName + ' = :' + Meta.PKFieldName + ')');
      PKVal := GetPropValue(AModel, Meta.PKPropName);
      if VarIsStr(PKVal) then
        Exit('(' + Meta.PKFieldName + ' = ' + TSQLCriteriaHelper.QuoteString(VarToStr(PKVal)) + ')')
      else
        Exit('(' + Meta.PKFieldName + ' = ' + VarToStr(PKVal) + ')');
    end;
  end;

  PropCount := GetPropList(AModel.ClassInfo, tkProperties, nil);
  if PropCount = 0 then
    Exit('');

  GetMem(PropList, PropCount * SizeOf(Pointer));
  SL := TStringList.Create;
  try
    GetPropList(AModel.ClassInfo, tkProperties, PropList, False);
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];
      if PropInfo^.PropType^.Kind = tkClass then
      begin
        NestedObj := GetObjectProp(AModel, PropInfo^.Name);
        if NestedObj is TDeltaField then
        begin
          Obj := TDeltaField(NestedObj);
          if Obj.IsVirtual or Obj.IsNull or not (dboPrimaryKey in Obj.DBOptions) then
            Continue;

          if not Obj.FieldName.IsEmpty then
            ColName := Obj.FieldName
          else
            ColName := LowerCase(PropInfo^.Name);

          if UseNamedParams then
            SL.Add('(' + ColName + ' = :' + ColName + ')')
          else if VarIsStr(Obj.Value) then
            SL.Add('(' + Obj.FieldName + ' = ' + TSQLCriteriaHelper.QuoteString(Obj.AsString) + ')')
          else
            SL.Add('(' + Obj.FieldName + ' = ' + VarToStr(Obj.Value) + ')');
        end;
      end;
    end;
    Result := string.Join(' AND ', SL.ToStringArray);
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
    SL.Free;
  end;
end;

function TDMSQLBuilder.Select: TDMSQLBuilder;
begin
  FCommand := 'SELECT';
  FIsCount := False;
  FFields.Clear;
  Result := Self;
end;

function TDMSQLBuilder.Select(const AColumns: string): TDMSQLBuilder;
var
  Parts: TStringArray;
  I: Integer;
begin
  FCommand := 'SELECT';
  FIsCount := False;
  FFields.Clear;
  Parts := AColumns.Split([',']);
  for I := 0 to High(Parts) do
    if not Parts[I].Trim.IsEmpty then
      FFields.Add(QuoteIdentifier(Parts[I].Trim));
  Result := Self;
end;

function TDMSQLBuilder.Select(const AColumns: array of string): TDMSQLBuilder;
var
  I: Integer;
begin
  FCommand := 'SELECT';
  FIsCount := False;
  FFields.Clear;
  for I := Low(AColumns) to High(AColumns) do
    if not AColumns[I].Trim.IsEmpty then
      FFields.Add(QuoteIdentifier(AColumns[I].Trim));
  Result := Self;
end;

function TDMSQLBuilder.Count: TDMSQLBuilder;
begin
  FCommand := 'SELECT';
  FIsCount := True;
  FFields.Clear;
  Result := Self;
end;

function TDMSQLBuilder.Insert(UseNamedParams: Boolean): TDMSQLBuilder;
begin
  FCommand := 'INSERT INTO';
  FFields.Clear;
  FValues.Clear;
  ExtractFieldsAndValues(UseNamedParams);
  Result := Self;
end;

function TDMSQLBuilder.Update(UseNamedParams: Boolean): TDMSQLBuilder;
begin
  FCommand := 'UPDATE';
  FOnlyDirty := False;
  FFields.Clear;
  FValues.Clear;
  ExtractFieldsAndValues(UseNamedParams);
  Result := Self;
end;

function TDMSQLBuilder.UpdateDirty(UseNamedParams: Boolean): TDMSQLBuilder;
begin
  FCommand := 'UPDATE';
  FOnlyDirty := True;
  FFields.Clear;
  FValues.Clear;
  ExtractFieldsAndValues(UseNamedParams);
  Result := Self;
end;

function TDMSQLBuilder.Delete: TDMSQLBuilder;
begin
  FCommand := 'DELETE FROM';
  Result := Self;
end;

function TDMSQLBuilder.TableAlias(const AAlias: string): TDMSQLBuilder;
begin
  FTableAlias := AAlias.Trim;
  Result := Self;
end;

function TDMSQLBuilder.Join(AJoinType: TJoinType; const ATable,
  AOnCondition: string; const AAlias: string): TDMSQLBuilder;
var
  Clause: string;
begin
  Clause := TSQLCriteriaHelper.JoinTypeToString(AJoinType) + ' ' + QuoteIdentifier(ATable);
  if not AAlias.Trim.IsEmpty then
    Clause := Clause + ' ' + QuoteIdentifier(AAlias.Trim);
  Clause := Clause + ' ON ' + AOnCondition;
  FJoins.Add(Clause);
  Result := Self;
end;

function TDMSQLBuilder.Join(const AJoinClause: string): TDMSQLBuilder;
begin
  if not AJoinClause.Trim.IsEmpty then
    FJoins.Add(AJoinClause.Trim);
  Result := Self;
end;

function TDMSQLBuilder.InnerJoin(const ATable, AOnCondition: string;
  const AAlias: string): TDMSQLBuilder;
begin
  Result := Join(jtInner, ATable, AOnCondition, AAlias);
end;

function TDMSQLBuilder.LeftJoin(const ATable, AOnCondition: string;
  const AAlias: string): TDMSQLBuilder;
begin
  Result := Join(jtLeft, ATable, AOnCondition, AAlias);
end;

function TDMSQLBuilder.RightJoin(const ATable, AOnCondition: string;
  const AAlias: string): TDMSQLBuilder;
begin
  Result := Join(jtRight, ATable, AOnCondition, AAlias);
end;

function TDMSQLBuilder.FullJoin(const ATable, AOnCondition: string;
  const AAlias: string): TDMSQLBuilder;
begin
  Result := Join(jtFull, ATable, AOnCondition, AAlias);
end;

function TDMSQLBuilder.Where(const ACondition: string): TDMSQLBuilder;
begin
  if not ACondition.IsEmpty then
    FWhereConditions.Add(ACondition);
  Result := Self;
end;

function TDMSQLBuilder.Where(const AField: string;
  const AValue: Variant): TDMSQLBuilder;
begin
  Result := Where(TSQLCriteriaHelper.BuildCondition(AField, AValue, FParams, FDialect));
end;

function TDMSQLBuilder.Where(const AField: string; AOp: TComparisonOp;
  const AValue: Variant): TDMSQLBuilder;
begin
  Result := Where(TSQLCriteriaHelper.BuildCondition(AField, AOp, AValue, FParams, FDialect));
end;

function TDMSQLBuilder.Where(const AField, AOpStr: string;
  const AValue: Variant): TDMSQLBuilder;
begin
  Result := Where(TSQLCriteriaHelper.BuildCondition(AField, AOpStr, AValue, FParams, FDialect));
end;

function TDMSQLBuilder.AndWhere(const ACondition: string): TDMSQLBuilder;
begin
  Result := Where(ACondition);
end;

function TDMSQLBuilder.AndWhere(const AField: string;
  const AValue: Variant): TDMSQLBuilder;
begin
  Result := AndWhere(TSQLCriteriaHelper.BuildCondition(AField, AValue, FParams, FDialect));
end;

function TDMSQLBuilder.AndWhere(const AField: string; AOp: TComparisonOp;
  const AValue: Variant): TDMSQLBuilder;
begin
  Result := AndWhere(TSQLCriteriaHelper.BuildCondition(AField, AOp, AValue, FParams, FDialect));
end;

function TDMSQLBuilder.AndWhere(const AField, AOpStr: string;
  const AValue: Variant): TDMSQLBuilder;
begin
  Result := AndWhere(TSQLCriteriaHelper.BuildCondition(AField, AOpStr, AValue, FParams, FDialect));
end;

function TDMSQLBuilder.OrWhere(const ACondition: string): TDMSQLBuilder;
var
  LastIdx: Integer;
begin
  if ACondition.IsEmpty then
    Exit(Self);
  if FWhereConditions.Count > 0 then
  begin
    LastIdx := FWhereConditions.Count - 1;
    FWhereConditions[LastIdx] := '(' + FWhereConditions[LastIdx] + ' OR ' + ACondition + ')';
  end
  else
    FWhereConditions.Add(ACondition);
  Result := Self;
end;

function TDMSQLBuilder.OrWhere(const AField: string;
  const AValue: Variant): TDMSQLBuilder;
begin
  Result := OrWhere(TSQLCriteriaHelper.BuildCondition(AField, AValue, FParams, FDialect));
end;

function TDMSQLBuilder.OrWhere(const AField: string; AOp: TComparisonOp;
  const AValue: Variant): TDMSQLBuilder;
begin
  Result := OrWhere(TSQLCriteriaHelper.BuildCondition(AField, AOp, AValue, FParams, FDialect));
end;

function TDMSQLBuilder.OrWhere(const AField, AOpStr: string;
  const AValue: Variant): TDMSQLBuilder;
begin
  Result := OrWhere(TSQLCriteriaHelper.BuildCondition(AField, AOpStr, AValue, FParams, FDialect));
end;

function TDMSQLBuilder.WhereBetween(const AField: string;
  const AVal1, AVal2: Variant): TDMSQLBuilder;
begin
  Result := Where(TSQLCriteriaHelper.BuildBetween(AField, AVal1, AVal2, FParams, FDialect));
end;

function TDMSQLBuilder.WhereIn(const AField: string;
  const AValues: array of Variant): TDMSQLBuilder;
begin
  Result := Where(TSQLCriteriaHelper.BuildIn(AField, AValues, FParams, FDialect, False));
end;

function TDMSQLBuilder.WhereNotIn(const AField: string;
  const AValues: array of Variant): TDMSQLBuilder;
begin
  Result := Where(TSQLCriteriaHelper.BuildIn(AField, AValues, FParams, FDialect, True));
end;

function TDMSQLBuilder.WhereNull(const AField: string): TDMSQLBuilder;
begin
  Result := Where(TSQLCriteriaHelper.BuildNullCondition(AField, True));
end;

function TDMSQLBuilder.WhereNotNull(const AField: string): TDMSQLBuilder;
begin
  Result := Where(TSQLCriteriaHelper.BuildNullCondition(AField, False));
end;

function TDMSQLBuilder.OrderBy(const AField: string): TDMSQLBuilder;
begin
  FOrderBy := AField;
  Result := Self;
end;

function TDMSQLBuilder.Limit(const ALimit: Integer): TDMSQLBuilder;
begin
  FLimit := ALimit;
  Result := Self;
end;

function TDMSQLBuilder.Offset(const AOffset: Integer): TDMSQLBuilder;
begin
  FOffset := AOffset;
  Result := Self;
end;

function TDMSQLBuilder.Page(const APageNumber, APageSize: Integer): TDMSQLBuilder;
begin
  if (APageNumber > 0) and (APageSize > 0) then
  begin
    FLimit := APageSize;
    FOffset := (APageNumber - 1) * APageSize;
  end;
  Result := Self;
end;

function TDMSQLBuilder.SelectFieldsText: string;
begin
  if FFields.Count > 0 then
    Result := FieldsToSQL
  else
    Result := '*';
end;

function TDMSQLBuilder.GroupBy(const AField: string): TDMSQLBuilder;
begin
  FGroupBy := QuoteIdentifier(AField);
  Result := Self;
end;

function TDMSQLBuilder.Clear: TDMSQLBuilder;
begin
  FFields.Clear;
  FValues.Clear;
  FWhereConditions.Clear;
  FJoins.Clear;
  FParams.Clear;
  FTableAlias := '';
  FLimit := -1;
  FOffset := -1;
  FGroupBy := '';
  FOrderBy := '';
  FIsCount := False;
  FOnlyDirty := False;
  Result := Self;
end;

procedure TDMSQLBuilder.BindParams(AParams: TDeltaParams);
begin
  if AParams <> nil then
    AParams.MergeFrom(FParams);
end;

function TDMSQLBuilder.Build: string;
var
  vFields, vOrderBy, vGroupBy: string;
begin
  case FCommand of
    'SELECT':
      begin
        if FIsCount then
          vFields := 'COUNT(*)'
        else
          vFields := SelectFieldsText;

        vGroupBy := IfThen(FGroupBy <> '', ' GROUP BY ' + FGroupBy, '');
        vOrderBy := IfThen(FOrderBy <> '', ' ORDER BY ' + FOrderBy, '');

        Result := FDialect.BuildSelect(vFields, GetTableNameWithAlias,
          JoinsToSQL, WhereToSQL, vGroupBy, vOrderBy, FLimit, FOffset);
      end;

    'INSERT INTO':
      Result := Format('INSERT INTO %s (%s) VALUES (%s)',
        [GetTableName, FieldsToSQL, ValuesToSQL]);

    'UPDATE':
      begin
        if FOnlyDirty and (FFields.Count = 0) then
          Exit('');
        Result := Format('UPDATE %s SET %s%s',
          [GetTableName, FieldAndValuesToSQL, WhereToSQL]);
      end;

    'DELETE FROM':
      Result := Format('DELETE FROM %s%s', [GetTableName, WhereToSQL]);
  else
    raise Exception.Create('Invalid SQL command.');
  end;
end;

function TDMSQLBuilder.FieldAndValuesToSQL: string;
var
  SL: TStringList;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    for I := 0 to FFields.Count - 1 do
      SL.Add(Format('%s = %s', [FFields[I], FValues[I]]));
    Result := string.Join(', ', SL.ToStringArray);
  finally
    SL.Free;
  end;
end;

class function TDMSQLBuilder.CreateInsert(AModel: TDeltaModel;
  const ADialect: IDeltaSQLDialect; UseNamedParams: Boolean): string;
var
  Builder: TDMSQLBuilder;
begin
  Builder := TDMSQLBuilder.Create(AModel, ADialect);
  try
    Result := Builder.Insert(UseNamedParams).Build;
  finally
    Builder.Free;
  end;
end;

class function TDMSQLBuilder.CreateInsert(AModel: TDeltaModel;
  const ADialectName: string; UseNamedParams: Boolean): string;
begin
  Result := CreateInsert(AModel, TDeltaORMRegistry.ResolveDialect(ADialectName).SQL, UseNamedParams);
end;

class function TDMSQLBuilder.CreateInsertReturning(AModel: TDeltaModel;
  const ADialect: IDeltaSQLDialect; UseNamedParams: Boolean;
  const AReturningCols: string): string;
var
  Builder: TDMSQLBuilder;
begin
  Builder := TDMSQLBuilder.Create(AModel, ADialect);
  try
    Builder.Insert(UseNamedParams);
    Result := Builder.FDialect.BuildInsertReturning(
      Builder.GetTableName, Builder.FieldsToSQL, Builder.ValuesToSQL, AReturningCols);
  finally
    Builder.Free;
  end;
end;

class function TDMSQLBuilder.CreateInsertReturning(AModel: TDeltaModel;
  const ADialectName: string; UseNamedParams: Boolean;
  const AReturningCols: string): string;
begin
  Result := CreateInsertReturning(AModel,
    TDeltaORMRegistry.ResolveDialect(ADialectName).SQL, UseNamedParams, AReturningCols);
end;

class function TDMSQLBuilder.CreateUpdate(AModel: TDeltaModel;
  const ADialect: IDeltaSQLDialect; const WhereClause: string;
  UseNamedParams: Boolean): string;
var
  Builder: TDMSQLBuilder;
  WherePK: string;
begin
  WherePK := WhereClausePK(AModel);
  Builder := TDMSQLBuilder.Create(AModel, ADialect);
  try
    Result := Builder
      .Update(UseNamedParams)
      .Where(IfThen(WhereClause.IsEmpty, WherePK, WhereClause))
      .Build;
  finally
    Builder.Free;
  end;
end;

class function TDMSQLBuilder.CreateUpdate(AModel: TDeltaModel;
  const ADialectName: string; const WhereClause: string;
  UseNamedParams: Boolean): string;
begin
  Result := CreateUpdate(AModel,
    TDeltaORMRegistry.ResolveDialect(ADialectName).SQL, WhereClause, UseNamedParams);
end;

class function TDMSQLBuilder.CreateUpdateReturning(AModel: TDeltaModel;
  const ADialect: IDeltaSQLDialect; const WhereClause: string;
  UseNamedParams: Boolean): string;
var
  Builder: TDMSQLBuilder;
  WherePK: string;
begin
  WherePK := IfThen(WhereClause.IsEmpty, WhereClausePK(AModel), WhereClause);
  Builder := TDMSQLBuilder.Create(AModel, ADialect);
  try
    Builder.Update(UseNamedParams).Where(WherePK);
    Result := Builder.FDialect.BuildUpdateReturning(
      Builder.GetTableName, Builder.FieldAndValuesToSQL, Builder.WhereToSQL);
  finally
    Builder.Free;
  end;
end;

class function TDMSQLBuilder.CreateUpdateReturning(AModel: TDeltaModel;
  const ADialectName: string; const WhereClause: string;
  UseNamedParams: Boolean): string;
begin
  Result := CreateUpdateReturning(AModel,
    TDeltaORMRegistry.ResolveDialect(ADialectName).SQL, WhereClause, UseNamedParams);
end;

class function TDMSQLBuilder.CreatePartialUpdate(AModel: TDeltaModel;
  const ADialect: IDeltaSQLDialect; const WhereClause: string;
  UseNamedParams: Boolean): string;
var
  Builder: TDMSQLBuilder;
  WherePK: string;
begin
  if (AModel = nil) or (not AModel.HasDirtyFields) then
    Exit('');

  WherePK := WhereClausePK(AModel);
  Builder := TDMSQLBuilder.Create(AModel, ADialect);
  try
    Result := Builder
      .UpdateDirty(UseNamedParams)
      .Where(IfThen(WhereClause.IsEmpty, WherePK, WhereClause))
      .Build;
  finally
    Builder.Free;
  end;
end;

class function TDMSQLBuilder.CreatePartialUpdate(AModel: TDeltaModel;
  const ADialectName: string; const WhereClause: string;
  UseNamedParams: Boolean): string;
begin
  Result := CreatePartialUpdate(AModel,
    TDeltaORMRegistry.ResolveDialect(ADialectName).SQL, WhereClause, UseNamedParams);
end;

class function TDMSQLBuilder.CreatePartialUpdateReturning(AModel: TDeltaModel;
  const ADialect: IDeltaSQLDialect; const WhereClause: string;
  UseNamedParams: Boolean): string;
var
  Builder: TDMSQLBuilder;
  WherePK, BaseSQL: string;
begin
  if (AModel = nil) or (not AModel.HasDirtyFields) then
    Exit('');

  WherePK := IfThen(WhereClause.IsEmpty, WhereClausePK(AModel), WhereClause);
  Builder := TDMSQLBuilder.Create(AModel, ADialect);
  try
    Builder.UpdateDirty(UseNamedParams).Where(WherePK);
    BaseSQL := Builder.Build;
    if BaseSQL.IsEmpty then
      Exit('');
    Result := Builder.FDialect.BuildUpdateReturning(
      Builder.GetTableName, Builder.FieldAndValuesToSQL, Builder.WhereToSQL);
  finally
    Builder.Free;
  end;
end;

class function TDMSQLBuilder.CreatePartialUpdateReturning(AModel: TDeltaModel;
  const ADialectName: string; const WhereClause: string;
  UseNamedParams: Boolean): string;
begin
  Result := CreatePartialUpdateReturning(AModel,
    TDeltaORMRegistry.ResolveDialect(ADialectName).SQL, WhereClause, UseNamedParams);
end;

class function TDMSQLBuilder.CreateDelete(AModel: TDeltaModel;
  const ADialect: IDeltaSQLDialect; const WhereClause: string): string;
var
  Builder: TDMSQLBuilder;
  WherePK: string;
begin
  WherePK := WhereClausePK(AModel);
  Builder := TDMSQLBuilder.Create(AModel, ADialect);
  try
    Result := Builder
      .Delete
      .Where(IfThen(WhereClause.IsEmpty, WherePK, WhereClause))
      .Build;
  finally
    Builder.Free;
  end;
end;

class function TDMSQLBuilder.CreateDelete(AModel: TDeltaModel;
  const ADialectName: string; const WhereClause: string): string;
begin
  Result := CreateDelete(AModel,
    TDeltaORMRegistry.ResolveDialect(ADialectName).SQL, WhereClause);
end;

class function TDMSQLBuilder.CreateSelect(AModel: TDeltaModel;
  const ADialect: IDeltaSQLDialect; const WhereClause: string): string;
var
  Builder: TDMSQLBuilder;
begin
  Builder := TDMSQLBuilder.Create(AModel, ADialect);
  try
    Result := Builder.Select.Where(WhereClause).Build;
  finally
    Builder.Free;
  end;
end;

class function TDMSQLBuilder.CreateSelect(AModel: TDeltaModel;
  const ADialectName: string; const WhereClause: string): string;
begin
  Result := CreateSelect(AModel,
    TDeltaORMRegistry.ResolveDialect(ADialectName).SQL, WhereClause);
end;

class function TDMSQLBuilder.CreateCount(AModel: TDeltaModel;
  const ADialect: IDeltaSQLDialect; const WhereClause: string): string;
var
  Builder: TDMSQLBuilder;
begin
  Builder := TDMSQLBuilder.Create(AModel, ADialect);
  try
    Result := Builder.Count.Where(WhereClause).Build;
  finally
    Builder.Free;
  end;
end;

class function TDMSQLBuilder.CreateCount(AModel: TDeltaModel;
  const ADialectName: string; const WhereClause: string): string;
begin
  Result := CreateCount(AModel,
    TDeltaORMRegistry.ResolveDialect(ADialectName).SQL, WhereClause);
end;

class function TDMSQLBuilder.ExtractFieldNames(AModel: TDeltaModel): TStringList;
var
  Meta: TModelMetadata;
  FieldMeta: PDeltaFieldMeta;
  Val: Variant;
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropCount, I: Integer;
  Obj: TDeltaField;
  NestedObj: TObject;
begin
  Result := TStringList.Create;
  if AModel = nil then
    Exit;

  Meta := TRTTICache.GetModelMetadata(AModel.ClassType);
  if Meta.FieldCount > 0 then
  begin
    for I := 0 to Meta.FieldCount - 1 do
    begin
      FieldMeta := @Meta.Fields[I];
      if FieldMeta^.IsVirtual then
        Continue;
      if not ((dboInsert in FieldMeta^.DBOptions) or
              (dboUpdate in FieldMeta^.DBOptions) or
              (dboAutoInc in FieldMeta^.DBOptions)) then
        Continue;

      if FieldMeta^.IsDeltaField then
      begin
        NestedObj := GetObjectProp(AModel, FieldMeta^.PropInfo);
        if NestedObj is TDeltaField then
        begin
          Obj := TDeltaField(NestedObj);
          if (dboAutoInc in Obj.DBOptions) and
             (Obj.IsNull or (VarIsNumeric(Obj.Value) and (Double(Obj.Value) = 0))) then
            Continue;

          if not Obj.FieldName.IsEmpty then
            Result.Add(Obj.FieldName)
          else
            Result.Add(FieldMeta^.PropName);
        end;
      end
      else
      begin
        if dboAutoInc in FieldMeta^.DBOptions then
        begin
          Val := GetPropValue(AModel, FieldMeta^.PropName);
          if AModel.IsNull(FieldMeta^.PropIndex) or
             (VarIsNumeric(Val) and (Double(Val) = 0)) then
            Continue;
        end;
        Result.Add(FieldMeta^.FieldName);
      end;
    end;
    Exit;
  end;

  PropCount := GetPropList(AModel.ClassInfo, tkProperties, nil);
  if PropCount = 0 then
    Exit;

  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropList(AModel.ClassInfo, tkProperties, PropList, False);
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];
      if PropInfo^.PropType^.Kind = tkClass then
      begin
        NestedObj := GetObjectProp(AModel, PropInfo^.Name);
        if NestedObj is TDeltaField then
        begin
          Obj := TDeltaField(NestedObj);
          if Obj.IsVirtual then
            Continue;
          if not ((dboInsert in Obj.DBOptions) or
                  (dboUpdate in Obj.DBOptions) or
                  (dboAutoInc in Obj.DBOptions)) then
            Continue;
          if (dboAutoInc in Obj.DBOptions) and
             (Obj.IsNull or (VarIsNumeric(Obj.Value) and (Double(Obj.Value) = 0))) then
            Continue;

          if not Obj.FieldName.IsEmpty then
            Result.Add(Obj.FieldName)
          else
            Result.Add(PropInfo^.Name);
        end;
      end
      else if PropInfo^.SetProc <> nil then
        Result.Add(PropInfo^.Name);
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;
end;

class function TDMSQLBuilder.ExtractFieldValues(AModel: TDeltaModel;
  ASuffix: string): TStringList;
var
  FieldNames: TStringList;
  I: Integer;
begin
  FieldNames := ExtractFieldNames(AModel);
  try
    Result := TStringList.Create;
    for I := 0 to FieldNames.Count - 1 do
      Result.Add(':' + FieldNames[I] + ASuffix);
  finally
    FieldNames.Free;
  end;
end;

class function TDMSQLBuilder.CreateBulkInsert(AModels: array of TDeltaModel;
  const ADialect: IDeltaSQLDialect): string;
var
  FieldNames: TStringList;
begin
  if Length(AModels) = 0 then
    raise Exception.Create('BulkInsert requires at least one model.');
  if ADialect = nil then
    raise Exception.Create('CreateBulkInsert requires a SQL dialect.');

  FieldNames := ExtractFieldNames(AModels[0]);
  try
    if FieldNames.Count = 0 then
      raise Exception.Create('BulkInsert requires at least one insertable field.');
    Result := ADialect.BuildBulkInsert(AModels[0].TableName, FieldNames, Length(AModels));
  finally
    FieldNames.Free;
  end;
end;

class function TDMSQLBuilder.CreateBulkInsert(AModels: array of TDeltaModel;
  const ADialectName: string): string;
begin
  Result := CreateBulkInsert(AModels,
    TDeltaORMRegistry.ResolveDialect(ADialectName).SQL);
end;

class function TDMSQLBuilder.CreateBulkInsertRow(AModel: TDeltaModel;
  const ADialect: IDeltaSQLDialect): string;
var
  FieldNames: TStringList;
  ColsSQL, ValsSQL: string;
  I: Integer;
begin
  if AModel = nil then
    raise Exception.Create('BulkInsert requires at least one model.');
  if ADialect = nil then
    raise Exception.Create('CreateBulkInsertRow requires a SQL dialect.');

  FieldNames := ExtractFieldNames(AModel);
  try
    if FieldNames.Count = 0 then
      raise Exception.Create('BulkInsert requires at least one insertable field.');

    ColsSQL := '';
    ValsSQL := '';
    for I := 0 to FieldNames.Count - 1 do
    begin
      if I > 0 then
      begin
        ColsSQL := ColsSQL + ', ';
        ValsSQL := ValsSQL + ', ';
      end;
      ColsSQL := ColsSQL + FieldNames[I];
      ValsSQL := ValsSQL + ':' + FieldNames[I];
    end;
    Result := Format('INSERT INTO %s (%s) VALUES (%s)',
      [AModel.TableName, ColsSQL, ValsSQL]);
  finally
    FieldNames.Free;
  end;
end;

end.
