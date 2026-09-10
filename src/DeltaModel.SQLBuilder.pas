unit DeltaModel.SQLBuilder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Variants, DeltaModel, DeltaModel.Fields, StrUtils,
  DeltaModel.ORM.Types;

type

  { TDMSQLBuilder }

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
    FDialect: TDatabaseDialect;
    FGroupBy: string;
    FUseQuotes: Boolean;
    FIsCount: Boolean;

    function FieldAndValuesToSQL: string;
    function GetTableName: string;
    function ExtractFieldsAndValues(UseNamedParams: Boolean): Boolean;
    function FieldsToSQL: string;
    function ValuesToSQL: string;
    function WhereToSQL: string;
    function QuoteIdentifier(const AIdentifier: string): string;
    function BuildLimitOffset: string;
  public
    constructor Create(AModel: TDeltaModel; ADialect: TDatabaseDialect);
    destructor Destroy; override;

    function Select: TDMSQLBuilder; overload;
    function Select(const AColumns: string): TDMSQLBuilder; overload;
    function Select(const AColumns: array of string): TDMSQLBuilder; overload;
    function Count: TDMSQLBuilder;
    function Insert(UseNamedParams: Boolean = True): TDMSQLBuilder;
    function Update(UseNamedParams: Boolean = True): TDMSQLBuilder;
    function Delete: TDMSQLBuilder;
    function Where(const ACondition: string): TDMSQLBuilder;
    function AndWhere(const ACondition: string): TDMSQLBuilder;
    function OrWhere(const ACondition: string): TDMSQLBuilder;
    function OrderBy(const AField: string): TDMSQLBuilder;
    function Limit(const ALimit: Integer): TDMSQLBuilder;
    function Offset(const AOffset: Integer): TDMSQLBuilder;
    function Page(const APageNumber, APageSize: Integer): TDMSQLBuilder;
    function GroupBy(const AField: string): TDMSQLBuilder;
    function UseQuotes(AValue: Boolean): TDMSQLBuilder;
    function Build: string;

    class function WhereClausePK(AModel: TDeltaModel): string;

    class function CreateInsert(AModel: TDeltaModel; ADialect: TDatabaseDialect; UseNamedParams: Boolean = True): string; static;
    class function CreateInsertReturning(AModel: TDeltaModel; ADialect: TDatabaseDialect; UseNamedParams: Boolean = True): string; static;
    class function CreateUpdate(AModel: TDeltaModel; ADialect: TDatabaseDialect; const WhereClause: string = ''; UseNamedParams: Boolean = True): string; static;
    class function CreateUpdateReturning(AModel: TDeltaModel; ADialect: TDatabaseDialect; const WhereClause: string = ''; UseNamedParams: Boolean = True): string; static;
    class function CreateDelete(AModel: TDeltaModel; ADialect: TDatabaseDialect; const WhereClause: string = ''): string; static;
    class function CreateSelect(AModel: TDeltaModel; ADialect: TDatabaseDialect; const WhereClause: string = ''): string; static;
    class function CreateCount(AModel: TDeltaModel; ADialect: TDatabaseDialect; const WhereClause: string = ''): string; static;
  end;

implementation

{ TDMSQLBuilder }

constructor TDMSQLBuilder.Create(AModel: TDeltaModel; ADialect: TDatabaseDialect);
begin
  FModel := AModel;
  FDialect := ADialect;
  FFields := TStringList.Create;
  FFields.Delimiter := ',';
  FFields.StrictDelimiter := True;
  FValues := TStringList.Create;
  FValues.Delimiter := ',';
  FValues.StrictDelimiter := True;
  FWhereConditions := TStringList.Create;
  FLimit := -1;
  FOffset := -1;
  FGroupBy := '';
  FOrderBy := '';
  FUseQuotes := False;
  FIsCount := False;
end;

destructor TDMSQLBuilder.Destroy;
begin
  FFields.Free;
  FValues.Free;
  FWhereConditions.Free;
  inherited Destroy;
end;

function TDMSQLBuilder.UseQuotes(AValue: Boolean): TDMSQLBuilder;
begin
  FUseQuotes := AValue;
  Result := Self;
end;

function TDMSQLBuilder.QuoteIdentifier(const AIdentifier: string): string;
begin
  if FUseQuotes then
    Result := TDatabaseDialectHelper.QuoteIdentifier(AIdentifier, FDialect)
  else
    Result := AIdentifier;
end;

function TDMSQLBuilder.GetTableName: string;
begin
  Result := QuoteIdentifier(FModel.TableName);
end;

function TDMSQLBuilder.ExtractFieldsAndValues(UseNamedParams: Boolean): Boolean;
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropCount, I: Integer;
  PropValue: Variant;
  Obj: TDeltaField;
  NestedObj: TObject;
  FS: TFormatSettings;
begin
  Result := False;
  FS.DecimalSeparator := '.';
  FS.ThousandSeparator := ',';
  PropCount := GetPropList(FModel.ClassInfo, tkProperties, nil);
  if PropCount = 0 then Exit;

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
          Obj := NestedObj as TDeltaField;
          if Obj.IsNull or not (dboUpdate in Obj.DBOptions) then
            Continue;

          FFields.Add(QuoteIdentifier(Obj.FieldName));

          if UseNamedParams then
            FValues.Add(':' + Obj.FieldName)
          else
          if Obj.IsNull then
            FValues.Add('NULL')
          else
          if VarIsNumeric(Obj.Value) then
            FValues.Add(Obj.AsString.Replace(',', '', [rfReplaceAll]))
          else
            FValues.Add(Obj.AsString.QuotedString);
        end;
      end
      else
      begin
        if VarIsNull(PropValue) or (PropInfo^.SetProc = nil) then
          Continue;

        FFields.Add(QuoteIdentifier(PropInfo^.Name));

        if UseNamedParams then
          FValues.Add(':' + PropInfo^.Name)
        else
        if VarIsNumeric(PropValue) then
          FValues.Add(StrToFloat(VarToStr(PropValue), FS).ToString().Replace(',', '', [rfReplaceAll]))
        else
        if VarIsStr(PropValue) then
          FValues.Add(QuotedStr(PropValue))
        else
          FValues.Add(VarToStr(PropValue));
      end;
    end;
    Result := True;
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

function TDMSQLBuilder.BuildLimitOffset: string;
var
  EffOffset, EffLimit: Integer;
begin
  Result := '';
  EffOffset := FOffset;
  EffLimit  := FLimit;

  case FDialect of
    ddPostgreSQL, ddSQLite, ddMySQL:
    begin
      if EffLimit > -1 then
        Result := Result + ' LIMIT ' + IntToStr(EffLimit);
      if EffOffset > -1 then
        Result := Result + ' OFFSET ' + IntToStr(EffOffset);
    end;

    ddFirebird:
    begin
      if EffLimit > -1 then
      begin
        if EffOffset > -1 then
          Result := Format(' ROWS %d TO %d', [EffOffset + 1, EffOffset + EffLimit])
        else
          Result := Format(' ROWS 1 TO %d', [EffLimit]);
      end
      else
      if EffOffset > -1 then
        Result := Format(' ROWS %d TO 2147483647', [EffOffset + 1]);
    end;

    ddMSSQL:
    begin
      if (EffOffset > -1) or (EffLimit > -1) then
      begin
        if EffOffset < 0 then EffOffset := 0;

        // MSSQL OFFSET-FETCH exige cláusula ORDER BY
        if FOrderBy.IsEmpty then
          Result := ' ORDER BY (SELECT NULL)';

        Result := Result + Format(' OFFSET %d ROWS', [EffOffset]);
        if EffLimit > -1 then
          Result := Result + Format(' FETCH NEXT %d ROWS ONLY', [EffLimit]);
      end;
    end;

    ddOracle:
    begin
      if (EffOffset > -1) or (EffLimit > -1) then
      begin
        if EffOffset < 0 then EffOffset := 0;
        Result := Format(' OFFSET %d ROWS', [EffOffset]);
        if EffLimit > -1 then
          Result := Result + Format(' FETCH NEXT %d ROWS ONLY', [EffLimit]);
      end;
    end;
  end;
end;

class function TDMSQLBuilder.WhereClausePK(AModel: TDeltaModel): string;
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropCount, I: Integer;
  Obj: TDeltaField;
  NestedObj: TObject;
  SL: TStringList;
begin
  PropCount := GetPropList(AModel.ClassInfo, tkProperties, nil);
  if PropCount = 0 then Exit('');

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
          Obj := NestedObj as TDeltaField;
          if Obj.IsNull or not (dboPrimaryKey in Obj.DBOptions) then
            Continue;

          if VarIsStr(Obj.Value) then
            SL.Add('(' + Obj.FieldName + ' = ' + QuotedStr(Obj.AsString) + ')')
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

function TDMSQLBuilder.Where(const ACondition: string): TDMSQLBuilder;
begin
  if not ACondition.IsEmpty then
    FWhereConditions.Add(ACondition);
  Result := Self;
end;

function TDMSQLBuilder.AndWhere(const ACondition: string): TDMSQLBuilder;
begin
  Result := Where(ACondition);
end;

function TDMSQLBuilder.OrWhere(const ACondition: string): TDMSQLBuilder;
var
  LastIdx: Integer;
  PrevCond: string;
begin
  if ACondition.IsEmpty then Exit(Self);

  if FWhereConditions.Count > 0 then
  begin
    LastIdx := FWhereConditions.Count - 1;
    PrevCond := FWhereConditions[LastIdx];
    FWhereConditions[LastIdx] := '(' + PrevCond + ' OR ' + ACondition + ')';
  end
  else
    FWhereConditions.Add(ACondition);

  Result := Self;
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
    FLimit  := APageSize;
    FOffset := (APageNumber - 1) * APageSize;
  end;
  Result := Self;
end;

function TDMSQLBuilder.GroupBy(const AField: string): TDMSQLBuilder;
begin
  FGroupBy := QuoteIdentifier(AField);
  Result := Self;
end;

function TDMSQLBuilder.Build: string;
var
  vFields, vOrderBy, vGroupBy, vLimitOffset: string;
begin
  case FCommand of
    'SELECT':
    begin
      if FIsCount then
        vFields := 'COUNT(*)'
      else
        vFields := IfThen(FFields.Count > 0, FieldsToSQL, '*');

      vGroupBy := IfThen(FGroupBy <> '', ' GROUP BY ' + FGroupBy, '');
      vOrderBy := IfThen(FOrderBy <> '', ' ORDER BY ' + FOrderBy, '');
      vLimitOffset := BuildLimitOffset;

      // Se for MSSQL e o BuildLimitOffset adicionou ORDER BY interno, evita duplicação
      if (FDialect = ddMSSQL) and vLimitOffset.StartsWith(' ORDER BY') and (vOrderBy <> '') then
      begin
        vLimitOffset := Copy(vLimitOffset, Length(' ORDER BY (SELECT NULL)') + 1, Length(vLimitOffset));
      end;

      Result := Format('%s %s FROM %s%s%s%s%s',
        [FCommand, vFields, GetTableName, WhereToSQL,
         vGroupBy, vOrderBy, vLimitOffset]);
    end;

    'INSERT INTO':
      Result := Format('%s %s (%s) VALUES (%s)',
        [FCommand, GetTableName, FieldsToSQL, ValuesToSQL]);

    'UPDATE':
      Result := Format('%s %s SET %s%s',
        [FCommand, GetTableName, FieldAndValuesToSQL, WhereToSQL]);

    'DELETE FROM':
      Result := Format('%s %s%s',
        [FCommand, GetTableName, WhereToSQL]);
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
    SL.Delimiter := ',';
    SL.StrictDelimiter := True;
    for I := 0 to Pred(FFields.Count) do
    begin
      SL.Add(Format('%s = %s', [FFields[I], FValues[I]]));
    end;
    Result := string.Join(', ', SL.ToStringArray);
  finally
    SL.Free;
  end;
end;

class function TDMSQLBuilder.CreateInsert(AModel: TDeltaModel;
  ADialect: TDatabaseDialect; UseNamedParams: Boolean): string;
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

class function TDMSQLBuilder.CreateInsertReturning(AModel: TDeltaModel;
  ADialect: TDatabaseDialect; UseNamedParams: Boolean): string;
var
  Builder: TDMSQLBuilder;
begin
  Builder := TDMSQLBuilder.Create(AModel, ADialect);
  try
    Builder.Insert(UseNamedParams);
    case ADialect of
      ddMSSQL:
        // Sintaxe MSSQL: INSERT INTO table (fields) OUTPUT INSERTED.* VALUES (values)
        Result := Format('INSERT INTO %s (%s) OUTPUT INSERTED.* VALUES (%s)',
          [Builder.GetTableName, Builder.FieldsToSQL, Builder.ValuesToSQL]);

      ddPostgreSQL, ddSQLite, ddFirebird:
        Result := Builder.Build + sLineBreak + 'RETURNING *';
    else
      // Outros dialetos (MySQL/Oracle): retorna o insert normal
      Result := Builder.Build;
    end;
  finally
    Builder.Free;
  end;
end;

class function TDMSQLBuilder.CreateUpdate(AModel: TDeltaModel;
  ADialect: TDatabaseDialect; const WhereClause: string; UseNamedParams: Boolean
  ): string;
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

class function TDMSQLBuilder.CreateUpdateReturning(AModel: TDeltaModel;
  ADialect: TDatabaseDialect; const WhereClause: string; UseNamedParams: Boolean
  ): string;
var
  Builder: TDMSQLBuilder;
  WherePK: string;
begin
  WherePK := IfThen(WhereClause.IsEmpty, WhereClausePK(AModel), WhereClause);
  Builder := TDMSQLBuilder.Create(AModel, ADialect);
  try
    Builder.Update(UseNamedParams).Where(WherePK);
    case ADialect of
      ddMSSQL:
        // Sintaxe MSSQL: UPDATE table SET col=val OUTPUT INSERTED.* WHERE ...
        Result := Format('UPDATE %s SET %s OUTPUT INSERTED.*%s',
          [Builder.GetTableName, Builder.FieldAndValuesToSQL, Builder.WhereToSQL]);

      ddPostgreSQL, ddSQLite, ddFirebird:
        Result := Builder.Build + sLineBreak + 'RETURNING *';
    else
      Result := Builder.Build;
    end;
  finally
    Builder.Free;
  end;
end;

class function TDMSQLBuilder.CreateDelete(AModel: TDeltaModel;
  ADialect: TDatabaseDialect; const WhereClause: string): string;
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

class function TDMSQLBuilder.CreateSelect(AModel: TDeltaModel;
  ADialect: TDatabaseDialect; const WhereClause: string): string;
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

class function TDMSQLBuilder.CreateCount(AModel: TDeltaModel;
  ADialect: TDatabaseDialect; const WhereClause: string): string;
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

end.
