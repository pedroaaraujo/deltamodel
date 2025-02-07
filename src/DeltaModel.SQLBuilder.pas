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

    function FieldAndValuesToSQL: string;
    function GetTableName: string;
    function ExtractFieldsAndValues: Boolean;
    function FieldsToSQL: string;
    function ValuesToSQL: string;
    function WhereToSQL: string;
    function QuoteIdentifier(const AIdentifier: string): string;
    function BuildLimitOffset: string;
    class function WhereClausePK(AModel: TDeltaModel): string;
  public
    constructor Create(AModel: TDeltaModel; ADialect: TDatabaseDialect);
    destructor Destroy; override;

    function Select: TDMSQLBuilder;
    function Insert: TDMSQLBuilder;
    function Update: TDMSQLBuilder;
    function Delete: TDMSQLBuilder;
    function Where(const ACondition: string): TDMSQLBuilder;
    function OrderBy(const AField: string; const ADescending: Boolean = False): TDMSQLBuilder;
    function Limit(const ALimit: Integer): TDMSQLBuilder;
    function Offset(const AOffset: Integer): TDMSQLBuilder;
    function GroupBy(const AField: string): TDMSQLBuilder;
    function Build: string;

    class function CreateInsert(AModel: TDeltaModel; ADialect: TDatabaseDialect): string; static;
    class function CreateInsertReturning(AModel: TDeltaModel; ADialect: TDatabaseDialect; const WhereClause: string = ''): string; static;
    class function CreateUpdate(AModel: TDeltaModel; ADialect: TDatabaseDialect; const WhereClause: string = ''): string; static;
    class function CreateUpdateReturning(AModel: TDeltaModel; ADialect: TDatabaseDialect; const WhereClause: string = ''): string; static;
    class function CreateDelete(AModel: TDeltaModel; ADialect: TDatabaseDialect; const WhereClause: string = ''): string; static;
    class function CreateSelect(AModel: TDeltaModel; ADialect: TDatabaseDialect; const WhereClause: string = ''): string; static;
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
  FWhereConditions.Delimiter := ',';
  FWhereConditions.StrictDelimiter := True;
  FLimit := -1;
  FOffset := -1;
  FGroupBy := '';
end;

destructor TDMSQLBuilder.Destroy;
begin
  FFields.Free;
  FValues.Free;
  FWhereConditions.Free;
  inherited Destroy;
end;

function TDMSQLBuilder.GetTableName: string;
begin
  Result := QuoteIdentifier(FModel.TableName);
end;

function TDMSQLBuilder.ExtractFieldsAndValues: Boolean;
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
          if Obj.IsNull then
            FValues.Add('NULL')
          else
          if VarIsNumeric(Obj.Value) then
            FValues.Add(Obj.ToString.Replace(',', '',[rfReplaceAll]))
          else
            FValues.Add(Obj.ToString.QuotedString);
        end;
      end
      else
      begin
        if VarIsNull(PropValue) or (PropInfo^.SetProc = nil) then
          Continue;

        FFields.Add(QuoteIdentifier(PropInfo^.Name));
        if VarIsNumeric(PropValue) then
          FValues.Add(StrToFloat(VarToStr(PropValue), FS).ToString().Replace(',', '',[rfReplaceAll]))
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
  if FWhereConditions.Count > 0 then
    Result := ' WHERE ' + String.Join(' AND ', FWhereConditions.ToStringArray)
  else
    Result := '';
end;

function TDMSQLBuilder.QuoteIdentifier(const AIdentifier: string): string;
begin
  case FDialect of
    ddPostgreSQL: Result := '"' + AIdentifier + '"';
  else
    Result := AIdentifier;
  end;
end;

function TDMSQLBuilder.BuildLimitOffset: string;
begin
  case FDialect of
    ddPostgreSQL, ddSQLite:
    begin
      Result := '';
      if FLimit > -1 then
        Result := Result + ' LIMIT ' + IntToStr(FLimit);
      if FOffset > -1 then
        Result := Result + ' OFFSET ' + IntToStr(FOffset);
    end;
    ddFirebird:
    begin
      if FLimit > -1 then
      begin
        if FOffset > -1 then
          Result := Format(' ROWS %d TO %d', [FOffset + 1, FOffset + FLimit])
        else
          Result := Format(' ROWS 1 TO %d', [FLimit]);
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
  if PropCount = 0 then Exit;

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
            SL.Add('(' + Obj.FieldName + ' = ' + QuotedStr(Obj.ToString) + ')')
          else
            SL.Add('(' + Obj.FieldName + ' = ' + VarToStr(Obj.Value) + ')');
        end;
      end;
    end;
    Result := SL.Text;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
    SL.Free;
  end;
end;

function TDMSQLBuilder.Select: TDMSQLBuilder;
begin
  FCommand := 'SELECT';
  FFields.Clear;
  Result := Self;
end;

function TDMSQLBuilder.Insert: TDMSQLBuilder;
begin
  FCommand := 'INSERT INTO';
  FFields.Clear;
  FValues.Clear;
  ExtractFieldsAndValues;
  Result := Self;
end;

function TDMSQLBuilder.Update: TDMSQLBuilder;
begin
  FCommand := 'UPDATE';
  FFields.Clear;
  FValues.Clear;
  ExtractFieldsAndValues;
  Result := Self;
end;

function TDMSQLBuilder.Delete: TDMSQLBuilder;
begin
  FCommand := 'DELETE FROM';
  Result := Self;
end;

function TDMSQLBuilder.Where(const ACondition: string): TDMSQLBuilder;
begin
  FWhereConditions.Add(ACondition);
  Result := Self;
end;

function TDMSQLBuilder.OrderBy(const AField: string; const ADescending: Boolean): TDMSQLBuilder;
begin
  if ADescending then
    FOrderBy := QuoteIdentifier(AField) + ' DESC'
  else
    FOrderBy := QuoteIdentifier(AField) + ' ASC';
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

function TDMSQLBuilder.GroupBy(const AField: string): TDMSQLBuilder;
begin
  FGroupBy := QuoteIdentifier(AField);
  Result := Self;
end;

function TDMSQLBuilder.Build: string;
var
  vFields, vOrderBy, vGroupBy: string;
begin
  case FCommand of
    'SELECT':
    begin
      vFields := IfThen(FFields.Count > 0, FieldsToSQL, '*');
      vOrderBy := IfThen(FOrderBy <> '', ' ORDER BY ' + FOrderBy, '');
      vGroupBy := IfThen(FGroupBy <> '', ' GROUP BY ' + FGroupBy, '');

      Result := Format('%s %s FROM %s%s%s%s%s',
        [FCommand, vFields, GetTableName, WhereToSQL,
         vGroupBy, vOrderBy, BuildLimitOffset]);
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
    Result := SL.DelimitedText;
  finally
    SL.Free;
  end;
end;

class function TDMSQLBuilder.CreateInsert(AModel: TDeltaModel; ADialect: TDatabaseDialect): string;
begin
  Result := TDMSQLBuilder
    .Create(AModel, ADialect)
    .Insert
    .Build;
end;

class function TDMSQLBuilder.CreateInsertReturning(AModel: TDeltaModel;
  ADialect: TDatabaseDialect; const WhereClause: string): string;
begin
  Result := CreateInsert(AModel, ADialect);
  Result :=
    Result + sLineBreak +
    'RETURNING (*)';
end;

class function TDMSQLBuilder.CreateUpdate(AModel: TDeltaModel; ADialect: TDatabaseDialect; const WhereClause: string): string;
var
  WherePK: string;
begin
  WherePK := WhereClausePK(AModel);
  Result := TDMSQLBuilder
    .Create(AModel, ADialect)
    .Update
    .Where(IfThen(WhereClause.IsEmpty, WherePK, WhereClause))
    .Build;
end;

class function TDMSQLBuilder.CreateUpdateReturning(AModel: TDeltaModel;
  ADialect: TDatabaseDialect; const WhereClause: string): string;
begin
  Result := CreateUpdate(AModel, ADialect, WhereClause);
  Result :=
    Result + sLineBreak +
    'RETURNING (*)';
end;

class function TDMSQLBuilder.CreateDelete(AModel: TDeltaModel; ADialect: TDatabaseDialect; const WhereClause: string): string;
var
  WherePK: string;
begin
  WherePK := WhereClausePK(AModel);
  Result := TDMSQLBuilder
    .Create(AModel, ADialect)
    .Delete
    .Where(IfThen(WhereClause.IsEmpty, WherePK, WhereClause))
    .Build;
end;

class function TDMSQLBuilder.CreateSelect(AModel: TDeltaModel; ADialect: TDatabaseDialect; const WhereClause: string): string;
begin
  Result := TDMSQLBuilder
    .Create(AModel, ADialect)
    .Select
    .Where(WhereClause)
    .Build;
end;

end.

