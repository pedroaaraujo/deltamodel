unit DeltaModel.ORM.Driver.Mock;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Variants, fgl,
  DatabaseURLParser, DeltaModel.ORM.Types, DeltaModel.ORM.Interfaces,
  DeltaModel.ORM.Connection, DeltaModel.ORM.Registry;

type
  TMockRow = array of Variant;

  { TMockResultTable: In-memory tabular result builder for unit tests }
  TMockResultTable = class
  private
    FColumnNames: array of string;
    FColumnTypes: array of TDeltaParamType;
    FRows: array of TMockRow;
  public
    procedure AddColumn(const AName: string; AType: TDeltaParamType = dptString);
    procedure AddRow(const AValues: array of Variant);
    function ColumnCount: Integer;
    function RowCount: Integer;
    function ColumnName(AIndex: Integer): string;
    function ColumnType(AIndex: Integer): TDeltaParamType;
    function Cell(ARow, ACol: Integer): Variant;
  end;

  TMockStatementLog = record
    SQL: string;
    Params: TDeltaParams;
  end;

  TMockQueryCallback = function(const ASQL: string; AParams: TDeltaParams): TMockResultTable of object;

  { IMockConnectionAccess: Inspection & scripting interface for unit tests }
  IMockConnectionAccess = interface
    ['{8E4F01A2-1122-4455-8899-AABBCCDD1102}']
    function ExecutedSQL: TStrings;
    function TableNamesList: TStrings;
    function StartTxCount: Integer;
    function CommitCount: Integer;
    function RollbackCount: Integer;
    procedure QueueResult(ATable: TMockResultTable);
    procedure QueueScalar(const AValue: Variant);
    procedure QueueRowsAffected(ACount: Int64);
    procedure SetFailNextExecute(AFail: Boolean; const AMessage: string = 'Simulated execution error');
    procedure SetQueryHandler(AHandler: TMockQueryCallback);
    function LastParams: TDeltaParams;
    procedure ResetHistory;
  end;

  { TMockResultSet }
  TMockResultSet = class(TBaseDeltaResultSet)
  private
    FTable: TMockResultTable;
    FOwnsTable: Boolean;
    FRowIndex: Integer;
  public
    constructor Create(ATable: TMockResultTable; AOwnsTable: Boolean = True);
    destructor Destroy; override;

    function Next: Boolean; override;
    function EOF: Boolean; override;
    function IsEmpty: Boolean; override;
    function FieldCount: Integer; override;
    function FieldName(AIndex: Integer): string; override;
    function FieldType(AIndex: Integer): TDeltaParamType; override;

    function IsNull(AIndex: Integer): Boolean; override;
    function AsInteger(AIndex: Integer): Integer; override;
    function AsLargeInt(AIndex: Integer): Int64; override;
    function AsFloat(AIndex: Integer): Double; override;
    function AsBoolean(AIndex: Integer): Boolean; override;
    function AsDateTime(AIndex: Integer): TDateTime; override;
    function AsString(AIndex: Integer): string; override;
    function AsVariant(AIndex: Integer): Variant; override;
    procedure Close; override;
  end;

  TMockConnection = class;

  { TMockStatement }
  TMockStatement = class(TInterfacedObject, IDeltaStatement)
  private
    FConn: TMockConnection;
    FSQL: string;
    FParams: TDeltaParams;
  public
    constructor Create(AConn: TMockConnection; const ASQL: string = '');
    destructor Destroy; override;

    function GetSQL: string;
    procedure SetSQL(const AValue: string);
    function GetParams: TDeltaParams;

    function Execute: Int64;
    function ExecuteQuery: IDeltaResultSet;
    function ExecuteScalar: Variant;
    procedure ClearParams;
  end;

  { TMockConnection }
  TMockConnection = class(TBaseDeltaConnection, IMockConnectionAccess)
  private
    FConnected: Boolean;
    FTransactionActive: Boolean;
    FStartTxCount: Integer;
    FCommitCount: Integer;
    FRollbackCount: Integer;
    FExecutedSQL: TStringList;
    FTableNames: TStringList;
    FQueuedResults: TFPList;
    FQueuedScalars: array of Variant;
    FQueuedRowsAffected: array of Int64;
    FLastParams: TDeltaParams;
    FFailNextExecute: Boolean;
    FFailMessage: string;
    FQueryHandler: TMockQueryCallback;
  protected
    procedure RecordExecution(const ASQL: string; AParams: TDeltaParams);
    function PopResultTable(const ASQL: string; AParams: TDeltaParams): TMockResultTable;
    function PopScalar(const ASQL: string; AParams: TDeltaParams): Variant;
    function PopRowsAffected: Int64;
  public
    constructor Create(const ADialect: IDeltaDialect);
    destructor Destroy; override;

    function DriverName: string; override;
    function Connected: Boolean; override;
    procedure Connect; override;
    procedure Disconnect; override;

    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
    function TransactionActive: Boolean; override;

    function CreateStatement(const ASQL: string = ''): IDeltaStatement; override;
    procedure GetTableNames(ATables: TStrings); override;

    { IMockConnectionAccess }
    function ExecutedSQL: TStrings;
    function TableNamesList: TStrings;
    function StartTxCount: Integer;
    function CommitCount: Integer;
    function RollbackCount: Integer;
    procedure QueueResult(ATable: TMockResultTable);
    procedure QueueScalar(const AValue: Variant);
    procedure QueueRowsAffected(ACount: Int64);
    procedure SetFailNextExecute(AFail: Boolean; const AMessage: string = 'Simulated execution error');
    procedure SetQueryHandler(AHandler: TMockQueryCallback);
    function LastParams: TDeltaParams;
    procedure ResetHistory;
  end;

  { TMockDriver }
  TMockDriver = class(TInterfacedObject, IDeltaDriver)
  public
    function Name: string;
    function SupportsProtocol(const AProtocol: string): Boolean;
    function CreateConnection(const AConfig: TDatabaseConfig;
      const ADialect: IDeltaDialect): IDeltaConnection;
  end;

implementation

{ TMockResultTable }

procedure TMockResultTable.AddColumn(const AName: string; AType: TDeltaParamType);
begin
  SetLength(FColumnNames, Length(FColumnNames) + 1);
  SetLength(FColumnTypes, Length(FColumnTypes) + 1);
  FColumnNames[High(FColumnNames)] := AName;
  FColumnTypes[High(FColumnTypes)] := AType;
end;

procedure TMockResultTable.AddRow(const AValues: array of Variant);
var
  RowIdx, I: Integer;
begin
  RowIdx := Length(FRows);
  SetLength(FRows, RowIdx + 1);
  SetLength(FRows[RowIdx], Length(FColumnNames));
  for I := 0 to High(FRows[RowIdx]) do
  begin
    if I <= High(AValues) then
      FRows[RowIdx][I] := AValues[I]
    else
      FRows[RowIdx][I] := Null;
  end;
end;

function TMockResultTable.ColumnCount: Integer;
begin
  Result := Length(FColumnNames);
end;

function TMockResultTable.RowCount: Integer;
begin
  Result := Length(FRows);
end;

function TMockResultTable.ColumnName(AIndex: Integer): string;
begin
  Result := FColumnNames[AIndex];
end;

function TMockResultTable.ColumnType(AIndex: Integer): TDeltaParamType;
begin
  Result := FColumnTypes[AIndex];
end;

function TMockResultTable.Cell(ARow, ACol: Integer): Variant;
begin
  Result := FRows[ARow][ACol];
end;

{ TMockResultSet }

constructor TMockResultSet.Create(ATable: TMockResultTable; AOwnsTable: Boolean);
begin
  inherited Create;
  FTable := ATable;
  FOwnsTable := AOwnsTable;
  FRowIndex := -1;
end;

destructor TMockResultSet.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TMockResultSet.Next: Boolean;
begin
  if FTable = nil then
    Exit(False);
  Inc(FRowIndex);
  Result := FRowIndex < FTable.RowCount;
end;

function TMockResultSet.EOF: Boolean;
begin
  Result := (FTable = nil) or (FTable.RowCount = 0) or (FRowIndex >= FTable.RowCount);
end;

function TMockResultSet.IsEmpty: Boolean;
begin
  Result := (FTable = nil) or (FTable.RowCount = 0);
end;

function TMockResultSet.FieldCount: Integer;
begin
  if FTable <> nil then
    Result := FTable.ColumnCount
  else
    Result := 0;
end;

function TMockResultSet.FieldName(AIndex: Integer): string;
begin
  Result := FTable.ColumnName(AIndex);
end;

function TMockResultSet.FieldType(AIndex: Integer): TDeltaParamType;
begin
  Result := FTable.ColumnType(AIndex);
end;

function TMockResultSet.IsNull(AIndex: Integer): Boolean;
var
  V: Variant;
begin
  if (FRowIndex < 0) or (FRowIndex >= FTable.RowCount) then
    Exit(True);
  V := FTable.Cell(FRowIndex, AIndex);
  Result := VarIsNull(V) or VarIsEmpty(V);
end;

function TMockResultSet.AsInteger(AIndex: Integer): Integer;
begin
  if IsNull(AIndex) then Exit(0);
  Result := Integer(FTable.Cell(FRowIndex, AIndex));
end;

function TMockResultSet.AsLargeInt(AIndex: Integer): Int64;
begin
  if IsNull(AIndex) then Exit(0);
  Result := Int64(FTable.Cell(FRowIndex, AIndex));
end;

function TMockResultSet.AsFloat(AIndex: Integer): Double;
begin
  if IsNull(AIndex) then Exit(0);
  Result := Double(FTable.Cell(FRowIndex, AIndex));
end;

function TMockResultSet.AsBoolean(AIndex: Integer): Boolean;
begin
  if IsNull(AIndex) then Exit(False);
  Result := Boolean(FTable.Cell(FRowIndex, AIndex));
end;

function TMockResultSet.AsDateTime(AIndex: Integer): TDateTime;
begin
  if IsNull(AIndex) then Exit(0);
  Result := VarToDateTime(FTable.Cell(FRowIndex, AIndex));
end;

function TMockResultSet.AsString(AIndex: Integer): string;
begin
  if IsNull(AIndex) then Exit('');
  Result := VarToStr(FTable.Cell(FRowIndex, AIndex));
end;

function TMockResultSet.AsVariant(AIndex: Integer): Variant;
begin
  if (FRowIndex < 0) or (FRowIndex >= FTable.RowCount) then
    Exit(Null);
  Result := FTable.Cell(FRowIndex, AIndex);
end;

procedure TMockResultSet.Close;
begin
  if FOwnsTable and (FTable <> nil) then
    FreeAndNil(FTable)
  else
    FTable := nil;
end;

{ TMockStatement }

constructor TMockStatement.Create(AConn: TMockConnection; const ASQL: string);
begin
  inherited Create;
  FConn := AConn;
  FSQL := ASQL;
  FParams := TDeltaParams.Create;
end;

destructor TMockStatement.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

function TMockStatement.GetSQL: string;
begin
  Result := FSQL;
end;

procedure TMockStatement.SetSQL(const AValue: string);
begin
  FSQL := AValue;
end;

function TMockStatement.GetParams: TDeltaParams;
begin
  Result := FParams;
end;

function TMockStatement.Execute: Int64;
begin
  FConn.Connect;
  FConn.RecordExecution(FSQL, FParams);
  Result := FConn.PopRowsAffected;
end;

function TMockStatement.ExecuteQuery: IDeltaResultSet;
var
  Tbl: TMockResultTable;
begin
  FConn.Connect;
  FConn.RecordExecution(FSQL, FParams);
  Tbl := FConn.PopResultTable(FSQL, FParams);
  Result := TMockResultSet.Create(Tbl, True);
end;

function TMockStatement.ExecuteScalar: Variant;
begin
  FConn.Connect;
  FConn.RecordExecution(FSQL, FParams);
  Result := FConn.PopScalar(FSQL, FParams);
end;

procedure TMockStatement.ClearParams;
begin
  FParams.Clear;
end;

{ TMockConnection }

constructor TMockConnection.Create(const ADialect: IDeltaDialect);
begin
  inherited Create(ADialect);
  FConnected := False;
  FTransactionActive := False;
  FStartTxCount := 0;
  FCommitCount := 0;
  FRollbackCount := 0;
  FExecutedSQL := TStringList.Create;
  FTableNames := TStringList.Create;
  FQueuedResults := TFPList.Create;
  SetLength(FQueuedScalars, 0);
  SetLength(FQueuedRowsAffected, 0);
  FLastParams := TDeltaParams.Create;
  FFailNextExecute := False;
  FFailMessage := 'Simulated execution error';
  FQueryHandler := nil;
end;

destructor TMockConnection.Destroy;
var
  I: Integer;
begin
  for I := 0 to FQueuedResults.Count - 1 do
    TObject(FQueuedResults[I]).Free;
  FQueuedResults.Free;
  FExecutedSQL.Free;
  FTableNames.Free;
  FLastParams.Free;
  inherited Destroy;
end;

function TMockConnection.DriverName: string;
begin
  Result := 'mock';
end;

function TMockConnection.Connected: Boolean;
begin
  Result := FConnected;
end;

procedure TMockConnection.Connect;
begin
  FConnected := True;
end;

procedure TMockConnection.Disconnect;
begin
  if FTransactionActive then
    Rollback;
  FConnected := False;
end;

procedure TMockConnection.StartTransaction;
begin
  Connect;
  if not FTransactionActive then
  begin
    FTransactionActive := True;
    Inc(FStartTxCount);
  end;
end;

procedure TMockConnection.Commit;
begin
  if FTransactionActive then
  begin
    FTransactionActive := False;
    Inc(FCommitCount);
  end;
end;

procedure TMockConnection.Rollback;
begin
  if FTransactionActive then
  begin
    FTransactionActive := False;
    Inc(FRollbackCount);
  end;
end;

function TMockConnection.TransactionActive: Boolean;
begin
  Result := FTransactionActive;
end;

function TMockConnection.CreateStatement(const ASQL: string): IDeltaStatement;
begin
  Result := TMockStatement.Create(Self, ASQL);
end;

procedure TMockConnection.GetTableNames(ATables: TStrings);
begin
  if ATables <> nil then
    ATables.Assign(FTableNames);
end;

procedure TMockConnection.RecordExecution(const ASQL: string; AParams: TDeltaParams);
begin
  if FFailNextExecute then
  begin
    FFailNextExecute := False;
    raise Exception.Create(FFailMessage);
  end;
  FExecutedSQL.Add(ASQL);
  FLastParams.Assign(AParams);
end;

function TMockConnection.PopResultTable(const ASQL: string; AParams: TDeltaParams): TMockResultTable;
begin
  if Assigned(FQueryHandler) then
  begin
    Result := FQueryHandler(ASQL, AParams);
    if Result <> nil then
      Exit;
  end;

  if FQueuedResults.Count > 0 then
  begin
    Result := TMockResultTable(FQueuedResults[0]);
    FQueuedResults.Delete(0);
    Exit;
  end;

  Result := TMockResultTable.Create;
end;

function TMockConnection.PopScalar(const ASQL: string; AParams: TDeltaParams): Variant;
var
  I: Integer;
  Tbl: TMockResultTable;
begin
  if Length(FQueuedScalars) > 0 then
  begin
    Result := FQueuedScalars[0];
    for I := 0 to High(FQueuedScalars) - 1 do
      FQueuedScalars[I] := FQueuedScalars[I + 1];
    SetLength(FQueuedScalars, Length(FQueuedScalars) - 1);
    Exit;
  end;

  if FQueuedResults.Count > 0 then
  begin
    Tbl := PopResultTable(ASQL, AParams);
    try
      if (Tbl.RowCount > 0) and (Tbl.ColumnCount > 0) then
        Result := Tbl.Cell(0, 0)
      else
        Result := Null;
    finally
      Tbl.Free;
    end;
    Exit;
  end;

  Result := Null;
end;

function TMockConnection.PopRowsAffected: Int64;
var
  I: Integer;
begin
  if Length(FQueuedRowsAffected) > 0 then
  begin
    Result := FQueuedRowsAffected[0];
    for I := 0 to High(FQueuedRowsAffected) - 1 do
      FQueuedRowsAffected[I] := FQueuedRowsAffected[I + 1];
    SetLength(FQueuedRowsAffected, Length(FQueuedRowsAffected) - 1);
  end
  else
    Result := 1;
end;

function TMockConnection.ExecutedSQL: TStrings;
begin
  Result := FExecutedSQL;
end;

function TMockConnection.TableNamesList: TStrings;
begin
  Result := FTableNames;
end;

function TMockConnection.StartTxCount: Integer;
begin
  Result := FStartTxCount;
end;

function TMockConnection.CommitCount: Integer;
begin
  Result := FCommitCount;
end;

function TMockConnection.RollbackCount: Integer;
begin
  Result := FRollbackCount;
end;

procedure TMockConnection.QueueResult(ATable: TMockResultTable);
begin
  if ATable <> nil then
    FQueuedResults.Add(ATable);
end;

procedure TMockConnection.QueueScalar(const AValue: Variant);
begin
  SetLength(FQueuedScalars, Length(FQueuedScalars) + 1);
  FQueuedScalars[High(FQueuedScalars)] := AValue;
end;

procedure TMockConnection.QueueRowsAffected(ACount: Int64);
begin
  SetLength(FQueuedRowsAffected, Length(FQueuedRowsAffected) + 1);
  FQueuedRowsAffected[High(FQueuedRowsAffected)] := ACount;
end;

procedure TMockConnection.SetFailNextExecute(AFail: Boolean; const AMessage: string);
begin
  FFailNextExecute := AFail;
  FFailMessage := AMessage;
end;

procedure TMockConnection.SetQueryHandler(AHandler: TMockQueryCallback);
begin
  FQueryHandler := AHandler;
end;

function TMockConnection.LastParams: TDeltaParams;
begin
  Result := FLastParams;
end;

procedure TMockConnection.ResetHistory;
var
  I: Integer;
begin
  FExecutedSQL.Clear;
  FLastParams.Clear;
  FStartTxCount := 0;
  FCommitCount := 0;
  FRollbackCount := 0;
  SetLength(FQueuedScalars, 0);
  SetLength(FQueuedRowsAffected, 0);
  for I := 0 to FQueuedResults.Count - 1 do
    TObject(FQueuedResults[I]).Free;
  FQueuedResults.Clear;
end;

{ TMockDriver }

function TMockDriver.Name: string;
begin
  Result := 'mock';
end;

function TMockDriver.SupportsProtocol(const AProtocol: string): Boolean;
begin
  Result := SameText(Trim(AProtocol), 'mock');
end;

function TMockDriver.CreateConnection(const AConfig: TDatabaseConfig;
  const ADialect: IDeltaDialect): IDeltaConnection;
begin
  Result := TMockConnection.Create(ADialect);
end;

initialization
  TDeltaORMRegistry.RegisterDriver(TMockDriver.Create);

end.
