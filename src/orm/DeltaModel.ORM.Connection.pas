unit DeltaModel.ORM.Connection;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Variants,
  DeltaModel.ORM.Types, DeltaModel.ORM.Interfaces;

type
  { TDeltaTransactionScope: RAII transaction guard that auto-rolls back if uncommitted }
  TDeltaTransactionScope = class(TInterfacedObject, IDeltaTransaction)
  private
    FConnection: IDeltaConnection;
    FActive: Boolean;
  public
    constructor Create(const AConnection: IDeltaConnection);
    destructor Destroy; override;
    procedure Commit;
    procedure Rollback;
    function IsActive: Boolean;
  end;

  { TBaseDeltaResultSet: Shared name-based convenience overloads over index accessors }
  TBaseDeltaResultSet = class(TInterfacedObject, IDeltaResultSet)
  protected
    function RequireFieldIndex(const AName: string): Integer;
  public
    function Next: Boolean; virtual; abstract;
    function EOF: Boolean; virtual; abstract;
    function IsEmpty: Boolean; virtual; abstract;
    function FieldCount: Integer; virtual; abstract;
    function FieldName(AIndex: Integer): string; virtual; abstract;
    function FindField(const AName: string): Integer; virtual;
    function FieldType(AIndex: Integer): TDeltaParamType; virtual; abstract;

    function IsNull(AIndex: Integer): Boolean; overload; virtual; abstract;
    function IsNull(const AName: string): Boolean; overload;
    function AsInteger(AIndex: Integer): Integer; overload; virtual; abstract;
    function AsInteger(const AName: string): Integer; overload;
    function AsLargeInt(AIndex: Integer): Int64; overload; virtual; abstract;
    function AsLargeInt(const AName: string): Int64; overload;
    function AsFloat(AIndex: Integer): Double; overload; virtual; abstract;
    function AsFloat(const AName: string): Double; overload;
    function AsBoolean(AIndex: Integer): Boolean; overload; virtual; abstract;
    function AsBoolean(const AName: string): Boolean; overload;
    function AsDateTime(AIndex: Integer): TDateTime; overload; virtual; abstract;
    function AsDateTime(const AName: string): TDateTime; overload;
    function AsString(AIndex: Integer): string; overload; virtual; abstract;
    function AsString(const AName: string): string; overload;
    function AsVariant(AIndex: Integer): Variant; overload; virtual; abstract;
    function AsVariant(const AName: string): Variant; overload;
    procedure Close; virtual; abstract;
  end;

  { TBaseDeltaConnection: Shared transaction and statement convenience methods }
  TBaseDeltaConnection = class(TInterfacedObject, IDeltaConnection)
  private
    FDialect: IDeltaDialect;
  public
    constructor Create(const ADialect: IDeltaDialect);

    function Dialect: IDeltaDialect; virtual;
    function DriverName: string; virtual; abstract;

    function Connected: Boolean; virtual; abstract;
    procedure Connect; virtual; abstract;
    procedure Disconnect; virtual; abstract;

    procedure StartTransaction; virtual; abstract;
    procedure Commit; virtual; abstract;
    procedure Rollback; virtual; abstract;
    function TransactionActive: Boolean; virtual; abstract;
    function BeginTransaction: IDeltaTransaction; virtual;
    procedure InTransaction(AProc: TDeltaTransactionProc); overload; virtual;
    procedure InTransaction(AProc: TDeltaTransactionStaticProc); overload; virtual;

    function CreateStatement(const ASQL: string = ''): IDeltaStatement; virtual; abstract;
    procedure ExecuteDirect(const ASQL: string); virtual;
    function ExecuteQuery(const ASQL: string): IDeltaResultSet; virtual;
    function ExecuteScalar(const ASQL: string): Variant; virtual;
    procedure GetTableNames(ATables: TStrings); virtual; abstract;
  end;

implementation

{ TDeltaTransactionScope }

constructor TDeltaTransactionScope.Create(const AConnection: IDeltaConnection);
begin
  inherited Create;
  FConnection := AConnection;
  FActive := False;
  if FConnection <> nil then
  begin
    if not FConnection.TransactionActive then
      FConnection.StartTransaction;
    FActive := True;
  end;
end;

destructor TDeltaTransactionScope.Destroy;
begin
  if FActive then
  begin
    try
      Rollback;
    except
    end;
  end;
  inherited Destroy;
end;

procedure TDeltaTransactionScope.Commit;
begin
  if FActive and (FConnection <> nil) then
  begin
    FConnection.Commit;
    FActive := False;
  end;
end;

procedure TDeltaTransactionScope.Rollback;
begin
  if FActive and (FConnection <> nil) then
  begin
    FConnection.Rollback;
    FActive := False;
  end;
end;

function TDeltaTransactionScope.IsActive: Boolean;
begin
  Result := FActive and (FConnection <> nil) and FConnection.TransactionActive;
end;

{ TBaseDeltaResultSet }

function TBaseDeltaResultSet.FindField(const AName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FieldCount - 1 do
    if SameText(FieldName(I), AName) then
      Exit(I);
end;

function TBaseDeltaResultSet.RequireFieldIndex(const AName: string): Integer;
begin
  Result := FindField(AName);
  if Result < 0 then
    raise Exception.CreateFmt('Field "%s" not found in result set.', [AName]);
end;

function TBaseDeltaResultSet.IsNull(const AName: string): Boolean;
begin
  Result := IsNull(RequireFieldIndex(AName));
end;

function TBaseDeltaResultSet.AsInteger(const AName: string): Integer;
begin
  Result := AsInteger(RequireFieldIndex(AName));
end;

function TBaseDeltaResultSet.AsLargeInt(const AName: string): Int64;
begin
  Result := AsLargeInt(RequireFieldIndex(AName));
end;

function TBaseDeltaResultSet.AsFloat(const AName: string): Double;
begin
  Result := AsFloat(RequireFieldIndex(AName));
end;

function TBaseDeltaResultSet.AsBoolean(const AName: string): Boolean;
begin
  Result := AsBoolean(RequireFieldIndex(AName));
end;

function TBaseDeltaResultSet.AsDateTime(const AName: string): TDateTime;
begin
  Result := AsDateTime(RequireFieldIndex(AName));
end;

function TBaseDeltaResultSet.AsString(const AName: string): string;
begin
  Result := AsString(RequireFieldIndex(AName));
end;

function TBaseDeltaResultSet.AsVariant(const AName: string): Variant;
begin
  Result := AsVariant(RequireFieldIndex(AName));
end;

{ TBaseDeltaConnection }

constructor TBaseDeltaConnection.Create(const ADialect: IDeltaDialect);
begin
  inherited Create;
  FDialect := ADialect;
end;

function TBaseDeltaConnection.Dialect: IDeltaDialect;
begin
  Result := FDialect;
end;

function TBaseDeltaConnection.BeginTransaction: IDeltaTransaction;
begin
  Result := TDeltaTransactionScope.Create(Self);
end;

procedure TBaseDeltaConnection.InTransaction(AProc: TDeltaTransactionProc);
begin
  StartTransaction;
  try
    if Assigned(AProc) then
      AProc();
    Commit;
  except
    Rollback;
    raise;
  end;
end;

procedure TBaseDeltaConnection.InTransaction(AProc: TDeltaTransactionStaticProc);
begin
  StartTransaction;
  try
    if Assigned(AProc) then
      AProc();
    Commit;
  except
    Rollback;
    raise;
  end;
end;

procedure TBaseDeltaConnection.ExecuteDirect(const ASQL: string);
var
  Stmt: IDeltaStatement;
begin
  Stmt := CreateStatement(ASQL);
  Stmt.Execute;
end;

function TBaseDeltaConnection.ExecuteQuery(const ASQL: string): IDeltaResultSet;
var
  Stmt: IDeltaStatement;
begin
  Stmt := CreateStatement(ASQL);
  Result := Stmt.ExecuteQuery;
end;

function TBaseDeltaConnection.ExecuteScalar(const ASQL: string): Variant;
var
  Stmt: IDeltaStatement;
begin
  Stmt := CreateStatement(ASQL);
  Result := Stmt.ExecuteScalar;
end;

end.
