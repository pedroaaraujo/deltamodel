unit DeltaModel.ORM.Interfaces;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Variants,
  DeltaModel.Fields, DeltaModel.ORM.Types, DatabaseURLParser;

type
  TDeltaTransactionProc = procedure of object;
  TDeltaTransactionStaticProc = procedure;

  IDeltaConnection = interface;

  { IDeltaTransaction: Safe RAII transaction scope }
  IDeltaTransaction = interface
    ['{B1A456C8-1122-3344-5566-778899AABBCC}']
    procedure Commit;
    procedure Rollback;
    function IsActive: Boolean;
  end;

  { IDeltaResultSet: Driver-agnostic forward-only cursor }
  IDeltaResultSet = interface
    ['{7D91B3A4-5F10-4D32-9C7A-102030405001}']
    function Next: Boolean;
    function EOF: Boolean;
    function IsEmpty: Boolean;
    function FieldCount: Integer;
    function FieldName(AIndex: Integer): string;
    function FieldType(AIndex: Integer): TDeltaParamType;
    function FindField(const AName: string): Integer;

    function IsNull(AIndex: Integer): Boolean; overload;
    function IsNull(const AName: string): Boolean; overload;

    function AsInteger(AIndex: Integer): Integer; overload;
    function AsInteger(const AName: string): Integer; overload;
    function AsLargeInt(AIndex: Integer): Int64; overload;
    function AsLargeInt(const AName: string): Int64; overload;
    function AsFloat(AIndex: Integer): Double; overload;
    function AsFloat(const AName: string): Double; overload;
    function AsBoolean(AIndex: Integer): Boolean; overload;
    function AsBoolean(const AName: string): Boolean; overload;
    function AsDateTime(AIndex: Integer): TDateTime; overload;
    function AsDateTime(const AName: string): TDateTime; overload;
    function AsString(AIndex: Integer): string; overload;
    function AsString(const AName: string): string; overload;
    function AsVariant(AIndex: Integer): Variant; overload;
    function AsVariant(const AName: string): Variant; overload;

    procedure Close;
  end;

  { IDeltaStatement: Parameterized SQL execution }
  IDeltaStatement = interface
    ['{7D91B3A4-5F10-4D32-9C7A-102030405002}']
    function GetSQL: string;
    procedure SetSQL(const AValue: string);
    function GetParams: TDeltaParams;

    function Execute: Int64;
    function ExecuteQuery: IDeltaResultSet;
    function ExecuteScalar: Variant;
    procedure ClearParams;

    property SQL: string read GetSQL write SetSQL;
    property Params: TDeltaParams read GetParams;
  end;

  { IDeltaSQLDialect: DML syntax, escaping, pagination and bulk SQL per DBMS }
  IDeltaSQLDialect = interface
    ['{7D91B3A4-5F10-4D32-9C7A-102030405003}']
    function Name: string;
    function DefaultPort: Integer;
    function QuoteIdentifier(const AIdentifier: string): string;
    function QuoteStringLiteral(const AValue: string): string;
    function FormatBooleanLiteral(AValue: Boolean): string;
    function OpToSQL(AOp: TComparisonOp): string;
    function SupportsReturning: Boolean;
    function SupportsMultiRowBulkInsert: Boolean;
    function UseReturningForAutoInc: Boolean;
    function LastInsertIdSQL: string;
    function HealthCheckSQL: string;
    function BuildSelect(const AFieldsSQL, ATableWithAlias, AJoinsSQL,
      AWhereSQL, AGroupBySQL, AOrderBySQL: string;
      ALimit, AOffset: Integer): string;
    function BuildInsertReturning(const ATable, AFieldsSQL, AValuesSQL: string;
      const AReturningCols: string = '*'): string;
    function BuildUpdateReturning(const ATable, ASetSQL, AWhereSQL: string): string;
    function BuildBulkInsert(const ATable: string; AFieldNames: TStrings;
      ARowCount: Integer): string;
  end;

  { IDeltaDDLDialect: DDL generation rules and type mapping per DBMS }
  IDeltaDDLDialect = interface
    ['{7D91B3A4-5F10-4D32-9C7A-102030405004}']
    function CreateTablePrefix: string;
    function InlineConstraintsInCreateTable: Boolean;
    function SupportsAlterAddForeignKey: Boolean;
    function StripNotNullOnAlterAdd: Boolean;
    function CreateIndexPrefix(AUnique: Boolean): string;
    function SupportsOnUpdateClause: Boolean;
    function OnRestrictClause: string;
    function RequiresCommitAfterInfraDDL: Boolean;
    function SanitizeIdentifier(const AName: string): string;
    function MigrationVersionColumnType: string;
    function ColumnTypeDDL(AField: TDeltaField;
      const AServerInfo: TDeltaServerInfo): string;
    function PrimitiveColumnTypeDDL(AKind: TTypeKind;
      const AServerInfo: TDeltaServerInfo): string;
    function FormatColumnDDL(AField: TDeltaField; const ASQLType: string;
      AIsPK, AIsAutoInc: Boolean;
      const AServerInfo: TDeltaServerInfo): string;
    function ForeignKeyDDL(const ATable, AColumn, ARefTable, ARefColumn: string;
      AOnDelete, AOnUpdate: TForeignKeyEvent): string;
    procedure ExtraAutoIncrementStatements(const ATableName: string;
      AField: TDeltaField; const AServerInfo: TDeltaServerInfo;
      AStatements: TStrings);
  end;

  { IDeltaSchemaIntrospector: Metadata queries per DBMS }
  IDeltaSchemaIntrospector = interface
    ['{7D91B3A4-5F10-4D32-9C7A-102030405005}']
    function DetectServerInfo(AConn: IDeltaConnection): TDeltaServerInfo;
    procedure GetIndexNames(AConn: IDeltaConnection; const ATable: string;
      ANames: TStrings);
    function ForeignKeyExists(AConn: IDeltaConnection; const ATable, AColumn,
      ARefTable, ARefColumn: string): Boolean;
  end;

  { IDeltaDialect: Complete SQL + DDL + Introspection bundle for one DBMS }
  IDeltaDialect = interface
    ['{7D91B3A4-5F10-4D32-9C7A-102030405006}']
    function Name: string;
    function SQL: IDeltaSQLDialect;
    function DDL: IDeltaDDLDialect;
    function Introspector: IDeltaSchemaIntrospector;
  end;

  { IDeltaConnection: Primary driver-agnostic connection and transaction handle }
  IDeltaConnection = interface
    ['{7D91B3A4-5F10-4D32-9C7A-102030405007}']
    function Dialect: IDeltaDialect;
    function DriverName: string;

    function Connected: Boolean;
    procedure Connect;
    procedure Disconnect;

    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    function TransactionActive: Boolean;
    function BeginTransaction: IDeltaTransaction;
    procedure InTransaction(AProc: TDeltaTransactionProc); overload;
    procedure InTransaction(AProc: TDeltaTransactionStaticProc); overload;

    function CreateStatement(const ASQL: string = ''): IDeltaStatement;
    procedure ExecuteDirect(const ASQL: string);
    function ExecuteQuery(const ASQL: string): IDeltaResultSet;
    function ExecuteScalar(const ASQL: string): Variant;
    procedure GetTableNames(ATables: TStrings);
  end;

  { IDeltaDriver: Pluggable database transport/driver factory }
  IDeltaDriver = interface
    ['{7D91B3A4-5F10-4D32-9C7A-102030405008}']
    function Name: string;
    function SupportsProtocol(const AProtocol: string): Boolean;
    function CreateConnection(const AConfig: TDatabaseConfig;
      const ADialect: IDeltaDialect): IDeltaConnection;
  end;

implementation

end.
