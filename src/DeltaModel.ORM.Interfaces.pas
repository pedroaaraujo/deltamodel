unit DeltaModel.ORM.Interfaces;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, DeltaModel.ORM.Types;

type
  TDeltaTransactionProc = procedure of object;
  TDeltaTransactionStaticProc = procedure;

  { IDeltaTransaction: Safe RAII transaction scope }
  IDeltaTransaction = interface
    ['{B1A456C8-1122-3344-5566-778899AABBCC}']
    procedure Commit;
    procedure Rollback;
    function IsActive: Boolean;
  end;

  IDeltaORMEngine = interface
  ['{5FCD6178-213B-42D9-8712-33B953EEDACD}']
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    function TransactionActive: Boolean;
    function BeginTransaction: IDeltaTransaction;
    procedure InTransaction(AProc: TDeltaTransactionProc); overload;
    procedure InTransaction(AProc: TDeltaTransactionStaticProc); overload;

    function Connection: TSQLConnector;
    function Dialect: TDatabaseDialect;

    function NewDataset: TSQLQuery;
    function ExecuteQuery(const ASQL: string): TDataSet;
    procedure ExecuteDirect(const ASQL: string);
    function ExecuteScalar(const ASQL: string): Variant;
  end;

implementation

end.
