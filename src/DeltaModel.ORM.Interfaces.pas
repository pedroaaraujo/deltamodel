unit DeltaModel.ORM.Interfaces;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, DeltaModel.ORM.Types;

type
  IDeltaORMEngine = interface
  ['{5FCD6178-213B-42D9-8712-33B953EEDACD}']
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    function NewDataset: TSQLQuery;
    function ExecuteQuery(const ASQL: string): TDataSet;
    function TransactionActive: Boolean;
    procedure ExecuteDirect(const ASQL: string);
    function Dialect: TDatabaseDialect;
    function Connection: TSQLConnector;
  end;

implementation

end.

