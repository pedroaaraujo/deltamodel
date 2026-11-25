{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit deltamodel_pkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  DeltaModel, DeltaModel.Fields, DeltaModel.List, DeltaSerialization, 
  DeltaValidator, DeltaAPISchema, DeltaModelMessages, 
  DeltaModel.DataSetConverter, DatabaseURLParser, DeltaModel.ORM.Types, 
  DeltaModel.ORM.Interfaces, DeltaModel.ORM.Registry, 
  DeltaModel.ORM.Connection, DeltaModel.ORM.Criteria, DeltaModel.ORM.Hydrator, 
  DeltaModel.SQLBuilder, DeltaModel.ORM.DDL, DeltaModel.ORM.Persister, 
  DeltaModel.ORM.DML, DeltaModel.ORM.Schema, DeltaModel.ORM.Pool, 
  DeltaModel.Generics, DeltaModel.ORM.Dialect.Base, 
  DeltaModel.ORM.Dialect.PostgreSQL, DeltaModel.ORM.Dialect.SQLite, 
  DeltaModel.ORM.Dialect.MySQL, DeltaModel.ORM.Dialect.Firebird, 
  DeltaModel.ORM.Dialect.MSSQL, DeltaModel.ORM.Dialect.Oracle, 
  DeltaModel.ORM.Dialects, DeltaModel.ORM.Driver.SQLDB, 
  DeltaModel.ORM.Driver.Mock, DeltaModel.Codec, DeltaModel.Codec.Bson, 
  DeltaModel.Codec.Cbor, DeltaModel.Codec.Xml, DeltaModel.Codec.Csv, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('deltamodel_pkg', @Register);
end.
