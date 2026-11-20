{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit deltamodel_pkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  DeltaModel, DeltaModel.Fields, DeltaModel.List, DeltaSerialization,
  DeltaValidator, DeltaAPISchema, DeltaModelMessages, DeltaModel.DataSetConverter,
  DatabaseURLParser, DeltaModel.SQLBuilder, DeltaModel.ORM.Types,
  DeltaModel.ORM.Interfaces, DeltaModel.ORM.Connection, DeltaModel.ORM.DDL,
  DeltaModel.ORM.DML, DeltaModel.ORM.Schema, DeltaModel.ORM.Pool,
  DeltaModel.Codec, DeltaModel.Codec.Bson, DeltaModel.Codec.Cbor,
  DeltaModel.Codec.Xml, DeltaModel.Codec.Csv,
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('deltamodel_pkg', @Register);
end.
