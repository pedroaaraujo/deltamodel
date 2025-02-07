unit DeltaModel.ORM.Schema;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, DB, DeltaModel, DeltaModel.ORM.Connection,
  DeltaModel.ORM.DDL;

type

  { TDeltaORMSchema }

  TDeltaORMSchema = class
  private type TTableList = specialize TFPGObjectList<TDeltaModel>;
  private
    FConnection: TDeltaORMEngine;
    FModels: TTableList;
    FDBTables: TStringList;
    FSQL: TStringList;
    procedure CreateTables;
    procedure AlterTables;
    procedure CreateForeignKeys;
  public
    property SQL: TStringList read FSQL;
    procedure RegisterModel(Model: TDeltaModel);
    procedure CreateDB;
    procedure PrepareDB(Persist: Boolean);
    constructor Create(AConnection: TDeltaORMEngine);
    destructor Destroy; override;
  end;

implementation

{ TDeltaORMSchema }

procedure TDeltaORMSchema.CreateDB;
begin
  try
    FConnection.Connection.Connected := True;
  except
    on E: EDatabaseError do
    begin
      FConnection.Connection.CreateDB;
      FConnection.Connection.Connected := True;
    end;
  end;

  if FConnection.Connection.Connected then
      PrepareDB(True);
end;

procedure TDeltaORMSchema.CreateTables;
var
  I: Integer;
  Obj: TDeltaModel;
begin
  for I := 0 to Pred(FModels.Count) do
  begin
    Obj := FModels.Items[I];
    if (FDBTables.IndexOf(Obj.TableName) > -1) then
      Continue;

    FSQL.Add(TDDLBuilder.CreateTableAndFields(Obj, FConnection.Dialect));
  end;
end;

procedure TDeltaORMSchema.AlterTables;
var
  I, F: Integer;
  Obj: TDeltaModel;
  FieldList: TStringList;
begin
  for I := 0 to Pred(FModels.Count) do
  begin
    Obj := FModels.Items[I];
    FieldList := TStringList.Create;
    try
      if (FDBTables.IndexOf(Obj.TableName) = -1) then
        Continue;

      try
        FConnection.Connection.GetFieldNames(Obj.TableName, FieldList);
      except
        with FConnection.NewDataset do
        try
          SQL.Text :=
            'SELECT * FROM ' + Obj.TableName + sLineBreak +
            'WHERE 1 = 0';
          Open;
          for F := 0 to Pred(FieldCount) do
          begin
            FieldList.Add(Fields[F].FieldName);
          end;
        finally
          Free;
        end;
      end;

      FSQL.Add(TDDLBuilder.CreateFields(Obj, FConnection.Dialect, FieldList));
    finally
      FieldList.Free;
    end;
  end;
end;

procedure TDeltaORMSchema.CreateForeignKeys;
begin
  // Implementar lógica para criar chaves estrangeiras
end;

procedure TDeltaORMSchema.RegisterModel(Model: TDeltaModel);
begin
  FModels.Add(Model);
end;

procedure TDeltaORMSchema.PrepareDB(Persist: Boolean);
var
  I: Integer;
  S: string;
begin
  FSQL.Clear;
  CreateTables;
  AlterTables;
  CreateForeignKeys;

  if Persist and (FSQL.Count > 0) then
  begin
    for I := 0 to Pred(FSQL.Count) do
    begin
      S := FSQL.Strings[I];
      if S.IsEmpty then Continue;
      FConnection.Connection.ExecuteDirect(S);
    end;
    if FConnection.Connection.Transaction.Active then
      FConnection.Connection.Transaction.Commit;
  end;
end;

constructor TDeltaORMSchema.Create(AConnection: TDeltaORMEngine);
begin
  FConnection := AConnection;
  FModels := TTableList.Create;
  FDBTables := TStringList.Create;
  FSQL := TStringList.Create;
  FSQL.Delimiter := ';';
  FSQL.StrictDelimiter := True;

  FConnection.Connection.GetTableNames(FDBTables);
end;

destructor TDeltaORMSchema.Destroy;
begin
  FModels.Free;
  FDBTables.Free;
  FSQL.Free;
  inherited Destroy;
end;

end.

