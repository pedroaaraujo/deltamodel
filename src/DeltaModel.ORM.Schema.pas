unit DeltaModel.ORM.Schema;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, DB, SQLDB, DeltaModel, DeltaModel.ORM.Interfaces,
  DeltaModel.ORM.DDL, DeltaModel.ORM.Types;

type

  { TDeltaORMSchema }

  TDeltaORMSchema = class
  private type TTableList = specialize TFPGObjectList<TDeltaModel>;
  private
    FConnection: IDeltaORMEngine;
    FModels: TTableList;
    FDBTables: TStringList;
    FSQL: TStringList;
    FConstraitList: TStringList;
    procedure CreateTables;
    procedure AlterTables;
    procedure CreateConstraints;
  public
    property SQL: TStringList read FSQL;
    procedure RegisterModel(Model: TDeltaModel); overload;
    procedure RegisterModel(ModelClass: TDeltaModelClass); overload;
    procedure PrepareDB(Persist: Boolean);
    constructor Create(AConnection: IDeltaORMEngine);
    destructor Destroy; override;
  end;

implementation

{ TDeltaORMSchema }

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

    FSQL.Add(TDDLBuilder.CreateTableAndFields(Obj, FConstraitList, FConnection.Dialect));
  end;
end;

procedure TDeltaORMSchema.AlterTables;
var
  I, F: Integer;
  Obj: TDeltaModel;
  FieldList: TStringList;
  DS: TSQLQuery;
begin
  for I := 0 to Pred(FModels.Count) do
  begin
    Obj := FModels.Items[I];
    FieldList := TStringList.Create;
    try
      if (FDBTables.IndexOf(Obj.TableName) = -1) then
        Continue;

      DS := FConnection.NewDataset;
      try
        DS.SQL.Text :=
          'SELECT * FROM ' + Obj.TableName + sLineBreak +
          'WHERE 1 = 0';
        DS.Open;
        for F := 0 to Pred(DS.FieldCount) do
          FieldList.Add(DS.Fields[F].FieldName);
        DS.Close;
      finally
        DS.Free;
      end;

      FSQL.Add(TDDLBuilder.CreateFields(Obj, FConnection.Dialect, FieldList, FConstraitList));
    finally
      FieldList.Free;
    end;
  end;
end;

procedure TDeltaORMSchema.CreateConstraints;
var
  I: Integer;
  S: string;
begin
  for I := 0 to Pred(FConstraitList.Count) do
  begin
    S := FConstraitList[I];
    if not S.IsEmpty then
    begin
      FSQL.Add(S);
    end;
  end;
end;

procedure TDeltaORMSchema.RegisterModel(Model: TDeltaModel);
begin
  FModels.Add(Model);
end;

procedure TDeltaORMSchema.RegisterModel(ModelClass: TDeltaModelClass);
begin
  RegisterModel(ModelClass.Create);
end;

procedure TDeltaORMSchema.PrepareDB(Persist: Boolean);
var
  I: Integer;
  S: string;
begin
  FSQL.Clear;
  FConstraitList.Clear;

  CreateTables;

  AlterTables;

  CreateConstraints;

  if Persist and (FSQL.Count > 0) then
  begin
    if FConnection.Dialect = ddMySQL then
      FConnection.ExecuteDirect('SET FOREIGN_KEY_CHECKS = 0;');

    for I := 0 to Pred(FSQL.Count) do
    begin
      S := FSQL.Strings[I];
      if S.Trim.IsEmpty then Continue;

      try
        FConnection.ExecuteDirect(S);

        if FConnection.TransactionActive then
        begin
          FConnection.Commit;
          FConnection.StartTransaction;
        end;
      except
        on E: Exception do
        begin
          if IsConsole then
            Writeln(Format('Aviso DDL ignorado: %s', [E.Message]));

          if FConnection.TransactionActive then
          begin
            FConnection.Rollback;
            FConnection.StartTransaction;
          end;
        end;
      end;
    end;

    if FConnection.Dialect = ddMySQL then
      FConnection.ExecuteDirect('SET FOREIGN_KEY_CHECKS = 1;');

    if FConnection.TransactionActive then
      FConnection.Commit;
  end;
end;

constructor TDeltaORMSchema.Create(AConnection: IDeltaORMEngine);
begin
  FConnection := AConnection;
  FModels := TTableList.Create;
  FDBTables := TStringList.Create;
  FConstraitList := TStringList.Create;
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
  FConstraitList.Free;
  inherited Destroy;
end;

end.

