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
    FConstraitList: TStringList;
    procedure CreateTables;
    procedure AlterTables;
    procedure CreateConstraints;
  public
    property SQL: TStringList read FSQL;
    procedure RegisterModel(Model: TDeltaModel); overload;
    procedure RegisterModel(ModelClass: TDeltaModelClass); overload;
    procedure PrepareDB(Persist: Boolean);
    constructor Create(AConnection: TDeltaORMEngine);
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
begin
  for I := 0 to Pred(FModels.Count) do
  begin
    Obj := FModels.Items[I];
    FieldList := TStringList.Create;
    try
      if (FDBTables.IndexOf(Obj.TableName) = -1) then
        Continue;

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

