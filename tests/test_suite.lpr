program test_suite;

{$mode ObjFPC}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  cwstring,
  {$ENDIF}
  Classes, SysUtils, Variants, TypInfo, DB, SQLDB, sqlite3conn, fpjson, jsonparser,
  DeltaModel, DeltaModel.Fields, DeltaModel.List, DeltaModel.ORM.Connection,
  DeltaModel.ORM.DML, DeltaModel.ORM.DDL, DeltaModel.ORM.Schema, DeltaModel.SQLBuilder,
  DeltaModel.ORM.Types, DeltaModel.ORM.Interfaces, DatabaseURLParser, DeltaValidator,
  DeltaSerialization, DeltaAPISchema, DeltaModel.Metadata, DeltaModel.DataSetConverter,
  DeltaModel.JsonStreamer, DeltaModel.Generics, DeltaModel.JsonParser, DeltaModel.Messages,
  DeltaModel.Codec, DeltaModel.Codec.Bson, DeltaModel.Codec.Cbor,
  DeltaModel.Codec.Xml, DeltaModel.Codec.Csv;

{==============================================================================}
{ Test Runner Infrastructure                                                   }
{==============================================================================}

var
  GTotalTests: Integer = 0;
  GPassedTests: Integer = 0;
  GFailedTests: Integer = 0;
  GCurrentSuite: string = '';

procedure Suite(const AName: string);
begin
  GCurrentSuite := AName;
  WriteLn('');
  WriteLn('=== ' + AName + ' ===');
end;

procedure AssertTrue(const ACondition: Boolean; const AMsg: string);
begin
  Inc(GTotalTests);
  if ACondition then
  begin
    Inc(GPassedTests);
    WriteLn('  [PASS] ' + AMsg);
  end
  else
  begin
    Inc(GFailedTests);
    WriteLn('  [FAIL] ' + AMsg);
  end;
end;

procedure AssertFalse(const ACondition: Boolean; const AMsg: string);
begin
  AssertTrue(not ACondition, AMsg);
end;

procedure AssertEquals(const AExpected, AActual: string; const AMsg: string);
begin
  AssertTrue(AExpected = AActual, Format('%s (esperado: "%s", obtido: "%s")', [AMsg, AExpected, AActual]));
end;

procedure AssertEqualsInt(const AExpected, AActual: Int64; const AMsg: string);
begin
  AssertTrue(AExpected = AActual, Format('%s (esperado: %d, obtido: %d)', [AMsg, AExpected, AActual]));
end;

{==============================================================================}
{ Test Models                                                                  }
{==============================================================================}

type
  { Complete model with diverse types for CRUD, DDL, Swagger, and Serialization }
  TProductModel = class(TDeltaModel)
  private
    FId: TDFIntRequired;
    FName: TDFStringRequired;
    FPrice: TDFCurrencyRequired;
    FDescription: TDFTextNull;
    FBigCode: TDFInt64Null;
    FActive: TDFBoolRequired;
  published
    property Id: TDFIntRequired read FId write FId;
    property Name: TDFStringRequired read FName write FName;
    property Price: TDFCurrencyRequired read FPrice write FPrice;
    property Description: TDFTextNull read FDescription write FDescription;
    property BigCode: TDFInt64Null read FBigCode write FBigCode;
    property Active: TDFBoolRequired read FActive write FActive;
  public
    procedure AfterConstruction; override;
    procedure Validate; override;
  end;

  TProductGenericList = specialize TGDeltaModelList<TProductModel>;
  TProductPaginatedGenericList = specialize TGDeltaModelPaginatedList<TProductModel>;

  { Approach B Model: Fluent Class Schema + Native Primitive Properties }
  TFastProductModel = class(TDeltaModel)
  private
    FId: Int64;
    FName: string;
    FPrice: Double;
    FCost: Double;
    FStock: Integer;
    FActive: Boolean;
  public
    class procedure ConfigureSchema(ASchema: TDeltaModelSchema); override;
  published
    property Id: Int64 read FId write FId;
    property Name: string read FName write FName;
    property Price: Double read FPrice write FPrice;
    property Cost: Double read FCost write FCost;
    property Stock: Integer read FStock write FStock;
    property Active: Boolean read FActive write FActive;
  end;

  TFastProductList = specialize TGDeltaModelList<TFastProductModel>;
  TFastProductQuery = specialize TDeltaQuery<TFastProductModel>;

  { Model with Declarative Schema Validations }
  TValidatedCustomerModel = class(TDeltaModel)
  private
    FId: Int64;
    FName: string;
    FAge: Integer;
    FEmail: string;
    FCpf: string;
    FScore: Double;
  public
    class procedure ConfigureSchema(ASchema: TDeltaModelSchema); override;
  published
    property Id: Int64 read FId write FId;
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
    property Email: string read FEmail write FEmail;
    property Cpf: string read FCpf write FCpf;
    property Score: Double read FScore write FScore;
  end;

  { Models for 1:N Eager Loading }
  TTestOrderItem = class;

  TTestOrder = class(TDeltaModel)
  private
    FId: Int64;
    FCustomerName: string;
    FItems: TDFHasMany;
  public
    class procedure ConfigureSchema(ASchema: TDeltaModelSchema); override;
  published
    property Id: Int64 read FId write FId;
    property CustomerName: string read FCustomerName write FCustomerName;
    property Items: TDFHasMany read FItems write FItems;
  end;

  TTestOrderItem = class(TDeltaModel)
  private
    FId: Int64;
    FOrderId: Int64;
    FProductName: string;
    FQuantity: Integer;
  public
    class procedure ConfigureSchema(ASchema: TDeltaModelSchema); override;
  published
    property Id: Int64 read FId write FId;
    property OrderId: Int64 read FOrderId write FOrderId;
    property ProductName: string read FProductName write FProductName;
    property Quantity: Integer read FQuantity write FQuantity;
  end;

class procedure TValidatedCustomerModel.ConfigureSchema(ASchema: TDeltaModelSchema);
begin
  inherited ConfigureSchema(ASchema);
  ASchema.Table('validated_customers');
  ASchema.Field('Id').Column('id').AsPK.AutoInc;
  ASchema.Field('Name').Column('name').MinLength(3).MaxLength(50).NotEmpty;
  ASchema.Field('Age').Column('age').Range(18, 120);
  ASchema.Field('Email').Column('email').Email;
  ASchema.Field('Cpf').Column('cpf').CPF;
  ASchema.Field('Score').Column('score').Positive;
end;

class procedure TTestOrder.ConfigureSchema(ASchema: TDeltaModelSchema);
begin
  inherited ConfigureSchema(ASchema);
  ASchema.Table('test_orders');
  ASchema.Field('Id').Column('id').AsPK.AutoInc;
  ASchema.Field('CustomerName').Column('customer_name').Required;
  ASchema.HasMany('Items', TTestOrderItem, 'order_id');
end;

class procedure TTestOrderItem.ConfigureSchema(ASchema: TDeltaModelSchema);
begin
  inherited ConfigureSchema(ASchema);
  ASchema.Table('test_order_items');
  ASchema.Field('Id').Column('id').AsPK.AutoInc;
  ASchema.Field('OrderId').Column('order_id').Required;
  ASchema.Field('ProductName').Column('product_name').Required;
  ASchema.Field('Quantity').Column('quantity').Required;
end;

class procedure TFastProductModel.ConfigureSchema(ASchema: TDeltaModelSchema);
begin
  ASchema.Table('fast_products');
  ASchema.Field('Id').Column('id').AsPK.AutoInc;
  ASchema.Field('Name').Column('name').Required.Size(150);
  ASchema.Field('Price').Column('price').Required;
  ASchema.Field('Cost').Column('cost').Nullable;
  ASchema.Field('Stock').Column('stock').Required;
  ASchema.Field('Active').Column('active').Required;
end;

procedure TProductModel.AfterConstruction;
begin
  inherited AfterConstruction;
  TableName := 'products';

  Id.FieldName := 'id';
  Id.DBOptions := [dboPrimaryKey, dboAutoInc];

  Name.FieldName := 'name';
  Name.Size := 100;
  Name.DBOptions := [dboInsert, dboUpdate];

  Price.FieldName := 'price';
  Price.DBOptions := [dboInsert, dboUpdate];

  Description.FieldName := 'description';
  Description.DBOptions := [dboInsert, dboUpdate];

  BigCode.FieldName := 'big_code';
  BigCode.DBOptions := [dboInsert, dboUpdate];

  Active.FieldName := 'active';
  Active.DBOptions := [dboInsert, dboUpdate];
end;

procedure TProductModel.Validate;
begin
  inherited Validate;
  Validator.AddField('name', Name.Value).AddValidator(TValidatorItemNotEmpty.Create);
  Validator.AddField('price', Price.Value).AddValidator(TValidatorItemGreaterThanZero.Create);
end;

type
  { Model with full lifecycle tracking }
  THookModel = class(TDeltaModel)
  private
    FId: TDFIntRequired;
    FName: TDFStringRequired;
  published
    property Id: TDFIntRequired read FId write FId;
    property Name: TDFStringRequired read FName write FName;
  public
    HookLog: TStringList;
    FailOnBeforeInsert: Boolean;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure BeforeInsert; override;
    procedure AfterInsert; override;
    procedure BeforeUpdate; override;
    procedure AfterUpdate; override;
    procedure BeforeDelete; override;
    procedure AfterDelete; override;
    procedure BeforeSave; override;
    procedure AfterSave; override;
  end;

procedure THookModel.AfterConstruction;
begin
  inherited AfterConstruction;
  TableName := 'hook_items';
  HookLog := TStringList.Create;
  FailOnBeforeInsert := False;

  Id.FieldName := 'id';
  Id.DBOptions := [dboPrimaryKey, dboAutoInc];

  Name.FieldName := 'name';
  Name.DBOptions := [dboInsert, dboUpdate];
end;

procedure THookModel.BeforeDestruction;
begin
  HookLog.Free;
  inherited BeforeDestruction;
end;

procedure THookModel.BeforeInsert;
begin
  inherited BeforeInsert;
  HookLog.Add('BeforeInsert');
  if FailOnBeforeInsert then
    raise Exception.Create('Falha simulada em BeforeInsert');
end;

procedure THookModel.AfterInsert;
begin
  inherited AfterInsert;
  HookLog.Add('AfterInsert');
end;

procedure THookModel.BeforeUpdate;
begin
  inherited BeforeUpdate;
  HookLog.Add('BeforeUpdate');
end;

procedure THookModel.AfterUpdate;
begin
  inherited AfterUpdate;
  HookLog.Add('AfterUpdate');
end;

procedure THookModel.BeforeDelete;
begin
  inherited BeforeDelete;
  HookLog.Add('BeforeDelete');
end;

procedure THookModel.AfterDelete;
begin
  inherited AfterDelete;
  HookLog.Add('AfterDelete');
end;

procedure THookModel.BeforeSave;
begin
  inherited BeforeSave;
  HookLog.Add('BeforeSave');
end;

procedure THookModel.AfterSave;
begin
  inherited AfterSave;
  HookLog.Add('AfterSave');
end;

type
  { Model with Unique constraints and Indexes }
  TConstraintItem = class(TDeltaModel)
  private
    FId: TDFIntRequired;
    FEmail: TDFStringRequired;
    FTenantId: TDFIntRequired;
    FCode: TDFStringRequired;
    FCreatedAt: TDFDateTimeNull;
    FScore: TDFIntNull;
  published
    property Id: TDFIntRequired read FId write FId;
    property Email: TDFStringRequired read FEmail write FEmail;
    property TenantId: TDFIntRequired read FTenantId write FTenantId;
    property Code: TDFStringRequired read FCode write FCode;
    property CreatedAt: TDFDateTimeNull read FCreatedAt write FCreatedAt;
    property Score: TDFIntNull read FScore write FScore;
  public
    procedure AfterConstruction; override;
    procedure Configure; override;
  end;

procedure TConstraintItem.AfterConstruction;
begin
  inherited AfterConstruction;
  TableName := 'constraint_items';
  Id.FieldName := 'id';
  Id.DBOptions := [dboPrimaryKey, dboAutoInc];

  Email.FieldName := 'email';
  Email.Size := 100;
  Email.IsUnique := True;

  TenantId.FieldName := 'tenant_id';
  Code.FieldName := 'code';
  Code.Size := 50;

  CreatedAt.FieldName := 'created_at';
  CreatedAt.IsIndexed := True;

  Score.FieldName := 'score';
end;

procedure TConstraintItem.Configure;
begin
  inherited Configure;
  AddUniqueConstraint('uq_tenant_code', ['tenant_id', 'code']);
  AddIndex('ix_tenant_created', ['tenant_id', 'created_at']);
  AddCheckConstraint('chk_score_pos', 'score >= 0');
end;

type
  { Dependent 1:1 Model }
  TProfileModel = class(TDeltaModel)
  private
    FId: TDFIntRequired;
    FBio: TDFStringNull;
    FTwitter: TDFStringNull;
  published
    property Id: TDFIntRequired read FId write FId;
    property Bio: TDFStringNull read FBio write FBio;
    property Twitter: TDFStringNull read FTwitter write FTwitter;
  public
    procedure AfterConstruction; override;
  end;

procedure TProfileModel.AfterConstruction;
begin
  inherited AfterConstruction;
  TableName := 'profiles';
  Id.FieldName := 'id';
  Id.DBOptions := [dboPrimaryKey, dboAutoInc];
  Bio.FieldName := 'bio';
  Bio.Size := 200;
  Twitter.FieldName := 'twitter';
  Twitter.Size := 50;
end;

type
  { Primary Model with TDFHasOne }
  TUserWithProfileModel = class(TDeltaModel)
  private
    FId: TDFIntRequired;
    FUsername: TDFStringRequired;
    FProfile: TDFHasOne;
  published
    property Id: TDFIntRequired read FId write FId;
    property Username: TDFStringRequired read FUsername write FUsername;
    property Profile: TDFHasOne read FProfile write FProfile;
  public
    procedure AfterConstruction; override;
    procedure Configure; override;
  end;

procedure TUserWithProfileModel.AfterConstruction;
begin
  inherited AfterConstruction;
  TableName := 'users';
  Id.FieldName := 'id';
  Id.DBOptions := [dboPrimaryKey, dboAutoInc];
  Username.FieldName := 'username';
  Username.Size := 50;
end;

procedure TUserWithProfileModel.Configure;
begin
  inherited Configure;
  Profile.References(TProfileModel, 'user_id', 'id');
end;

{==============================================================================}
{ Test 1: Database URL Parser                                                  }
{==============================================================================}

procedure TestDatabaseURLParser;
var
  Cfg: TDatabaseConfig;
begin
  Suite('1. DatabaseURLParser');

  // SQLite caminhos relativos e absolutos
  Cfg := ParseDatabaseURL('sqlite://data/test.db');
  AssertEquals('SQLite3', Cfg.Protocol, 'SQLite3 protocolo normalizado');
  AssertEquals('data/test.db', Cfg.Database, 'SQLite caminho relativo preservado');

  Cfg := ParseDatabaseURL('sqlite:///./data/test.db');
  AssertEquals('./data/test.db', Cfg.Database, 'SQLite caminho relativo explicito ./ preservado');

  Cfg := ParseDatabaseURL('sqlite:///dados.db');
  AssertEquals('dados.db', Cfg.Database, 'SQLite arquivo relativo simples com 3 barras');

  Cfg := ParseDatabaseURL('sqlite:////var/lib/app.db');
  AssertEquals('/var/lib/app.db', Cfg.Database, 'SQLite caminho absoluto unix com 4 barras');

  Cfg := ParseDatabaseURL('sqlite:///var/lib/app.db');
  AssertEquals('/var/lib/app.db', Cfg.Database, 'SQLite caminho absoluto unix com 3 barras');

  Cfg := ParseDatabaseURL('sqlite:///C:/data/test.db');
  AssertEquals('C:/data/test.db', Cfg.Database, 'SQLite caminho Windows com drive');

  Cfg := ParseDatabaseURL('sqlite:///:memory:');
  AssertEquals(':memory:', Cfg.Database, 'SQLite in-memory database');

  // PostgreSQL
  Cfg := ParseDatabaseURL('postgres://user:secret@db.local:5432/myapp?sslmode=require');
  AssertEquals('PostgreSQL', Cfg.Protocol, 'PostgreSQL protocolo');
  AssertEquals('user', Cfg.Username, 'PostgreSQL usuario');
  AssertEquals('secret', Cfg.Password, 'PostgreSQL senha');
  AssertEquals('db.local', Cfg.Host, 'PostgreSQL host');
  AssertEqualsInt(5432, Cfg.Port, 'PostgreSQL porta');
  AssertEquals('myapp', Cfg.Database, 'PostgreSQL database');
  AssertTrue(Assigned(Cfg.Params) and (Cfg.Params.Values['sslmode'] = 'require'), 'PostgreSQL params parsed');
  Cfg.Params.Free;

  // MySQL with parameters
  Cfg := ParseDatabaseURL('mysql://admin:123456@127.0.0.1:3306/shop');
  AssertEquals('MySQL 5.7', Cfg.Protocol, 'MySQL protocolo normalizado');
  AssertEquals('admin', Cfg.Username, 'MySQL usuario');
  AssertEquals('123456', Cfg.Password, 'MySQL senha');
  AssertEquals('127.0.0.1', Cfg.Host, 'MySQL host');
  AssertEqualsInt(3306, Cfg.Port, 'MySQL porta');
  AssertEquals('shop', Cfg.Database, 'MySQL database');
  if Assigned(Cfg.Params) then Cfg.Params.Free;

  // Firebird
  Cfg := ParseDatabaseURL('firebird://SYSDBA:masterkey@localhost:3050//opt/firebird/data/test.fdb');
  AssertEquals('Firebird', Cfg.Protocol, 'Firebird protocolo');
  AssertEquals('/opt/firebird/data/test.fdb', Cfg.Database, 'Firebird database path');
  if Assigned(Cfg.Params) then Cfg.Params.Free;
end;

{==============================================================================}
{ Test 2: Validation Engine                                                    }
{==============================================================================}

procedure TestValidators;
var
  ValItem: IDeltaValidatorItem;
  Res: TValid;
  Prod: TProductModel;
  ValCPF: TValidatorItemCPF;
  ValCNPJ: TValidatorItemCNPJ;
  ValEmail: TValidatorEmail;
  ValMinLen: TValidatorItemMinLength;
  ValMaxLen: TValidatorItemMaxLength;
  ValRegex: TValidatorItemRegex;
  ValBetween: TValidatorItemBetween;
  FailedValidation: Boolean;
begin
  Suite('2. Validation Engine');

  // CPF
  ValCPF := TValidatorItemCPF.Create;
  try
    // Known valid CPFs (unmasked and masked)
    Res := ValCPF.Validate('52998224725');
    AssertTrue(Res.OK, 'CPF válido sem formatação deve passar');

    Res := ValCPF.Validate('529.982.247-25');
    AssertTrue(Res.OK, 'CPF válido com máscara deve passar');

    // Invalid CPFs
    Res := ValCPF.Validate('52998224726');
    AssertFalse(Res.OK, 'CPF com dígito verificador errado deve falhar');

    Res := ValCPF.Validate('11111111111');
    AssertFalse(Res.OK, 'CPF com dígitos repetidos deve falhar');

    Res := ValCPF.Validate('123');
    AssertFalse(Res.OK, 'CPF com tamanho menor deve falhar');
  finally
    ValCPF.Free;
  end;

  // CNPJ (Numeric and Alphanumeric)
  ValCNPJ := TValidatorItemCNPJ.Create;
  try
    // Known valid numeric CNPJ (unmasked and masked)
    Res := ValCNPJ.Validate('11222333000181');
    AssertTrue(Res.OK, 'CNPJ numérico válido sem formatação deve passar');

    Res := ValCNPJ.Validate('11.222.333/0001-81');
    AssertTrue(Res.OK, 'CNPJ numérico válido com formatação deve passar');

    // Invalid numeric CNPJ
    Res := ValCNPJ.Validate('11222333000182');
    AssertFalse(Res.OK, 'CNPJ numérico com dígito verificador errado deve falhar');

    Res := ValCNPJ.Validate('00000000000000');
    AssertFalse(Res.OK, 'CNPJ com dígitos todos zeros deve falhar');

    // Valid alphanumeric CNPJ (new RFB standard)
    Res := ValCNPJ.Validate('12ABC34501DE35');
    AssertTrue(Res.OK, 'CNPJ alfanumérico válido sem formatação deve passar');

    Res := ValCNPJ.Validate('12.ABC.345/01DE-35');
    AssertTrue(Res.OK, 'CNPJ alfanumérico válido com formatação deve passar');

    Res := ValCNPJ.Validate('12.abc.345/01de-35');
    AssertTrue(Res.OK, 'CNPJ alfanumérico minúsculo deve passar');

    // Invalid alphanumeric CNPJ
    Res := ValCNPJ.Validate('12ABC34501DE36');
    AssertFalse(Res.OK, 'CNPJ alfanumérico com DV errado deve falhar');

    Res := ValCNPJ.Validate('AAAAAAAAAAAAAA');
    AssertFalse(Res.OK, 'CNPJ alfanumérico com todos caracteres iguais deve falhar');

    AssertTrue(IsValidCnpj('12.ABC.345/01DE-35'), 'Chamada direta IsValidCnpj deve passar');
  finally
    ValCNPJ.Free;
  end;

  // Email
  ValEmail := TValidatorEmail.Create;
  try
    Res := ValEmail.Validate('usuario@dominio.com.br');
    AssertTrue(Res.OK, 'Email válido simples deve passar');

    Res := ValEmail.Validate('usuario.teste+label@sub.dominio.org');
    AssertTrue(Res.OK, 'Email válido com subdomínio e alias deve passar');

    Res := ValEmail.Validate('invalido-sem-arroba');
    AssertFalse(Res.OK, 'Email sem @ deve falhar');

    Res := ValEmail.Validate('@semusuario.com');
    AssertFalse(Res.OK, 'Email sem usuário antes do @ deve falhar');

    Res := ValEmail.Validate('usuario@');
    AssertFalse(Res.OK, 'Email sem domínio após o @ deve falhar');
  finally
    ValEmail.Free;
  end;

  // UTF-8 Min e Max Length
  ValMinLen := TValidatorItemMinLength.Create(5);
  try
    Res := ValMinLen.Validate('12345');
    AssertTrue(Res.OK, 'MinLength 5 com string ASCII 5 chars deve passar');

    Res := ValMinLen.Validate('1234');
    AssertFalse(Res.OK, 'MinLength 5 com string ASCII 4 chars deve falhar');

    // 'Ações' has 5 visual characters but 7 UTF-8 bytes (ç and õ = 2 bytes each)
    Res := ValMinLen.Validate('Ações');
    AssertTrue(Res.OK, 'MinLength 5 com UTF-8 multibyte "Ações" (5 caracteres) deve passar');

    // 'Ação' has 4 visual characters
    Res := ValMinLen.Validate('Ação');
    AssertFalse(Res.OK, 'MinLength 5 com UTF-8 "Ação" (4 caracteres) deve falhar');
  finally
    ValMinLen.Free;
  end;

  ValMaxLen := TValidatorItemMaxLength.Create(5);
  try
    Res := ValMaxLen.Validate('Ações');
    AssertTrue(Res.OK, 'MaxLength 5 com UTF-8 "Ações" (5 caracteres) deve passar');

    Res := ValMaxLen.Validate('Ações+');
    AssertFalse(Res.OK, 'MaxLength 5 com 6 caracteres deve falhar');
  finally
    ValMaxLen.Free;
  end;

  // Regex
  ValRegex := TValidatorItemRegex.Create('^[A-Z]{3}-[0-9]{4}$');
  try
    Res := ValRegex.Validate('ABC-1234');
    AssertTrue(Res.OK, 'Regex placa antiga ABC-1234 deve passar');

    Res := ValRegex.Validate('abc-1234');
    AssertFalse(Res.OK, 'Regex com minúsculas deve falhar');

    Res := ValRegex.Validate('123-ABCD');
    AssertFalse(Res.OK, 'Regex fora do formato deve falhar');
  finally
    ValRegex.Free;
  end;

  // Between
  ValBetween := TValidatorItemBetween.Create(10.0, 50.0);
  try
    Res := ValBetween.Validate(10.0);
    AssertTrue(Res.OK, 'Between com valor no limite inferior deve passar');

    Res := ValBetween.Validate(30.0);
    AssertTrue(Res.OK, 'Between com valor intermediário deve passar');

    Res := ValBetween.Validate(50.0);
    AssertTrue(Res.OK, 'Between com valor no limite superior deve passar');

    Res := ValBetween.Validate(9.99);
    AssertFalse(Res.OK, 'Between com valor abaixo do mínimo deve falhar');

    Res := ValBetween.Validate(50.01);
    AssertFalse(Res.OK, 'Between com valor acima do máximo deve falhar');
  finally
    ValBetween.Free;
  end;

  // Model validation integration
  Prod := TProductModel.Create;
  try
    // Empty fields: must fail raising EDeltaValidation for Required fields
    FailedValidation := False;
    try
      Prod.Validate;
    except
      on E: EDeltaValidation do
        FailedValidation := True;
    end;
    AssertTrue(FailedValidation, 'Produto sem campos obrigatórios deve levantar EDeltaValidation');

    // Populating valid data
    Prod.Name.Value := 'Notebook Dell';
    Prod.Price.Value := 4500.50;
    Prod.Active.Value := True;
    Prod.Validate;
    Res := Prod.Validator.Validate;
    AssertTrue(Res.OK, 'Produto com nome e preço válidos deve passar na validação');

    // Zero or negative price: must fail business-rule validator
    Prod.Price.Value := 0;
    Prod.Validate;
    Res := Prod.Validator.Validate;
    AssertFalse(Res.OK, 'Produto com preço 0 deve falhar no validador GreaterThanZero');
  finally
    Prod.Free;
  end;
end;

{==============================================================================}
{ Test 3: Serialization (JSON, CSV, 64-bit Ints, UTF-8, Control Chars)         }
{==============================================================================}

procedure TestSerialization;
var
  P1, P2: TProductModel;
  PList, PList2: TDeltaModelList;
  JsonStr: string;
  CsvStr: string;
  BigVal: Int64;
begin
  Suite('3. Serialization (JSON, CSV, Int64, UTF-8)');

  P1 := TProductModel.Create;
  P2 := TProductModel.Create;
  try
    BigVal := 8589934592; // 2^33 (excede 32-bit Integer)
    P1.Id.Value := 42;
    P1.Name.Value := 'Monitor Ultrawide 34" LG';
    P1.Price.Value := 2899.90;
    P1.Description.Value := 'Linha 1: Monitor topo de linha' + sLineBreak + 'Linha 2: Resolução 4K (UTF-8: ação, coração, 100%)';
    P1.BigCode.Value := BigVal;
    P1.Active.Value := True;

    JsonStr := P1.ToJson;
    AssertTrue(Length(JsonStr) > 0, 'ToJson produziu string não vazia');
    AssertTrue(Pos('34\" LG', JsonStr) > 0, 'Aspas duplas foram devidamente escapadas');
    AssertTrue(Pos(IntToStr(BigVal), JsonStr) > 0, 'Int64 grande presente no JSON');

    // Desserializa em P2
    P2.FromJson(JsonStr);
    AssertEqualsInt(42, P2.Id.AsLargeInt, 'Id 42 desserializado corretamente');
    AssertEquals('Monitor Ultrawide 34" LG', P2.Name.AsString, 'Nome com aspas desserializado perfeitamente');
    AssertEqualsInt(BigVal, P2.BigCode.AsLargeInt, 'Int64 desserializado sem perda de precisão');
    AssertTrue(P2.Active.AsBoolean, 'Booleano true desserializado');
    AssertTrue(Pos('ação, coração', P2.Description.AsString) > 0, 'UTF-8 multibyte preservado na desserialização');

    // Clone test
    P2.Free;
    P2 := TProductModel(P1.Clone);
    AssertEquals(P1.Name.AsString, P2.Name.AsString, 'Clone copiou nome com precisão');
    AssertEqualsInt(P1.BigCode.AsLargeInt, P2.BigCode.AsLargeInt, 'Clone copiou BigCode int64 com precisão');
  finally
    P1.Free;
    P2.Free;
  end;

  // List ToCSV / FromCSV test
  PList := TDeltaModelList.Create;
  PList2 := TDeltaModelList.Create;
  try
    PList.SetDeltaModelClass(TProductModel);
    PList2.SetDeltaModelClass(TProductModel);

    P1 := TProductModel.Create;
    P1.Id.Value := 1;
    P1.Name.Value := 'Mouse Sem Fio';
    P1.Price.Value := 99.90;
    P1.Active.Value := True;
    PList.Add(P1);

    P1 := TProductModel.Create;
    P1.Id.Value := 2;
    P1.Name.Value := 'Teclado Mecânico';
    P1.Price.Value := 299.00;
    P1.Active.Value := True;
    PList.Add(P1);

    AssertEqualsInt(2, PList.Count, 'Lista contém 2 itens');

    // List ToJson / FromJson
    JsonStr := PList.ToJson;
    AssertTrue(Pos('Teclado Mecânico', JsonStr) > 0, 'Lista ToJson contém itens');

    PList2.FromJson(JsonStr);
    AssertEqualsInt(2, PList2.Count, 'Lista desserializada tem 2 itens');
    AssertEquals('Mouse Sem Fio', TProductModel(PList2[0]).Name.AsString, 'Item 0 recuperado via FromJson');
    AssertEquals('Teclado Mecânico', TProductModel(PList2[1]).Name.AsString, 'Item 1 recuperado via FromJson');

    // ToCSV / FromCSV
    CsvStr := PList.ToCSV(';');
    AssertTrue(Pos('Mouse Sem Fio', CsvStr) > 0, 'CSV gerado contém produtos');

    PList2.ClearList;
    PList2.FromCSV(CsvStr, ';');
    AssertEqualsInt(2, PList2.Count, 'Lista restaurada de CSV contém 2 itens');
    AssertEquals('Mouse Sem Fio', TProductModel(PList2[0]).Name.AsString, 'Item 0 restaurado via CSV');
  finally
    PList.Free;
    PList2.Free;
  end;
end;

{==============================================================================}
{ Test 4: OpenAPI / Swagger Schema Generation                                  }
{==============================================================================}

var
  GCapturedModelName, GCapturedModelSchema: string;

procedure MockSchemaRegisterProc(const AName, ASchemaJson: string);
begin
  GCapturedModelName := AName;
  GCapturedModelSchema := ASchemaJson;
end;

procedure TestSwaggerSchema;
var
  SchemaJson: string;
  Doc: TJSONObject;
  Props: TJSONObject;
  NameProp, PriceProp, BigCodeProp: TJSONObject;
begin
  Suite('4. OpenAPI / Swagger Schema Generation');

  SchemaJson := TProductModel.SwaggerSchema(False);
  Doc := TJSONObject(GetJSON(SchemaJson));
  try
    AssertEquals('object', Doc.Get('type', ''), 'Schema raiz é do tipo object');
      Props := Doc.Get('properties', TJSONObject(nil));
      AssertTrue(Assigned(Props), 'Schema contém seção properties');

      NameProp := Props.Get('Name', TJSONObject(nil));
      AssertTrue(Assigned(NameProp), 'Propriedade Name existe no schema');
      if Assigned(NameProp) then
        AssertEquals('string', NameProp.Get('type', ''), 'Tipo de Name é string');

      PriceProp := Props.Get('Price', TJSONObject(nil));
      AssertTrue(Assigned(PriceProp), 'Propriedade Price existe no schema');
      if Assigned(PriceProp) then
        AssertEquals('number', PriceProp.Get('type', ''), 'Tipo de Price é number');

      BigCodeProp := Props.Get('BigCode', TJSONObject(nil));
      AssertTrue(Assigned(BigCodeProp), 'Propriedade BigCode existe no schema');
      if Assigned(BigCodeProp) then
      begin
        AssertEquals('integer', BigCodeProp.Get('type', ''), 'Tipo de BigCode é integer');
        AssertEquals('int64', BigCodeProp.Get('format', ''), 'Formato de BigCode é int64');
      end;
    finally
      Doc.Free;
    end;

    // SwaggerSchemaObject test (explicit method)
    SchemaJson := TProductModel.SwaggerSchemaObject(False);
    Doc := TJSONObject(GetJSON(SchemaJson));
    try
      AssertEquals('object', Doc.Get('type', ''), 'SwaggerSchemaObject raiz é do tipo object');
      AssertTrue(Assigned(Doc.Get('properties', TJSONObject(nil))), 'SwaggerSchemaObject contém properties');
    finally
      Doc.Free;
    end;

    // SwaggerSchemaArray test (explicit method)
    SchemaJson := TProductModel.SwaggerSchemaArray(False);
    Doc := TJSONObject(GetJSON(SchemaJson));
    try
      AssertEquals('array', Doc.Get('type', ''), 'SwaggerSchemaArray raiz é do tipo array');
      AssertTrue(Assigned(Doc.Get('items', TJSONObject(nil))), 'SwaggerSchemaArray contém items');
      if Assigned(Doc.Get('items', TJSONObject(nil))) then
        AssertEquals('object', Doc.Get('items', TJSONObject(nil)).Get('type', ''), 'items de SwaggerSchemaArray é object');
    finally
      Doc.Free;
    end;

    // Paginated OpenAPI Schema test
    SchemaJson := TProductModel.SwaggerSchemaPaginated(False);
    Doc := TJSONObject(GetJSON(SchemaJson));
    try
      AssertEquals('object', Doc.Get('type', ''), 'Schema paginado raiz é do tipo object');
      Props := Doc.Get('properties', TJSONObject(nil));
      AssertTrue(Assigned(Props), 'Schema paginado contém seção properties');

      // items
      AssertTrue(Assigned(Props.Get('items', TJSONObject(nil))), 'Schema paginado contém items');
      if Assigned(Props.Get('items', TJSONObject(nil))) then
        AssertEquals('array', Props.Get('items', TJSONObject(nil)).Get('type', ''), 'items é do tipo array');

      // page, page_size, total_records, total_pages
      AssertTrue(Assigned(Props.Get('page', TJSONObject(nil))), 'Schema paginado contém page');
      AssertTrue(Assigned(Props.Get('page_size', TJSONObject(nil))), 'Schema paginado contém page_size');
      AssertTrue(Assigned(Props.Get('total_records', TJSONObject(nil))), 'Schema paginado contém total_records');
      AssertTrue(Assigned(Props.Get('total_pages', TJSONObject(nil))), 'Schema paginado contém total_pages');
    finally
      Doc.Free;
    end;

    // TASK-1214 tests: SchemaName, RegisterSwaggerSchema, and SwaggerSchemaRef*
    AssertEquals('ProductModel', TProductModel.SchemaName, 'SchemaName remove prefixo T');

    GCapturedModelName := '';
    GCapturedModelSchema := '';
    SetDeltaModelRegisterSchemaProc(@MockSchemaRegisterProc);
    try
      TProductModel.RegisterSwaggerSchema(False);
      AssertEquals('ProductModel', GCapturedModelName, 'RegisterSwaggerSchema invocou callback com nome correto');
      Doc := TJSONObject(GetJSON(GCapturedModelSchema));
      try
        AssertEquals('object', Doc.Get('type', ''), 'RegisterSwaggerSchema passou schema JSON válido do tipo object');
      finally
        Doc.Free;
      end;

      // SwaggerSchemaRefObject
      SchemaJson := TProductModel.SwaggerSchemaRefObject;
      Doc := TJSONObject(GetJSON(SchemaJson));
      try
        AssertEquals('#/components/schemas/ProductModel', Doc.Get('$ref', ''), 'SwaggerSchemaRefObject retorna $ref canônico');
      finally
        Doc.Free;
      end;

      // SwaggerSchemaRefArray
      SchemaJson := TProductModel.SwaggerSchemaRefArray;
      Doc := TJSONObject(GetJSON(SchemaJson));
      try
        AssertEquals('array', Doc.Get('type', ''), 'SwaggerSchemaRefArray raiz é array');
        AssertTrue(Assigned(Doc.Get('items', TJSONObject(nil))), 'SwaggerSchemaRefArray contém items');
        if Assigned(Doc.Get('items', TJSONObject(nil))) then
          AssertEquals('#/components/schemas/ProductModel', Doc.Get('items', TJSONObject(nil)).Get('$ref', ''), 'items aponta para $ref');
      finally
        Doc.Free;
      end;

      // SwaggerSchemaRefPaginated
      SchemaJson := TProductModel.SwaggerSchemaRefPaginated;
      Doc := TJSONObject(GetJSON(SchemaJson));
      try
        AssertEquals('object', Doc.Get('type', ''), 'SwaggerSchemaRefPaginated raiz é object');
        Props := Doc.Get('properties', TJSONObject(nil));
        AssertTrue(Assigned(Props), 'SwaggerSchemaRefPaginated contém properties');
        AssertTrue(Assigned(Props.Get('items', TJSONObject(nil))), 'SwaggerSchemaRefPaginated contém items');
        if Assigned(Props.Get('items', TJSONObject(nil))) then
        begin
          AssertEquals('array', Props.Get('items', TJSONObject(nil)).Get('type', ''), 'items é array');
          if Assigned(Props.Get('items', TJSONObject(nil)).Get('items', TJSONObject(nil))) then
            AssertEquals('#/components/schemas/ProductModel', Props.Get('items', TJSONObject(nil)).Get('items', TJSONObject(nil)).Get('$ref', ''), 'items.items aponta para $ref');
        end;
        AssertTrue(Assigned(Props.Get('total_records', TJSONObject(nil))), 'SwaggerSchemaRefPaginated contém total_records');
      finally
        Doc.Free;
      end;
    finally
      SetDeltaModelRegisterSchemaProc(nil);
    end;
end;

{==============================================================================}
{ Test 5: DDL Generator Across Dialects                                        }
{==============================================================================}

type
  { Campos TDF* herdados e também citados no schema fluente (formato do demo todolist) }
  TFluentTdfBase = class(TDeltaModel)
  private
    FDescription: TDFStringRequired;
  published
    property description: TDFStringRequired read FDescription write FDescription;
  public
    class procedure ConfigureSchema(ASchema: TDeltaModelSchema); override;
  end;

  TFluentTdfItem = class(TFluentTdfBase)
  private
    FDone: TDFBooleanRequired;
    FId: TDFIntRequired;
  published
    property done: TDFBooleanRequired read FDone write FDone;
    property id: TDFIntRequired read FId write FId;
  public
    class procedure ConfigureSchema(ASchema: TDeltaModelSchema); override;
  end;

class procedure TFluentTdfBase.ConfigureSchema(ASchema: TDeltaModelSchema);
begin
  inherited ConfigureSchema(ASchema);
  ASchema.Field('description').MinLength(2);
end;

class procedure TFluentTdfItem.ConfigureSchema(ASchema: TDeltaModelSchema);
begin
  inherited ConfigureSchema(ASchema);
  ASchema.Table('fluent_tdf_item');
  ASchema.Field('id').AsPK.AutoInc;
end;

procedure TestFluentSchemaOnTdfFields;
var
  Item, Loaded: TFluentTdfItem;
  Meta: TModelMetadata;
  F: PDeltaFieldMeta;
  Engine: TDeltaORMEngine;
  Query: specialize TDeltaQuery<TFluentTdfItem>;
  Json: string;
  Raised: Boolean;
begin
  Suite('4c. Schema fluente sobre campos TDF*');
  Meta := TRTTICache.GetModelMetadata(TFluentTdfItem);
  F := Meta.FindFieldByProp('description');
  AssertTrue((F <> nil) and F^.IsDeltaField, 'description (TDF citado no schema) é tratado como TDeltaField');
  F := Meta.FindFieldByProp('id');
  AssertTrue((F <> nil) and F^.IsDeltaField and F^.IsPrimaryKey and F^.IsAutoInc,
    'id mantém AsPK/AutoInc do schema fluente');
  AssertEquals('id', Meta.PKFieldName, 'PK vinda do schema fluente');

  Item := TFluentTdfItem.Create;
  try
    Item.FromJson('{"description":"primeira","done":true,"id":7}');
    Json := Item.ToJson;
    AssertTrue(Pos('"description":"primeira"', Json) > 0, 'ToJson serializa o texto do TDF: ' + Json);
    AssertTrue(Pos('"id":7', Json) > 0, 'ToJson serializa o inteiro do TDF como número: ' + Json);
  finally
    Item.Free;
  end;

  Item := TFluentTdfItem.Create;
  try
    Item.FromJson('{"description":"x","done":false}');
    Raised := False;
    try
      Item.Validate;
    except
      on E: EDeltaValidation do Raised := True;
    end;
    AssertTrue(Raised, 'MinLength do schema fluente vale para TDFStringRequired');
  finally
    Item.Free;
  end;

  Engine := TDeltaORMEngine.Create('sqlite:///:memory:');
  try
    Engine.Connection.Open;
    Engine.ExecuteDirect('CREATE TABLE fluent_tdf_item (id INTEGER PRIMARY KEY AUTOINCREMENT, description VARCHAR(100) NOT NULL, done INTEGER NOT NULL)');
    Item := TFluentTdfItem.Create;
    try
      Item.FromJson('{"description":"persistida","done":false}');
      AssertTrue(Engine.Insert(Item), 'Insert de modelo com schema fluente sobre TDF');
    finally
      Item.Free;
    end;
    Query := specialize TDeltaQuery<TFluentTdfItem>.Create(Engine);
    try
      Loaded := Query.First;
    finally
      Query.Free;
    end;
    try
      AssertTrue(Loaded <> nil, 'Registro inserido é lido de volta');
      if Loaded <> nil then
        AssertEquals('persistida', Loaded.description.AsString, 'Valor do TDF persistido no INSERT');
    finally
      Loaded.Free;
    end;
  finally
    Engine.Free;
  end;
end;

procedure TestStringLiteralEscaping;
begin
  Suite('4b. Escape de literais por dialeto');
  AssertEquals('''a''''b''', TSQLCriteriaHelper.QuoteString('a''b', ddPostgreSQL), 'Aspa simples duplicada (PostgreSQL)');
  AssertEquals('''a\''''b''', TSQLCriteriaHelper.QuoteString('a\''b', ddPostgreSQL), 'Barra invertida preservada fora do MySQL');
  AssertEquals('''a\\''''b''', TSQLCriteriaHelper.QuoteString('a\''b', ddMySQL), 'Barra invertida duplicada no MySQL/MariaDB');
  AssertEquals('name = ''x\\'''' OR 1=1 -- ''',
    TSQLCriteriaHelper.BuildCondition('name', opEqual, 'x\'' OR 1=1 -- ', ddMySQL),
    'Payload com barra invertida não encerra literal no MySQL');
end;

procedure TestDDLGenerator;
var
  Prod: TProductModel;
  DDL: string;
begin
  Suite('5. DDL Generator Across Dialects');

  Prod := TProductModel.Create;
  try
    // SQLite
    DDL := TDDLBuilder.CreateTableAndFields(Prod, nil, ddSQLite);
    AssertTrue(Pos('CREATE TABLE IF NOT EXISTS products', DDL) > 0, 'SQLite usa IF NOT EXISTS');
    AssertTrue(Pos('INTEGER PRIMARY KEY AUTOINCREMENT', DDL) > 0, 'SQLite usa AUTOINCREMENT para chave primária');
    AssertTrue(Pos('description TEXT', DDL) > 0, 'SQLite usa TEXT para TDFText');

    // PostgreSQL
    DDL := TDDLBuilder.CreateTableAndFields(Prod, nil, ddPostgreSQL);
    AssertTrue(Pos('price DECIMAL(18,4)', DDL) > 0, 'PostgreSQL mapeia TDFCurrency para DECIMAL(18,4)');
    AssertTrue(Pos('active BOOLEAN', DDL) > 0, 'PostgreSQL mapeia TDFBool para BOOLEAN');
    AssertTrue(Pos('big_code BIGINT', DDL) > 0, 'PostgreSQL mapeia TDFInt64 para BIGINT');

    // Firebird
    DDL := TDDLBuilder.CreateTableAndFields(Prod, nil, ddFirebird);
    AssertTrue(Pos('price DECIMAL(18,4)', DDL) > 0, 'Firebird mapeia TDFCurrency para DECIMAL(18,4)');
    AssertTrue(Pos('description BLOB SUB_TYPE TEXT', DDL) > 0, 'Firebird mapeia TDFText para BLOB SUB_TYPE TEXT');

    // Oracle
    DDL := TDDLBuilder.CreateTableAndFields(Prod, nil, ddOracle);
    AssertTrue(Pos('description CLOB', DDL) > 0, 'Oracle mapeia TDFText para CLOB');
    AssertTrue(Pos('big_code NUMBER(19)', DDL) > 0, 'Oracle mapeia TDFInt64 para NUMBER(19)');

    // MSSQL
    DDL := TDDLBuilder.CreateTableAndFields(Prod, nil, ddMSSQL);
    AssertTrue(Pos('description VARCHAR(MAX)', DDL) > 0, 'MSSQL mapeia TDFText para VARCHAR(MAX)');
    AssertTrue(Pos('active BIT', DDL) > 0, 'MSSQL mapeia TDFBool para BIT');
  finally
    Prod.Free;
  end;
end;

{==============================================================================}
{ Test 6: Model Lifecycle Hooks                                                }
{==============================================================================}

procedure TestModelLifecycleHooks;
var
  Engine: TDeltaORMEngine;
  Item: THookModel;
  Failed: Boolean;
begin
  Suite('6. Model Lifecycle Hooks');

  Engine := TDeltaORMEngine.Create('sqlite:///:memory:');
  try
    Engine.ExecuteDirect(
      'CREATE TABLE hook_items (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  name TEXT NOT NULL' +
      ');'
    );

    Item := THookModel.Create;
    try
      Item.Name.Value := 'Item Teste Hooks';

      // 1. Insert test
      Item.HookLog.Clear;
      Engine.Insert(Item);
      AssertEqualsInt(2, Item.HookLog.Count, 'Insert deve disparar exatamente 2 hooks');
      AssertEquals('BeforeInsert', Item.HookLog[0], 'Primeiro hook deve ser BeforeInsert');
      AssertEquals('AfterInsert', Item.HookLog[1], 'Segundo hook deve ser AfterInsert');
      AssertTrue(Item.Id.AsLargeInt > 0, 'Id foi gerado após Insert');

      // 2. Update test
      Item.HookLog.Clear;
      Item.Name.Value := 'Item Modificado';
      Engine.Merge(Item);
      AssertEqualsInt(2, Item.HookLog.Count, 'Update deve disparar exatamente 2 hooks');
      AssertEquals('BeforeUpdate', Item.HookLog[0], 'Primeiro hook deve ser BeforeUpdate');
      AssertEquals('AfterUpdate', Item.HookLog[1], 'Segundo hook deve ser AfterUpdate');

      // 3. Save with existing PK (performs Update internally)
      Item.HookLog.Clear;
      Item.Name.Value := 'Item Salvo Novamente';
      Engine.Save(Item);
      AssertEqualsInt(4, Item.HookLog.Count, 'Save com PK deve disparar 4 hooks');
      AssertEquals('BeforeSave', Item.HookLog[0], 'Hook 0 deve ser BeforeSave');
      AssertEquals('BeforeUpdate', Item.HookLog[1], 'Hook 1 deve ser BeforeUpdate');
      AssertEquals('AfterUpdate', Item.HookLog[2], 'Hook 2 deve ser AfterUpdate');
      AssertEquals('AfterSave', Item.HookLog[3], 'Hook 3 deve ser AfterSave');

      // 4. Delete test
      Item.HookLog.Clear;
      Engine.Delete(Item);
      AssertEqualsInt(2, Item.HookLog.Count, 'Delete deve disparar exatamente 2 hooks');
      AssertEquals('BeforeDelete', Item.HookLog[0], 'Primeiro hook deve ser BeforeDelete');
      AssertEquals('AfterDelete', Item.HookLog[1], 'Segundo hook deve ser AfterDelete');

      // 5. Aborto no BeforeInsert
      Item.Free;
      Item := THookModel.Create;
      Item.Name.Value := 'Item que vai falhar';
      Item.FailOnBeforeInsert := True;
      Failed := False;
      try
        Engine.Insert(Item);
      except
        Failed := True;
      end;
      AssertTrue(Failed, 'Exceção em BeforeInsert deve interromper o Insert');
      AssertEqualsInt(1, Item.HookLog.Count, 'Apenas BeforeInsert deve ter executado');
      AssertEquals('BeforeInsert', Item.HookLog[0], 'AfterInsert NÃO deve ser executado após falha');
    finally
      Item.Free;
    end;
  finally
    Engine.Free;
  end;
end;

var
  GTxEngine: TDeltaORMEngine;

procedure TxSuccessProc;
begin
  GTxEngine.ExecuteDirect('INSERT INTO products (name, price, active) VALUES (''Produto Confirmado'', 20, 1)');
end;

procedure TxFailProc;
begin
  GTxEngine.ExecuteDirect('INSERT INTO products (name, price, active) VALUES (''Produto Erro'', 30, 1)');
  raise Exception.Create('Falha na transação forçada');
end;

procedure TestORMCRUDAndQueries;
var
  Engine: TDeltaORMEngine;
  P: TProductModel;
  FoundP: TProductModel;
  List: TDeltaModelList;
  Q: TQuery;
  TotalCount: Int64;
  JsonRes: RawByteString;
  InTxFailed: Boolean;
begin
  Suite('7. ORM CRUD, Advanced Query Builder & Transactions');

  Engine := TDeltaORMEngine.Create('sqlite:///:memory:');
  try
    // Prepare table
    Engine.ExecuteDirect(
      'CREATE TABLE products (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  name TEXT NOT NULL,' +
      '  price REAL NOT NULL,' +
      '  description TEXT,' +
      '  big_code BIGINT,' +
      '  active INTEGER NOT NULL' +
      ');'
    );
    if Engine.TransactionActive then
      Engine.Commit;

    // Individual inserts
    P := TProductModel.Create;
    try
      P.Name.Value := 'Notebook Pro';
      P.Price.Value := 8500.00;
      P.BigCode.Value := 1001;
      P.Active.Value := True;
      AssertTrue(Engine.Insert(P), 'Insert de produto 1 deve retornar True');

      P.Free;
      P := TProductModel.Create;
      P.Name.Value := 'Mouse Gamer';
      P.Price.Value := 150.00;
      P.BigCode.Value := 1002;
      P.Active.Value := True;
      AssertTrue(Engine.Insert(P), 'Insert de produto 2 deve retornar True');

      P.Free;
      P := TProductModel.Create;
      P.Name.Value := 'Mousepad Simples';
      P.Price.Value := 35.00;
      P.BigCode.Value := 1003;
      P.Active.Value := False;
      AssertTrue(Engine.Insert(P), 'Insert de produto 3 deve retornar True');

      P.Free;
      P := TProductModel.Create;
      P.Name.Value := 'Monitor 27 Polegadas';
      P.Price.Value := 1600.00;
      P.BigCode.Value := 1004;
      P.Active.Value := True;
      AssertTrue(Engine.Insert(P), 'Insert de produto 4 deve retornar True');
    finally
      P.Free;
    end;

    // Total and filtered Count
    TotalCount := Engine.Count(TProductModel);
    AssertEqualsInt(4, TotalCount, 'Total de registros inseridos deve ser 4');

    TotalCount := Engine.Count(TProductModel, 'active = 1');
    AssertEqualsInt(3, TotalCount, 'Total de produtos ativos deve ser 3');

    // FindById
    FoundP := TProductModel(Engine.Find(TProductModel, 1));
    try
      AssertTrue(Assigned(FoundP), 'Find com Id 1 deve retornar modelo');
      AssertEquals('Notebook Pro', FoundP.Name.AsString, 'Nome do produto 1 corresponde');
      AssertTrue(FoundP.Price.AsFloat > 8000, 'Preço do produto 1 corresponde');
    finally
      FoundP.Free;
    end;

    // Chained Where with AND test (FFilter overwrite fix)
    Q := Engine.Query(TProductModel);
    try
      Q.Where('active = 1').Where('price > 1000');
      List := Q.All;
      try
        AssertEqualsInt(2, List.Count, 'Where encadeado deve aplicar AND (ativos E preço > 1000 = 2 itens)');
      finally
        List.Free;
      end;
    finally
      Q.Free;
    end;

    // WhereBetween test
    Q := Engine.Query(TProductModel);
    try
      Q.WhereBetween('price', 100, 2000);
      List := Q.All;
      try
        AssertEqualsInt(2, List.Count, 'WhereBetween 100..2000 deve retornar 2 produtos (Mouse Gamer e Monitor)');
      finally
        List.Free;
      end;
    finally
      Q.Free;
    end;

    // WhereIn test
    Q := Engine.Query(TProductModel);
    try
      Q.WhereIn('name', ['Notebook Pro', 'Mouse Gamer']);
      List := Q.All;
      try
        AssertEqualsInt(2, List.Count, 'WhereIn deve retornar 2 registros correspondentes');
      finally
        List.Free;
      end;
    finally
      Q.Free;
    end;

    // OrderBy, Limit, and Offset test
    Q := Engine.Query(TProductModel);
    try
      Q.OrderBy('price DESC').Limit(2).Offset(1);
      List := Q.All;
      try
        AssertEqualsInt(2, List.Count, 'Limit 2 Offset 1 deve retornar 2 registros');
        // Prices descending: 8500 (skipped by offset), 1600 (Monitor), 150 (Mouse)
        AssertEquals('Monitor 27 Polegadas', TProductModel(List[0]).Name.AsString, 'Primeiro item retornado com Offset 1 deve ser o Monitor');
        AssertEquals('Mouse Gamer', TProductModel(List[1]).Name.AsString, 'Segundo item retornado deve ser o Mouse Gamer');
      finally
        List.Free;
      end;
    finally
      Q.Free;
    end;

    // AsJsonString test
    Q := Engine.Query(TProductModel);
    try
      Q.Where('price < 50');
      JsonRes := Q.AsJsonString;
      AssertTrue(Pos('Mousepad Simples', string(JsonRes)) > 0, 'AsJsonString retorna JSON com o produto');
    finally
      Q.Free;
    end;

    // Transaction tests: Commit and Rollback
    if Engine.TransactionActive then
      Engine.Commit;

    // Rollback must undo insert
    Engine.StartTransaction;
    Engine.ExecuteDirect('INSERT INTO products (name, price, active) VALUES (''Produto Cancelado'', 10, 1)');
    Engine.Rollback;

    TotalCount := Engine.Count(TProductModel, 'name = ''Produto Cancelado''');
    AssertEqualsInt(0, TotalCount, 'Registro inserido em transação cancelada não deve persistir');

    // Static InTransaction with automatic commit
    GTxEngine := Engine;
    Engine.InTransaction(@TxSuccessProc);

    TotalCount := Engine.Count(TProductModel, 'name = ''Produto Confirmado''');
    AssertEqualsInt(1, TotalCount, 'InTransaction com sucesso deve persistir registro');

    // InTransaction on error must automatically Rollback
    InTxFailed := False;
    try
      Engine.InTransaction(@TxFailProc);
    except
      InTxFailed := True;
    end;
    AssertTrue(InTxFailed, 'InTransaction deve propagar exceção');
    TotalCount := Engine.Count(TProductModel, 'name = ''Produto Erro''');
    AssertEqualsInt(0, TotalCount, 'InTransaction deve reverter inserção ao lançar exceção');

    // DeleteById
    AssertTrue(Engine.DeleteById(TProductModel, 1), 'DeleteById(1) deve retornar True');
    TotalCount := Engine.Count(TProductModel, 'id = 1');
    AssertEqualsInt(0, TotalCount, 'Produto 1 deve ter sido excluído');
  finally
    Engine.Free;
  end;
end;

procedure TestConstraintsAndIndexes;
var
  Item: TConstraintItem;
  DDL: string;
  ConstraintsList, IdxList: TStringList;
  Engine: TDeltaORMEngine;
  Schema: TDeltaORMSchema;
  Item1, Item2, Item3, Item4: TConstraintItem;
  Failed: Boolean;
  TotalCount: Int64;
  LongIdx: TDeltaIndex;
  LongIdxDDL: string;
begin
  Suite('8. Constraints (Unique & Check) and Database Indexes');

  Item := TConstraintItem.Create;
  ConstraintsList := TStringList.Create;
  IdxList := TStringList.Create;
  try
    // 1. DDL SQLite
    DDL := TDDLBuilder.CreateTableAndFields(Item, ConstraintsList, ddSQLite);
    AssertTrue(Pos('CONSTRAINT UQ_constraint_items_email UNIQUE (email)', DDL) > 0,
      'SQLite deve conter constraint inline de coluna UNIQUE');
    AssertTrue(Pos('CONSTRAINT uq_tenant_code UNIQUE (tenant_id, code)', DDL) > 0,
      'SQLite deve conter constraint inline composta UNIQUE');
    AssertTrue(Pos('CONSTRAINT chk_score_pos CHECK (score >= 0)', DDL) > 0,
      'SQLite deve conter constraint inline CHECK');

    // 2. SQLite indexes
    IdxList.Clear;
    TDDLBuilder.GetIndexes(Item, ddSQLite, IdxList);
    AssertTrue(Pos('CREATE INDEX IF NOT EXISTS IX_constraint_items_created_at ON constraint_items (created_at);', IdxList.Text) > 0,
      'SQLite deve gerar CREATE INDEX para campo marcado com IsIndexed');
    AssertTrue(Pos('CREATE INDEX IF NOT EXISTS ix_tenant_created ON constraint_items (tenant_id, created_at);', IdxList.Text) > 0,
      'SQLite deve gerar CREATE INDEX para índice composto de tabela');

    // 3. DDL PostgreSQL (Constraints out-of-line)
    ConstraintsList.Clear;
    DDL := TDDLBuilder.CreateTableAndFields(Item, ConstraintsList, ddPostgreSQL);
    AssertTrue(Pos('ALTER TABLE constraint_items ADD CONSTRAINT UQ_constraint_items_email UNIQUE (email);', ConstraintsList.Text) > 0,
      'PostgreSQL deve gerar ADD CONSTRAINT UNIQUE para coluna');
    AssertTrue(Pos('ALTER TABLE constraint_items ADD CONSTRAINT uq_tenant_code UNIQUE (tenant_id, code);', ConstraintsList.Text) > 0,
      'PostgreSQL deve gerar ADD CONSTRAINT UNIQUE para chave composta');
    AssertTrue(Pos('ALTER TABLE constraint_items ADD CONSTRAINT chk_score_pos CHECK (score >= 0);', ConstraintsList.Text) > 0,
      'PostgreSQL deve gerar ADD CONSTRAINT CHECK');

    // 4. Long identifier sanitization for Oracle (30-char limit)
    LongIdx := TDeltaIndex.Create('IX_nome_muito_longo_que_ultrapassa_trinta_caracteres', ['score']);
    try
      LongIdxDDL := TDDLBuilder.IndexDDL('constraint_items', LongIdx, ddOracle);
      AssertTrue(Pos('IX_nome_muito_longo_que_ultrap', LongIdxDDL) > 0,
        'Oracle deve truncar identificador de índice para no máximo 30 caracteres');
      AssertTrue(Length('IX_nome_muito_longo_que_ultrap') <= 30, 'Identificador truncado tem 30 caracteres ou menos');
    finally
      LongIdx.Free;
    end;
  finally
    Item.Free;
    ConstraintsList.Free;
    IdxList.Free;
  end;

  // 5. Runtime test with in-memory SQLite via TDeltaORMSchema
  Engine := TDeltaORMEngine.Create('sqlite:///:memory:');
  try
    Schema := TDeltaORMSchema.Create(Engine);
    try
      Schema.RegisterModel(TConstraintItem);
      Schema.PrepareDB(True);

      AssertTrue(Schema.SQL.Count > 0, 'PrepareDB gerou comandos DDL');

      // Insert 1: Valid record
      Item1 := TConstraintItem.Create;
      try
        Item1.Email.Value := 'usuario1@empresa.com';
        Item1.TenantId.Value := 10;
        Item1.Code.Value := 'USR_A';
        Item1.Score.Value := 100;
        Item1.CreatedAt.Value := Now;
        AssertTrue(Engine.Insert(Item1), 'Inserção inicial deve ter sucesso');
      finally
        Item1.Free;
      end;

      // Insert 2: Duplicate email (must be rejected by UNIQUE email constraint)
      Item2 := TConstraintItem.Create;
      try
        Item2.Email.Value := 'usuario1@empresa.com'; // Duplicado!
        Item2.TenantId.Value := 20;
        Item2.Code.Value := 'USR_B';
        Item2.Score.Value := 200;
        Failed := False;
        try
          Engine.Insert(Item2);
        except
          Failed := True;
        end;
        AssertTrue(Failed, 'Inserção com e-mail duplicado deve falhar por violação de UNIQUE');
      finally
        Item2.Free;
      end;

      // Insert 3: Different email, same (tenant_id, code) pair (must fail composite UNIQUE)
      Item3 := TConstraintItem.Create;
      try
        Item3.Email.Value := 'outro@empresa.com';
        Item3.TenantId.Value := 10; // Same tenant
        Item3.Code.Value := 'USR_A'; // Same code
        Item3.Score.Value := 300;
        Failed := False;
        try
          Engine.Insert(Item3);
        except
          Failed := True;
        end;
        AssertTrue(Failed, 'Inserção com par (tenant_id, code) duplicado deve falhar por UNIQUE composto');
      finally
        Item3.Free;
      end;

      // Insert 4: Same code in another tenant (must succeed!)
      Item4 := TConstraintItem.Create;
      try
        Item4.Email.Value := 'usuario2@empresa.com';
        Item4.TenantId.Value := 20; // Different tenant
        Item4.Code.Value := 'USR_A'; // Same code
        Item4.Score.Value := 400;
        AssertTrue(Engine.Insert(Item4), 'Mesmo code em tenant diferente deve ser aceito');
      finally
        Item4.Free;
      end;

      TotalCount := Engine.Count(TConstraintItem);
      AssertEqualsInt(2, TotalCount, 'Total de registros aceitos no banco deve ser 2');
    finally
      Schema.Free;
    end;
  finally
    Engine.Free;
  end;
end;

procedure TestHasOneRelation;
var
  User1, User2, User3: TUserWithProfileModel;
  Prof: TProfileModel;
  DDL, JsonStr, SchemaJson: string;
  Doc, Props, ProfileProp: TJSONObject;
begin
  Suite('9. TDFHasOne (1:1 Relation Field)');

  User1 := TUserWithProfileModel.Create;
  try
    // 1. Metadata verification
    AssertTrue(User1.Profile.IsVirtual, 'TDFHasOne deve ser campo virtual');
    AssertEquals('object', User1.Profile.SwaggerDataType, 'SwaggerDataType de TDFHasOne deve ser object');
    AssertTrue(User1.Profile.RelationClass = TProfileModel, 'RelationClass de Profile deve ser TProfileModel');
    AssertTrue(User1.Profile.IsNull, 'Profile recém-criado deve ser IsNull');

    // 2. DDL must not emit a column for TDFHasOne
    DDL := TDDLBuilder.CreateTableAndFields(User1, nil, ddSQLite);
    AssertTrue(Pos('username TEXT', DDL) > 0, 'DDL deve conter campo normal');
    AssertFalse(Pos('Profile', DDL) > 0, 'DDL NÃO deve criar coluna física para TDFHasOne');

    // 3. Serialization with empty HasOne (null)
    User1.Id.Value := 1;
    User1.Username.Value := 'pedro';
    JsonStr := User1.ToJson;
    AssertTrue((Pos('"Profile" : null', JsonStr) > 0) or (Pos('"Profile": null', JsonStr) > 0) or (Pos('"Profile":null', JsonStr) > 0), 'HasOne não instanciado deve serializar como null');

    // 4. Serialization with populated HasOne
    Prof := TProfileModel.Create;
    Prof.Id.Value := 99;
    Prof.Bio.Value := 'Engenheiro de Software';
    Prof.Twitter.Value := '@pedro_dev';
    User1.Profile.Model := Prof;

    AssertFalse(User1.Profile.IsNull, 'Com Model atribuído, HasOne não deve ser IsNull');

    JsonStr := User1.ToJson;
    AssertTrue(Pos('Engenheiro de Software', JsonStr) > 0, 'ToJson deve conter bio do modelo relacionado');
    AssertTrue(Pos('@pedro_dev', JsonStr) > 0, 'ToJson deve conter twitter do modelo relacionado');

    // 5. JSON deserialization reconstructing the 1:1 object
    User2 := TUserWithProfileModel.Create;
    try
      User2.FromJson(JsonStr);
      AssertEquals('pedro', User2.Username.AsString, 'Username desserializado corretamente');
      AssertTrue(Assigned(User2.Profile.Model), 'Profile.Model deve ter sido instanciado pelo FromJson');
      if Assigned(User2.Profile.Model) then
      begin
        AssertEquals('Engenheiro de Software', TProfileModel(User2.Profile.Model).Bio.AsString, 'Bio do Profile recuperada');
        AssertEquals('@pedro_dev', TProfileModel(User2.Profile.Model).Twitter.AsString, 'Twitter do Profile recuperado');
        AssertEqualsInt(99, TProfileModel(User2.Profile.Model).Id.AsInteger, 'Id do Profile recuperado');
      end;

      // 6. Clone test (deep copy of HasOne)
      User3 := TUserWithProfileModel(User2.Clone);
      try
        AssertTrue(Assigned(User3.Profile.Model), 'Clone deve copiar Profile.Model');
        if Assigned(User3.Profile.Model) then
        begin
          AssertEquals('Engenheiro de Software', TProfileModel(User3.Profile.Model).Bio.AsString, 'Bio copiada no Clone');
          // Mutating the copy does not affect the original
          TProfileModel(User3.Profile.Model).Bio.Value := 'Bio Modificada';
          AssertEquals('Engenheiro de Software', TProfileModel(User2.Profile.Model).Bio.AsString, 'Original não deve ser alterado');
        end;
      finally
        User3.Free;
      end;
    finally
      User2.Free;
    end;

    // 7. OpenAPI / Swagger Schema for TDFHasOne
    SchemaJson := TUserWithProfileModel.SwaggerSchema(False);
    Doc := TJSONObject(GetJSON(SchemaJson));
    try
      Props := Doc.Get('properties', TJSONObject(nil));
      AssertTrue(Assigned(Props), 'Schema contém properties');
      if Assigned(Props) then
      begin
        ProfileProp := Props.Get('Profile', TJSONObject(nil));
        AssertTrue(Assigned(ProfileProp), 'Schema contém propriedade Profile');
        if Assigned(ProfileProp) then
        begin
          AssertEquals('object', ProfileProp.Get('type', ''), 'Tipo da relação HasOne no Swagger deve ser object');
        end;
      end;
    finally
      Doc.Free;
    end;
  finally
    User1.Free;
  end;
end;

{==============================================================================}
{ Test 10: TDeltaModelPaginatedList Pagination & Resilient Deserialization    }
{==============================================================================}

procedure TestPaginatedList;
var
  PagList: TDeltaModelPaginatedList;
  RegList: TDeltaModelList;
  GenList: TProductGenericList;
  GenPagList: TProductPaginatedGenericList;
  Prod1, Prod2: TProductModel;
  JsonOut: RawByteString;
  Doc: TJSONObject;
  ParsedJson: TJSONData;
begin
  Suite('10. TDeltaModelPaginatedList & Serialization');

  PagList := TDeltaModelPaginatedList.Create;
  try
    PagList.SetDeltaModelClass(TProductModel);

    Prod1 := TProductModel.Create;
    Prod1.Id.Value := 1;
    Prod1.Name.Value := 'Produto Pag 1';
    Prod1.Price.Value := 10.50;
    Prod1.Active.Value := True;
    PagList.Add(Prod1);

    Prod2 := TProductModel.Create;
    Prod2.Id.Value := 2;
    Prod2.Name.Value := 'Produto Pag 2';
    Prod2.Price.Value := 25.00;
    Prod2.Active.Value := True;
    PagList.Add(Prod2);

    PagList.total_records := 50;
    PagList.page := 2;
    PagList.page_size := 2;

    JsonOut := PagList.ToJson;
    Doc := TJSONObject(GetJSON(JsonOut));
    try
      AssertTrue(Assigned(Doc.Get('items', TJSONArray(nil))), 'Payload contém items array');
      if Assigned(Doc.Get('items', TJSONArray(nil))) then
      begin
        AssertEqualsInt(2, Doc.Get('items', TJSONArray(nil)).Count, 'items contém 2 registros');
      end;
      AssertEqualsInt(50, Doc.Get('total_records', 0), 'total_records correto');
      AssertEqualsInt(2, Doc.Get('page', 0), 'page correto');
      AssertEqualsInt(2, Doc.Get('page_size', 0), 'page_size correto');
      AssertEqualsInt(25, Doc.Get('total_pages', 0), 'total_pages correto');
    finally
      Doc.Free;
    end;

    // TDeltaModelPaginatedList.FromJson test
    PagList.ClearList;
    PagList.FromJson(JsonOut);
    AssertEqualsInt(50, PagList.total_records, 'PagList.total_records recuperado do JSON');
    AssertEqualsInt(2, PagList.page, 'PagList.page recuperado do JSON');
    AssertEqualsInt(2, PagList.page_size, 'PagList.page_size recuperado do JSON');
    AssertEqualsInt(25, PagList.total_pages, 'PagList.total_pages recuperado do JSON');
    AssertEqualsInt(2, PagList.Count, 'PagList.Records.Count recuperado');
    AssertEquals('Produto Pag 1', TProductModel(PagList[0]).Name.AsString, 'Item 0 recuperado');
    AssertEquals('Produto Pag 2', TProductModel(PagList[1]).Name.AsString, 'Item 1 recuperado');

    // Canonical test: traditional TDeltaModelList receiving paginated payload { items: [...], page: ..., page_size: ..., total_records: ..., total_pages: ... }
    RegList := TDeltaModelList.Create;
    try
      RegList.SetDeltaModelClass(TProductModel);
      RegList.FromJson(JsonOut);
      AssertEqualsInt(2, RegList.Count, 'TDeltaModelList padrão desserializou items com sucesso');
      AssertEqualsInt(50, RegList.total_records, 'RegList.total_records desserializado com sucesso');
      AssertEqualsInt(2, RegList.page, 'RegList.page desserializado com sucesso');
      AssertEquals('Produto Pag 1', TProductModel(RegList[0]).Name.AsString, 'Item 0 do RegList correto');
    finally
      RegList.Free;
    end;

    // Test: TDeltaModelList.ToJson emits only a JSON array of objects
    RegList := TDeltaModelList.Create;
    try
      RegList.SetDeltaModelClass(TProductModel);
      Prod1 := TProductModel.Create;
      Prod1.Id.Value := 10;
      Prod1.Name.Value := 'Produto Lista Simples';
      Prod1.Price.Value := 99.00;
      Prod1.Active.Value := True;
      RegList.Add(Prod1);

      JsonOut := RegList.ToJson;
      AssertTrue((Length(JsonOut) > 0) and (JsonOut[1] = '['), 'TDeltaModelList.ToJson inicia com array "["');
      ParsedJson := GetJSON(JsonOut);
      try
        AssertTrue(ParsedJson is TJSONArray, 'TDeltaModelList.ToJson produz exclusivamente um TJSONArray');
        AssertEqualsInt(1, TJSONArray(ParsedJson).Count, 'TDeltaModelList.ToJson contém 1 item');
      finally
        ParsedJson.Free;
      end;
    finally
      RegList.Free;
    end;

    // Test: TGDeltaModelList<TProductModel>.ToJson emits only a JSON array of objects
    GenList := TProductGenericList.Create;
    try
      Prod1 := TProductModel.Create;
      Prod1.Id.Value := 20;
      Prod1.Name.Value := 'Produto Generico';
      Prod1.Price.Value := 150.00;
      Prod1.Active.Value := True;
      GenList.Add(Prod1);

      JsonOut := GenList.ToJson;
      AssertTrue((Length(JsonOut) > 0) and (JsonOut[1] = '['), 'TGDeltaModelList.ToJson inicia com array "["');
      ParsedJson := GetJSON(JsonOut);
      try
        AssertTrue(ParsedJson is TJSONArray, 'TGDeltaModelList.ToJson produz exclusivamente um TJSONArray');
      finally
        ParsedJson.Free;
      end;

      // Deserialization into generic list
      GenList.ClearList;
      GenList.FromJson(JsonOut);
      AssertEqualsInt(1, GenList.Count, 'TGDeltaModelList desserializou do JSON Array');
      AssertEquals('Produto Generico', GenList[0].Name.AsString, 'Item tipado acessível diretamente via Item[0]');
    finally
      GenList.Free;
    end;

    // Test: TGDeltaModelPaginatedList<TProductModel> serializes paginated envelope
    GenPagList := TProductPaginatedGenericList.Create;
    try
      Prod1 := TProductModel.Create;
      Prod1.Id.Value := 30;
      Prod1.Name.Value := 'Produto Paginado Gen';
      Prod1.Price.Value := 200.00;
      Prod1.Active.Value := True;
      GenPagList.Add(Prod1);
      GenPagList.page := 1;
      GenPagList.page_size := 10;
      GenPagList.total_records := 1;

      JsonOut := GenPagList.ToJson;
      AssertTrue((Length(JsonOut) > 0) and (JsonOut[1] = '{'), 'TGDeltaModelPaginatedList.ToJson inicia com objeto "{"');
      Doc := TJSONObject(GetJSON(JsonOut));
      try
        AssertTrue(Assigned(Doc.Get('items', TJSONArray(nil))), 'Payload genérico paginado contém items');
        AssertEqualsInt(1, Doc.Get('total_records', 0), 'total_records correto em lista genérica paginada');
      finally
        Doc.Free;
      end;

      // Deserialization
      GenPagList.ClearList;
      GenPagList.FromJson(JsonOut);
      AssertEqualsInt(1, GenPagList.Count, 'TGDeltaModelPaginatedList desserializou items');
      AssertEquals('Produto Paginado Gen', GenPagList[0].Name.AsString, 'Item tipado acessível diretamente via Item[0]');
    finally
      GenPagList.Free;
    end;

  finally
    PagList.Free;
  end;
end;

procedure TestRTTICacheAndMetadata;
var
  ClassProps1, ClassProps2: TRTTIClassInfo;
  Meta1, Meta2: TModelMetadata;
  PropItem: PDeltaPropItem;
  FieldItem: PDeltaFieldMeta;
begin
  Suite('11. TRTTICache & TModelMetadata');

  // Test 1: Fetch cached RTTI properties
  ClassProps1 := TRTTICache.GetClassProps(TProductModel);
  AssertTrue(Assigned(ClassProps1), 'TRTTICache.GetClassProps retornou objeto');
  AssertTrue(ClassProps1.PropCount >= 6, Format('ClassProps possui propriedades (%d)', [ClassProps1.PropCount]));

  // Test 2: Cache hit (returns same instance without reallocation)
  ClassProps2 := TRTTICache.GetClassProps(TProductModel);
  AssertTrue(ClassProps1 = ClassProps2, 'TRTTICache.GetClassProps retorna mesma instância de cache (hit)');

  // Test 3: Fast FindProp
  PropItem := ClassProps1.FindProp('Name');
  AssertTrue(PropItem <> nil, 'FindProp localizou Name');
  if PropItem <> nil then
  begin
    AssertTrue(PropItem^.IsDeltaField, 'Name é TDeltaField');
    AssertEquals('Name', PropItem^.Name, 'Nome da propriedade confere');
  end;

  PropItem := ClassProps1.FindProp('PropriedadeInexistente');
  AssertTrue(PropItem = nil, 'FindProp para propriedade inexistente retorna nil');

  // Test 4: TModelMetadata
  Meta1 := TRTTICache.GetModelMetadata(TProductModel);
  AssertTrue(Assigned(Meta1), 'TRTTICache.GetModelMetadata retornou objeto');
  AssertEquals('products', Meta1.TableName, 'TableName em metadata confere');
  AssertEquals('Id', Meta1.PKPropName, 'PKPropName em metadata confere');
  AssertEquals('id', Meta1.PKFieldName, 'PKFieldName em metadata confere');
  AssertTrue(Meta1.FieldCount >= 6, Format('Metadata possui campos (%d)', [Meta1.FieldCount]));

  // Test 5: ModelMetadata cache hit
  Meta2 := TRTTICache.GetModelMetadata(TProductModel);
  AssertTrue(Meta1 = Meta2, 'TRTTICache.GetModelMetadata retorna mesma instância de cache (hit)');

  // Test 6: FindField
  FieldItem := Meta1.FindField('price');
  AssertTrue(FieldItem <> nil, 'FindField localizou price');
  if FieldItem <> nil then
    AssertEquals('price', FieldItem^.FieldName, 'FieldName de price confere');
end;

procedure TestColumnBindingMap;
var
  Engine: TDeltaORMEngine;
  DS: TSQLQuery;
  P, P2: TProductModel;
  BindingMap: TColumnBindingMap;
  List: TDeltaModelPaginatedList;
begin
  Suite('12. TColumnBindingMap & Direct Typed ORM Binding');

  Engine := TDeltaORMEngine.Create('sqlite:///:memory:');
  try
    Engine.ExecuteDirect(
      'CREATE TABLE products (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  name TEXT NOT NULL,' +
      '  price REAL NOT NULL,' +
      '  description TEXT,' +
      '  big_code BIGINT,' +
      '  active INTEGER NOT NULL' +
      ');'
    );
    if Engine.TransactionActive then Engine.Commit;

    // Insert test record
    P := TProductModel.Create;
    try
      P.Name.AsText := 'Monitor Ultrawide';
      P.Price.AsFloat := 2499.90;
      P.BigCode.AsLargeInt := 9876543210;
      P.Active.Value := True;
      // Description left null
      AssertTrue(Engine.Insert(P), 'Insert para teste de binding retornou True');
    finally
      P.Free;
    end;

    // Test 1: Direct TColumnBindingMap on TDataSet
    DS := Engine.NewDataset;
    try
      DS.SQL.Text := 'SELECT id, name, price, description, big_code, active FROM products WHERE id = 1';
      DS.Open;
      AssertTrue(not DS.IsEmpty, 'Dataset retornou registro');

      BindingMap := TColumnBindingMap.Create(TProductModel, DS);
      try
        AssertEqualsInt(6, BindingMap.BindingCount, 'BindingMap vinculou 6 colunas');

        P := TProductModel.Create;
        try
          BindingMap.BindRecord(P);
          AssertEqualsInt(1, P.Id.AsInteger, 'P.Id preenchido tipado');
          AssertEquals('Monitor Ultrawide', P.Name.AsString, 'P.Name preenchido tipado');
          AssertTrue(Abs(P.Price.AsFloat - 2499.90) < 0.01, 'P.Price preenchido tipado');
          AssertEqualsInt(9876543210, P.BigCode.AsLargeInt, 'P.BigCode preenchido tipado');
          AssertTrue(P.Active.AsBoolean, 'P.Active preenchido tipado');
          AssertTrue(P.Description.IsNull, 'P.Description nulo preservado');
        finally
          P.Free;
        end;

        // Test 2: FromDataSet with TColumnBindingMap
        P2 := TProductModel.Create;
        try
          FromDataSet(P2, BindingMap);
          AssertEquals('Monitor Ultrawide', P2.Name.AsString, 'FromDataSet com BindingMap preencheu Name');
        finally
          P2.Free;
        end;

      finally
        BindingMap.Free;
      end;
      DS.Close;
    finally
      DS.Free;
    end;

    // Test 3: TQuery.All using TColumnBindingMap
    List := Engine.Query(TProductModel).All;
    try
      AssertTrue(Assigned(List), 'TQuery.All retornou lista paginada');
      AssertEqualsInt(1, List.Records.Count, 'TQuery.All retornou 1 registro');
      if List.Records.Count > 0 then
      begin
        P := TProductModel(List.Records[0]);
        AssertEquals('Monitor Ultrawide', P.Name.AsString, 'Registro lido por TQuery.All via binding');
        AssertTrue(Abs(P.Price.AsFloat - 2499.90) < 0.01, 'Preço lido por TQuery.All via binding');
      end;
    finally
      List.Free;
    end;

  finally
    Engine.Free;
  end;
end;

procedure TestFastJsonStreamer;
var
  S: string;
  P: TProductModel;
  List: TProductGenericList;
  JsonOut: RawByteString;
  Parser: TJSONParser;
  Data: TJSONData;
begin
  Suite('13. TDeltaFastJsonStreamer & Fixed-Point Floats Without Scientific Notation');

  // Test 1: Fixed-point float formatting - zero and integers
  S := TDeltaFastJsonStreamer.FormatFloatFixed(0.0);
  AssertEquals('0', S, 'Float zero formata como "0"');

  S := TDeltaFastJsonStreamer.FormatFloatFixed(100.0);
  AssertEquals('100', S, 'Float 100.0 formata como "100"');

  S := TDeltaFastJsonStreamer.FormatFloatFixed(1234.56);
  AssertEquals('1234.56', S, 'Float 1234.56 formata como "1234.56"');

  // Test 2: Very large float (no E+ / scientific notation)
  S := TDeltaFastJsonStreamer.FormatFloatFixed(123456789.9876);
  AssertTrue(Pos('E+', S) = 0, 'Float grande não contém E+');
  AssertTrue(Pos('E-', S) = 0, 'Float grande não contém E-');
  AssertEquals('123456789.9876', S, 'Float grande 123456789.9876 formatado em ponto fixo');

  // Test 3: Very small float (no E- / scientific notation)
  S := TDeltaFastJsonStreamer.FormatFloatFixed(0.0001234);
  AssertTrue(Pos('E+', S) = 0, 'Float pequeno não contém E+');
  AssertTrue(Pos('E-', S) = 0, 'Float pequeno não contém E-');
  AssertEquals('0.0001234', S, 'Float pequeno 0.0001234 formatado em ponto fixo');

  // Test 4: Float in billions range (no E+)
  S := TDeltaFastJsonStreamer.FormatFloatFixed(9999999999.5);
  AssertTrue(Pos('E+', S) = 0, 'Float na casa de bilhões não contém E+');
  AssertEquals('9999999999.5', S, 'Float bilhões formatado em ponto fixo');

  // Test 5: Model serialization with large float via ToJson
  P := TProductModel.Create;
  try
    P.Id.AsLargeInt := 42;
    P.Name.AsText := 'Servidor Enterprise "Rack"';
    P.Price.AsFloat := 98765.43;
    P.BigCode.AsLargeInt := 1234567890123;
    P.Active.Value := True;
    // Null description

    JsonOut := P.ToJson;
    AssertTrue(Pos('98765.43', JsonOut) > 0, 'JSON do modelo contém preço em ponto fixo');
    AssertTrue(Pos('E+', JsonOut) = 0, 'JSON do modelo não contém E+');
    AssertTrue(Pos('E-', JsonOut) = 0, 'JSON do modelo não contém E-');
    AssertTrue(Pos('"Description":null', JsonOut) > 0, 'JSON do modelo contém Description como null');
    AssertTrue(Pos('Servidor Enterprise \"Rack\"', JsonOut) > 0, 'JSON do modelo realizou escape correto de aspas');

    // Validate that emitted JSON is strictly parseable
    Parser := TJSONParser.Create(JsonOut, True);
    try
      Data := Parser.Parse;
      AssertTrue(Assigned(Data), 'JSON do modelo parseado com sucesso');
      AssertTrue(Data is TJSONObject, 'JSON do modelo é TJSONObject');
      Data.Free;
    finally
      Parser.Free;
    end;
  finally
    P.Free;
  end;

  // Test 6: List serialization with multiple records
  List := TProductGenericList.Create;
  try
    P := TProductModel.Create;
    P.Id.AsLargeInt := 1;
    P.Name.AsText := 'Item 1';
    P.Price.AsFloat := 1500000.75;
    P.Active.Value := True;
    List.Add(P);

    P := TProductModel.Create;
    P.Id.AsLargeInt := 2;
    P.Name.AsText := 'Item 2';
    P.Price.AsFloat := 0.005;
    P.Active.Value := False;
    List.Add(P);

    JsonOut := List.ToJson;
    AssertTrue(JsonOut[1] = '[', 'List.ToJson inicia com array "["');
    AssertTrue(JsonOut[Length(JsonOut)] = ']', 'List.ToJson encerra com array "]"');
    AssertTrue(Pos('1500000.75', JsonOut) > 0, 'List.ToJson contém float grande em ponto fixo');
    AssertTrue(Pos('0.005', JsonOut) > 0, 'List.ToJson contém float pequeno em ponto fixo');
    AssertTrue(Pos('E+', JsonOut) = 0, 'List.ToJson não contém E+');
    AssertTrue(Pos('E-', JsonOut) = 0, 'List.ToJson não contém E-');

    Parser := TJSONParser.Create(JsonOut, True);
    try
      Data := Parser.Parse;
      AssertTrue(Assigned(Data), 'List.ToJson parseado com sucesso');
      AssertTrue(Data is TJSONArray, 'List.ToJson é TJSONArray');
      AssertEqualsInt(2, TJSONArray(Data).Count, 'Array contém 2 itens');
      Data.Free;
    finally
      Parser.Free;
    end;
  finally
    List.Free;
  end;
end;

{==============================================================================}
{ Test 14: Direct Typed Field Properties (AsInt64, AsInteger, AsFloat, etc.)   }
{==============================================================================}

procedure TestDirectTypedProperties;
var
  P: TProductModel;
begin
  WriteLn('=== 14. Direct Typed Field Properties (No Variant Boxing) ===');
  P := TProductModel.Create;
  try
    // Integer read/write tests
    P.Id.AsInt64 := 987654321;
    AssertTrue(P.Id.AsInt64 = 987654321, 'P.Id.AsInt64 atribuição e leitura direta');
    P.Id.AsLargeInt := 123456789;
    AssertTrue(P.Id.AsLargeInt = 123456789, 'P.Id.AsLargeInt atribuição e leitura direta');
    P.Id.AsInteger := 42;
    AssertEqualsInt(42, P.Id.AsInteger, 'P.Id.AsInteger atribuição e leitura direta');

    // String read/write tests
    P.Name.AsText := 'Teclado Mecanico';
    AssertEquals('Teclado Mecanico', P.Name.AsString, 'P.Name.AsString conferido apos AsText');
    P.Name.SetAsString('Teclado RGB');
    AssertEquals('Teclado RGB', P.Name.AsText, 'P.Name.AsText conferido apos SetAsString');

    // Floating-point read/write tests
    P.Price.AsFloat := 499.90;
    AssertTrue(Abs(P.Price.AsFloat - 499.90) < 0.001, 'P.Price.AsFloat atribuição direta');

    // Boolean read/write tests
    P.Active.AsBoolean := True;
    AssertTrue(P.Active.AsBoolean, 'P.Active.AsBoolean := True');
    P.Active.AsBoolean := False;
    AssertFalse(P.Active.AsBoolean, 'P.Active.AsBoolean := False');

    // Null field tests
    AssertTrue(P.Description.IsNull, 'P.Description inicialmente nulo');
    P.Description.AsText := 'Descricao de teste';
    AssertFalse(P.Description.IsNull, 'P.Description não é mais nulo após AsText := ...');
    AssertEquals('Descricao de teste', P.Description.AsString, 'P.Description.AsString conferido');

    // Int64Nullable field test
    AssertTrue(P.BigCode.IsNull, 'P.BigCode inicialmente nulo');
    P.BigCode.AsInt64 := 998877665544;
    AssertFalse(P.BigCode.IsNull, 'P.BigCode não é mais nulo após AsInt64 := ...');
    AssertTrue(P.BigCode.AsInt64 = 998877665544, 'P.BigCode.AsInt64 conferido');
  finally
    P.Free;
  end;
end;

{==============================================================================}
{ Test 15: Fluent Class Schema, Generics & Pure Primitive Properties           }
{==============================================================================}

procedure TestApproachBFluentSchemaAndGenerics;
const
  TEST_DB = '/tmp/deltamodel_test_approach_b.sqlite';
var
  Engine: TDeltaORMEngine;
  Query: TFastProductQuery;
  Meta: TModelMetadata;
  P, Found: TFastProductModel;
  List, List2: TFastProductList;
  JsonStr: RawByteString;
  I: Integer;
  ParsedData: TJSONData;
begin
  Suite('15. Fluent Class Schema, Generics & Pure Primitive Properties (Approach B)');

  if FileExists(TEST_DB) then DeleteFile(TEST_DB);

  Engine := TDeltaORMEngine.Create('sqlite://' + TEST_DB);
  try
    Engine.ExecuteDirect(
      'CREATE TABLE fast_products (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  name VARCHAR(150) NOT NULL,' +
      '  price REAL NOT NULL,' +
      '  cost REAL,' +
      '  stock INTEGER NOT NULL,' +
      '  active INTEGER NOT NULL' +
      ');'
    );
    if Engine.TransactionActive then Engine.Commit;

    // 1. Class metadata validation
    Meta := TRTTICache.GetModelMetadata(TFastProductModel);
    AssertTrue(Meta <> nil, 'Metadados da classe TFastProductModel extraídos com sucesso');
    AssertEquals('fast_products', Meta.TableName, 'Nome da tabela correto via ConfigureSchema');
    AssertEquals('id', Meta.PKFieldName, 'Nome do campo PK correto');
    AssertEquals('Id', Meta.PKPropName, 'Nome da propriedade PK correto');
    AssertEqualsInt(6, Meta.FieldCount, 'Quantidade de campos configurados no Schema');
    AssertTrue(Meta.FindField('name') <> nil, 'Campo name encontrado');
    AssertTrue(Meta.FindField('price') <> nil, 'Campo price encontrado');
    AssertTrue(Meta.FindField('cost') <> nil, 'Campo cost encontrado');

    // 2. Nullability validation (ultra-light bitmask)
    P := TFastProductModel.Create;
    try
      P.Name := 'Teclado Mecanico';
      P.Price := 350.00;
      P.Cost := 200.00;
      P.Stock := 15;
      P.Active := True;

      AssertFalse(P.IsNull('Cost'), 'Cost inicialmente não é nulo');
      P.SetNull('Cost');
      AssertTrue(P.IsNull('Cost'), 'Cost marcado como nulo via SetNull(Cost)');
      P.ClearNull('Cost');
      AssertFalse(P.IsNull('Cost'), 'Cost desmarcado como nulo via ClearNull(Cost)');
    finally
      P.Free;
    end;

    // 3. Typed insert and query via TDeltaQuery<T>
    Query := TFastProductQuery.Create(Engine);
    try
      P := TFastProductModel.Create;
      P.Name := 'Monitor Gamer 240Hz';
      P.Price := 1899.90;
      P.Cost := 1200.00;
      P.Stock := 25;
      P.Active := True;
      AssertTrue(Query.Insert(P), 'Query.Insert(P) retornou True');
      P.Free;

      P := TFastProductModel.Create;
      P.Name := 'Mouse Sem Fio RGB';
      P.Price := 149.50;
      P.SetNull('Cost'); // Test nullability in database
      P.Stock := 50;
      P.Active := True;
      AssertTrue(Query.Insert(P), 'Query.Insert(P) com Cost nulo retornou True');
      P.Free;

      P := TFastProductModel.Create;
      P.Name := 'Headset 7.1 Surround';
      P.Price := 499.00;
      P.Cost := 310.00;
      P.Stock := 0;
      P.Active := False;
      AssertTrue(Query.Insert(P), 'Query.Insert(P) inativo retornou True');
      P.Free;

      // Count and Exists
      AssertEqualsInt(3, Query.Count, 'Query.Count retornou 3 produtos');
      AssertTrue(Query.Where('price > 1000').Exists, 'Query.Exists com preco > 1000 retornou True');

      // Typed lookup by ID
      Found := Query.FindById(1);
      AssertTrue(Found <> nil, 'Query.FindById(1) retornou objeto');
      if Found <> nil then
      try
        AssertEqualsInt(1, Found.Id, 'Id confere');
        AssertEquals('Monitor Gamer 240Hz', Found.Name, 'Name confere');
        AssertTrue(Abs(Found.Price - 1899.90) < 0.001, 'Price confere');
        AssertFalse(Found.IsNull('Cost'), 'Cost não é nulo');
        AssertEqualsInt(25, Found.Stock, 'Stock confere');
        AssertTrue(Found.Active, 'Active confere');
      finally
        Found.Free;
      end;

      // Lookup by ID with null field
      Found := Query.FindById(2);
      AssertTrue(Found <> nil, 'Query.FindById(2) retornou objeto');
      if Found <> nil then
      try
        AssertEquals('Mouse Sem Fio RGB', Found.Name, 'Name confere');
        AssertTrue(Found.IsNull('Cost'), 'Cost nulo preservado corretamente ao ler do banco');
      finally
        Found.Free;
      end;

      // 4. Typed collection query and "for P in List do" iteration
      Query.Clear;
      List := Query.Where('price > 200').OrderBy('price DESC').All;
      try
        AssertEqualsInt(2, List.Count, 'List.Count retornou 2 produtos com preco > 200');

        // Typed idiomatic loop without typecasting!
        I := 0;
        for P in List do
        begin
          Inc(I);
          AssertTrue(P.Price > 200, Format('Item %d (%s) tem preco R$ %.2f > 200', [I, P.Name, P.Price]));
        end;
        AssertEqualsInt(2, I, 'Iterador MoveNext percorreu exatamente 2 itens tipados');

        // 5. JSON Serialization and Deserialization
        JsonStr := List.ToJson;
        AssertTrue(Length(JsonStr) > 0, 'Json gerado com sucesso via ToJson');
        AssertTrue(Pos('Monitor Gamer 240Hz', JsonStr) > 0, 'JSON contém nome do monitor');
        AssertTrue(Pos('1899.9', JsonStr) > 0, 'JSON contém preco em ponto fixo');

        ParsedData := GetJSON(JsonStr);
        try
          AssertTrue(ParsedData is TJSONArray, 'JSON gerado é um array puro TJSONArray');
          AssertEqualsInt(2, TJSONArray(ParsedData).Count, 'Array contém 2 elementos');
        finally
          ParsedData.Free;
        end;

        // Deserialization with FromJson
        List2 := TFastProductList.Create;
        try
          List2.FromJson(JsonStr);
          AssertEqualsInt(2, List2.Count, 'FromJson desserializou 2 itens na nova lista');
          AssertEquals('Monitor Gamer 240Hz', List2[0].Name, 'Primeiro item desserializado confere');
          AssertTrue(Abs(List2[0].Price - 1899.90) < 0.001, 'Preço do primeiro item confere');
        finally
          List2.Free;
        end;
      finally
        List.Free;
      end;

      // 6. Typed Update and BulkInsert
      Found := Query.FindById(1);
      if Found <> nil then
      try
        Found.Price := 1750.00;
        Found.Stock := 30;
        AssertTrue(Query.Update(Found), 'Query.Update(Found) retornou True');
      finally
        Found.Free;
      end;

      Found := Query.FindById(1);
      if Found <> nil then
      try
        AssertTrue(Abs(Found.Price - 1750.00) < 0.001, 'Preço atualizado conferido');
        AssertEqualsInt(30, Found.Stock, 'Estoque atualizado conferido');
      finally
        Found.Free;
      end;

      // BulkInsert tipado
      List := TFastProductList.Create;
      try
        for I := 1 to 10 do
        begin
          P := TFastProductModel.Create;
          P.Name := 'Produto em Lote #' + IntToStr(I);
          P.Price := 10.0 + I;
          P.Stock := 100;
          P.Active := True;
          List.Add(P);
        end;
        AssertEqualsInt(10, Query.BulkInsert(List, 5), 'BulkInsert inseriu 10 itens em lotes');
      finally
        List.Free;
      end;

      Query.Clear;
      AssertEqualsInt(13, Query.Count, 'Total de produtos no banco após BulkInsert é 13');
    finally
      Query.Free;
    end;
  finally
    Engine.Free;
    if FileExists(TEST_DB) then DeleteFile(TEST_DB);
  end;
end;

procedure TestDeclarativeSchemaValidation;
var
  M: TValidatedCustomerModel;
  Passed: Boolean;
begin
  Suite('16. Declarative Schema Validations (Min/Max, Length, Range, Email, CPF, Positive)');

  M := TValidatedCustomerModel.Create;
  try
    M.Name := 'Carlos Silva';
    M.Age := 30;
    M.Email := 'carlos.silva@empresa.com';
    M.Cpf := '52998224725';
    M.Score := 95.5;

    Passed := False;
    try
      M.Validate;
      Passed := True;
    except
      Passed := False;
    end;
    AssertTrue(Passed, 'Modelo válido passou em todas as regras declarativas');

    M.Name := 'AB';
    Passed := False;
    try
      M.Validate;
    except
      on E: EDeltaValidation do
        Passed := True;
    end;
    AssertTrue(Passed, 'Nome com menos de 3 caracteres gerou EDeltaValidation');

    M.Name := 'Carlos Silva';
    M.Age := 15;
    Passed := False;
    try
      M.Validate;
    except
      on E: EDeltaValidation do
        Passed := True;
    end;
    AssertTrue(Passed, 'Idade 15 fora de Range(18, 120) gerou EDeltaValidation');

    M.Age := 25;
    M.Email := 'email-invalido';
    Passed := False;
    try
      M.Validate;
    except
      on E: EDeltaValidation do
        Passed := True;
    end;
    AssertTrue(Passed, 'Email sem formato válido gerou EDeltaValidation');

    M.Email := 'carlos@empresa.com';
    M.Cpf := '11111111111';
    Passed := False;
    try
      M.Validate;
    except
      on E: EDeltaValidation do
        Passed := True;
    end;
    AssertTrue(Passed, 'CPF com dígitos repetidos gerou EDeltaValidation');

    M.Cpf := '52998224725';
    M.Score := -5.0;
    Passed := False;
    try
      M.Validate;
    except
      on E: EDeltaValidation do
        Passed := True;
    end;
    AssertTrue(Passed, 'Score negativo violando Positive gerou EDeltaValidation');
  finally
    M.Free;
  end;
end;

procedure TestTransactionScopeRAII;
var
  Engine: TDeltaORMEngine;
  Tx: IDeltaTransaction;
  CountVal: Int64;
const
  TEST_DB = '/tmp/test_tx_scope.sqlite';
begin
  Suite('17. Safe Transaction Scope & RAII Rollback');

  if FileExists(TEST_DB) then DeleteFile(TEST_DB);
  Engine := TDeltaORMEngine.Create('sqlite://' + TEST_DB);
  try
    Engine.Connection.Open;
    Engine.ExecuteDirect('CREATE TABLE tx_records (id INTEGER PRIMARY KEY, title TEXT);');

    Tx := Engine.BeginTransaction;
    AssertTrue(Tx.IsActive, 'Tx.IsActive é True após BeginTransaction');
    AssertTrue(Engine.TransactionActive, 'Engine.TransactionActive é True');
    Engine.ExecuteDirect('INSERT INTO tx_records (id, title) VALUES (1, ''Item Confirmado'');');
    Tx.Commit;
    AssertFalse(Tx.IsActive, 'Tx.IsActive é False após Commit');
    AssertFalse(Engine.TransactionActive, 'Engine.TransactionActive é False após Commit');

    CountVal := Engine.ExecuteScalar('SELECT COUNT(*) FROM tx_records;');
    AssertEqualsInt(1, CountVal, 'Registro persistido após Commit');

    begin
      Tx := Engine.BeginTransaction;
      AssertTrue(Tx.IsActive, 'Tx.IsActive é True dentro do escopo RAII');
      Engine.ExecuteDirect('INSERT INTO tx_records (id, title) VALUES (2, ''Item Abandonado'');');
      Tx := nil;
    end;
    AssertFalse(Engine.TransactionActive, 'Engine.TransactionActive é False após auto-rollback de Tx');

    CountVal := Engine.ExecuteScalar('SELECT COUNT(*) FROM tx_records;');
    AssertEqualsInt(1, CountVal, 'Registro não persistido após auto-rollback automático');

    Tx := Engine.BeginTransaction;
    Engine.ExecuteDirect('INSERT INTO tx_records (id, title) VALUES (3, ''Item Revertido'');');
    Tx.Rollback;
    AssertFalse(Tx.IsActive, 'Tx.IsActive é False após Rollback explícito');
    Tx := nil;

    CountVal := Engine.ExecuteScalar('SELECT COUNT(*) FROM tx_records;');
    AssertEqualsInt(1, CountVal, 'Total no banco permanece 1 após rollback explícito');
  finally
    Engine.Free;
    if FileExists(TEST_DB) then DeleteFile(TEST_DB);
  end;
end;

procedure TestFastJsonParser;
var
  P: TFastProductModel;
  List: TFastProductList;
  Order: TTestOrder;
  Parser: TDeltaFastJsonParser;
  JsonStr: string;
begin
  Suite('18. High-Performance Lexical Fast JSON Parser');

  JsonStr := '{"Id": 42, "Name": "Teclado Sem Fio", "Price": 350.50, "Cost": 180.0, "Stock": 15, "Active": true}';
  P := TFastProductModel.Create;
  Parser := TDeltaFastJsonParser.Create(JsonStr);
  try
    Parser.ParseIntoModel(P);
    AssertEqualsInt(42, P.Id, 'Id parseado com sucesso');
    AssertEquals('Teclado Sem Fio', P.Name, 'Name parseado com sucesso');
    AssertTrue(Abs(P.Price - 350.50) < 0.001, 'Price parseado com sucesso');
    AssertEqualsInt(15, P.Stock, 'Stock parseado com sucesso');
    AssertTrue(P.Active, 'Active parseado com sucesso');
  finally
    Parser.Free;
    P.Free;
  end;

  JsonStr := '{"items": [{"Id": 101, "Name": "Produto A", "Price": 10.0, "Stock": 5, "Active": true}, ' +
             '{"Id": 102, "Name": "Produto B", "Price": 20.0, "Stock": 8, "Active": false}], ' +
             '"page": 2, "page_size": 25, "total_records": 100, "total_pages": 4}';
  List := TFastProductList.Create;
  Parser := TDeltaFastJsonParser.Create(JsonStr);
  try
    Parser.ParseIntoList(List);
    AssertEqualsInt(2, List.Count, 'Lista contém 2 itens parseados');
    AssertEquals('Produto A', List[0].Name, 'Primeiro item tem nome Produto A');
    AssertEqualsInt(102, List[1].Id, 'Segundo item tem Id 102');
    AssertEqualsInt(2, List.page, 'Metadado page parseado');
    AssertEqualsInt(25, List.page_size, 'Metadado page_size parseado');
    AssertEqualsInt(100, List.total_records, 'Metadado total_records parseado');
    AssertEqualsInt(4, List.total_pages, 'Metadado total_pages parseado');
  finally
    Parser.Free;
    List.Free;
  end;

  JsonStr := '{"Id": 500, "CustomerName": "Tech Corp", "Items": [' +
             '{"Id": 1, "OrderId": 500, "ProductName": "Mouse Gamer", "Quantity": 2}, ' +
             '{"Id": 2, "OrderId": 500, "ProductName": "Teclado RGB", "Quantity": 1}]}';
  Order := TTestOrder.Create;
  Order.Items.References(TTestOrderItem, 'order_id');
  Parser := TDeltaFastJsonParser.Create(JsonStr);
  try
    Parser.ParseIntoModel(Order);
    AssertEqualsInt(500, Order.Id, 'Order.Id parseado');
    AssertEquals('Tech Corp', Order.CustomerName, 'Order.CustomerName parseado');
    AssertEqualsInt(2, Order.Items.Count, 'Order.Items contém 2 itens filhos');
    AssertEquals('Mouse Gamer', TTestOrderItem(Order.Items[0]).ProductName, 'Filho 1 ProductName parseado');
    AssertEqualsInt(2, TTestOrderItem(Order.Items[0]).Quantity, 'Filho 1 Quantity parseado');
    AssertEquals('Teclado RGB', TTestOrderItem(Order.Items[1]).ProductName, 'Filho 2 ProductName parseado');
  finally
    Parser.Free;
    Order.Free;
  end;
end;

procedure TestLocalizationMessages;
begin
  Suite('19. Unified i18n Localization Messages');

  SetDeltaLocale(dlEnUS);
  AssertEquals('en-US', GetDeltaLocaleName, 'Locale padrão é en-US');
  AssertEquals('Field Customer.Name is required.', MsgFieldRequired('Customer', 'Name'), 'MsgFieldRequired em inglês');
  AssertEquals('Field Customer.Name minimum length is 3 characters.', MsgFieldMinLength('Customer', 'Name', 3), 'MsgFieldMinLength em inglês');
  AssertEquals('Field Customer.Email is not a valid email address.', MsgFieldInvalidEmail('Customer', 'Email'), 'MsgFieldInvalidEmail em inglês');

  SetDeltaLocale(dlPtBR);
  AssertEquals('pt-BR', GetDeltaLocaleName, 'Locale alterado para pt-BR');
  AssertEquals('O campo Customer.Name é obrigatório.', MsgFieldRequired('Customer', 'Name'), 'MsgFieldRequired em português');
  AssertEquals('O campo Customer.Name deve ter no mínimo 3 caracteres.', MsgFieldMinLength('Customer', 'Name', 3), 'MsgFieldMinLength em português');
  AssertEquals('O campo Customer.Email não é um e-mail válido.', MsgFieldInvalidEmail('Customer', 'Email'), 'MsgFieldInvalidEmail em português');

  SetDeltaLocaleByName('es-es');
  AssertEquals('es-ES', GetDeltaLocaleName, 'Locale alterado para es-ES por nome');
  AssertEquals('El campo Customer.Name es obligatorio.', MsgFieldRequired('Customer', 'Name'), 'MsgFieldRequired em espanhol');
  AssertEquals('El campo Customer.Name debe tener un mínimo de 3 caracteres.', MsgFieldMinLength('Customer', 'Name', 3), 'MsgFieldMinLength em espanhol');

  SetDeltaLocaleByName('en-us');
  AssertEquals('en-US', GetDeltaLocaleName, 'Locale retornado para en-US');
end;

procedure TestEagerLoadingAndStreaming;
var
  Engine: TDeltaORMEngine;
  OrderQuery: specialize TDeltaQuery<TTestOrder>;
  OrderList: specialize TGDeltaModelList<TTestOrder>;
  SingleOrder: TTestOrder;
  MemStream: TMemoryStream;
  StreamStr: string;
  StreamedCount: Int64;
const
  TEST_DB = '/tmp/test_eager_loading.sqlite';
begin
  Suite('20. 1:N Eager Loading (Include) & HTTP Chunked Streaming');

  if FileExists(TEST_DB) then DeleteFile(TEST_DB);
  Engine := TDeltaORMEngine.Create('sqlite://' + TEST_DB);
  try
    Engine.Connection.Open;
    Engine.ExecuteDirect('CREATE TABLE test_orders (id INTEGER PRIMARY KEY, customer_name TEXT);');
    Engine.ExecuteDirect('CREATE TABLE test_order_items (id INTEGER PRIMARY KEY, order_id INTEGER, product_name TEXT, quantity INTEGER);');

    Engine.ExecuteDirect('INSERT INTO test_orders (id, customer_name) VALUES (1, ''Alpha Inc'');');
    Engine.ExecuteDirect('INSERT INTO test_orders (id, customer_name) VALUES (2, ''Beta Ltd'');');

    Engine.ExecuteDirect('INSERT INTO test_order_items (id, order_id, product_name, quantity) VALUES (10, 1, ''Server Rack'', 1);');
    Engine.ExecuteDirect('INSERT INTO test_order_items (id, order_id, product_name, quantity) VALUES (11, 1, ''Ethernet Switch'', 4);');
    Engine.ExecuteDirect('INSERT INTO test_order_items (id, order_id, product_name, quantity) VALUES (12, 2, ''Fiber Cable'', 20);');

    OrderQuery := specialize TDeltaQuery<TTestOrder>.Create(Engine);
    try
      OrderList := OrderQuery.Include('Items').All;
      try
        AssertEqualsInt(2, OrderList.Count, 'Consulta retornou 2 pedidos');
        AssertEquals('Alpha Inc', OrderList[0].CustomerName, 'Primeiro pedido é Alpha Inc');
        AssertEqualsInt(2, OrderList[0].Items.Count, 'Pedido 1 carregou eager 2 itens filhos');
        AssertEquals('Server Rack', TTestOrderItem(OrderList[0].Items[0]).ProductName, 'Item 1 do Pedido 1 confere');
        AssertEquals('Ethernet Switch', TTestOrderItem(OrderList[0].Items[1]).ProductName, 'Item 2 do Pedido 1 confere');

        AssertEquals('Beta Ltd', OrderList[1].CustomerName, 'Segundo pedido é Beta Ltd');
        AssertEqualsInt(1, OrderList[1].Items.Count, 'Pedido 2 carregou eager 1 item filho');
        AssertEquals('Fiber Cable', TTestOrderItem(OrderList[1].Items[0]).ProductName, 'Item 1 do Pedido 2 confere');
      finally
        OrderList.Free;
      end;

      SingleOrder := OrderQuery.Include('Items').FindById(1);
      try
        AssertTrue(SingleOrder <> nil, 'FindById(1) retornou pedido');
        AssertEquals('Alpha Inc', SingleOrder.CustomerName, 'SingleOrder é Alpha Inc');
        AssertEqualsInt(2, SingleOrder.Items.Count, 'SingleOrder carregou 2 itens associados');
      finally
        SingleOrder.Free;
      end;

      MemStream := TMemoryStream.Create;
      try
        OrderQuery.Clear;
        StreamedCount := OrderQuery.StreamJsonTo(MemStream, 1);
        AssertEqualsInt(2, StreamedCount, 'StreamJsonTo transmitiu 2 registros em lotes de tamanho 1');
        SetString(StreamStr, PAnsiChar(MemStream.Memory), MemStream.Size);
        AssertTrue(StreamStr.StartsWith('['), 'Stream JSON inicia com array "["');
        AssertTrue(StreamStr.EndsWith(']'), 'Stream JSON encerra com array "]"');
        AssertTrue(Pos('Alpha Inc', StreamStr) > 0, 'Stream contém Alpha Inc');
        AssertTrue(Pos('Beta Ltd', StreamStr) > 0, 'Stream contém Beta Ltd');
      finally
        MemStream.Free;
      end;
    finally
      OrderQuery.Free;
    end;
  finally
    Engine.Free;
    if FileExists(TEST_DB) then DeleteFile(TEST_DB);
  end;
end;

{==============================================================================}
{ Test 21: Pluggable Codec Architecture (IDeltaCodec & TDeltaCodecRegistry)    }
{==============================================================================}

type
  { Dummy Codec to validate pluggability and extensibility }
  TDummyYAMLCodec = class(TInterfacedObject, IDeltaCodec)
  public
    function GetFormatName: string;
    function GetMimeType: string;
    function CanHandleMimeType(const AMimeType: string): Boolean;
    procedure EncodeModel(AModel: TDeltaModel; AOutputStream: TStream);
    procedure EncodeList(AList: TObject; AOutputStream: TStream);
    procedure DecodeModel(AInputStream: TStream; AModel: TDeltaModel);
    procedure DecodeList(AInputStream: TStream; AList: TObject; AModelClass: TClass);
  end;

function TDummyYAMLCodec.GetFormatName: string;
begin
  Result := 'yaml';
end;

function TDummyYAMLCodec.GetMimeType: string;
begin
  Result := 'application/x-yaml';
end;

function TDummyYAMLCodec.CanHandleMimeType(const AMimeType: string): Boolean;
begin
  Result := (Pos('yaml', LowerCase(AMimeType)) > 0);
end;

procedure TDummyYAMLCodec.EncodeModel(AModel: TDeltaModel; AOutputStream: TStream);
var
  S: RawByteString;
begin
  S := 'id: 1' + sLineBreak;
  if (AOutputStream <> nil) and (Length(S) > 0) then
    AOutputStream.WriteBuffer(S[1], Length(S));
end;

procedure TDummyYAMLCodec.EncodeList(AList: TObject; AOutputStream: TStream);
var
  S: RawByteString;
begin
  S := '- id: 1' + sLineBreak;
  if (AOutputStream <> nil) and (Length(S) > 0) then
    AOutputStream.WriteBuffer(S[1], Length(S));
end;

procedure TDummyYAMLCodec.DecodeModel(AInputStream: TStream; AModel: TDeltaModel);
begin
  // No-op for testing
end;

procedure TDummyYAMLCodec.DecodeList(AInputStream: TStream; AList: TObject; AModelClass: TClass);
begin
  // No-op for testing
end;

procedure TestCodecArchitecture;
var
  Codec: IDeltaCodec;
  SupportedMimes, SupportedFormats: TStringArray;
  P: TProductModel;
  EncodedStr: string;
  DecodedP: TProductModel;
  CustomCodec: IDeltaCodec;
  CaughtException: Boolean;
begin
  Suite('Arquitetura Plugável de Codecs (IDeltaCodec & TDeltaCodecRegistry)');

  // 1. Auto-registered codecs validation
  AssertTrue(TDeltaCodecRegistry.HasCodec('json'), 'Codec "json" está registrado');
  AssertTrue(TDeltaCodecRegistry.HasCodec('application/json'), 'Codec "application/json" está registrado');
  AssertTrue(TDeltaCodecRegistry.HasCodec('cbor'), 'Codec "cbor" está registrado');
  AssertTrue(TDeltaCodecRegistry.HasCodec('bson'), 'Codec "bson" está registrado');
  AssertTrue(TDeltaCodecRegistry.HasCodec('xml'), 'Codec "xml" está registrado');
  AssertTrue(TDeltaCodecRegistry.HasCodec('csv'), 'Codec "csv" está registrado');
  AssertFalse(TDeltaCodecRegistry.HasCodec('yaml'), 'Codec "yaml" não está registrado inicialmente');

  // 2. JSON Codec resolution
  Codec := TDeltaCodecRegistry.GetCodec('json');
  AssertTrue(Codec <> nil, 'GetCodec("json") retornou instância válida');
  AssertEquals('json', Codec.GetFormatName, 'GetFormatName é "json"');
  AssertEquals('application/json', Codec.GetMimeType, 'GetMimeType é "application/json"');
  AssertTrue(Codec.CanHandleMimeType('application/problem+json'), 'CanHandleMimeType aceita problem+json');

  // 3. Model Serialization and Deserialization via Registry Helpers
  P := TProductModel.Create;
  try
    P.Id.Value := 101;
    P.Name.Value := 'Monitor Gamer UltraWide 34';
    P.Price.Value := 3499.50;
    P.Active.Value := True;

    EncodedStr := TDeltaCodecRegistry.EncodeModelToString(P, 'json');
    AssertTrue(Pos('Monitor Gamer UltraWide 34', EncodedStr) > 0, 'EncodeModelToString gerou JSON com nome');
    AssertTrue(Pos('3499.5', EncodedStr) > 0, 'EncodeModelToString gerou JSON com preço');

    DecodedP := TProductModel.Create;
    try
      TDeltaCodecRegistry.DecodeModelFromString(EncodedStr, DecodedP, 'application/json');
      AssertEqualsInt(101, DecodedP.Id.Value, 'DecodeModelFromString restaurou Id 101');
      AssertEquals('Monitor Gamer UltraWide 34', DecodedP.Name.Value, 'DecodeModelFromString restaurou Name');
      AssertTrue(Abs(DecodedP.Price.Value - 3499.50) < 0.001, 'DecodeModelFromString restaurou Price');
    finally
      DecodedP.Free;
    end;
  finally
    P.Free;
  end;

  // 4. Dynamic Custom Codec Registration (YAML)
  CustomCodec := TDummyYAMLCodec.Create;
  TDeltaCodecRegistry.RegisterCodec(CustomCodec);
  AssertTrue(TDeltaCodecRegistry.HasCodec('yaml'), 'Novo codec "yaml" foi registrado com sucesso');
  AssertTrue(TDeltaCodecRegistry.HasCodec('application/x-yaml'), 'Novo codec responde a "application/x-yaml"');

  SupportedFormats := TDeltaCodecRegistry.GetSupportedFormats;
  AssertTrue(Length(SupportedFormats) >= 5, 'Há pelo menos 5 formatos suportados no Registry');

  SupportedMimes := TDeltaCodecRegistry.GetSupportedMimeTypes;
  AssertTrue(Length(SupportedMimes) >= 5, 'Há pelo menos 5 MIME types suportados no Registry');

  // 5. Desregistro de Codec
  TDeltaCodecRegistry.UnregisterCodec('yaml');
  AssertFalse(TDeltaCodecRegistry.HasCodec('yaml'), 'Codec "yaml" foi desregistrado');

  // 6. Error handling when looking up missing codec
  CaughtException := False;
  try
    TDeltaCodecRegistry.GetCodec('inexistente_formato_xyz');
  except
    on E: EDeltaCodecNotFound do
      CaughtException := True;
  end;
  AssertTrue(CaughtException, 'Busca por codec inexistente dispara EDeltaCodecNotFound');
end;

{==============================================================================}
{ Test 22: Binary BSON v1.1 Codec (Zero-DOM)                                   }
{==============================================================================}

procedure TestBsonCodec;
var
  Codec: IDeltaCodec;
  MS: TMemoryStream;
  P, DecodedP: TProductModel;
  List, DecodedList: TProductGenericList;
  I: Integer;
  DocSize: Int32;
begin
  Suite('Codec BSON v1.1 Binário (Zero-DOM)');

  // 1. BSON Codec resolution
  AssertTrue(TDeltaCodecRegistry.HasCodec('bson'), 'Codec "bson" registrado');
  AssertTrue(TDeltaCodecRegistry.HasCodec('application/bson'), 'Codec "application/bson" registrado');
  Codec := TDeltaCodecRegistry.GetCodec('bson');
  AssertTrue(Codec <> nil, 'GetCodec("bson") retornou instância');
  AssertEquals('bson', Codec.GetFormatName, 'GetFormatName é "bson"');
  AssertEquals('application/bson', Codec.GetMimeType, 'GetMimeType é "application/bson"');

  // 2. Simple model and UTF-8 round-trip
  P := TProductModel.Create;
  try
    P.Id.Value := 202;
    P.Name.Value := 'SSD NVMe 2TB Gen4 Alta Precisão & Performance';
    P.Price.Value := 899.90;
    P.Active.Value := True;
    P.BigCode.Value := 9876543210123;

    MS := TMemoryStream.Create;
    try
      Codec.EncodeModel(P, MS);
      AssertTrue(MS.Size > 4, 'BSON gerou stream com conteúdo');

      MS.Position := 0;
      MS.ReadBuffer(DocSize, 4);
      AssertEqualsInt(MS.Size, LEtoN(DocSize), 'Tamanho do cabeçalho BSON confere com o stream');

      DecodedP := TProductModel.Create;
      try
        MS.Position := 0;
        Codec.DecodeModel(MS, DecodedP);

        AssertEqualsInt(202, DecodedP.Id.Value, 'BSON restaurou Id');
        AssertEquals('SSD NVMe 2TB Gen4 Alta Precisão & Performance', DecodedP.Name.Value, 'BSON restaurou UTF-8 Name');
        AssertTrue(Abs(DecodedP.Price.Value - 899.90) < 0.001, 'BSON restaurou Price Double');
        AssertTrue(DecodedP.Active.Value, 'BSON restaurou Active Boolean');
        AssertEqualsInt(9876543210123, DecodedP.BigCode.Value, 'BSON restaurou BigCode Int64');
        AssertTrue(DecodedP.Description.IsNull, 'BSON preservou campo nulo Description');
      finally
        DecodedP.Free;
      end;
    finally
      MS.Free;
    end;
  finally
    P.Free;
  end;

  // 3. Model List round-trip
  List := TProductGenericList.Create;
  try
    for I := 1 to 5 do
    begin
      P := TProductModel.Create;
      P.Id.Value := I;
      P.Name.Value := 'Produto BSON #' + IntToStr(I);
      P.Price.Value := I * 100.50;
      P.Active.Value := (I mod 2 = 1);
      List.Add(P);
    end;

    MS := TMemoryStream.Create;
    try
      Codec.EncodeList(List, MS);
      AssertTrue(MS.Size > 20, 'BSON EncodeList gerou payload binário');

      DecodedList := TProductGenericList.Create;
      try
        MS.Position := 0;
        Codec.DecodeList(MS, DecodedList, TProductModel);
        AssertEqualsInt(5, DecodedList.Count, 'BSON DecodeList restaurou todos os 5 itens');
        for I := 0 to 4 do
        begin
          AssertEqualsInt(I + 1, DecodedList[I].Id.Value, 'Item ID confere');
          AssertEquals('Produto BSON #' + IntToStr(I + 1), DecodedList[I].Name.Value, 'Item Name confere');
          AssertTrue(Abs(DecodedList[I].Price.Value - ((I + 1) * 100.50)) < 0.001, 'Item Price confere');
        end;
      finally
        DecodedList.Free;
      end;
    finally
      MS.Free;
    end;
  finally
    List.Free;
  end;
end;

{==============================================================================}
{ Test 23: Binary CBOR (RFC 8949) Codec (Zero-DOM)                             }
{==============================================================================}

procedure TestCborCodec;
var
  Codec: IDeltaCodec;
  MS: TMemoryStream;
  P, DecodedP: TProductModel;
  List, DecodedList: TProductGenericList;
  I: Integer;
begin
  Suite('Codec CBOR (RFC 8949) Binário (Zero-DOM)');

  // 1. CBOR Codec resolution
  AssertTrue(TDeltaCodecRegistry.HasCodec('cbor'), 'Codec "cbor" registrado');
  AssertTrue(TDeltaCodecRegistry.HasCodec('application/cbor'), 'Codec "application/cbor" registrado');
  Codec := TDeltaCodecRegistry.GetCodec('cbor');
  AssertTrue(Codec <> nil, 'GetCodec("cbor") retornou instância');
  AssertEquals('cbor', Codec.GetFormatName, 'GetFormatName é "cbor"');
  AssertEquals('application/cbor', Codec.GetMimeType, 'GetMimeType é "application/cbor"');

  // 2. Simple model and UTF-8 round-trip
  P := TProductModel.Create;
  try
    P.Id.Value := 303;
    P.Name.Value := 'Placa de Vídeo RTX 4080 Super - Edição Especial';
    P.Price.Value := 7299.99;
    P.Active.Value := True;
    P.BigCode.Value := 123456789012345;

    MS := TMemoryStream.Create;
    try
      Codec.EncodeModel(P, MS);
      AssertTrue(MS.Size > 4, 'CBOR gerou stream com conteúdo');

      DecodedP := TProductModel.Create;
      try
        MS.Position := 0;
        Codec.DecodeModel(MS, DecodedP);

        AssertEqualsInt(303, DecodedP.Id.Value, 'CBOR restaurou Id');
        AssertEquals('Placa de Vídeo RTX 4080 Super - Edição Especial', DecodedP.Name.Value, 'CBOR restaurou Name');
        AssertTrue(Abs(DecodedP.Price.Value - 7299.99) < 0.001, 'CBOR restaurou Price Double');
        AssertTrue(DecodedP.Active.Value, 'CBOR restaurou Active Boolean');
        AssertEqualsInt(123456789012345, DecodedP.BigCode.Value, 'CBOR restaurou BigCode Int64');
        AssertTrue(DecodedP.Description.IsNull, 'CBOR preservou campo nulo Description');
      finally
        DecodedP.Free;
      end;
    finally
      MS.Free;
    end;
  finally
    P.Free;
  end;

  // 3. Model List round-trip
  List := TProductGenericList.Create;
  try
    for I := 1 to 4 do
    begin
      P := TProductModel.Create;
      P.Id.Value := I * 10;
      P.Name.Value := 'Produto CBOR #' + IntToStr(I);
      P.Price.Value := I * 50.25;
      P.Active.Value := True;
      List.Add(P);
    end;

    MS := TMemoryStream.Create;
    try
      Codec.EncodeList(List, MS);
      AssertTrue(MS.Size > 15, 'CBOR EncodeList gerou payload binário');

      DecodedList := TProductGenericList.Create;
      try
        MS.Position := 0;
        Codec.DecodeList(MS, DecodedList, TProductModel);
        AssertEqualsInt(4, DecodedList.Count, 'CBOR DecodeList restaurou todos os 4 itens');
        for I := 0 to 3 do
        begin
          AssertEqualsInt((I + 1) * 10, DecodedList[I].Id.Value, 'Item ID confere');
          AssertEquals('Produto CBOR #' + IntToStr(I + 1), DecodedList[I].Name.Value, 'Item Name confere');
          AssertTrue(Abs(DecodedList[I].Price.Value - ((I + 1) * 50.25)) < 0.001, 'Item Price confere');
        end;
      finally
        DecodedList.Free;
      end;
    finally
      MS.Free;
    end;
  finally
    List.Free;
  end;
end;

{==============================================================================}
{ Test 24: High-Performance XML Codec (Zero-DOM)                               }
{==============================================================================}

procedure TestXmlCodec;
var
  Codec: IDeltaCodec;
  MS: TMemoryStream;
  P, DecodedP: TProductModel;
  List, DecodedList: TProductGenericList;
  I: Integer;
begin
  Suite('Codec XML de Alta Performance (Zero-DOM)');

  // 1. XML Codec resolution
  AssertTrue(TDeltaCodecRegistry.HasCodec('xml'), 'Codec "xml" registrado');
  AssertTrue(TDeltaCodecRegistry.HasCodec('application/xml'), 'Codec "application/xml" registrado');
  AssertTrue(TDeltaCodecRegistry.HasCodec('text/xml'), 'Codec "text/xml" registrado');
  Codec := TDeltaCodecRegistry.GetCodec('xml');
  AssertTrue(Codec <> nil, 'GetCodec("xml") retornou instância');
  AssertEquals('xml', Codec.GetFormatName, 'GetFormatName é "xml"');
  AssertEquals('application/xml', Codec.GetMimeType, 'GetMimeType é "application/xml"');

  // 2. Simple model round-trip with accents and XML special characters
  P := TProductModel.Create;
  try
    P.Id.Value := 404;
    P.Name.Value := 'Monitor Curvo 49" QLED & HDR — Edição Especial <Pro>';
    P.Price.Value := 5899.90;
    P.Active.Value := True;
    P.BigCode.Value := 1234567890;

    MS := TMemoryStream.Create;
    try
      Codec.EncodeModel(P, MS);
      AssertTrue(MS.Size > 20, 'XML gerou stream com conteúdo');

      DecodedP := TProductModel.Create;
      try
        MS.Position := 0;
        Codec.DecodeModel(MS, DecodedP);

        AssertEqualsInt(404, DecodedP.Id.Value, 'XML restaurou Id');
        AssertEquals('Monitor Curvo 49" QLED & HDR — Edição Especial <Pro>', DecodedP.Name.Value, 'XML restaurou Name com entidades escapadas');
        AssertTrue(Abs(DecodedP.Price.Value - 5899.90) < 0.001, 'XML restaurou Price Double');
        AssertTrue(DecodedP.Active.Value, 'XML restaurou Active Boolean');
        AssertEqualsInt(1234567890, DecodedP.BigCode.Value, 'XML restaurou BigCode Int64');
        AssertTrue(DecodedP.Description.IsNull, 'XML preservou campo nulo Description');
      finally
        DecodedP.Free;
      end;
    finally
      MS.Free;
    end;
  finally
    P.Free;
  end;

  // 3. Model List round-trip
  List := TProductGenericList.Create;
  try
    for I := 1 to 4 do
    begin
      P := TProductModel.Create;
      P.Id.Value := I;
      P.Name.Value := 'Produto XML #' + IntToStr(I) + ' & Cia';
      P.Price.Value := I * 75.50;
      P.Active.Value := (I mod 2 = 1);
      List.Add(P);
    end;

    MS := TMemoryStream.Create;
    try
      Codec.EncodeList(List, MS);
      AssertTrue(MS.Size > 30, 'XML EncodeList gerou payload');

      DecodedList := TProductGenericList.Create;
      try
        MS.Position := 0;
        Codec.DecodeList(MS, DecodedList, TProductModel);
        AssertEqualsInt(4, DecodedList.Count, 'XML DecodeList restaurou todos os 4 itens');
        for I := 0 to 3 do
        begin
          AssertEqualsInt(I + 1, DecodedList[I].Id.Value, 'Item ID confere');
          AssertEquals('Produto XML #' + IntToStr(I + 1) + ' & Cia', DecodedList[I].Name.Value, 'Item Name confere');
          AssertTrue(Abs(DecodedList[I].Price.Value - ((I + 1) * 75.50)) < 0.001, 'Item Price confere');
        end;
      finally
        DecodedList.Free;
      end;
    finally
      MS.Free;
    end;
  finally
    List.Free;
  end;
end;

{==============================================================================}
{ Test 25: Codec CSV RFC 4180 (Zero-DOM)                                       }
{==============================================================================}

procedure TestCsvCodec;
var
  Codec: IDeltaCodec;
  MS: TMemoryStream;
  P, DecodedP: TProductModel;
  List, DecodedList: TProductGenericList;
  I: Integer;
begin
  Suite('Codec CSV RFC 4180 (Zero-DOM)');

  // 1. CSV Codec resolution
  AssertTrue(TDeltaCodecRegistry.HasCodec('csv'), 'Codec "csv" registrado');
  AssertTrue(TDeltaCodecRegistry.HasCodec('text/csv'), 'Codec "text/csv" registrado');
  Codec := TDeltaCodecRegistry.GetCodec('csv');
  AssertTrue(Codec <> nil, 'GetCodec("csv") retornou instância');
  AssertEquals('csv', Codec.GetFormatName, 'GetFormatName é "csv"');
  AssertEquals('text/csv', Codec.GetMimeType, 'GetMimeType é "text/csv"');

  // 2. Simple model round-trip with commas and double quotes in text
  P := TProductModel.Create;
  try
    P.Id.Value := 505;
    P.Name.Value := 'Café Gourmet, Torrado e Moído "Seleção Ouro"';
    P.Price.Value := 42.50;
    P.Active.Value := True;
    P.BigCode.Value := 9988776655;

    MS := TMemoryStream.Create;
    try
      Codec.EncodeModel(P, MS);
      AssertTrue(MS.Size > 20, 'CSV gerou stream com conteúdo');

      DecodedP := TProductModel.Create;
      try
        MS.Position := 0;
        Codec.DecodeModel(MS, DecodedP);

        AssertEqualsInt(505, DecodedP.Id.Value, 'CSV restaurou Id');
        AssertEquals('Café Gourmet, Torrado e Moído "Seleção Ouro"', DecodedP.Name.Value, 'CSV restaurou Name com vírgulas e aspas');
        AssertTrue(Abs(DecodedP.Price.Value - 42.50) < 0.001, 'CSV restaurou Price Double');
        AssertTrue(DecodedP.Active.Value, 'CSV restaurou Active Boolean');
        AssertEqualsInt(9988776655, DecodedP.BigCode.Value, 'CSV restaurou BigCode Int64');
        AssertTrue(DecodedP.Description.IsNull, 'CSV preservou campo nulo Description');
      finally
        DecodedP.Free;
      end;
    finally
      MS.Free;
    end;
  finally
    P.Free;
  end;

  // 3. Model List round-trip
  List := TProductGenericList.Create;
  try
    for I := 1 to 5 do
    begin
      P := TProductModel.Create;
      P.Id.Value := I * 100;
      P.Name.Value := 'Item CSV #' + IntToStr(I) + ', Lote Especial';
      P.Price.Value := I * 33.30;
      P.Active.Value := True;
      List.Add(P);
    end;

    MS := TMemoryStream.Create;
    try
      Codec.EncodeList(List, MS);
      AssertTrue(MS.Size > 30, 'CSV EncodeList gerou payload');

      DecodedList := TProductGenericList.Create;
      try
        MS.Position := 0;
        Codec.DecodeList(MS, DecodedList, TProductModel);
        AssertEqualsInt(5, DecodedList.Count, 'CSV DecodeList restaurou todos os 5 itens');
        for I := 0 to 4 do
        begin
          AssertEqualsInt((I + 1) * 100, DecodedList[I].Id.Value, 'Item ID confere');
          AssertEquals('Item CSV #' + IntToStr(I + 1) + ', Lote Especial', DecodedList[I].Name.Value, 'Item Name confere');
          AssertTrue(Abs(DecodedList[I].Price.Value - ((I + 1) * 33.30)) < 0.001, 'Item Price confere');
        end;
      finally
        DecodedList.Free;
      end;
    finally
      MS.Free;
    end;
  finally
    List.Free;
  end;
end;

type
  TCursorCallbackHelper = class
  public
    ItemCount: Integer;
    LastId: Int64;
    procedure ProcessItem(AModel: TTestOrder);
  end;

procedure TCursorCallbackHelper.ProcessItem(AModel: TTestOrder);
begin
  Inc(ItemCount);
  LastId := AModel.Id;
end;

procedure TestReusableCursorAndFlyweightSchema;
var
  Schema1, Schema2: TDeltaModelSchema;
  FieldNome: TDeltaSchemaField;
  Engine: TDeltaORMEngine;
  Query: specialize TDeltaQuery<TTestOrder>;
  FirstPtr, CurrPtr: Pointer;
  I, LoopCount: Integer;
  M: TTestOrder;
  Helper: TCursorCallbackHelper;
const
  TEST_DB = '/tmp/test_cursor_flyweight.sqlite';
begin
  Suite('26. Schema Flyweight & Reusable Cursor (Zero-Alloc Loop)');

  // 1. Flyweight Schema Singleton per class
  Schema1 := TTestOrder.Schema;
  Schema2 := TTestOrder.Schema;
  AssertTrue(Schema1 <> nil, 'TTestOrder.Schema retorna schema válido');
  AssertTrue(Pointer(Schema1) = Pointer(Schema2), 'TTestOrder.Schema é singleton Flyweight único na memória');
  AssertEquals('test_orders', Schema1.TableName, 'Schema TableName confere');

  FieldNome := Schema1.FindField('CustomerName');
  AssertTrue(FieldNome <> nil, 'Schema.FindField encontra campo CustomerName');
  AssertEquals('customer_name', FieldNome.FieldName, 'FieldName do schema confere');

  // 2. Reusable Cursor
  if FileExists(TEST_DB) then DeleteFile(TEST_DB);
  Engine := TDeltaORMEngine.Create('sqlite://' + TEST_DB);
  try
    Engine.Connection.Open;
    Engine.ExecuteDirect('CREATE TABLE test_orders (id INTEGER PRIMARY KEY, customer_name TEXT);');

    for I := 1 to 25 do
      Engine.ExecuteDirect(Format('INSERT INTO test_orders (id, customer_name) VALUES (%d, ''Cliente %d'');', [I, I]));

    Query := specialize TDeltaQuery<TTestOrder>.Create(Engine);
    try
      // Test with while Query.Next do
      Query.OrderBy('id ASC');
      Query.Open;
      FirstPtr := Pointer(Query.Current);
      AssertTrue(FirstPtr <> nil, 'Query.Current não é nulo após Open');

      LoopCount := 0;
      while Query.Next do
      begin
        Inc(LoopCount);
        CurrPtr := Pointer(Query.Current);
        AssertTrue(CurrPtr = FirstPtr, 'Query.Current reutiliza exatamente a mesma instância em cada linha');
        AssertEqualsInt(LoopCount, Query.Current.Id, 'Id atualizado no mesmo objeto da linha');
        AssertEquals(Format('Cliente %d', [LoopCount]), Query.Current.CustomerName, 'CustomerName atualizado');
      end;
      AssertEqualsInt(25, LoopCount, 'Cursor while processou todos os 25 registros com 1 único objeto no heap');

      // Test with for M in Query.Cursor do
      LoopCount := 0;
      for M in Query.Cursor do
      begin
        Inc(LoopCount);
        AssertTrue(Pointer(M) = FirstPtr, 'for M in Query.Cursor reaproveita a mesma instância de modelo');
        AssertEqualsInt(LoopCount, M.Id, 'Id confere no for-in');
      end;
      AssertEqualsInt(25, LoopCount, 'for M in Query.Cursor processou 25 registros com 1 objeto');

      // Test with Query.Each(Callback)
      Helper := TCursorCallbackHelper.Create;
      try
        Helper.ItemCount := 0;
        Query.Each(@Helper.ProcessItem);
        AssertEqualsInt(25, Helper.ItemCount, 'Query.Each processou 25 registros via callback');
        AssertEqualsInt(25, Helper.LastId, 'Último Id processado confere');
      finally
        Helper.Free;
      end;
    finally
      Query.Free;
    end;
  finally
    Engine.Free;
    if FileExists(TEST_DB) then DeleteFile(TEST_DB);
  end;
end;

{==============================================================================}
{ Main Entry Point                                                             }
{==============================================================================}

begin
  WriteLn('===================================================================');
  WriteLn('          DeltaModel ORM - Suíte Completa de Testes                ');
  WriteLn('===================================================================');

  try
    TestDatabaseURLParser;
    TestValidators;
    TestSerialization;
    TestSwaggerSchema;
    TestStringLiteralEscaping;
    TestFluentSchemaOnTdfFields;
    TestDDLGenerator;
    TestModelLifecycleHooks;
    TestORMCRUDAndQueries;
    TestConstraintsAndIndexes;
    TestHasOneRelation;
    TestPaginatedList;
    TestRTTICacheAndMetadata;
    TestColumnBindingMap;
    TestFastJsonStreamer;
    TestDirectTypedProperties;
    TestApproachBFluentSchemaAndGenerics;
    TestDeclarativeSchemaValidation;
    TestTransactionScopeRAII;
    TestFastJsonParser;
    TestLocalizationMessages;
    TestEagerLoadingAndStreaming;
    TestCodecArchitecture;
    TestBsonCodec;
    TestCborCodec;
    TestXmlCodec;
    TestCsvCodec;
    TestReusableCursorAndFlyweightSchema;
  except
    on E: Exception do
    begin
      Inc(GFailedTests);
      WriteLn('  [EXCEPTION FATAL]: ' + E.Message);
    end;
  end;

  WriteLn('');
  WriteLn('===================================================================');
  WriteLn(Format('RESULTADO: Total: %d | Passaram: %d | Falharam: %d',
    [GTotalTests, GPassedTests, GFailedTests]));
  WriteLn('===================================================================');

  if GFailedTests > 0 then
    Halt(1)
  else
    Halt(0);
end.
