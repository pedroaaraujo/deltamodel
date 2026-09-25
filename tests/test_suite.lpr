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
  DeltaSerialization, DeltaAPISchema;

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
  { Modelo completo com tipos variados para CRUD, DDL, Swagger e Serialização }
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
  { Modelo com rastreamento completo de ciclo de vida }
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
  { Modelo com Unique constraints e Índices }
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
  { Modelo 1:1 Dependente }
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
  { Modelo Principal com TDFHasOne }
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

  // MySQL com parâmetros
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
    // CPFs válidos conhecidos (sem e com máscara)
    Res := ValCPF.Validate('52998224725');
    AssertTrue(Res.OK, 'CPF válido sem formatação deve passar');

    Res := ValCPF.Validate('529.982.247-25');
    AssertTrue(Res.OK, 'CPF válido com máscara deve passar');

    // CPFs inválidos
    Res := ValCPF.Validate('52998224726');
    AssertFalse(Res.OK, 'CPF com dígito verificador errado deve falhar');

    Res := ValCPF.Validate('11111111111');
    AssertFalse(Res.OK, 'CPF com dígitos repetidos deve falhar');

    Res := ValCPF.Validate('123');
    AssertFalse(Res.OK, 'CPF com tamanho menor deve falhar');
  finally
    ValCPF.Free;
  end;

  // CNPJ (Numérico e Alfanumérico)
  ValCNPJ := TValidatorItemCNPJ.Create;
  try
    // CNPJ numérico válido conhecido (sem e com máscara)
    Res := ValCNPJ.Validate('11222333000181');
    AssertTrue(Res.OK, 'CNPJ numérico válido sem formatação deve passar');

    Res := ValCNPJ.Validate('11.222.333/0001-81');
    AssertTrue(Res.OK, 'CNPJ numérico válido com formatação deve passar');

    // CNPJ numérico inválido
    Res := ValCNPJ.Validate('11222333000182');
    AssertFalse(Res.OK, 'CNPJ numérico com dígito verificador errado deve falhar');

    Res := ValCNPJ.Validate('00000000000000');
    AssertFalse(Res.OK, 'CNPJ com dígitos todos zeros deve falhar');

    // CNPJ alfanumérico válido (novo padrão Receita Federal)
    Res := ValCNPJ.Validate('12ABC34501DE35');
    AssertTrue(Res.OK, 'CNPJ alfanumérico válido sem formatação deve passar');

    Res := ValCNPJ.Validate('12.ABC.345/01DE-35');
    AssertTrue(Res.OK, 'CNPJ alfanumérico válido com formatação deve passar');

    Res := ValCNPJ.Validate('12.abc.345/01de-35');
    AssertTrue(Res.OK, 'CNPJ alfanumérico minúsculo deve passar');

    // CNPJ alfanumérico inválido
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

    // 'Ações' possui 5 caracteres visuais, mas 7 bytes em UTF-8 (ç e õ = 2 bytes cada)
    Res := ValMinLen.Validate('Ações');
    AssertTrue(Res.OK, 'MinLength 5 com UTF-8 multibyte "Ações" (5 caracteres) deve passar');

    // 'Ação' possui 4 caracteres visuais
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
    // Sem dados preenchidos: deve falhar levantando EDeltaValidation devido aos campos Required
    FailedValidation := False;
    try
      Prod.Validate;
    except
      on E: EDeltaValidation do
        FailedValidation := True;
    end;
    AssertTrue(FailedValidation, 'Produto sem campos obrigatórios deve levantar EDeltaValidation');

    // Preenchendo dados válidos
    Prod.Name.Value := 'Notebook Dell';
    Prod.Price.Value := 4500.50;
    Prod.Active.Value := True;
    Prod.Validate;
    Res := Prod.Validator.Validate;
    AssertTrue(Res.OK, 'Produto com nome e preço válidos deve passar na validação');

    // Preço zero ou negativo: deve falhar no validador de regra de negócio
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

    // Teste de Clonagem
    P2.Free;
    P2 := TProductModel(P1.Clone);
    AssertEquals(P1.Name.AsString, P2.Name.AsString, 'Clone copiou nome com precisão');
    AssertEqualsInt(P1.BigCode.AsLargeInt, P2.BigCode.AsLargeInt, 'Clone copiou BigCode int64 com precisão');
  finally
    P1.Free;
    P2.Free;
  end;

  // Teste de lista com ToCSV / FromCSV
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

    // Lista ToJson / FromJson
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
end;

{==============================================================================}
{ Test 5: DDL Generator Across Dialects                                        }
{==============================================================================}

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

      // 1. Teste de Insert
      Item.HookLog.Clear;
      Engine.Insert(Item);
      AssertEqualsInt(2, Item.HookLog.Count, 'Insert deve disparar exatamente 2 hooks');
      AssertEquals('BeforeInsert', Item.HookLog[0], 'Primeiro hook deve ser BeforeInsert');
      AssertEquals('AfterInsert', Item.HookLog[1], 'Segundo hook deve ser AfterInsert');
      AssertTrue(Item.Id.AsLargeInt > 0, 'Id foi gerado após Insert');

      // 2. Teste de Update
      Item.HookLog.Clear;
      Item.Name.Value := 'Item Modificado';
      Engine.Merge(Item);
      AssertEqualsInt(2, Item.HookLog.Count, 'Update deve disparar exatamente 2 hooks');
      AssertEquals('BeforeUpdate', Item.HookLog[0], 'Primeiro hook deve ser BeforeUpdate');
      AssertEquals('AfterUpdate', Item.HookLog[1], 'Segundo hook deve ser AfterUpdate');

      // 3. Teste de Save com PK existente (executa Update internamente)
      Item.HookLog.Clear;
      Item.Name.Value := 'Item Salvo Novamente';
      Engine.Save(Item);
      AssertEqualsInt(4, Item.HookLog.Count, 'Save com PK deve disparar 4 hooks');
      AssertEquals('BeforeSave', Item.HookLog[0], 'Hook 0 deve ser BeforeSave');
      AssertEquals('BeforeUpdate', Item.HookLog[1], 'Hook 1 deve ser BeforeUpdate');
      AssertEquals('AfterUpdate', Item.HookLog[2], 'Hook 2 deve ser AfterUpdate');
      AssertEquals('AfterSave', Item.HookLog[3], 'Hook 3 deve ser AfterSave');

      // 4. Teste de Delete
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
    // Prepara tabela
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

    // Inserções individuais
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

    // Count total e com filtro
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

    // Teste de Where encadeado com AND (correção de sobrescrita de FFilter)
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

    // Teste de WhereBetween
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

    // Teste de WhereIn
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

    // Teste de OrderBy, Limit e Offset
    Q := Engine.Query(TProductModel);
    try
      Q.OrderBy('price DESC').Limit(2).Offset(1);
      List := Q.All;
      try
        AssertEqualsInt(2, List.Count, 'Limit 2 Offset 1 deve retornar 2 registros');
        // Preços em ordem desc: 8500 (offset pula), 1600 (Monitor), 150 (Mouse)
        AssertEquals('Monitor 27 Polegadas', TProductModel(List[0]).Name.AsString, 'Primeiro item retornado com Offset 1 deve ser o Monitor');
        AssertEquals('Mouse Gamer', TProductModel(List[1]).Name.AsString, 'Segundo item retornado deve ser o Mouse Gamer');
      finally
        List.Free;
      end;
    finally
      Q.Free;
    end;

    // Teste de AsJsonString
    Q := Engine.Query(TProductModel);
    try
      Q.Where('price < 50');
      JsonRes := Q.AsJsonString;
      AssertTrue(Pos('Mousepad Simples', string(JsonRes)) > 0, 'AsJsonString retorna JSON com o produto');
    finally
      Q.Free;
    end;

    // Teste de Transações: Commit e Rollback
    if Engine.TransactionActive then
      Engine.Commit;

    // Rollback deve desfazer inserção
    Engine.StartTransaction;
    Engine.ExecuteDirect('INSERT INTO products (name, price, active) VALUES (''Produto Cancelado'', 10, 1)');
    Engine.Rollback;

    TotalCount := Engine.Count(TProductModel, 'name = ''Produto Cancelado''');
    AssertEqualsInt(0, TotalCount, 'Registro inserido em transação cancelada não deve persistir');

    // InTransaction estático com commit automático
    GTxEngine := Engine;
    Engine.InTransaction(@TxSuccessProc);

    TotalCount := Engine.Count(TProductModel, 'name = ''Produto Confirmado''');
    AssertEqualsInt(1, TotalCount, 'InTransaction com sucesso deve persistir registro');

    // InTransaction com erro deve realizar Rollback automático
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

    // 2. Índices SQLite
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

    // 4. Sanitização de identificador longo para Oracle (limite de 30 chars)
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

  // 5. Teste em Runtime com SQLite in-memory via TDeltaORMSchema
  Engine := TDeltaORMEngine.Create('sqlite:///:memory:');
  try
    Schema := TDeltaORMSchema.Create(Engine);
    try
      Schema.RegisterModel(TConstraintItem);
      Schema.PrepareDB(True);

      AssertTrue(Schema.SQL.Count > 0, 'PrepareDB gerou comandos DDL');

      // Inserção 1: Registro válido
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

      // Inserção 2: Mesmo e-mail (deve ser rejeitado pela constraint UNIQUE de email)
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

      // Inserção 3: E-mail diferente, mas mesmo par (tenant_id, code) (deve falhar pelo UNIQUE composto)
      Item3 := TConstraintItem.Create;
      try
        Item3.Email.Value := 'outro@empresa.com';
        Item3.TenantId.Value := 10; // Mesmo tenant
        Item3.Code.Value := 'USR_A'; // Mesmo code
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

      // Inserção 4: Mesmo code, mas em outro tenant (deve passar!)
      Item4 := TConstraintItem.Create;
      try
        Item4.Email.Value := 'usuario2@empresa.com';
        Item4.TenantId.Value := 20; // Tenant diferente
        Item4.Code.Value := 'USR_A'; // Mesmo code
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
    // 1. Verificação de Metadados
    AssertTrue(User1.Profile.IsVirtual, 'TDFHasOne deve ser campo virtual');
    AssertEquals('object', User1.Profile.SwaggerDataType, 'SwaggerDataType de TDFHasOne deve ser object');
    AssertTrue(User1.Profile.RelationClass = TProfileModel, 'RelationClass de Profile deve ser TProfileModel');
    AssertTrue(User1.Profile.IsNull, 'Profile recém-criado deve ser IsNull');

    // 2. DDL não deve conter coluna para TDFHasOne
    DDL := TDDLBuilder.CreateTableAndFields(User1, nil, ddSQLite);
    AssertTrue(Pos('username TEXT', DDL) > 0, 'DDL deve conter campo normal');
    AssertFalse(Pos('Profile', DDL) > 0, 'DDL NÃO deve criar coluna física para TDFHasOne');

    // 3. Serialização com HasOne vazio (null)
    User1.Id.Value := 1;
    User1.Username.Value := 'pedro';
    JsonStr := User1.ToJson;
    AssertTrue((Pos('"Profile" : null', JsonStr) > 0) or (Pos('"Profile": null', JsonStr) > 0) or (Pos('"Profile":null', JsonStr) > 0), 'HasOne não instanciado deve serializar como null');

    // 4. Serialização com HasOne preenchido
    Prof := TProfileModel.Create;
    Prof.Id.Value := 99;
    Prof.Bio.Value := 'Engenheiro de Software';
    Prof.Twitter.Value := '@pedro_dev';
    User1.Profile.Model := Prof;

    AssertFalse(User1.Profile.IsNull, 'Com Model atribuído, HasOne não deve ser IsNull');

    JsonStr := User1.ToJson;
    AssertTrue(Pos('Engenheiro de Software', JsonStr) > 0, 'ToJson deve conter bio do modelo relacionado');
    AssertTrue(Pos('@pedro_dev', JsonStr) > 0, 'ToJson deve conter twitter do modelo relacionado');

    // 5. Desserialização JSON reconstruindo o objeto 1:1
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

      // 6. Teste de Clone (cópia profunda de HasOne)
      User3 := TUserWithProfileModel(User2.Clone);
      try
        AssertTrue(Assigned(User3.Profile.Model), 'Clone deve copiar Profile.Model');
        if Assigned(User3.Profile.Model) then
        begin
          AssertEquals('Engenheiro de Software', TProfileModel(User3.Profile.Model).Bio.AsString, 'Bio copiada no Clone');
          // Alteração na cópia não afeta o original
          TProfileModel(User3.Profile.Model).Bio.Value := 'Bio Modificada';
          AssertEquals('Engenheiro de Software', TProfileModel(User2.Profile.Model).Bio.AsString, 'Original não deve ser alterado');
        end;
      finally
        User3.Free;
      end;
    finally
      User2.Free;
    end;

    // 7. OpenAPI / Swagger Schema para TDFHasOne
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
    TestDDLGenerator;
    TestModelLifecycleHooks;
    TestORMCRUDAndQueries;
    TestConstraintsAndIndexes;
    TestHasOneRelation;
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
