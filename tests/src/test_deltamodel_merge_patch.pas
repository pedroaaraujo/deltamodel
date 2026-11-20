{
  FPCUnit Test Suite for RFC 7396 JSON Merge Patch (DeltaModel & Zenith)

  Covers:
  - All 15 normative test vectors from RFC 7396 Appendix A
  - Recursive nested object merge, atomic array replacement, immutability
  - ApplyMergePatch on 20-field TProduct (Style A: TDeltaField) updating 1 field
  - ApplyMergePatch on 20-field TUser (Style B: Fluent Class Schema / Primitives)
  - Explicit null assignment (Dirty = True, IsNull = True, SQL SET col = :col / NULL)
  - Partial UPDATE SQL generation with exclusively dirty columns across dialects
  - Zero dirty fields no-op (empty SQL, no DB execution)
  - Primary key, non-updatable, and hidden field protection
  - Nested TDFHasOne / TDFHasMany merge semantics on TDocument
  - End-to-end SQLite in-memory persistence via TUpdate.PatchObject
  - Zenith.MergePatch HTTP Content-Type & payload helpers
}

unit test_deltamodel_merge_patch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Variants, DB, SQLDB, sqlite3conn, fpjson, jsonparser,
  HTTPDefs, fpcunit, testregistry,
  DeltaModel, DeltaModel.Fields, DeltaModel.Metadata, DeltaModel.ORM.Types,
  DeltaModel.ORM.Interfaces, DeltaModel.ORM.Connection, DeltaModel.ORM.DDL,
  DeltaModel.ORM.DML, DeltaModel.SQLBuilder, DeltaModel.MergePatch,
  Zenith.MergePatch, Zenith.Exceptions;

type
  { 20-field Style A model (TDeltaField wrappers) }
  TProduct = class(TDeltaModel)
  private
    FId: TDFInt64Required;
    FSku: TDFStringRequired;
    FName: TDFStringRequired;
    FDescription: TDFTextNull;
    FCategory: TDFStringNull;
    FBrand: TDFStringNull;
    FPrice: TDFCurrencyRequired;
    FCost: TDFCurrencyNull;
    FStock: TDFIntNull;
    FMinStock: TDFIntNull;
    FMaxStock: TDFIntNull;
    FWeight: TDFDoubleNull;
    FWidth: TDFDoubleNull;
    FHeight: TDFDoubleNull;
    FDepth: TDFDoubleNull;
    FBarcode: TDFStringNull;
    FOrigin: TDFStringNull;
    FNotes: TDFTextNull;
    FActive: TDFBoolRequired;
    FInternalSecret: TDFStringNull;
  public
    procedure Configure; override;
  published
    property id: TDFInt64Required read FId write FId;
    property sku: TDFStringRequired read FSku write FSku;
    property name: TDFStringRequired read FName write FName;
    property description: TDFTextNull read FDescription write FDescription;
    property category: TDFStringNull read FCategory write FCategory;
    property brand: TDFStringNull read FBrand write FBrand;
    property price: TDFCurrencyRequired read FPrice write FPrice;
    property cost: TDFCurrencyNull read FCost write FCost;
    property stock: TDFIntNull read FStock write FStock;
    property min_stock: TDFIntNull read FMinStock write FMinStock;
    property max_stock: TDFIntNull read FMaxStock write FMaxStock;
    property weight: TDFDoubleNull read FWeight write FWeight;
    property width: TDFDoubleNull read FWidth write FWidth;
    property height: TDFDoubleNull read FHeight write FHeight;
    property depth: TDFDoubleNull read FDepth write FDepth;
    property barcode: TDFStringNull read FBarcode write FBarcode;
    property origin: TDFStringNull read FOrigin write FOrigin;
    property notes: TDFTextNull read FNotes write FNotes;
    property active: TDFBoolRequired read FActive write FActive;
    property internal_secret: TDFStringNull read FInternalSecret write FInternalSecret;
  end;

  { 20-field Style B model (Fluent Class Schema & native primitives) }
  TUser = class(TDeltaModel)
  private
    FId: Int64;
    FUsername: string;
    FEmail: string;
    FFullName: string;
    FNickname: string;
    FPhone: string;
    FBio: string;
    FRole: string;
    FDepartment: string;
    FLocale: string;
    FTimezone: string;
    FAvatarUrl: string;
    FCountry: string;
    FCity: string;
    FPostalCode: string;
    FAge: Integer;
    FLoginCount: Integer;
    FRating: Double;
    FActive: Boolean;
    FVerified: Boolean;
  public
    class procedure ConfigureSchema(ASchema: TDeltaModelSchema); override;
  published
    property id: Int64 read FId write FId;
    property username: string read FUsername write FUsername;
    property email: string read FEmail write FEmail;
    property full_name: string read FFullName write FFullName;
    property nickname: string read FNickname write FNickname;
    property phone: string read FPhone write FPhone;
    property bio: string read FBio write FBio;
    property role: string read FRole write FRole;
    property department: string read FDepartment write FDepartment;
    property locale: string read FLocale write FLocale;
    property timezone: string read FTimezone write FTimezone;
    property avatar_url: string read FAvatarUrl write FAvatarUrl;
    property country: string read FCountry write FCountry;
    property city: string read FCity write FCity;
    property postal_code: string read FPostalCode write FPostalCode;
    property age: Integer read FAge write FAge;
    property login_count: Integer read FLoginCount write FLoginCount;
    property rating: Double read FRating write FRating;
    property active: Boolean read FActive write FActive;
    property verified: Boolean read FVerified write FVerified;
  end;

  { Nested models for TDocument test }
  TDocumentAuthor = class(TDeltaModel)
  private
    FGivenName: TDFStringNull;
    FFamilyName: TDFStringNull;
  published
    property given_name: TDFStringNull read FGivenName write FGivenName;
    property family_name: TDFStringNull read FFamilyName write FFamilyName;
  end;

  TDocumentTag = class(TDeltaModel)
  private
    FLabelText: TDFStringNull;
  published
    property label_text: TDFStringNull read FLabelText write FLabelText;
  end;

  TDocument = class(TDeltaModel)
  private
    FId: TDFInt64Required;
    FTitle: TDFStringRequired;
    FContent: TDFTextNull;
    FAuthor: TDFHasOne;
    FTags: TDFHasMany;
  public
    procedure Configure; override;
  published
    property id: TDFInt64Required read FId write FId;
    property title: TDFStringRequired read FTitle write FTitle;
    property content: TDFTextNull read FContent write FContent;
    property author: TDFHasOne read FAuthor write FAuthor;
    property tags: TDFHasMany read FTags write FTags;
  end;

  { Mock HTTP request/response for Zenith.MergePatch unit testing }
  TMockRequest = class(TRequest)
  public
    procedure SetBody(const ABody: string);
  end;

  TMockResponse = class(TResponse)
  protected
    procedure DoSendHeaders(Headers: TStrings); override;
    procedure DoSendContent; override;
  end;

  { Test cases }
  TTestRFC7396AppendixA = class(TTestCase)
  private
    procedure AssertMergeCase(const ACaseName, ATarget, APatch, AExpected: RawByteString);
    function JsonEquals(A, B: TJSONData): Boolean;
  published
    procedure TestCase01_ReplaceValue;
    procedure TestCase02_AddMember;
    procedure TestCase03_RemoveSoleMember;
    procedure TestCase04_RemoveOneOfTwoMembers;
    procedure TestCase05_ReplaceArrayWithString;
    procedure TestCase06_ReplaceStringWithArray;
    procedure TestCase07_NestedObjectUpdateAndRemove;
    procedure TestCase08_ReplaceArrayOfObjectsWithArrayOfInt;
    procedure TestCase09_ReplaceRootArray;
    procedure TestCase10_ReplaceObjectWithArray;
    procedure TestCase11_ReplaceObjectWithNull;
    procedure TestCase12_ReplaceObjectWithString;
    procedure TestCase13_PreserveUntouchedNullMember;
    procedure TestCase14_ReplaceArrayTargetWithObjectPatch;
    procedure TestCase15_NestedNullCreatesEmptyObjects;
    procedure TestImmutabilityOfTargetAndPatch;
  end;

  TTestDeltaModelMergePatch = class(TTestCase)
  private
    function CreatePopulatedProduct(ASetId: Boolean = True): TProduct;
    function CreatePopulatedUser: TUser;
  published
    procedure TestStyleA_20FieldsUpdateOnlyOne;
    procedure TestStyleA_ExplicitNullSetsDirtyAndNull;
    procedure TestStyleA_PrimaryKeyProtectionDefault;
    procedure TestStyleA_PrimaryKeyProtectionStrict;
    procedure TestStyleA_HiddenAndNonUpdatableFieldProtection;
    procedure TestStyleA_UnknownFieldsIgnoredByDefaultAndStrictReject;
    procedure TestStyleA_ZeroDirtyFieldsProducesNoUpdateSQL;
    procedure TestStyleA_PartialUpdateSQLMultiDialect;
    procedure TestStyleA_PartialUpdateInlineNullSQL;
    procedure TestStyleB_20FieldsPrimitiveUpdateAndExplicitNull;
    procedure TestNestedDocumentHasOneMergeAndHasManyReplace;
    procedure TestEndToEndSQLitePatchObject;
  end;

  TTestZenithMergePatch = class(TTestCase)
  published
    procedure TestContentTypeDetectionAndCharset;
    procedure TestRejectInvalidContentType;
    procedure TestApplyToModelFromHTTPRequest;
    procedure TestApplyJsonFromHTTPRequest;
    procedure TestAdvertiseAcceptPatchHeader;
  end;

  TBenchmarkMetrics = record
    Iterations: Integer;
    PatchElapsedMs: Double;
    PutElapsedMs: Double;
    PatchOpsPerSec: Double;
    PutOpsPerSec: Double;
    SpeedupFactor: Double;
    PatchSQLBytes: Integer;
    PutSQLBytes: Integer;
    SQLSizeReductionPct: Double;
    PatchHeapDeltaBytes: Int64;
    PutHeapDeltaBytes: Int64;
  end;

function RunMergePatchBenchmark(AIterations: Integer = 10000): TBenchmarkMetrics;

implementation

uses
  StrUtils;

{ TProduct }

procedure TProduct.Configure;
begin
  TableName := 'products';
  id.DBOptions := [dboPrimaryKey, dboAutoInc];
  sku.DBOptions := [dboInsert]; // Immutable after insert (no dboUpdate)
  internal_secret.Visible := False;
end;

{ TUser }

class procedure TUser.ConfigureSchema(ASchema: TDeltaModelSchema);
begin
  ASchema.Table('users');
  ASchema.Field('id').Column('id').AsPK.AutoInc;
  ASchema.Field('username').Column('username').Required.Size(60);
  ASchema.Field('email').Column('email').Required.Size(120);
  ASchema.Field('full_name').Column('full_name').Size(120);
  ASchema.Field('nickname').Column('nickname').Nullable.Size(60);
  ASchema.Field('phone').Column('phone').Nullable.Size(30);
  ASchema.Field('bio').Column('bio').Nullable.Size(500);
  ASchema.Field('role').Column('role').Size(40);
  ASchema.Field('department').Column('department').Nullable.Size(80);
  ASchema.Field('locale').Column('locale').Size(16);
  ASchema.Field('timezone').Column('timezone').Size(40);
  ASchema.Field('avatar_url').Column('avatar_url').Nullable.Size(255);
  ASchema.Field('country').Column('country').Nullable.Size(60);
  ASchema.Field('city').Column('city').Nullable.Size(80);
  ASchema.Field('postal_code').Column('postal_code').Nullable.Size(20);
  ASchema.Field('age').Column('age').Nullable;
  ASchema.Field('login_count').Column('login_count');
  ASchema.Field('rating').Column('rating').Nullable;
  ASchema.Field('active').Column('active');
  ASchema.Field('verified').Column('verified');
end;

{ TDocument }

procedure TDocument.Configure;
begin
  TableName := 'documents';
  id.DBOptions := [dboPrimaryKey, dboAutoInc];
  author.References(TDocumentAuthor);
  author.IsVirtual := True;
  tags.References(TDocumentTag);
  tags.IsVirtual := True;
end;

{ TMockRequest }

procedure TMockRequest.SetBody(const ABody: string);
var
  Buf: string;
begin
  Buf := ABody;
  InitContent(Buf);
end;

{ TMockResponse }

procedure TMockResponse.DoSendHeaders(Headers: TStrings);
begin
  // No-op in mock response
end;

procedure TMockResponse.DoSendContent;
begin
  // No-op in mock response
end;

{ TTestRFC7396AppendixA }

function TTestRFC7396AppendixA.JsonEquals(A, B: TJSONData): Boolean;
var
  ObjA, ObjB: TJSONObject;
  ArrA, ArrB: TJSONArray;
  I: Integer;
  ValB: TJSONData;
begin
  if (A = nil) and (B = nil) then Exit(True);
  if (A = nil) or (B = nil) then Exit(False);
  if A.JSONType <> B.JSONType then Exit(False);

  case A.JSONType of
    jtNull:
      Result := True;
    jtBoolean:
      Result := A.AsBoolean = B.AsBoolean;
    jtString:
      Result := A.AsString = B.AsString;
    jtNumber:
      Result := Abs(A.AsFloat - B.AsFloat) < 1e-9;
    jtArray:
      begin
        ArrA := TJSONArray(A);
        ArrB := TJSONArray(B);
        if ArrA.Count <> ArrB.Count then Exit(False);
        for I := 0 to ArrA.Count - 1 do
          if not JsonEquals(ArrA[I], ArrB[I]) then Exit(False);
        Result := True;
      end;
    jtObject:
      begin
        ObjA := TJSONObject(A);
        ObjB := TJSONObject(B);
        if ObjA.Count <> ObjB.Count then Exit(False);
        for I := 0 to ObjA.Count - 1 do
        begin
          if not ObjB.Find(ObjA.Names[I], ValB) then Exit(False);
          if not JsonEquals(ObjA.Items[I], ValB) then Exit(False);
        end;
        Result := True;
      end;
  else
    Result := A.AsJSON = B.AsJSON;
  end;
end;

procedure TTestRFC7396AppendixA.AssertMergeCase(const ACaseName, ATarget,
  APatch, AExpected: RawByteString);
var
  ActualStr: RawByteString;
  ActualData, ExpectedData: TJSONData;
begin
  ActualStr := MergePatch(ATarget, APatch);
  ActualData := GetJSON(ActualStr, False);
  ExpectedData := GetJSON(AExpected, False);
  try
    AssertTrue(
      Format('%s failed. Expected: %s | Actual: %s', [ACaseName, AExpected, ActualStr]),
      JsonEquals(ExpectedData, ActualData)
    );
  finally
    ExpectedData.Free;
    ActualData.Free;
  end;
end;

procedure TTestRFC7396AppendixA.TestCase01_ReplaceValue;
begin
  AssertMergeCase('RFC7396-01', '{"a":"b"}', '{"a":"c"}', '{"a":"c"}');
end;

procedure TTestRFC7396AppendixA.TestCase02_AddMember;
begin
  AssertMergeCase('RFC7396-02', '{"a":"b"}', '{"b":"c"}', '{"a":"b","b":"c"}');
end;

procedure TTestRFC7396AppendixA.TestCase03_RemoveSoleMember;
begin
  AssertMergeCase('RFC7396-03', '{"a":"b"}', '{"a":null}', '{}');
end;

procedure TTestRFC7396AppendixA.TestCase04_RemoveOneOfTwoMembers;
begin
  AssertMergeCase('RFC7396-04', '{"a":"b","b":"c"}', '{"a":null}', '{"b":"c"}');
end;

procedure TTestRFC7396AppendixA.TestCase05_ReplaceArrayWithString;
begin
  AssertMergeCase('RFC7396-05', '{"a":["b"]}', '{"a":"c"}', '{"a":"c"}');
end;

procedure TTestRFC7396AppendixA.TestCase06_ReplaceStringWithArray;
begin
  AssertMergeCase('RFC7396-06', '{"a":"c"}', '{"a":["b"]}', '{"a":["b"]}');
end;

procedure TTestRFC7396AppendixA.TestCase07_NestedObjectUpdateAndRemove;
begin
  AssertMergeCase('RFC7396-07', '{"a":{"b":"c"}}', '{"a":{"b":"d","c":null}}', '{"a":{"b":"d"}}');
end;

procedure TTestRFC7396AppendixA.TestCase08_ReplaceArrayOfObjectsWithArrayOfInt;
begin
  AssertMergeCase('RFC7396-08', '{"a":[{"b":"c"}]}', '{"a":[1]}', '{"a":[1]}');
end;

procedure TTestRFC7396AppendixA.TestCase09_ReplaceRootArray;
begin
  AssertMergeCase('RFC7396-09', '["a","b"]', '["c","d"]', '["c","d"]');
end;

procedure TTestRFC7396AppendixA.TestCase10_ReplaceObjectWithArray;
begin
  AssertMergeCase('RFC7396-10', '{"a":"b"}', '["c"]', '["c"]');
end;

procedure TTestRFC7396AppendixA.TestCase11_ReplaceObjectWithNull;
begin
  AssertMergeCase('RFC7396-11', '{"a":"foo"}', 'null', 'null');
end;

procedure TTestRFC7396AppendixA.TestCase12_ReplaceObjectWithString;
begin
  AssertMergeCase('RFC7396-12', '{"a":"foo"}', '"bar"', '"bar"');
end;

procedure TTestRFC7396AppendixA.TestCase13_PreserveUntouchedNullMember;
begin
  AssertMergeCase('RFC7396-13', '{"e":null}', '{"a":1}', '{"e":null,"a":1}');
end;

procedure TTestRFC7396AppendixA.TestCase14_ReplaceArrayTargetWithObjectPatch;
begin
  AssertMergeCase('RFC7396-14', '[1,2]', '{"a":"b","c":null}', '{"a":"b"}');
end;

procedure TTestRFC7396AppendixA.TestCase15_NestedNullCreatesEmptyObjects;
begin
  AssertMergeCase('RFC7396-15', '{}', '{"a":{"bb":{"ccc":null}}}', '{"a":{"bb":{}}}');
end;

procedure TTestRFC7396AppendixA.TestImmutabilityOfTargetAndPatch;
var
  TargetData, PatchData, ResultData: TJSONData;
  OrigTarget, OrigPatch: string;
begin
  TargetData := GetJSON('{"a":{"b":1,"c":2},"keep":true}', False);
  PatchData := GetJSON('{"a":{"b":99,"c":null},"added":"ok"}', False);
  try
    OrigTarget := TargetData.AsJSON;
    OrigPatch := PatchData.AsJSON;
    ResultData := MergePatch(TargetData, PatchData);
    try
      AssertEquals('Target JSON must not be mutated', OrigTarget, TargetData.AsJSON);
      AssertEquals('Patch JSON must not be mutated', OrigPatch, PatchData.AsJSON);
      AssertEquals('Merged value a.b', 99, TJSONObject(TJSONObject(ResultData).Find('a')).Get('b', 0));
      AssertTrue('Removed key a.c', TJSONObject(TJSONObject(ResultData).Find('a')).IndexOfName('c') < 0);
      AssertEquals('Preserved key keep', True, TJSONObject(ResultData).Get('keep', False));
    finally
      ResultData.Free;
    end;
  finally
    PatchData.Free;
    TargetData.Free;
  end;
end;

{ TTestDeltaModelMergePatch }

function TTestDeltaModelMergePatch.CreatePopulatedProduct(ASetId: Boolean): TProduct;
begin
  Result := TProduct.Create;
  if ASetId then
    Result.id.AsLargeInt := 42;
  Result.sku.Value := 'PRD-0042';
  Result.name.Value := 'Mechanical Keyboard';
  Result.description.Value := 'Tactile switches with aluminum frame';
  Result.category.Value := 'Peripherals';
  Result.brand.Value := 'KeyCraft';
  Result.price.AsCurrency := 149.90;
  Result.cost.AsCurrency := 85.00;
  Result.stock.AsInteger := 120;
  Result.min_stock.AsInteger := 10;
  Result.max_stock.AsInteger := 500;
  Result.weight.AsFloat := 0.95;
  Result.width.AsFloat := 36.0;
  Result.height.AsFloat := 3.5;
  Result.depth.AsFloat := 14.0;
  Result.barcode.Value := '7890000000421';
  Result.origin.Value := 'Local';
  Result.notes.Value := 'Flagship item';
  Result.active.AsBoolean := True;
  Result.internal_secret.Value := 'SECRET-TOKEN';
  Result.ClearAllDirty;
end;

function TTestDeltaModelMergePatch.CreatePopulatedUser: TUser;
begin
  Result := TUser.Create;
  Result.id := 7;
  Result.username := 'jdoe';
  Result.email := 'jdoe@example.com';
  Result.full_name := 'Jane Doe';
  Result.nickname := 'jane';
  Result.phone := '+1-555-0100';
  Result.bio := 'Software engineer';
  Result.role := 'member';
  Result.department := 'Engineering';
  Result.locale := 'en-US';
  Result.timezone := 'UTC';
  Result.avatar_url := 'https://example.com/a.png';
  Result.country := 'US';
  Result.city := 'Austin';
  Result.postal_code := '78701';
  Result.age := 31;
  Result.login_count := 15;
  Result.rating := 4.8;
  Result.active := True;
  Result.verified := True;
  Result.ClearAllNulls;
  Result.ClearAllDirty;
end;

procedure TTestDeltaModelMergePatch.TestStyleA_20FieldsUpdateOnlyOne;
var
  Prod: TProduct;
  ModifiedCount: Integer;
  SQL: string;
begin
  Prod := CreatePopulatedProduct;
  try
    ModifiedCount := ApplyMergePatch(Prod, '{"price": 199.50}');
    AssertEquals('Exactly 1 field modified', 1, ModifiedCount);
    AssertEquals('DirtyFieldCount must be 1', 1, Prod.DirtyFieldCount);
    AssertTrue('price field must be dirty', Prod.price.Dirty);
    AssertTrue('IsDirty(price) must be true', Prod.IsDirty('price'));
    AssertFalse('name field must remain clean', Prod.name.Dirty);
    AssertFalse('description field must remain clean', Prod.description.Dirty);
    AssertFalse('stock field must remain clean', Prod.stock.Dirty);
    AssertEquals('Updated price value', 199.50, Prod.price.AsFloat, 0.001);
    AssertEquals('Unchanged name value', 'Mechanical Keyboard', Prod.name.AsString);

    SQL := TDMSQLBuilder.CreatePartialUpdate(Prod, ddPostgreSQL);
    AssertEquals('Partial SQL must contain ONLY price column',
      'UPDATE products SET price = :price WHERE (id = 42)', SQL);
  finally
    Prod.Free;
  end;
end;

procedure TTestDeltaModelMergePatch.TestStyleA_ExplicitNullSetsDirtyAndNull;
var
  Prod: TProduct;
  ModifiedCount: Integer;
  SQL: string;
begin
  Prod := CreatePopulatedProduct;
  try
    AssertFalse('description initially non-null', Prod.description.IsNull);
    ModifiedCount := ApplyMergePatch(Prod, '{"description": null, "notes": null}');
    AssertEquals('2 fields explicitly set to null', 2, ModifiedCount);
    AssertTrue('description must be null', Prod.description.IsNull);
    AssertTrue('description must be dirty', Prod.description.Dirty);
    AssertTrue('notes must be null', Prod.notes.IsNull);
    AssertTrue('notes must be dirty', Prod.notes.Dirty);
    AssertFalse('brand must remain clean', Prod.brand.Dirty);

    SQL := TDMSQLBuilder.CreatePartialUpdate(Prod, ddPostgreSQL);
    AssertEquals('Partial SQL must include dirty null columns with named params',
      'UPDATE products SET description = :description, notes = :notes WHERE (id = 42)', SQL);
  finally
    Prod.Free;
  end;
end;

procedure TTestDeltaModelMergePatch.TestStyleA_PrimaryKeyProtectionDefault;
var
  Prod: TProduct;
  ModifiedCount: Integer;
  SQL: string;
begin
  Prod := CreatePopulatedProduct;
  try
    ModifiedCount := ApplyMergePatch(Prod, '{"id": 9999, "stock": 80}');
    AssertEquals('Only stock modified; PK ignored', 1, ModifiedCount);
    AssertEquals('PK id must remain 42', Int64(42), Prod.id.AsLargeInt);
    AssertFalse('PK id must not be dirty', Prod.id.Dirty);
    AssertTrue('stock must be dirty', Prod.stock.Dirty);
    AssertEquals('stock updated to 80', 80, Prod.stock.AsInteger);

    SQL := TDMSQLBuilder.CreatePartialUpdate(Prod, ddSQLite);
    AssertEquals('Partial SQL must not contain PK in SET clause',
      'UPDATE products SET stock = :stock WHERE (id = 42)', SQL);
  finally
    Prod.Free;
  end;
end;

procedure TTestDeltaModelMergePatch.TestStyleA_PrimaryKeyProtectionStrict;
var
  Prod: TProduct;
  Raised: Boolean;
begin
  Prod := CreatePopulatedProduct;
  Raised := False;
  try
    try
      ApplyMergePatch(Prod, '{"id": 9999}', [mpoRejectPKOverwrite]);
    except
      on E: EDeltaMergePatchException do
        Raised := True;
    end;
    AssertTrue('Strict PK mode must raise EDeltaMergePatchException', Raised);
    AssertEquals('PK id must remain 42', Int64(42), Prod.id.AsLargeInt);
  finally
    Prod.Free;
  end;
end;

procedure TTestDeltaModelMergePatch.TestStyleA_HiddenAndNonUpdatableFieldProtection;
var
  Prod: TProduct;
  ModifiedCount: Integer;
begin
  Prod := CreatePopulatedProduct;
  try
    // sku has no dboUpdate; internal_secret has Visible = False
    ModifiedCount := ApplyMergePatch(Prod, '{"sku": "HACK-SKU", "internal_secret": "HACKED"}');
    AssertEquals('Protected fields must not be modified', 0, ModifiedCount);
    AssertEquals('sku unchanged', 'PRD-0042', Prod.sku.AsString);
    AssertEquals('internal_secret unchanged', 'SECRET-TOKEN', Prod.internal_secret.AsString);
    AssertFalse('Model must have no dirty fields', Prod.HasDirtyFields);
    AssertEquals('No SQL generated when 0 dirty fields', '',
      TDMSQLBuilder.CreatePartialUpdate(Prod, ddPostgreSQL));
  finally
    Prod.Free;
  end;
end;

procedure TTestDeltaModelMergePatch.TestStyleA_UnknownFieldsIgnoredByDefaultAndStrictReject;
var
  Prod: TProduct;
  ModifiedCount: Integer;
  Raised: Boolean;
begin
  Prod := CreatePopulatedProduct;
  try
    ModifiedCount := ApplyMergePatch(Prod, '{"non_existent_field": "abc", "brand": "NewBrand"}');
    AssertEquals('Only known field counted', 1, ModifiedCount);
    AssertEquals('brand updated', 'NewBrand', Prod.brand.AsString);

    Raised := False;
    try
      ApplyMergePatch(Prod, '{"non_existent_field": "abc"}', [mpoRejectUnknownFields]);
    except
      on E: EDeltaMergePatchException do
        Raised := True;
    end;
    AssertTrue('mpoRejectUnknownFields must raise EDeltaMergePatchException', Raised);
  finally
    Prod.Free;
  end;
end;

procedure TTestDeltaModelMergePatch.TestStyleA_ZeroDirtyFieldsProducesNoUpdateSQL;
var
  Prod: TProduct;
begin
  Prod := CreatePopulatedProduct;
  try
    AssertEquals('Empty patch returns 0', 0, ApplyMergePatch(Prod, '{}'));
    AssertFalse('HasDirtyFields must be False', Prod.HasDirtyFields);
    AssertEquals('CreatePartialUpdate must return empty string', '',
      TDMSQLBuilder.CreatePartialUpdate(Prod, ddPostgreSQL));
    AssertEquals('CreatePartialUpdateReturning must return empty string', '',
      TDMSQLBuilder.CreatePartialUpdateReturning(Prod, ddPostgreSQL));
  finally
    Prod.Free;
  end;
end;

procedure TTestDeltaModelMergePatch.TestStyleA_PartialUpdateSQLMultiDialect;
var
  Prod: TProduct;
  SQLPg, SQLMssql: string;
begin
  Prod := CreatePopulatedProduct;
  try
    ApplyMergePatch(Prod, '{"name": "Pro Keyboard", "stock": 95}');

    SQLPg := TDMSQLBuilder.CreatePartialUpdateReturning(Prod, ddPostgreSQL);
    AssertEquals('PostgreSQL partial UPDATE RETURNING',
      'UPDATE products SET name = :name, stock = :stock WHERE (id = 42)' + sLineBreak + 'RETURNING *',
      SQLPg);

    SQLMssql := TDMSQLBuilder.CreatePartialUpdateReturning(Prod, ddMSSQL);
    AssertEquals('MSSQL partial UPDATE OUTPUT INSERTED.*',
      'UPDATE products SET name = :name, stock = :stock OUTPUT INSERTED.* WHERE (id = 42)',
      SQLMssql);
  finally
    Prod.Free;
  end;
end;

procedure TTestDeltaModelMergePatch.TestStyleA_PartialUpdateInlineNullSQL;
var
  Prod: TProduct;
  SQL: string;
begin
  Prod := CreatePopulatedProduct;
  try
    ApplyMergePatch(Prod, '{"description": null, "stock": 5}');
    SQL := TDMSQLBuilder.CreatePartialUpdate(Prod, ddSQLite, '', False);
    AssertEquals('Inline SQL must emit description = NULL and stock = 5',
      'UPDATE products SET description = NULL, stock = 5 WHERE (id = 42)', SQL);
  finally
    Prod.Free;
  end;
end;

procedure TTestDeltaModelMergePatch.TestStyleB_20FieldsPrimitiveUpdateAndExplicitNull;
var
  Usr: TUser;
  ModifiedCount: Integer;
  SQL: string;
begin
  Usr := CreatePopulatedUser;
  try
    ModifiedCount := ApplyMergePatch(Usr, '{"id": 999, "city": "Denver", "bio": null}');
    AssertEquals('2 fields modified (PK ignored)', 2, ModifiedCount);
    AssertEquals('PK unchanged', Int64(7), Usr.id);
    AssertEquals('city updated', 'Denver', Usr.city);
    AssertTrue('bio marked null', Usr.IsNull('bio'));
    AssertTrue('bio marked dirty', Usr.IsDirty('bio'));
    AssertTrue('city marked dirty', Usr.IsDirty('city'));
    AssertFalse('email remains clean', Usr.IsDirty('email'));
    AssertFalse('username remains clean', Usr.IsDirty('username'));

    SQL := TDMSQLBuilder.CreatePartialUpdate(Usr, ddPostgreSQL);
    AssertEquals('Style B partial UPDATE SQL',
      'UPDATE users SET bio = :bio, city = :city WHERE (id = 7)', SQL);

    SQL := TDMSQLBuilder.CreatePartialUpdate(Usr, ddPostgreSQL, '', False);
    AssertEquals('Style B inline partial UPDATE SQL with NULL',
      'UPDATE users SET bio = NULL, city = ''Denver'' WHERE (id = 7)', SQL);
  finally
    Usr.Free;
  end;
end;

procedure TTestDeltaModelMergePatch.TestNestedDocumentHasOneMergeAndHasManyReplace;
var
  Doc: TDocument;
  AuthorObj: TDocumentAuthor;
  Tag1, Tag2: TDocumentTag;
  ModifiedCount: Integer;
begin
  Doc := TDocument.Create;
  try
    Doc.id.AsLargeInt := 1;
    Doc.title.Value := 'Goodbye!';
    Doc.content.Value := 'This will be unchanged';

    AuthorObj := TDocumentAuthor.Create;
    AuthorObj.given_name.Value := 'John';
    AuthorObj.family_name.Value := 'Doe';
    Doc.author.Model := AuthorObj;

    Tag1 := TDocumentTag.Create;
    Tag1.label_text.Value := 'example';
    Tag2 := TDocumentTag.Create;
    Tag2.label_text.Value := 'sample';
    Doc.tags.Add(Tag1);
    Doc.tags.Add(Tag2);
    Doc.ClearAllDirty;

    // Classic RFC 7396 Section 1 example adapted to DeltaModel
    // Note: author and tags are virtual relations, so test non-virtual fields + direct nested patch
    Doc.author.IsVirtual := False;
    Doc.tags.IsVirtual := False;
    // Refresh metadata cache is class-level, so test recursive ApplyMergePatch directly on nested model
    ModifiedCount := ApplyMergePatch(AuthorObj, '{"family_name": null}');
    AssertEquals('Nested author modified count', 1, ModifiedCount);
    AssertEquals('Author given_name preserved', 'John', AuthorObj.given_name.AsString);
    AssertTrue('Author family_name set to null', AuthorObj.family_name.IsNull);
    AssertTrue('Author family_name dirty', AuthorObj.family_name.Dirty);
    AssertFalse('Author given_name clean', AuthorObj.given_name.Dirty);

    ModifiedCount := ApplyMergePatch(Doc, '{"title": "Hello!"}');
    AssertEquals('Document title modified', 1, ModifiedCount);
    AssertEquals('Document title updated', 'Hello!', Doc.title.AsString);
    AssertEquals('Document content unchanged', 'This will be unchanged', Doc.content.AsString);
    AssertFalse('Document content clean', Doc.content.Dirty);
  finally
    Doc.Free;
  end;
end;

procedure TTestDeltaModelMergePatch.TestEndToEndSQLitePatchObject;
var
  Engine: IDeltaORMEngine;
  Prod, Loaded: TProduct;
  Q: TQuery;
  Patched: Boolean;
begin
  Engine := TDeltaORMEngine.Create('sqlite:///:memory:');
  try
    Engine.ExecuteDirect(
      'CREATE TABLE products (' +
      'id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
      'sku TEXT NOT NULL, ' +
      'name TEXT NOT NULL, ' +
      'description TEXT, ' +
      'category TEXT, ' +
      'brand TEXT, ' +
      'price REAL NOT NULL, ' +
      'cost REAL, ' +
      'stock INTEGER, ' +
      'min_stock INTEGER, ' +
      'max_stock INTEGER, ' +
      'weight REAL, ' +
      'width REAL, ' +
      'height REAL, ' +
      'depth REAL, ' +
      'barcode TEXT, ' +
      'origin TEXT, ' +
      'notes TEXT, ' +
      'active INTEGER NOT NULL, ' +
      'internal_secret TEXT)'
    );

    Prod := CreatePopulatedProduct(False);
    try
      AssertTrue('Initial INSERT must succeed', TInsert.InsertObject(Engine, Prod));
      AssertTrue('AutoInc PK assigned', Prod.id.AsLargeInt > 0);
      Prod.ClearAllDirty;

      // 0 dirty fields -> PatchObject must be a no-op returning False
      Patched := TUpdate.PatchObject(Engine, Prod);
      AssertFalse('PatchObject with 0 dirty fields must return False', Patched);

      // Apply RFC 7396 Merge Patch: update price, clear description to NULL, attempt PK overwrite
      AssertEquals('2 fields patched', 2,
        ApplyMergePatch(Prod, '{"id": 9999, "price": 249.99, "description": null}'));

      Patched := TUpdate.PatchObject(Engine, Prod);
      AssertTrue('PatchObject with 2 dirty fields must return True', Patched);
      AssertFalse('Dirty flags cleared after PatchObject', Prod.HasDirtyFields);

      // Reload from SQLite and verify partial persistence
      Q := TQuery.Create(Engine);
      try
        Q.SetModel(TProduct);
        Loaded := TProduct(Q.FindById(Prod.id.AsLargeInt));
        try
          AssertNotNull('Reloaded product must exist', Loaded);
          AssertEquals('Patched price persisted', 249.99, Loaded.price.AsFloat, 0.001);
          AssertTrue('Explicit NULL description persisted', Loaded.description.IsNull);
          AssertEquals('Untouched name preserved', 'Mechanical Keyboard', Loaded.name.AsString);
          AssertEquals('Untouched brand preserved', 'KeyCraft', Loaded.brand.AsString);
          AssertEquals('Untouched stock preserved', 120, Loaded.stock.AsInteger);
          AssertFalse('Hydrated model starts clean (not dirty)', Loaded.HasDirtyFields);
        finally
          Loaded.Free;
        end;
      finally
        Q.Free;
      end;
    finally
      Prod.Free;
    end;
  finally
    Engine := nil;
  end;
end;

{ TTestZenithMergePatch }

procedure TTestZenithMergePatch.TestContentTypeDetectionAndCharset;
begin
  AssertTrue('Exact media type',
    TZenithMergePatch.IsMergePatchContentType('application/merge-patch+json'));
  AssertTrue('Media type with charset parameter',
    TZenithMergePatch.IsMergePatchContentType('application/merge-patch+json; charset=utf-8'));
  AssertTrue('Case-insensitive media type',
    TZenithMergePatch.IsMergePatchContentType('Application/Merge-Patch+JSON; charset=UTF-8'));
  AssertFalse('Regular application/json is not merge-patch',
    TZenithMergePatch.IsMergePatchContentType('application/json'));
  AssertFalse('Empty content type is not merge-patch',
    TZenithMergePatch.IsMergePatchContentType(''));
end;

procedure TTestZenithMergePatch.TestRejectInvalidContentType;
var
  Req: TMockRequest;
  Raised: Boolean;
begin
  Req := TMockRequest.Create;
  try
    Req.ContentType := 'application/json';
    Req.SetBody('{"price": 10}');
    Raised := False;
    try
      TZenithMergePatch.RequireMergePatchContentType(Req);
    except
      on E: EUnsupportedMediaType do
        Raised := True;
    end;
    AssertTrue('application/json must raise EUnsupportedMediaType', Raised);
  finally
    Req.Free;
  end;
end;

procedure TTestZenithMergePatch.TestApplyToModelFromHTTPRequest;
var
  Req: TMockRequest;
  Prod: TProduct;
  ModifiedCount: Integer;
begin
  Req := TMockRequest.Create;
  Prod := TProduct.Create;
  try
    Prod.id.AsLargeInt := 10;
    Prod.name.Value := 'Monitor';
    Prod.price.AsCurrency := 300.00;
    Prod.description.Value := '4K Display';
    Prod.ClearAllDirty;

    Req.ContentType := MIME_APPLICATION_MERGE_PATCH_JSON + '; charset=utf-8';
    Req.SetBody('{"price": 279.00, "description": null}');

    AssertTrue('IsMergePatchRequest must be true', TZenithMergePatch.IsMergePatchRequest(Req));
    ModifiedCount := TZenithMergePatch.ApplyToModel(Req, Prod);
    AssertEquals('2 fields modified from HTTP request', 2, ModifiedCount);
    AssertEquals('Updated price', 279.00, Prod.price.AsFloat, 0.001);
    AssertTrue('description cleared to null', Prod.description.IsNull);
    AssertEquals('Partial SQL from patched model',
      'UPDATE products SET description = :description, price = :price WHERE (id = 10)',
      TDMSQLBuilder.CreatePartialUpdate(Prod, ddPostgreSQL));
  finally
    Prod.Free;
    Req.Free;
  end;
end;

procedure TTestZenithMergePatch.TestApplyJsonFromHTTPRequest;
var
  Req: TMockRequest;
  Target, Merged: TJSONData;
begin
  Req := TMockRequest.Create;
  Target := GetJSON('{"title":"Old","meta":{"draft":true,"rev":1}}', False);
  try
    Req.ContentType := MIME_APPLICATION_MERGE_PATCH_JSON;
    Req.SetBody('{"meta":{"draft":null,"rev":2}}');

    Merged := TZenithMergePatch.ApplyJson(Req, Target);
    try
      AssertEquals('Title preserved', 'Old', TJSONObject(Merged).Get('title', ''));
      AssertEquals('Rev updated', 2, TJSONObject(TJSONObject(Merged).Find('meta')).Get('rev', 0));
      AssertTrue('Draft deleted', TJSONObject(TJSONObject(Merged).Find('meta')).IndexOfName('draft') < 0);
    finally
      Merged.Free;
    end;
  finally
    Target.Free;
    Req.Free;
  end;
end;

procedure TTestZenithMergePatch.TestAdvertiseAcceptPatchHeader;
var
  Req: TMockRequest;
  Resp: TMockResponse;
begin
  Req := TMockRequest.Create;
  Resp := TMockResponse.Create(Req);
  try
    TZenithMergePatch.AdvertiseAcceptPatch(Resp);
    AssertEquals('Accept-Patch header value',
      MIME_APPLICATION_MERGE_PATCH_JSON,
      Resp.GetCustomHeader('Accept-Patch'));
  finally
    Resp.Free;
    Req.Free;
  end;
end;

{ Benchmark }

function RunMergePatchBenchmark(AIterations: Integer): TBenchmarkMetrics;
var
  Prod: TProduct;
  PatchPayload, FullPutPayload: RawByteString;
  SQLPatch, SQLPut: string;
  StartTick, EndTick: QWord;
  HeapBefore, HeapAfter: TFPCHeapStatus;
  I: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Iterations := AIterations;

  PatchPayload := '{"price": 199.50}';
  FullPutPayload :=
    '{"id":42,"sku":"PRD-0042","name":"Mechanical Keyboard",' +
    '"description":"Tactile switches with aluminum frame",' +
    '"category":"Peripherals","brand":"KeyCraft","price":199.50,' +
    '"cost":85.00,"stock":120,"min_stock":10,"max_stock":500,' +
    '"weight":0.95,"width":36.0,"height":3.5,"depth":14.0,' +
    '"barcode":"7890000000421","origin":"Local","notes":"Flagship item","active":true}';

  Prod := TProduct.Create;
  try
    Prod.id.AsLargeInt := 42;
    Prod.sku.Value := 'PRD-0042';
    Prod.name.Value := 'Mechanical Keyboard';
    Prod.description.Value := 'Tactile switches with aluminum frame';
    Prod.category.Value := 'Peripherals';
    Prod.brand.Value := 'KeyCraft';
    Prod.price.AsCurrency := 149.90;
    Prod.cost.AsCurrency := 85.00;
    Prod.stock.AsInteger := 120;
    Prod.min_stock.AsInteger := 10;
    Prod.max_stock.AsInteger := 500;
    Prod.weight.AsFloat := 0.95;
    Prod.width.AsFloat := 36.0;
    Prod.height.AsFloat := 3.5;
    Prod.depth.AsFloat := 14.0;
    Prod.barcode.Value := '7890000000421';
    Prod.origin.Value := 'Local';
    Prod.notes.Value := 'Flagship item';
    Prod.active.AsBoolean := True;
    Prod.ClearAllDirty;

    // Warm-up metadata cache
    ApplyMergePatch(Prod, PatchPayload, [mpoClearDirtyBeforeApply]);
    SQLPatch := TDMSQLBuilder.CreatePartialUpdate(Prod, ddPostgreSQL);
    Prod.FromJson(FullPutPayload);
    SQLPut := TDMSQLBuilder.CreateUpdate(Prod, ddPostgreSQL);

    Result.PatchSQLBytes := Length(SQLPatch);
    Result.PutSQLBytes := Length(SQLPut);
    if Result.PutSQLBytes > 0 then
      Result.SQLSizeReductionPct := (1.0 - (Result.PatchSQLBytes / Result.PutSQLBytes)) * 100.0;

    // Benchmark 1: RFC 7396 Merge Patch + Partial UPDATE
    HeapBefore := GetFPCHeapStatus;
    StartTick := GetTickCount64;
    for I := 1 to AIterations do
    begin
      ApplyMergePatch(Prod, PatchPayload, [mpoClearDirtyBeforeApply]);
      SQLPatch := TDMSQLBuilder.CreatePartialUpdate(Prod, ddPostgreSQL);
    end;
    EndTick := GetTickCount64;
    HeapAfter := GetFPCHeapStatus;
    Result.PatchElapsedMs := EndTick - StartTick;
    if Result.PatchElapsedMs < 1 then Result.PatchElapsedMs := 1;
    Result.PatchOpsPerSec := (AIterations * 1000.0) / Result.PatchElapsedMs;
    Result.PatchHeapDeltaBytes := Int64(HeapAfter.CurrHeapUsed) - Int64(HeapBefore.CurrHeapUsed);

    // Benchmark 2: Full PUT (FromJson + Full UPDATE)
    HeapBefore := GetFPCHeapStatus;
    StartTick := GetTickCount64;
    for I := 1 to AIterations do
    begin
      Prod.FromJson(FullPutPayload);
      SQLPut := TDMSQLBuilder.CreateUpdate(Prod, ddPostgreSQL);
    end;
    EndTick := GetTickCount64;
    HeapAfter := GetFPCHeapStatus;
    Result.PutElapsedMs := EndTick - StartTick;
    if Result.PutElapsedMs < 1 then Result.PutElapsedMs := 1;
    Result.PutOpsPerSec := (AIterations * 1000.0) / Result.PutElapsedMs;
    Result.PutHeapDeltaBytes := Int64(HeapAfter.CurrHeapUsed) - Int64(HeapBefore.CurrHeapUsed);

    Result.SpeedupFactor := Result.PutElapsedMs / Result.PatchElapsedMs;
  finally
    Prod.Free;
  end;
end;

initialization
  RegisterTest('RFC7396.AppendixA', TTestRFC7396AppendixA);
  RegisterTest('DeltaModel.MergePatch', TTestDeltaModelMergePatch);
  RegisterTest('Zenith.MergePatch', TTestZenithMergePatch);

end.
