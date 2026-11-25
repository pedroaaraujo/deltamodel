unit test_deltamodel_api_doc;

{$mode ObjFPC}{$H+}

{ Tests for TDeltaModel.DescribeApiSchema and typed examples in GenerateSchema. }

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser,
  DeltaModel, DeltaModel.Fields, DeltaModel.ApiDoc, DeltaAPISchema;

type

  { TApiDocSupplier }

  TApiDocSupplier = class(TDeltaModel)
  private
    FTaxId: string;
  public
    class procedure DescribeApiSchema(ADoc: TDeltaApiDoc); override;
  published
    property taxId: string read FTaxId write FTaxId;
  end;

  { TApiDocInstallment }

  TApiDocInstallment = class(TDeltaModel)
  private
    FAmount: TDFCurrencyRequired;
    FInstallment: TDFIntNull;
    FStatus: string;
    FSupplier: TApiDocSupplier;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class procedure DescribeApiSchema(ADoc: TDeltaApiDoc); override;
  published
    property amount: TDFCurrencyRequired read FAmount write FAmount;
    property installment: TDFIntNull read FInstallment write FInstallment;
    property status: string read FStatus write FStatus;
    property supplier: TApiDocSupplier read FSupplier;
  end;

  { TApiDocUndescribed }

  TApiDocUndescribed = class(TDeltaModel)
  private
    FName: string;
  published
    property name: string read FName write FName;
  end;

  { TTestDeltaModelApiDoc }

  TTestDeltaModelApiDoc = class(TTestCase)
  private
    function BuildSchema(IsArray: Boolean): TJSONObject;
    function Props(ASchema: TJSONObject): TJSONObject;
  published
    procedure TestModelDescription;
    procedure TestFieldDescriptionAndEnum;
    procedure TestNestedModelUsesOwnDoc;
    procedure TestParentDescribesNestedProperty;
    procedure TestArraySchemaKeepsDocsOnItems;
    procedure TestUnknownPropertyIsIgnored;
    procedure TestUndescribedModelUnchanged;
    procedure TestNumericExamplesAreNumbers;
    procedure TestFieldLookupIsCaseInsensitive;
  end;

implementation

{ TApiDocSupplier }

class procedure TApiDocSupplier.DescribeApiSchema(ADoc: TDeltaApiDoc);
begin
  ADoc.Field('taxId').Description('Formatted CNPJ or CPF');
end;

{ TApiDocInstallment }

procedure TApiDocInstallment.AfterConstruction;
begin
  inherited AfterConstruction;
  FSupplier := TApiDocSupplier.Create;
end;

procedure TApiDocInstallment.BeforeDestruction;
begin
  FSupplier.Free;
  inherited BeforeDestruction;
end;

class procedure TApiDocInstallment.DescribeApiSchema(ADoc: TDeltaApiDoc);
begin
  ADoc.Description('Accounts payable installment');
  ADoc.Field('status').Description('P = paid, O = open').Enum(['P', 'O']);
  ADoc.Field('amount').Description('Original amount');
  ADoc.Field('supplier').Description('Creditor');
  ADoc.Field('doesNotExist').Description('ignored');
end;

{ TTestDeltaModelApiDoc }

function TTestDeltaModelApiDoc.BuildSchema(IsArray: Boolean): TJSONObject;
var
  Obj: TApiDocInstallment;
begin
  Obj := TApiDocInstallment.Create;
  try
    Obj.amount.Value := 1262.5;
    Obj.installment.Value := 3;
    Obj.status := 'P';
    Obj.supplier.taxId := '12.345.678/0001-90';
    Result := GenerateSchema(Obj, True, IsArray);
  finally
    Obj.Free;
  end;
end;

function TTestDeltaModelApiDoc.Props(ASchema: TJSONObject): TJSONObject;
begin
  Result := ASchema.Objects['properties'];
end;

procedure TTestDeltaModelApiDoc.TestModelDescription;
var
  S: TJSONObject;
begin
  S := BuildSchema(False);
  try
    AssertEquals('Accounts payable installment', S.Get('description', ''));
  finally
    S.Free;
  end;
end;

procedure TTestDeltaModelApiDoc.TestFieldDescriptionAndEnum;
var
  S, P: TJSONObject;
begin
  S := BuildSchema(False);
  try
    P := Props(S).Objects['status'];
    AssertEquals('P = paid, O = open', P.Get('description', ''));
    AssertEquals(2, P.Arrays['enum'].Count);
    AssertEquals('O', P.Arrays['enum'].Strings[1]);
    AssertEquals('Original amount', Props(S).Objects['amount'].Get('description', ''));
  finally
    S.Free;
  end;
end;

procedure TTestDeltaModelApiDoc.TestNestedModelUsesOwnDoc;
var
  S: TJSONObject;
begin
  S := BuildSchema(False);
  try
    AssertEquals('Formatted CNPJ or CPF',
      Props(Props(S).Objects['supplier']).Objects['taxId'].Get('description', ''));
  finally
    S.Free;
  end;
end;

procedure TTestDeltaModelApiDoc.TestParentDescribesNestedProperty;
var
  S: TJSONObject;
begin
  S := BuildSchema(False);
  try
    AssertEquals('Creditor', Props(S).Objects['supplier'].Get('description', ''));
  finally
    S.Free;
  end;
end;

procedure TTestDeltaModelApiDoc.TestArraySchemaKeepsDocsOnItems;
var
  S: TJSONObject;
begin
  S := BuildSchema(True);
  try
    AssertEquals('array', S.Get('type', ''));
    AssertEquals('P = paid, O = open',
      Props(S.Objects['items']).Objects['status'].Get('description', ''));
  finally
    S.Free;
  end;
end;

procedure TTestDeltaModelApiDoc.TestUnknownPropertyIsIgnored;
var
  S: TJSONObject;
begin
  S := BuildSchema(False);
  try
    AssertTrue(Props(S).Find('doesNotExist') = nil);
  finally
    S.Free;
  end;
end;

procedure TTestDeltaModelApiDoc.TestUndescribedModelUnchanged;
var
  Obj: TApiDocUndescribed;
  S: TJSONObject;
begin
  Obj := TApiDocUndescribed.Create;
  try
    S := GenerateSchema(Obj, False, False);
    try
      AssertTrue(S.Find('description') = nil);
      AssertTrue(Props(S).Objects['name'].Find('description') = nil);
    finally
      S.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestDeltaModelApiDoc.TestNumericExamplesAreNumbers;
var
  S: TJSONObject;
  Ex: TJSONData;
begin
  S := BuildSchema(False);
  try
    Ex := Props(S).Objects['amount'].Find('example');
    AssertNotNull('amount example', Ex);
    AssertTrue('amount example must be a JSON number', Ex.JSONType = jtNumber);
    AssertEquals(1262.5, Ex.AsFloat, 0.001);

    Ex := Props(S).Objects['installment'].Find('example');
    AssertNotNull('installment example', Ex);
    AssertTrue('installment example must be a JSON number', Ex.JSONType = jtNumber);
    AssertEquals(3, Ex.AsInteger);
  finally
    S.Free;
  end;
end;

procedure TTestDeltaModelApiDoc.TestFieldLookupIsCaseInsensitive;
var
  Doc: TDeltaApiDoc;
begin
  Doc := TDeltaApiDoc.Create;
  try
    Doc.Field('Status').Description('a');
    AssertTrue(Doc.Field('status') = Doc.FindField('STATUS'));
    AssertEquals(1, Doc.FieldCount);
  finally
    Doc.Free;
  end;
end;

initialization
  RegisterTest(TTestDeltaModelApiDoc);

end.
