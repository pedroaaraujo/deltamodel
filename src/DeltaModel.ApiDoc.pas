unit DeltaModel.ApiDoc;

{$mode ObjFPC}{$H+}

{ Documentation-only metadata for OpenAPI schemas.

  Models describe themselves by overriding TDeltaModel.DescribeApiSchema:

    class procedure TInvoice.DescribeApiSchema(ADoc: TDeltaApiDoc);
    begin
      ADoc.Description('Invoice installment');
      ADoc.Field('status').Description('P = paid, O = open').Enum(['P', 'O']);
      ADoc.Field('dueDate').Description('Due date (dd/mm/yyyy)');
    end;

  GenerateSchema (DeltaAPISchema) reads it and emits "description" and "enum".
  It is intentionally separate from ConfigureSchema: nothing here affects the
  ORM, validation or serialization, so it is safe for TDF* (style A) models. }

interface

uses
  Classes, SysUtils, fgl;

type

  { TDeltaApiFieldDoc }

  TDeltaApiFieldDoc = class
  private
    FPropName: string;
    FDescription: string;
    FEnumValues: TStringArray;
  public
    constructor Create(const APropName: string);
    function Description(const AText: string): TDeltaApiFieldDoc;
    function Enum(const AValues: array of string): TDeltaApiFieldDoc;

    property PropName: string read FPropName;
    property DescriptionText: string read FDescription;
    property EnumValues: TStringArray read FEnumValues;
  end;

  TDeltaApiFieldDocList = specialize TFPGObjectList<TDeltaApiFieldDoc>;

  { TDeltaApiDoc }

  TDeltaApiDoc = class
  private
    FDescription: string;
    FFields: TDeltaApiFieldDocList;
    function GetField(Index: Integer): TDeltaApiFieldDoc;
    function GetFieldCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    { Model-level description (schema "description"). }
    function Description(const AText: string): TDeltaApiDoc;
    { Returns the doc of a published property, creating it on first use. }
    function Field(const APropName: string): TDeltaApiFieldDoc;
    { Case-insensitive lookup; nil when the property was not described. }
    function FindField(const APropName: string): TDeltaApiFieldDoc;

    property DescriptionText: string read FDescription;
    property Fields[Index: Integer]: TDeltaApiFieldDoc read GetField; default;
    property FieldCount: Integer read GetFieldCount;
  end;

implementation

{ TDeltaApiFieldDoc }

constructor TDeltaApiFieldDoc.Create(const APropName: string);
begin
  inherited Create;
  FPropName := APropName;
  SetLength(FEnumValues, 0);
end;

function TDeltaApiFieldDoc.Description(const AText: string): TDeltaApiFieldDoc;
begin
  FDescription := AText;
  Result := Self;
end;

function TDeltaApiFieldDoc.Enum(const AValues: array of string): TDeltaApiFieldDoc;
var
  I: Integer;
begin
  SetLength(FEnumValues, Length(AValues));
  for I := 0 to High(AValues) do
    FEnumValues[I] := AValues[I];
  Result := Self;
end;

{ TDeltaApiDoc }

constructor TDeltaApiDoc.Create;
begin
  inherited Create;
  FFields := TDeltaApiFieldDocList.Create(True);
end;

destructor TDeltaApiDoc.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;

function TDeltaApiDoc.GetField(Index: Integer): TDeltaApiFieldDoc;
begin
  Result := FFields[Index];
end;

function TDeltaApiDoc.GetFieldCount: Integer;
begin
  Result := FFields.Count;
end;

function TDeltaApiDoc.Description(const AText: string): TDeltaApiDoc;
begin
  FDescription := AText;
  Result := Self;
end;

function TDeltaApiDoc.Field(const APropName: string): TDeltaApiFieldDoc;
begin
  Result := FindField(APropName);
  if Result = nil then
  begin
    Result := TDeltaApiFieldDoc.Create(APropName);
    FFields.Add(Result);
  end;
end;

function TDeltaApiDoc.FindField(const APropName: string): TDeltaApiFieldDoc;
var
  I: Integer;
begin
  for I := 0 to FFields.Count - 1 do
    if SameText(FFields[I].PropName, APropName) then
      Exit(FFields[I]);
  Result := nil;
end;

end.
