unit DeltaModel.Fields;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Variants, DeltaModelMessages;

type

  TDBOptions = (dboPrimaryKey, dboUpdate, dboAutoInc);
  TFKOption = (fkNone, fkCascade, fkSetNull, fkRestrict);
  TDBOptionsSet = set of TDBOptions;

  { TForeignKey }

  TForeignKey = class
  private
    FOnDelete: TFKOption;
    FOnUpdate: TFKOption;
    FReferencesField: string;
    FReferencesTable: TClass;
  public
    property ReferencesTable: TClass read FReferencesTable write FReferencesTable;
    property ReferencesField: string read FReferencesField write FReferencesField;
    property OnDelete: TFKOption read FOnDelete write FOnDelete;
    property OnUpdate: TFKOption read FOnUpdate write FOnUpdate;
  end;

  { TDeltaField }

  TDeltaField = class
  private
    FDBOptions: TDBOptionsSet;
    FFieldName: string;
    FForeignKey: TForeignKey;
    FVisible: Boolean;
    procedure SetFieldName(AValue: string);
  protected
    FValue: Variant;
    function GetValue: Variant; virtual; abstract;
    procedure SetValue(AValue: Variant); virtual; abstract;
    function FS: TFormatSettings;
  public
    property Value: Variant read GetValue write SetValue;
    property Visible: Boolean read FVisible write FVisible;

    ///to be used with ORM
    property FieldName: string read FFieldName write SetFieldName;
    property DBOptions: TDBOptionsSet read FDBOptions write FDBOptions;
    property ForeignKey: TForeignKey read FForeignKey;

    procedure Clear; virtual; abstract;
    procedure AfterConstruction; override;
    function IsNull: Boolean; virtual; abstract;
    function AsString: string; virtual;
    function IsValid: Boolean; virtual; abstract;
    function SwaggerDataType: string; virtual; abstract;
    constructor Create;
    destructor Destroy; override;
  end;

  { TDeltaFieldNullable }

  TDeltaFieldNullable = class(TDeltaField)
  protected
    function GetValue: Variant; override;
    procedure SetValue(AValue: Variant); override;
  public
    procedure Clear; override;
    procedure AfterConstruction; override;
    function IsNull: Boolean; override;
    function IsValid: Boolean; override;
    function SwaggerDataType: string; override;
    constructor Create;
  end;

  { TDeltaFieldRequired }

  TDeltaFieldRequired = class(TDeltaField)
  protected
    function GetValue: Variant; override;
    procedure SetValue(AValue: Variant); override;
  public
    procedure Clear; override;
    function IsNull: Boolean; override;
    function IsValid: Boolean; override;
    function SwaggerDataType: string; override;
    constructor Create;
  end;

  { TDFIntNull }

  TDFIntNull = class(TDeltaFieldNullable)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    function AsString: string; override;
    function SwaggerDataType: string; override;
  end;

  { TDFDoubleNull }

  TDFDoubleNull = class(TDeltaFieldNullable)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    function AsString: string; override;
    function SwaggerDataType: string; override;
  end;

  { TDFCurrencyNull }

  TDFCurrencyNull = class(TDeltaFieldNullable)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    function AsString: string; override;
    function SwaggerDataType: string; override;
  end;

  { TDFStringNull }

  TDFStringNull = class(TDeltaFieldNullable)
  private
    FSize: Integer;
  protected
    procedure SetValue(AValue: Variant); override;
  public
    property Size: Integer read FSize write FSize;
    procedure AfterConstruction; override;
  end;

  { TDFIntRequired }

  TDFIntRequired = class(TDeltaFieldRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    function AsString: string; override;
    function SwaggerDataType: string; override;
  end;

  { TDFDoubleRequired }

  TDFDoubleRequired = class(TDeltaFieldRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    function AsString: string; override;
    function SwaggerDataType: string; override;
  end;

  { TDFCurrencyRequired }

  TDFCurrencyRequired = class(TDeltaFieldRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    function AsString: string; override;
    function SwaggerDataType: string; override;
  end;

  { TDFStringRequired }

  TDFStringRequired = class(TDeltaFieldRequired)
  private
    FSize: Integer;
  protected
    procedure SetValue(AValue: Variant); override;
  public
    property Size: Integer read FSize write FSize;
    procedure AfterConstruction; override;
  end;

  { TDFDateNull }

  TDFDateNull = class(TDeltaFieldNullable)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    function AsString: string; override;
  end;

  { TDFTimeNull }

  TDFTimeNull = class(TDeltaFieldNullable)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    function AsString: string; override;
  end;

  { TDFDateTimeNull }

  TDFDateTimeNull = class(TDeltaFieldNullable)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    function AsString: string; override;
  end;

  { TDFDateRequired }

  TDFDateRequired = class(TDeltaFieldRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    function AsString: string; override;
  end;

  { TDFTimeRequired }

  TDFTimeRequired = class(TDeltaFieldRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    function AsString: string; override;
  end;

  { TDFDateTimeRequired }

  TDFDateTimeRequired = class(TDeltaFieldRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    function AsString: string; override;
  end;

  { TDFBooleanRequired }

  TDFBooleanRequired = class(TDeltaFieldRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    function AsString: string; override;
    function SwaggerDataType: string; override;
  end;

  { TDFBooleanNull }

  TDFBooleanNull = class(TDeltaFieldNullable)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    function AsString: string; override;
    function SwaggerDataType: string; override;
  end;

procedure DateTimeToField(AField: TDeltaField; const DateTime: string);

implementation

const
  DEFAULT_STR_SIZE = 255;

procedure DateTimeToField(AField: TDeltaField; const DateTime: string);
begin
  AField.Value := ISO8601ToDateDef(
    DateTime,
    0
  );
end;

{ TDeltaField }

procedure TDeltaField.SetFieldName(AValue: string);
begin
  if FFieldName = AValue then Exit;

  FFieldName := AValue;
end;

function TDeltaField.FS: TFormatSettings;
begin
  Result.DecimalSeparator := '.';
  Result.ThousandSeparator := ',';
end;

procedure TDeltaField.AfterConstruction;
begin
  inherited AfterConstruction;
  DBOptions := [dboUpdate];
  FForeignKey := TForeignKey.Create;
end;

function TDeltaField.AsString: string;
begin
  Result := VarToStrDef(Self.Value, EmptyStr);
end;

constructor TDeltaField.Create;
begin
  inherited;
  FValue := Null;
  FVisible := True;
end;

destructor TDeltaField.Destroy;
begin
  FForeignKey.Free;
  inherited Destroy;
end;

{ TDeltaFieldNullable }

function TDeltaFieldNullable.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TDeltaFieldNullable.SetValue(AValue: Variant);
begin
  if VarIsNull(AValue) then
    FValue := Null
  else
    FValue := AValue;
end;

procedure TDeltaFieldNullable.Clear;
begin
  FValue := Null;
end;

procedure TDeltaFieldNullable.AfterConstruction;
begin
  inherited AfterConstruction;
  Self.Clear;
end;

function TDeltaFieldNullable.IsNull: Boolean;
begin
  Result := VarIsNull(FValue);
end;

function TDeltaFieldNullable.IsValid: Boolean;
begin
  Result := not VarIsNull(FValue);
end;

function TDeltaFieldNullable.SwaggerDataType: string;
begin
  if VarIsNull(FValue) then
    Result := 'null'
  else
    Result := 'string';
end;

constructor TDeltaFieldNullable.Create;
begin
  inherited;
  Self.AfterConstruction;
end;

{ TDeltaFieldRequired }

function TDeltaFieldRequired.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TDeltaFieldRequired.SetValue(AValue: Variant);
begin
  if VarIsNull(AValue) then
    raise Exception.CreateFmt(ValidationFailedForField, [Self.FieldName, 'NULL']);

  FValue := AValue;
end;

procedure TDeltaFieldRequired.Clear;
begin
  raise Exception.CreateFmt(ValidationFailedForField, [Self.FieldName, 'NULL']);
end;

function TDeltaFieldRequired.IsNull: Boolean;
begin
  Result := VarIsNull(FValue);
end;

function TDeltaFieldRequired.IsValid: Boolean;
begin
  Result := True;
end;

function TDeltaFieldRequired.SwaggerDataType: string;
begin
  Result := 'string';
end;

constructor TDeltaFieldRequired.Create;
begin
  inherited;
  Self.AfterConstruction;
end;

{ TDFIntNull }

procedure TDFIntNull.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
  if not VarIsNull(AValue) then
  begin
    Self.FValue := Integer(AValue);
  end;
end;

function TDFIntNull.AsString: string;
begin
  Result := StrToIntDef(inherited AsString, 0).ToString;
end;

function TDFIntNull.SwaggerDataType: string;
begin
  Result := 'integer';
end;

{ TDFDoubleNull }

procedure TDFDoubleNull.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
  if not VarIsNull(AValue) then
  begin
    Self.FValue := Double(AValue);
  end;
end;

function TDFDoubleNull.AsString: string;
begin
  Result := FloatToStr(StrToFloatDef(inherited AsString, 0), FS);
end;

function TDFDoubleNull.SwaggerDataType: string;
begin
  Result := 'number';
end;

{ TDFCurrencyNull }

procedure TDFCurrencyNull.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
  if not VarIsNull(AValue) then
  begin
    Self.FValue := Currency(AValue);
  end;
end;

function TDFCurrencyNull.AsString: string;
begin
  Result := FloatToStr(StrToFloatDef(inherited AsString, 0), FS);
end;

function TDFCurrencyNull.SwaggerDataType: string;
begin
  Result := 'number';
end;

{ TDFStringNull }

procedure TDFStringNull.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
  if not VarIsNull(AValue) then
  begin
    Self.FValue := Copy(string(AValue).Trim, 1, Self.FSize);
    Self.FValue := Trim(FValue);
  end;
end;

procedure TDFStringNull.AfterConstruction;
begin
  inherited AfterConstruction;
  Size := DEFAULT_STR_SIZE;
end;

{ TDFIntRequired }

procedure TDFIntRequired.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
  if not VarIsNull(AValue) then
  begin
    Self.FValue := Integer(AValue);
  end;
end;

function TDFIntRequired.AsString: string;
begin
  Result := StrToIntDef(inherited AsString, 0).ToString;
end;

function TDFIntRequired.SwaggerDataType: string;
begin
  Result := 'integer';
end;

{ TDFDoubleRequired }

procedure TDFDoubleRequired.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
  if not VarIsNull(AValue) then
  begin
    Self.FValue := Double(AValue);
  end;
end;

function TDFDoubleRequired.AsString: string;
begin
  Result := FloatToStr(StrToFloatDef(inherited AsString, 0), FS);
end;

function TDFDoubleRequired.SwaggerDataType: string;
begin
  Result := 'number';
end;

{ TDFCurrencyRequired }

procedure TDFCurrencyRequired.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
  if not VarIsNull(AValue) then
  begin
    Self.FValue := Currency(AValue);
  end;
end;

function TDFCurrencyRequired.AsString: string;
begin
  Result := FloatToStr(StrToFloatDef(inherited AsString, 0), FS);
end;

function TDFCurrencyRequired.SwaggerDataType: string;
begin
  Result := 'number';
end;

{ TDFStringRequired }

procedure TDFStringRequired.SetValue(AValue: Variant);
var
  S: string;
begin
  inherited SetValue(AValue);
  if not VarIsNull(AValue) then
  begin
    S := string(AValue);
    Self.FValue := Copy(S.Trim, 1, Self.FSize);
    Self.FValue := Trim(FValue);
  end;
end;

procedure TDFStringRequired.AfterConstruction;
begin
  inherited AfterConstruction;
  Size := DEFAULT_STR_SIZE;
end;

{ TDFDateNull }

procedure TDFDateNull.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
  if AValue = 0 then
  begin
    Self.Clear;
  end;
end;

function TDFDateNull.AsString: string;
begin
  Result := DateToISO8601(Self.Value);
end;

{ TDFTimeNull }

procedure TDFTimeNull.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
  if AValue = 0 then
  begin
    Self.Clear;
  end;
end;

function TDFTimeNull.AsString: string;
begin
  if Self.IsNull then
  begin
    Exit('00:00:00');
  end;
  Result := FormatDateTime('hh:nn:ss', Self.Value);
end;

{ TDFDateTimeNull }

procedure TDFDateTimeNull.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
  if AValue = 0 then
  begin
    Self.Clear;
  end;
end;

function TDFDateTimeNull.AsString: string;
begin
  Result := DateToISO8601(Self.Value);
end;

{ TDFDateRequired }

procedure TDFDateRequired.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
end;

function TDFDateRequired.AsString: string;
begin
  Result := DateToISO8601(Self.Value);
end;

{ TDFTimeRequired }

procedure TDFTimeRequired.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
end;

function TDFTimeRequired.AsString: string;
begin
  Result := FormatDateTime('hh:nn:ss', Self.Value);
end;

{ TDFDateTimeRequired }

procedure TDFDateTimeRequired.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
end;

function TDFDateTimeRequired.AsString: string;
begin
  Result := DateToISO8601(Self.Value);
end;

{ TDFBooleanRequired }

procedure TDFBooleanRequired.SetValue(AValue: Variant);
begin
  inherited SetValue(Boolean(AValue));
end;

function TDFBooleanRequired.AsString: string;
begin
  Result := 'F';
  if FValue then
    Exit('T');
end;

function TDFBooleanRequired.SwaggerDataType: string;
begin
  Result := 'boolean';
end;

{ TDFBooleanNull }

procedure TDFBooleanNull.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
end;

function TDFBooleanNull.AsString: string;
begin
  Result := 'F';
  if FValue then
    Exit('T');
end;

function TDFBooleanNull.SwaggerDataType: string;
begin
  Result := 'boolean';
end;

end.


