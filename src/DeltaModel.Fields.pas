unit DeltaModel.Fields;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Variants, DeltaModelMessages;

type
  TDBOption = (dboPrimaryKey, dboUpdate, dboAutoInc, dboInsert);
  TDBOptions = set of TDBOption;
  TDBOptionsSet = TDBOptions;

  TForeignKeyEvent = (fkNone, fkCascade, fkSetNull, fkRestrict);
  TFKOption = TForeignKeyEvent;

  TDeltaFieldKind = (dfkInteger, dfkFloat, dfkString, dfkBoolean, dfkDateTime, dfkVirtual);

  { TForeignKey }

  TForeignKey = class
  private
    FOnDelete: TForeignKeyEvent;
    FOnUpdate: TForeignKeyEvent;
    FReferencesField: string;
    FReferencesTable: TClass;
  public
    procedure References(ATable: TClass; const AField: string = 'id';
      AOnDelete: TForeignKeyEvent = fkRestrict; AOnUpdate: TForeignKeyEvent = fkCascade);
    property ReferencesTable: TClass read FReferencesTable write FReferencesTable;
    property ReferencesField: string read FReferencesField write FReferencesField;
    property OnDelete: TForeignKeyEvent read FOnDelete write FOnDelete;
    property OnUpdate: TForeignKeyEvent read FOnUpdate write FOnUpdate;
  end;

  TRelationConfig = TForeignKey;

  { TDeltaField }

  TDeltaField = class
  private
    FDBOptions: TDBOptionsSet;
    FFieldName: string;
    FForeignKey: TForeignKey;
    FVisible: Boolean;
    FIsVirtual: Boolean;
    FIsRequired: Boolean;
    FFieldKind: TDeltaFieldKind;
    procedure SetFieldName(AValue: string);
    function GetForeignKey: TForeignKey;
  protected
    FValue: Variant;
    function GetValue: Variant; virtual; abstract;
    procedure SetValue(AValue: Variant); virtual; abstract;

    function GetAsLargeInt: Int64; virtual;
    procedure SetAsLargeInt(const AValue: Int64); virtual;
    function GetAsFloat: Double; virtual;
    procedure SetAsFloat(const AValue: Double); virtual;
    function GetAsBoolean: Boolean; virtual;
    procedure SetAsBoolean(const AValue: Boolean); virtual;
    function GetAsDateTime: TDateTime; virtual;
    procedure SetAsDateTime(const AValue: TDateTime); virtual;
  public
    property Value: Variant read GetValue write SetValue;
    property Visible: Boolean read FVisible write FVisible;

    ///to be used with ORM
    property FieldName: string read FFieldName write SetFieldName;
    property DBOptions: TDBOptionsSet read FDBOptions write FDBOptions;
    property ForeignKey: TForeignKey read GetForeignKey;
    property IsVirtual: Boolean read FIsVirtual write FIsVirtual;
    property IsRequired: Boolean read FIsRequired write FIsRequired;
    property FieldKind: TDeltaFieldKind read FFieldKind write FFieldKind;

    // Acesso tipado direto de alta performance
    property AsLargeInt: Int64 read GetAsLargeInt write SetAsLargeInt;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;

    procedure Clear; virtual; abstract;
    procedure AfterConstruction; override;
    function IsNull: Boolean; virtual; abstract;
    function AsString: string; virtual;
    function IsValid: Boolean; virtual; abstract;
    function SwaggerDataType: string; virtual; abstract;
    function SwaggerFormat: string; virtual;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TDeltaFieldClass = class of TDeltaField;

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
    constructor Create; override;
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
    constructor Create; override;
  end;

  { TDFIntNull }

  TDFIntNull = class(TDeltaFieldNullable)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    function AsString: string; override;
    function AsInteger: Integer;
    function SwaggerDataType: string; override;
  end;

  { TDFInt64Null }

  TDFInt64Null = class(TDeltaFieldNullable)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    function AsString: string; override;
    function AsInt64: Int64;
    function SwaggerDataType: string; override;
    function SwaggerFormat: string; override;
  end;

  { TDFDoubleNull }

  TDFDoubleNull = class(TDeltaFieldNullable)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    function AsString: string; override;
    function AsFloat: Double;
    function SwaggerDataType: string; override;
    function SwaggerFormat: string; override;
  end;

  TDFFloatNull = class(TDFDoubleNull);

  { TDFCurrencyNull }

  TDFCurrencyNull = class(TDeltaFieldNullable)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    function AsString: string; override;
    function AsCurrency: Currency;
    function SwaggerDataType: string; override;
  end;

  { TDFStringNull }

  TDFStringNull = class(TDeltaFieldNullable)
  private
    FSize: Integer;
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    property Size: Integer read FSize write FSize;
    procedure AfterConstruction; override;
  end;

  TDFTextNull = class(TDFStringNull);

  { TDFIntRequired }

  TDFIntRequired = class(TDeltaFieldRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    function AsString: string; override;
    function AsInteger: Integer;
    function SwaggerDataType: string; override;
  end;

  { TDFInt64Required }

  TDFInt64Required = class(TDeltaFieldRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    function AsString: string; override;
    function AsInt64: Int64;
    function SwaggerDataType: string; override;
    function SwaggerFormat: string; override;
  end;

  { TDFDoubleRequired }

  TDFDoubleRequired = class(TDeltaFieldRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    function AsString: string; override;
    function AsFloat: Double;
    function SwaggerDataType: string; override;
    function SwaggerFormat: string; override;
  end;

  TDFFloatRequired = class(TDFDoubleRequired);

  { TDFCurrencyRequired }

  TDFCurrencyRequired = class(TDeltaFieldRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    function AsString: string; override;
    function AsCurrency: Currency;
    function SwaggerDataType: string; override;
  end;

  { TDFStringRequired }

  TDFStringRequired = class(TDeltaFieldRequired)
  private
    FSize: Integer;
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    property Size: Integer read FSize write FSize;
    procedure AfterConstruction; override;
  end;

  TDFTextRequired = class(TDFStringRequired);

  { TDFDateNull }

  TDFDateNull = class(TDeltaFieldNullable)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    function AsString: string; override;
    function SwaggerDataType: string; override;
    function SwaggerFormat: string; override;
  end;

  { TDFTimeNull }

  TDFTimeNull = class(TDeltaFieldNullable)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    function AsString: string; override;
    function SwaggerDataType: string; override;
    function SwaggerFormat: string; override;
  end;

  { TDFDateTimeNull }

  TDFDateTimeNull = class(TDeltaFieldNullable)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    function AsString: string; override;
    function SwaggerDataType: string; override;
    function SwaggerFormat: string; override;
  end;

  { TDFDateRequired }

  TDFDateRequired = class(TDeltaFieldRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    function AsString: string; override;
    function SwaggerDataType: string; override;
    function SwaggerFormat: string; override;
  end;

  { TDFTimeRequired }

  TDFTimeRequired = class(TDeltaFieldRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    function AsString: string; override;
    function SwaggerDataType: string; override;
    function SwaggerFormat: string; override;
  end;

  { TDFDateTimeRequired }

  TDFDateTimeRequired = class(TDeltaFieldRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    function AsString: string; override;
    function SwaggerDataType: string; override;
    function SwaggerFormat: string; override;
  end;

  { TDFBooleanRequired }

  TDFBooleanRequired = class(TDeltaFieldRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    function AsBoolean: Boolean;
    function AsString: string; override;
    function SwaggerDataType: string; override;
  end;

  { TDFBooleanNull }

  TDFBooleanNull = class(TDeltaFieldNullable)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    function AsBoolean: Boolean;
    function AsString: string; override;
    function SwaggerDataType: string; override;
  end;

  { Convenient aliases }
  TDFBoolRequired = TDFBooleanRequired;
  TDFBoolNull = TDFBooleanNull;

  { TDFUUIDNull }

  TDFUUIDNull = class(TDFStringNull)
  public
    function SwaggerDataType: string; override;
    function SwaggerFormat: string; override;
    function IsValid: Boolean; override;
  end;

  { TDFUUIDRequired }

  TDFUUIDRequired = class(TDFStringRequired)
  public
    function SwaggerDataType: string; override;
    function SwaggerFormat: string; override;
    function IsValid: Boolean; override;
  end;

  { TDFForeignKey }

  TDFForeignKey = class(TDFInt64Null)
  public
    procedure References(ATable: TClass; const AField: string = 'id';
      AOnDelete: TForeignKeyEvent = fkRestrict; AOnUpdate: TForeignKeyEvent = fkCascade);
  end;

  { TDFHasMany }

  TDFHasMany = class(TDeltaField)
  private
    FRelationClass: TClass;
  protected
    function GetValue: Variant; override;
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    procedure References(ARelationClass: TClass);
    function IsNull: Boolean; override;
    function IsValid: Boolean; override;
    function SwaggerDataType: string; override;
    procedure Clear; override;
    property RelationClass: TClass read FRelationClass;
  end;

procedure DateTimeToField(AField: TDeltaField; const DateTime: string);
function IsValidUUID(const S: string): Boolean;

implementation

const
  DEFAULT_STR_SIZE = 255;
  UUID_LENGTH = 36;

var
  GlobalDeltaFS: TFormatSettings;

function IsValidUUID(const S: string): Boolean;
var
  I: Integer;
  C: Char;
begin
  Result := False;
  if Length(S) <> UUID_LENGTH then Exit;
  for I := 1 to UUID_LENGTH do
  begin
    C := S[I];
    case I of
      9, 14, 19, 24:
        if C <> '-' then Exit;
    else
      if not (C in ['0'..'9', 'a'..'f', 'A'..'F']) then Exit;
    end;
  end;
  Result := True;
end;

procedure DateTimeToField(AField: TDeltaField; const DateTime: string);
begin
  AField.Value := ISO8601ToDateDef(DateTime, 0);
end;

{ TForeignKey }

procedure TForeignKey.References(ATable: TClass; const AField: string;
  AOnDelete: TForeignKeyEvent; AOnUpdate: TForeignKeyEvent);
begin
  FReferencesTable := ATable;
  FReferencesField := AField;
  FOnDelete := AOnDelete;
  FOnUpdate := AOnUpdate;
end;

{ TDeltaField }

procedure TDeltaField.SetFieldName(AValue: string);
begin
  if FFieldName = AValue then Exit;
  FFieldName := AValue;
end;

function TDeltaField.GetForeignKey: TForeignKey;
begin
  if FForeignKey = nil then
    FForeignKey := TForeignKey.Create;
  Result := FForeignKey;
end;

procedure TDeltaField.AfterConstruction;
begin
  inherited AfterConstruction;
  FDBOptions := [dboInsert, dboUpdate];
end;

function TDeltaField.AsString: string;
begin
  Result := VarToStrDef(Self.Value, EmptyStr);
end;

function TDeltaField.SwaggerFormat: string;
begin
  Result := '';
end;

constructor TDeltaField.Create;
begin
  inherited Create;
  FValue := Null;
  FVisible := True;
  FIsVirtual := False;
  FIsRequired := False;
  FFieldKind := dfkString;
  FDBOptions := [dboInsert, dboUpdate];
end;

destructor TDeltaField.Destroy;
begin
  FForeignKey.Free;
  inherited Destroy;
end;

function TDeltaField.GetAsLargeInt: Int64;
begin
  if IsNull then Result := 0
  else Result := Int64(FValue);
end;

procedure TDeltaField.SetAsLargeInt(const AValue: Int64);
begin
  SetValue(AValue);
end;

function TDeltaField.GetAsFloat: Double;
begin
  if IsNull then Result := 0.0
  else Result := Double(FValue);
end;

procedure TDeltaField.SetAsFloat(const AValue: Double);
begin
  SetValue(AValue);
end;

function TDeltaField.GetAsBoolean: Boolean;
begin
  if IsNull then Result := False
  else Result := Boolean(FValue);
end;

procedure TDeltaField.SetAsBoolean(const AValue: Boolean);
begin
  SetValue(AValue);
end;

function TDeltaField.GetAsDateTime: TDateTime;
begin
  if IsNull then Result := 0
  else Result := VarToDateTime(FValue);
end;

procedure TDeltaField.SetAsDateTime(const AValue: TDateTime);
begin
  SetValue(AValue);
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
  FIsRequired := False;
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
  Result := 'string';
end;

constructor TDeltaFieldNullable.Create;
begin
  inherited Create;
  FIsRequired := False;
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
  inherited Create;
  FIsRequired := True;
  Self.AfterConstruction;
end;

{ TDFIntNull }

constructor TDFIntNull.Create;
begin
  inherited Create;
  FFieldKind := dfkInteger;
end;

procedure TDFIntNull.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
  if not VarIsNull(AValue) then
    Self.FValue := Integer(AValue);
end;

function TDFIntNull.AsString: string;
begin
  if IsNull then Result := ''
  else Result := IntToStr(Integer(FValue));
end;

function TDFIntNull.AsInteger: Integer;
begin
  if IsNull then
    Result := 0
  else
    Result := Integer(FValue);
end;

function TDFIntNull.SwaggerDataType: string;
begin
  Result := 'integer';
end;

{ TDFInt64Null }

constructor TDFInt64Null.Create;
begin
  inherited Create;
  FFieldKind := dfkInteger;
end;

procedure TDFInt64Null.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
  if not VarIsNull(AValue) then
    Self.FValue := Int64(AValue);
end;

function TDFInt64Null.AsString: string;
begin
  if IsNull then Result := ''
  else Result := IntToStr(Int64(FValue));
end;

function TDFInt64Null.AsInt64: Int64;
begin
  if IsNull then Result := 0
  else Result := Int64(FValue);
end;

function TDFInt64Null.SwaggerDataType: string;
begin
  Result := 'integer';
end;

function TDFInt64Null.SwaggerFormat: string;
begin
  Result := 'int64';
end;

{ TDFDoubleNull }

constructor TDFDoubleNull.Create;
begin
  inherited Create;
  FFieldKind := dfkFloat;
end;

procedure TDFDoubleNull.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
  if not VarIsNull(AValue) then
    Self.FValue := Double(AValue);
end;

function TDFDoubleNull.AsString: string;
begin
  if IsNull then Result := ''
  else Result := FloatToStr(Double(FValue), GlobalDeltaFS);
end;

function TDFDoubleNull.AsFloat: Double;
begin
  if IsNull then Result := 0
  else Result := Double(FValue);
end;

function TDFDoubleNull.SwaggerDataType: string;
begin
  Result := 'number';
end;

function TDFDoubleNull.SwaggerFormat: string;
begin
  Result := 'double';
end;

{ TDFCurrencyNull }

constructor TDFCurrencyNull.Create;
begin
  inherited Create;
  FFieldKind := dfkFloat;
end;

procedure TDFCurrencyNull.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
  if not VarIsNull(AValue) then
    Self.FValue := Currency(AValue);
end;

function TDFCurrencyNull.AsString: string;
begin
  if IsNull then Result := ''
  else Result := FloatToStr(Double(Currency(FValue)), GlobalDeltaFS);
end;

function TDFCurrencyNull.AsCurrency: Currency;
begin
  if IsNull then Result := 0
  else Result := Currency(FValue);
end;

function TDFCurrencyNull.SwaggerDataType: string;
begin
  Result := 'number';
end;

{ TDFStringNull }

constructor TDFStringNull.Create;
begin
  inherited Create;
  FFieldKind := dfkString;
  FSize := DEFAULT_STR_SIZE;
end;

procedure TDFStringNull.SetValue(AValue: Variant);
var
  TmpStr: string;
begin
  inherited SetValue(AValue);
  if not VarIsNull(AValue) then
  begin
    TmpStr := string(AValue);
    if (Self.FSize > 0) and (Length(TmpStr) > Self.FSize) then
      TmpStr := Copy(Trim(TmpStr), 1, Self.FSize);
    Self.FValue := TmpStr;
  end;
end;

procedure TDFStringNull.AfterConstruction;
begin
  inherited AfterConstruction;
  Size := DEFAULT_STR_SIZE;
end;

{ TDFIntRequired }

constructor TDFIntRequired.Create;
begin
  inherited Create;
  FFieldKind := dfkInteger;
end;

procedure TDFIntRequired.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
  if not VarIsNull(AValue) then
    Self.FValue := Integer(AValue);
end;

function TDFIntRequired.AsString: string;
begin
  if IsNull then Result := '0'
  else Result := IntToStr(Integer(FValue));
end;

function TDFIntRequired.AsInteger: Integer;
begin
  if IsNull then Result := 0
  else Result := Integer(FValue);
end;

function TDFIntRequired.SwaggerDataType: string;
begin
  Result := 'integer';
end;

{ TDFInt64Required }

constructor TDFInt64Required.Create;
begin
  inherited Create;
  FFieldKind := dfkInteger;
end;

procedure TDFInt64Required.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
  if not VarIsNull(AValue) then
    Self.FValue := Int64(AValue);
end;

function TDFInt64Required.AsString: string;
begin
  if IsNull then Result := '0'
  else Result := IntToStr(Int64(FValue));
end;

function TDFInt64Required.AsInt64: Int64;
begin
  if IsNull then Result := 0
  else Result := Int64(FValue);
end;

function TDFInt64Required.SwaggerDataType: string;
begin
  Result := 'integer';
end;

function TDFInt64Required.SwaggerFormat: string;
begin
  Result := 'int64';
end;

{ TDFDoubleRequired }

constructor TDFDoubleRequired.Create;
begin
  inherited Create;
  FFieldKind := dfkFloat;
end;

procedure TDFDoubleRequired.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
  if not VarIsNull(AValue) then
    Self.FValue := Double(AValue);
end;

function TDFDoubleRequired.AsString: string;
begin
  if IsNull then Result := '0'
  else Result := FloatToStr(Double(FValue), GlobalDeltaFS);
end;

function TDFDoubleRequired.AsFloat: Double;
begin
  if IsNull then Result := 0
  else Result := Double(FValue);
end;

function TDFDoubleRequired.SwaggerDataType: string;
begin
  Result := 'number';
end;

function TDFDoubleRequired.SwaggerFormat: string;
begin
  Result := 'double';
end;

{ TDFCurrencyRequired }

constructor TDFCurrencyRequired.Create;
begin
  inherited Create;
  FFieldKind := dfkFloat;
end;

procedure TDFCurrencyRequired.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
  if not VarIsNull(AValue) then
    Self.FValue := Currency(AValue);
end;

function TDFCurrencyRequired.AsString: string;
begin
  if IsNull then Result := '0'
  else Result := FloatToStr(Double(Currency(FValue)), GlobalDeltaFS);
end;

function TDFCurrencyRequired.AsCurrency: Currency;
begin
  if IsNull then Result := 0
  else Result := Currency(FValue);
end;

function TDFCurrencyRequired.SwaggerDataType: string;
begin
  Result := 'number';
end;

{ TDFStringRequired }

constructor TDFStringRequired.Create;
begin
  inherited Create;
  FFieldKind := dfkString;
  FSize := DEFAULT_STR_SIZE;
end;

procedure TDFStringRequired.SetValue(AValue: Variant);
var
  TmpStr: string;
begin
  inherited SetValue(AValue);
  if not VarIsNull(AValue) then
  begin
    TmpStr := string(AValue);
    if (Self.FSize > 0) and (Length(TmpStr) > Self.FSize) then
      TmpStr := Copy(Trim(TmpStr), 1, Self.FSize);
    Self.FValue := TmpStr;
  end;
end;

procedure TDFStringRequired.AfterConstruction;
begin
  inherited AfterConstruction;
  Size := DEFAULT_STR_SIZE;
end;

{ TDFDateNull }

constructor TDFDateNull.Create;
begin
  inherited Create;
  FFieldKind := dfkDateTime;
end;

procedure TDFDateNull.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
  if not VarIsNull(AValue) and (AValue = 0) then
    Self.Clear;
end;

function TDFDateNull.AsString: string;
begin
  if IsNull then Result := ''
  else Result := DateToISO8601(Self.Value);
end;

function TDFDateNull.SwaggerDataType: string;
begin
  Result := 'string';
end;

function TDFDateNull.SwaggerFormat: string;
begin
  Result := 'date';
end;

{ TDFTimeNull }

constructor TDFTimeNull.Create;
begin
  inherited Create;
  FFieldKind := dfkDateTime;
end;

procedure TDFTimeNull.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
  if not VarIsNull(AValue) and (AValue = 0) then
    Self.Clear;
end;

function TDFTimeNull.AsString: string;
begin
  if Self.IsNull then
    Exit('');
  Result := FormatDateTime('hh:nn:ss', Self.Value);
end;

function TDFTimeNull.SwaggerDataType: string;
begin
  Result := 'string';
end;

function TDFTimeNull.SwaggerFormat: string;
begin
  Result := 'time';
end;

{ TDFDateTimeNull }

constructor TDFDateTimeNull.Create;
begin
  inherited Create;
  FFieldKind := dfkDateTime;
end;

procedure TDFDateTimeNull.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
  if not VarIsNull(AValue) and (AValue = 0) then
    Self.Clear;
end;

function TDFDateTimeNull.AsString: string;
begin
  if IsNull then Result := ''
  else Result := DateToISO8601(Self.Value);
end;

function TDFDateTimeNull.SwaggerDataType: string;
begin
  Result := 'string';
end;

function TDFDateTimeNull.SwaggerFormat: string;
begin
  Result := 'date-time';
end;

{ TDFDateRequired }

constructor TDFDateRequired.Create;
begin
  inherited Create;
  FFieldKind := dfkDateTime;
end;

procedure TDFDateRequired.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
end;

function TDFDateRequired.AsString: string;
begin
  Result := DateToISO8601(Self.Value);
end;

function TDFDateRequired.SwaggerDataType: string;
begin
  Result := 'string';
end;

function TDFDateRequired.SwaggerFormat: string;
begin
  Result := 'date';
end;

{ TDFTimeRequired }

constructor TDFTimeRequired.Create;
begin
  inherited Create;
  FFieldKind := dfkDateTime;
end;

procedure TDFTimeRequired.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
end;

function TDFTimeRequired.AsString: string;
begin
  Result := FormatDateTime('hh:nn:ss', Self.Value);
end;

function TDFTimeRequired.SwaggerDataType: string;
begin
  Result := 'string';
end;

function TDFTimeRequired.SwaggerFormat: string;
begin
  Result := 'time';
end;

{ TDFDateTimeRequired }

constructor TDFDateTimeRequired.Create;
begin
  inherited Create;
  FFieldKind := dfkDateTime;
end;

procedure TDFDateTimeRequired.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
end;

function TDFDateTimeRequired.AsString: string;
begin
  Result := DateToISO8601(Self.Value);
end;

function TDFDateTimeRequired.SwaggerDataType: string;
begin
  Result := 'string';
end;

function TDFDateTimeRequired.SwaggerFormat: string;
begin
  Result := 'date-time';
end;

{ TDFBooleanRequired }

constructor TDFBooleanRequired.Create;
begin
  inherited Create;
  FFieldKind := dfkBoolean;
end;

procedure TDFBooleanRequired.SetValue(AValue: Variant);
var
  S: string;
begin
  if VarIsNull(AValue) then
    inherited SetValue(AValue)
  else if VarIsNumeric(AValue) then
    inherited SetValue(AValue <> 0)
  else if VarIsStr(AValue) then
  begin
    S := UpperCase(Trim(string(AValue)));
    inherited SetValue((S = 'TRUE') or (S = 'T') or (S = '1') or (S = 'Y') or (S = 'S'));
  end
  else
    inherited SetValue(Boolean(AValue));
end;

function TDFBooleanRequired.AsBoolean: Boolean;
begin
  if IsNull then Result := False
  else Result := Boolean(FValue);
end;

function TDFBooleanRequired.AsString: string;
begin
  if AsBoolean then Result := 'T'
  else Result := 'F';
end;

function TDFBooleanRequired.SwaggerDataType: string;
begin
  Result := 'boolean';
end;

{ TDFBooleanNull }

constructor TDFBooleanNull.Create;
begin
  inherited Create;
  FFieldKind := dfkBoolean;
end;

procedure TDFBooleanNull.SetValue(AValue: Variant);
var
  S: string;
begin
  if VarIsNull(AValue) then
    inherited SetValue(AValue)
  else if VarIsNumeric(AValue) then
    inherited SetValue(AValue <> 0)
  else if VarIsStr(AValue) then
  begin
    S := UpperCase(Trim(string(AValue)));
    inherited SetValue((S = 'TRUE') or (S = 'T') or (S = '1') or (S = 'Y') or (S = 'S'));
  end
  else
    inherited SetValue(Boolean(AValue));
end;

function TDFBooleanNull.AsBoolean: Boolean;
begin
  if IsNull then Result := False
  else Result := Boolean(FValue);
end;

function TDFBooleanNull.AsString: string;
begin
  if IsNull then Result := ''
  else if AsBoolean then Result := 'T'
  else Result := 'F';
end;

function TDFBooleanNull.SwaggerDataType: string;
begin
  Result := 'boolean';
end;

{ TDFUUIDNull }

function TDFUUIDNull.SwaggerDataType: string;
begin
  Result := 'string';
end;

function TDFUUIDNull.SwaggerFormat: string;
begin
  Result := 'uuid';
end;

function TDFUUIDNull.IsValid: Boolean;
begin
  if IsNull then
    Result := True
  else
    Result := IsValidUUID(AsString);
end;

{ TDFUUIDRequired }

function TDFUUIDRequired.SwaggerDataType: string;
begin
  Result := 'string';
end;

function TDFUUIDRequired.SwaggerFormat: string;
begin
  Result := 'uuid';
end;

function TDFUUIDRequired.IsValid: Boolean;
begin
  Result := (not IsNull) and IsValidUUID(AsString);
end;

{ TDFForeignKey }

procedure TDFForeignKey.References(ATable: TClass; const AField: string;
  AOnDelete: TForeignKeyEvent; AOnUpdate: TForeignKeyEvent);
begin
  ForeignKey.References(ATable, AField, AOnDelete, AOnUpdate);
end;

{ TDFHasMany }

constructor TDFHasMany.Create;
begin
  inherited Create;
  FFieldKind := dfkVirtual;
  FIsVirtual := True;
end;

procedure TDFHasMany.References(ARelationClass: TClass);
begin
  FRelationClass := ARelationClass;
end;

function TDFHasMany.GetValue: Variant;
begin
  Result := Null;
end;

procedure TDFHasMany.SetValue(AValue: Variant);
begin
  // Campos virtuais não gravam valor escalar
end;

function TDFHasMany.IsNull: Boolean;
begin
  Result := True;
end;

function TDFHasMany.IsValid: Boolean;
begin
  Result := True;
end;

function TDFHasMany.SwaggerDataType: string;
begin
  Result := 'array';
end;

procedure TDFHasMany.Clear;
begin
end;

initialization
  GlobalDeltaFS := DefaultFormatSettings;
  GlobalDeltaFS.DecimalSeparator := '.';
  GlobalDeltaFS.ThousandSeparator := ',';

end.
