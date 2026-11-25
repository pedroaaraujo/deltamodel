unit DeltaModel.Fields;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Variants, Math, fgl, DeltaModelMessages;

type
  TDBOption = (dboPrimaryKey, dboUpdate, dboAutoInc, dboInsert, dboUnique, dboIndex);
  TDBOptions = set of TDBOption;
  TDBOptionsSet = TDBOptions;

const
  { Marcas que o schema fluente pode acrescentar a um TDF* com opções próprias.
    dboInsert/dboUpdate ficam de fora: são permissões decididas pelo campo. }
  DELTA_STRUCTURAL_DB_OPTIONS: TDBOptionsSet = [dboPrimaryKey, dboAutoInc, dboUnique, dboIndex];

type

  TForeignKeyEvent = (fkNone, fkCascade, fkSetNull, fkRestrict);
  TFKOption = TForeignKeyEvent;

  TDeltaFieldKind = (dfkInteger, dfkFloat, dfkString, dfkBoolean, dfkDateTime, dfkVirtual);

  TDeltaField = class;
  TDeltaSchemaField = class;

  TDeltaOperator = (
    doEq,
    doNotEq,
    doGt,
    doGte,
    doLt,
    doLte,
    doLike,
    doBetween,
    doIn,
    doIsNull,
    doIsNotNull
  );

  TDeltaOrderDirection = (
    odAsc,
    odDesc
  );

  { TDeltaCondition - Stack-allocated record for Zero Heap Churn }
  TDeltaCondition = record
    Field: TDeltaField;
    &Operator: TDeltaOperator;
    Value: Variant;
    SecondaryValue: Variant;
    Values: array of Variant;
  end;

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
    FSchemaField: TDeltaSchemaField;
    FDBOptions: TDBOptionsSet;
    FFieldName: string;
    FForeignKey: TForeignKey;
    FVisible: Boolean;
    FIsVirtual: Boolean;
    FIsRequired: Boolean;
    FDirty: Boolean;
    FFieldKind: TDeltaFieldKind;
    function GetFieldName: string;
    procedure SetFieldName(AValue: string);
    function GetIsRequired: Boolean;
    procedure SetIsRequired(AValue: Boolean);
    function GetDBOptions: TDBOptionsSet;
    procedure SetDBOptions(AValue: TDBOptionsSet);
    function GetForeignKey: TForeignKey;
    function GetIsUnique: Boolean;
    procedure SetIsUnique(AValue: Boolean);
    function GetIsIndexed: Boolean;
    procedure SetIsIndexed(AValue: Boolean);
  protected
    FValue: Variant;
    function GetValue: Variant; virtual; abstract;
    procedure SetValue(AValue: Variant); virtual; abstract;

    function GetAsInteger: Integer; virtual;
    procedure SetAsInteger(const AValue: Integer); virtual;
    function GetAsLargeInt: Int64; virtual;
    procedure SetAsLargeInt(const AValue: Int64); virtual;
    function GetAsFloat: Double; virtual;
    procedure SetAsFloat(const AValue: Double); virtual;
    function GetAsCurrency: Currency; virtual;
    procedure SetAsCurrency(const AValue: Currency); virtual;
    function GetAsBoolean: Boolean; virtual;
    procedure SetAsBoolean(const AValue: Boolean); virtual;
    function GetAsDateTime: TDateTime; virtual;
    procedure SetAsDateTime(const AValue: TDateTime); virtual;
  public
    property Value: Variant read GetValue write SetValue;
    property Visible: Boolean read FVisible write FVisible;

    ///to be used with ORM
    property SchemaField: TDeltaSchemaField read FSchemaField write FSchemaField;
    property FieldName: string read GetFieldName write SetFieldName;
    property DBOptions: TDBOptionsSet read GetDBOptions write SetDBOptions;
    property ForeignKey: TForeignKey read GetForeignKey;
    property IsVirtual: Boolean read FIsVirtual write FIsVirtual;
    property IsRequired: Boolean read GetIsRequired write SetIsRequired;
    property FieldKind: TDeltaFieldKind read FFieldKind write FFieldKind;
    property IsUnique: Boolean read GetIsUnique write SetIsUnique;
    property IsIndexed: Boolean read GetIsIndexed write SetIsIndexed;
    property Dirty: Boolean read FDirty write FDirty;

    procedure Clear; virtual; abstract;
    procedure AfterConstruction; override;
    function IsNull: Boolean; virtual; abstract;
    function AsString: string; virtual;
    procedure SetAsString(const AValue: string); virtual;
    function IsValid: Boolean; virtual; abstract;
    function SwaggerDataType: string; virtual; abstract;
    function SwaggerFormat: string; virtual;

    // High-performance direct typed access
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsLargeInt: Int64 read GetAsLargeInt write SetAsLargeInt;
    property AsInt64: Int64 read GetAsLargeInt write SetAsLargeInt;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsText: string read AsString write SetAsString;

    // Fluent type-safe expression methods (Zero Heap Churn)
    function Eq(const AValue: Variant): TDeltaCondition;
    function NotEq(const AValue: Variant): TDeltaCondition;
    function Gt(const AValue: Variant): TDeltaCondition;
    function Gte(const AValue: Variant): TDeltaCondition;
    function Lt(const AValue: Variant): TDeltaCondition;
    function Lte(const AValue: Variant): TDeltaCondition;
    function Like(const AValue: Variant): TDeltaCondition;
    function Between(const AVal1, AVal2: Variant): TDeltaCondition;
    function InList(const AValues: array of Variant): TDeltaCondition; overload;
    function InList(const AValues: array of string): TDeltaCondition; overload;
    function InList(const AValues: array of Int64): TDeltaCondition; overload;
    function IsDbNull: TDeltaCondition;
    function IsNotDbNull: TDeltaCondition;

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
    function SwaggerDataType: string; override;
  end;

  { TDFInt64Null }

  TDFInt64Null = class(TDeltaFieldNullable)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    function AsString: string; override;
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
    function SwaggerDataType: string; override;
  end;

  { TDFStringNull }

  TDFStringNull = class(TDeltaFieldNullable)
  private
    FSize: Integer;
    function GetSize: Integer;
  protected
    procedure SetValue(AValue: Variant); override;
  public
    procedure SetAsString(const AValue: string); override;
    constructor Create; override;
    property Size: Integer read GetSize write FSize;
    procedure AfterConstruction; override;
  end;

  { TDFTextNull }

  TDFTextNull = class(TDFStringNull)
  public
    constructor Create; override;
    procedure AfterConstruction; override;
  end;

  { TDFIntRequired }

  TDFIntRequired = class(TDeltaFieldRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    function AsString: string; override;
    function SwaggerDataType: string; override;
  end;

  { TDFInt64Required }

  TDFInt64Required = class(TDeltaFieldRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    function AsString: string; override;
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
    function SwaggerDataType: string; override;
  end;

  { TDFStringRequired }

  TDFStringRequired = class(TDeltaFieldRequired)
  private
    FSize: Integer;
    function GetSize: Integer;
  protected
    procedure SetValue(AValue: Variant); override;
  public
    procedure SetAsString(const AValue: string); override;
    constructor Create; override;
    property Size: Integer read GetSize write FSize;
    procedure AfterConstruction; override;
  end;

  { TDFTextRequired }

  TDFTextRequired = class(TDFStringRequired)
  public
    constructor Create; override;
    procedure AfterConstruction; override;
  end;

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
    function AsString: string; override;
    function SwaggerDataType: string; override;
  end;

  { TDFBooleanNull }

  TDFBooleanNull = class(TDeltaFieldNullable)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
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

  TDFHasManyList = specialize TFPGObjectList<TObject>;

  { TDFHasMany }

  TDFHasMany = class(TDeltaField)
  private
    FRelationClass: TClass;
    FForeignKeyField: string;
    FReferencesField: string;
    FItems: TDFHasManyList;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TObject;
  protected
    function GetValue: Variant; override;
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure References(ARelationClass: TClass; const AForeignKey: string = ''; const AReferencesField: string = 'id');
    procedure Add(AItem: TObject);
    procedure Clear; override;
    function IsNull: Boolean; override;
    function IsValid: Boolean; override;
    function SwaggerDataType: string; override;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TObject read GetItem; default;
    property List: TDFHasManyList read FItems;
    property RelationClass: TClass read FRelationClass;
    property ForeignKeyField: string read FForeignKeyField;
    property ReferencesField: string read FReferencesField;
  end;

  { TDFHasOne }

  TDFHasOne = class(TDeltaField)
  private
    FRelationClass: TClass;
    FForeignKeyField: string;
    FReferencesField: string;
    FModel: TObject;
    FOwnsModel: Boolean;
    function GetModel: TObject;
    procedure SetModel(AValue: TObject);
  protected
    function GetValue: Variant; override;
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure References(ARelationClass: TClass; const AForeignKey: string = ''; const AReferencesField: string = 'id');
    function IsNull: Boolean; override;
    function IsValid: Boolean; override;
    function SwaggerDataType: string; override;
    procedure Clear; override;
    property RelationClass: TClass read FRelationClass;
    property ForeignKeyField: string read FForeignKeyField write FForeignKeyField;
    property ReferencesField: string read FReferencesField write FReferencesField;
    property Model: TObject read GetModel write SetModel;
    property OwnsModel: Boolean read FOwnsModel write FOwnsModel;
  end;

procedure DateTimeToField(AField: TDeltaField; const DateTime: string);
function IsValidUUID(const S: string): Boolean;

type
  { TDeltaSchemaField: Fluent metadata for primitive properties }
  TDeltaSchemaField = class
  private
    FPropName: string;
    FFieldName: string;
    FDBOptions: TDBOptionsSet;
    FIsRequired: Boolean;
    FFieldSize: Integer;
    FFieldKind: TDeltaFieldKind;
    FDefaultValue: Variant;
    // Declarative validations
    FHasMin: Boolean;
    FHasMax: Boolean;
    FMin: Double;
    FMax: Double;
    FHasMinLength: Boolean;
    FHasMaxLength: Boolean;
    FMinLength: Integer;
    FMaxLength: Integer;
    FIsPositive: Boolean;
    FIsNotEmpty: Boolean;
    FIsEmail: Boolean;
    FIsCPF: Boolean;
    FIsCNPJ: Boolean;
    FIsCPF_CNPJ: Boolean;
    FIsURL: Boolean;
    FIsPhone: Boolean;
    FIsCEP: Boolean;
  public
    constructor Create(const APropName: string);
    function Column(const AName: string): TDeltaSchemaField;
    function AsPK: TDeltaSchemaField;
    function AutoInc: TDeltaSchemaField;
    function Required: TDeltaSchemaField; overload;
    function Required(ARequired: Boolean): TDeltaSchemaField; overload;
    function Nullable: TDeltaSchemaField;
    function Size(ASize: Integer): TDeltaSchemaField;
    function Default(AValue: Variant): TDeltaSchemaField;
    function ReadOnly: TDeltaSchemaField;

    // Fluent declarative validation methods
    function Min(AVal: Double): TDeltaSchemaField;
    function Max(AVal: Double): TDeltaSchemaField;
    function Range(AMin, AMax: Double): TDeltaSchemaField;
    function MinLength(ALen: Integer): TDeltaSchemaField;
    function MaxLength(ALen: Integer): TDeltaSchemaField;
    function LengthRange(AMin, AMax: Integer): TDeltaSchemaField;
    function Positive: TDeltaSchemaField;
    function NotEmpty: TDeltaSchemaField;
    function Email: TDeltaSchemaField;
    function CPF: TDeltaSchemaField;
    function CNPJ: TDeltaSchemaField;
    function CPF_CNPJ: TDeltaSchemaField;
    function URL: TDeltaSchemaField;
    function Phone: TDeltaSchemaField;
    function Telefone: TDeltaSchemaField;
    function CEP: TDeltaSchemaField;

    property PropName: string read FPropName write FPropName;
    property FieldName: string read FFieldName write FFieldName;
    property DBOptions: TDBOptionsSet read FDBOptions write FDBOptions;
    property IsRequired: Boolean read FIsRequired write FIsRequired;
    property FieldSize: Integer read FFieldSize write FFieldSize;
    property FieldKind: TDeltaFieldKind read FFieldKind write FFieldKind;

    property HasMin: Boolean read FHasMin;
    property MinValue: Double read FMin;
    property HasMax: Boolean read FHasMax;
    property MaxValue: Double read FMax;
    property HasMinLength: Boolean read FHasMinLength;
    property MinLengthValue: Integer read FMinLength;
    property HasMaxLength: Boolean read FHasMaxLength;
    property MaxLengthValue: Integer read FMaxLength;
    property IsPositive: Boolean read FIsPositive;
    property IsNotEmpty: Boolean read FIsNotEmpty;
    property IsEmail: Boolean read FIsEmail;
    property IsCPF: Boolean read FIsCPF;
    property IsCNPJ: Boolean read FIsCNPJ;
    property IsCPF_CNPJ: Boolean read FIsCPF_CNPJ;
    property IsURL: Boolean read FIsURL;
    property IsPhone: Boolean read FIsPhone;
    property IsCEP: Boolean read FIsCEP;
  end;

  TDeltaSchemaFieldList = specialize TFPGObjectList<TDeltaSchemaField>;

  { TDeltaModelRelation: Relationship metadata between models }
  TDeltaRelationKind = (drkHasMany, drkBelongsTo, drkHasOne);

  TDeltaModelRelation = class
  private
    FPropName: string;
    FRelationKind: TDeltaRelationKind;
    FRelatedClass: TClass;
    FForeignKeyField: string;
  public
    constructor Create(const APropName: string; AKind: TDeltaRelationKind; ARelatedClass: TClass; const AFKField: string = '');
    function ForeignKey(const AFK: string): TDeltaModelRelation;
    property PropName: string read FPropName;
    property RelationKind: TDeltaRelationKind read FRelationKind;
    property RelatedClass: TClass read FRelatedClass;
    property ForeignKeyField: string read FForeignKeyField;
  end;

  TDeltaModelRelationList = specialize TFPGObjectList<TDeltaModelRelation>;

  { TDeltaModelSchema: Fluent class data schema builder }
  TDeltaModelSchema = class
  private
    FTableName: string;
    FFields: TDeltaSchemaFieldList;
    FRelations: TDeltaModelRelationList;
    function GetField(Index: Integer): TDeltaSchemaField;
    function GetFieldCount: Integer;
    function GetRelation(Index: Integer): TDeltaModelRelation;
    function GetRelationCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Table(const AName: string): TDeltaModelSchema;
    function Field(const APropName: string): TDeltaSchemaField;
    function FindField(const APropName: string): TDeltaSchemaField;

    function HasMany(const APropName: string; AChildClass: TClass; const AFKField: string = ''): TDeltaModelRelation;
    function BelongsTo(const APropName: string; AParentClass: TClass; const AFKField: string = ''): TDeltaModelRelation;
    function FindRelation(const APropName: string): TDeltaModelRelation;

    property TableName: string read FTableName;
    property Fields[Index: Integer]: TDeltaSchemaField read GetField; default;
    property FieldCount: Integer read GetFieldCount;
    property Relations[Index: Integer]: TDeltaModelRelation read GetRelation;
    property RelationCount: Integer read GetRelationCount;
  end;

{ Formats a Double in pure fixed-point decimal notation without scientific notation }
function DeltaFormatFloatFixed(const AValue: Double; AMaxDecimals: Integer = 8): string;

{ Formats a Currency using exact 64-bit scaled integer arithmetic without scientific notation }
function DeltaFormatCurrencyFixed(const AValue: Currency): string;

implementation

{ TDeltaSchemaField }

constructor TDeltaSchemaField.Create(const APropName: string);
begin
  inherited Create;
  FPropName := APropName;
  FFieldName := LowerCase(APropName);
  FDBOptions := [dboInsert, dboUpdate];
  FIsRequired := False;
  FFieldSize := 0;
  FFieldKind := dfkString;
  FDefaultValue := Null;
  FHasMin := False;
  FHasMax := False;
  FMin := 0;
  FMax := 0;
  FHasMinLength := False;
  FHasMaxLength := False;
  FMinLength := 0;
  FMaxLength := 0;
  FIsPositive := False;
  FIsNotEmpty := False;
  FIsEmail := False;
  FIsCPF := False;
  FIsCNPJ := False;
  FIsCPF_CNPJ := False;
  FIsURL := False;
  FIsPhone := False;
  FIsCEP := False;
end;

function TDeltaSchemaField.Column(const AName: string): TDeltaSchemaField;
begin
  FFieldName := AName;
  Result := Self;
end;

function TDeltaSchemaField.AsPK: TDeltaSchemaField;
begin
  Include(FDBOptions, dboPrimaryKey);
  FIsRequired := True;
  Result := Self;
end;

function TDeltaSchemaField.AutoInc: TDeltaSchemaField;
begin
  Include(FDBOptions, dboAutoInc);
  Include(FDBOptions, dboPrimaryKey);
  Result := Self;
end;

function TDeltaSchemaField.Required: TDeltaSchemaField;
begin
  FIsRequired := True;
  Result := Self;
end;

function TDeltaSchemaField.Required(ARequired: Boolean): TDeltaSchemaField;
begin
  FIsRequired := ARequired;
  Result := Self;
end;

function TDeltaSchemaField.Nullable: TDeltaSchemaField;
begin
  FIsRequired := False;
  Result := Self;
end;

function TDeltaSchemaField.Size(ASize: Integer): TDeltaSchemaField;
begin
  FFieldSize := ASize;
  Result := Self;
end;

function TDeltaSchemaField.Default(AValue: Variant): TDeltaSchemaField;
begin
  FDefaultValue := AValue;
  Result := Self;
end;

function TDeltaSchemaField.ReadOnly: TDeltaSchemaField;
begin
  Exclude(FDBOptions, dboInsert);
  Exclude(FDBOptions, dboUpdate);
  Result := Self;
end;

function TDeltaSchemaField.Min(AVal: Double): TDeltaSchemaField;
begin
  FHasMin := True;
  FMin := AVal;
  Result := Self;
end;

function TDeltaSchemaField.Max(AVal: Double): TDeltaSchemaField;
begin
  FHasMax := True;
  FMax := AVal;
  Result := Self;
end;

function TDeltaSchemaField.Range(AMin, AMax: Double): TDeltaSchemaField;
begin
  FHasMin := True;
  FMin := AMin;
  FHasMax := True;
  FMax := AMax;
  Result := Self;
end;

function TDeltaSchemaField.MinLength(ALen: Integer): TDeltaSchemaField;
begin
  FHasMinLength := True;
  FMinLength := ALen;
  Result := Self;
end;

function TDeltaSchemaField.MaxLength(ALen: Integer): TDeltaSchemaField;
begin
  FHasMaxLength := True;
  FMaxLength := ALen;
  Result := Self;
end;

function TDeltaSchemaField.LengthRange(AMin, AMax: Integer): TDeltaSchemaField;
begin
  FHasMinLength := True;
  FMinLength := AMin;
  FHasMaxLength := True;
  FMaxLength := AMax;
  Result := Self;
end;

function TDeltaSchemaField.Positive: TDeltaSchemaField;
begin
  FIsPositive := True;
  Result := Self;
end;

function TDeltaSchemaField.NotEmpty: TDeltaSchemaField;
begin
  FIsNotEmpty := True;
  Result := Self;
end;

function TDeltaSchemaField.Email: TDeltaSchemaField;
begin
  FIsEmail := True;
  Result := Self;
end;

function TDeltaSchemaField.CPF: TDeltaSchemaField;
begin
  FIsCPF := True;
  Result := Self;
end;

function TDeltaSchemaField.CNPJ: TDeltaSchemaField;
begin
  FIsCNPJ := True;
  Result := Self;
end;

function TDeltaSchemaField.CPF_CNPJ: TDeltaSchemaField;
begin
  FIsCPF_CNPJ := True;
  Result := Self;
end;

function TDeltaSchemaField.URL: TDeltaSchemaField;
begin
  FIsURL := True;
  Result := Self;
end;

function TDeltaSchemaField.Phone: TDeltaSchemaField;
begin
  FIsPhone := True;
  Result := Self;
end;

function TDeltaSchemaField.Telefone: TDeltaSchemaField;
begin
  FIsPhone := True;
  Result := Self;
end;

function TDeltaSchemaField.CEP: TDeltaSchemaField;
begin
  FIsCEP := True;
  Result := Self;
end;

{ TDeltaModelRelation }

constructor TDeltaModelRelation.Create(const APropName: string; AKind: TDeltaRelationKind; ARelatedClass: TClass; const AFKField: string);
begin
  inherited Create;
  FPropName := APropName;
  FRelationKind := AKind;
  FRelatedClass := ARelatedClass;
  FForeignKeyField := AFKField;
end;

function TDeltaModelRelation.ForeignKey(const AFK: string): TDeltaModelRelation;
begin
  FForeignKeyField := AFK;
  Result := Self;
end;

{ TDeltaModelSchema }

constructor TDeltaModelSchema.Create;
begin
  inherited Create;
  FFields := TDeltaSchemaFieldList.Create(True);
  FRelations := TDeltaModelRelationList.Create(True);
end;

destructor TDeltaModelSchema.Destroy;
begin
  FRelations.Free;
  FFields.Free;
  inherited Destroy;
end;

function TDeltaModelSchema.GetField(Index: Integer): TDeltaSchemaField;
begin
  Result := FFields[Index];
end;

function TDeltaModelSchema.GetFieldCount: Integer;
begin
  Result := FFields.Count;
end;

function TDeltaModelSchema.GetRelation(Index: Integer): TDeltaModelRelation;
begin
  Result := FRelations[Index];
end;

function TDeltaModelSchema.GetRelationCount: Integer;
begin
  Result := FRelations.Count;
end;

function TDeltaModelSchema.Table(const AName: string): TDeltaModelSchema;
begin
  FTableName := AName;
  Result := Self;
end;

function TDeltaModelSchema.Field(const APropName: string): TDeltaSchemaField;
begin
  Result := FindField(APropName);
  if Result = nil then
  begin
    Result := TDeltaSchemaField.Create(APropName);
    FFields.Add(Result);
  end;
end;

function TDeltaModelSchema.FindField(const APropName: string): TDeltaSchemaField;
var
  I: Integer;
begin
  for I := 0 to FFields.Count - 1 do
    if SameText(FFields[I].PropName, APropName) or SameText(FFields[I].FieldName, APropName) then
      Exit(FFields[I]);
  Result := nil;
end;

function TDeltaModelSchema.HasMany(const APropName: string; AChildClass: TClass; const AFKField: string): TDeltaModelRelation;
begin
  Result := FindRelation(APropName);
  if Result = nil then
  begin
    Result := TDeltaModelRelation.Create(APropName, drkHasMany, AChildClass, AFKField);
    FRelations.Add(Result);
  end;
end;

function TDeltaModelSchema.BelongsTo(const APropName: string; AParentClass: TClass; const AFKField: string): TDeltaModelRelation;
begin
  Result := FindRelation(APropName);
  if Result = nil then
  begin
    Result := TDeltaModelRelation.Create(APropName, drkBelongsTo, AParentClass, AFKField);
    FRelations.Add(Result);
  end;
end;

function TDeltaModelSchema.FindRelation(const APropName: string): TDeltaModelRelation;
var
  I: Integer;
begin
  for I := 0 to FRelations.Count - 1 do
    if SameText(FRelations[I].PropName, APropName) then
      Exit(FRelations[I]);
  Result := nil;
end;

const
  DEFAULT_STR_SIZE = 255;
  UUID_LENGTH = 36;

var
  GlobalDeltaFS: TFormatSettings;

function ExpandScientificToFixed(const AStr: string): string;
var
  EPos, DotPos, ExpVal, Shift, I: Integer;
  Neg: Boolean;
  Mantissa, Digits: string;
begin
  EPos := Pos('E', AStr);
  if EPos = 0 then
    EPos := Pos('e', AStr);
  if EPos = 0 then
    Exit(AStr);

  Mantissa := Copy(AStr, 1, EPos - 1);
  if not TryStrToInt(Copy(AStr, EPos + 1, MaxInt), ExpVal) then
    Exit(AStr);

  Neg := (Length(Mantissa) > 0) and (Mantissa[1] = '-');
  if Neg or ((Length(Mantissa) > 0) and (Mantissa[1] = '+')) then
    Delete(Mantissa, 1, 1);

  DotPos := Pos('.', Mantissa);
  if DotPos > 0 then
  begin
    Digits := Copy(Mantissa, 1, DotPos - 1) + Copy(Mantissa, DotPos + 1, MaxInt);
    Shift := (DotPos - 1) + ExpVal;
  end
  else
  begin
    Digits := Mantissa;
    Shift := Length(Digits) + ExpVal;
  end;

  if Shift <= 0 then
  begin
    Result := '0.' + StringOfChar('0', -Shift) + Digits;
  end
  else if Shift >= Length(Digits) then
  begin
    Result := Digits + StringOfChar('0', Shift - Length(Digits));
  end
  else
  begin
    Result := Copy(Digits, 1, Shift) + '.' + Copy(Digits, Shift + 1, MaxInt);
  end;

  { Strip leading redundant zeros in integer part }
  while (Length(Result) > 1) and (Result[1] = '0') and (Result[2] <> '.') do
    Delete(Result, 1, 1);

  { Strip trailing zeros after decimal point }
  if Pos('.', Result) > 0 then
  begin
    I := Length(Result);
    while (I > 0) and (Result[I] = '0') do
      Dec(I);
    if (I > 0) and (Result[I] = '.') then
      Dec(I);
    SetLength(Result, I);
  end;

  if (Result = '') or (Result = '0') then
    Exit('0');

  if Neg then
    Result := '-' + Result;
end;

function DeltaFormatCurrencyFixed(const AValue: Currency): string;
var
  Scaled: Int64;
  AbsVal, IntPart: QWord;
  FracPart: Word;
  Neg: Boolean;
  FracBuf: array[1..4] of Char;
  FracLen: Integer;
begin
  Move(AValue, Scaled, SizeOf(Int64));
  if Scaled = 0 then
    Exit('0');

  Neg := Scaled < 0;
  if Neg then
    AbsVal := QWord(not Scaled) + 1
  else
    AbsVal := QWord(Scaled);

  IntPart := AbsVal div 10000;
  FracPart := Word(AbsVal mod 10000);

  if FracPart = 0 then
  begin
    if Neg then
      Result := '-' + IntToStr(IntPart)
    else
      Result := IntToStr(IntPart);
    Exit;
  end;

  FracBuf[1] := Chr(Ord('0') + (FracPart div 1000));
  FracBuf[2] := Chr(Ord('0') + ((FracPart div 100) mod 10));
  FracBuf[3] := Chr(Ord('0') + ((FracPart div 10) mod 10));
  FracBuf[4] := Chr(Ord('0') + (FracPart mod 10));

  FracLen := 4;
  while (FracLen > 0) and (FracBuf[FracLen] = '0') do
    Dec(FracLen);

  if Neg then
    Result := '-' + IntToStr(IntPart) + '.' + Copy(FracBuf, 1, FracLen)
  else
    Result := IntToStr(IntPart) + '.' + Copy(FracBuf, 1, FracLen);
end;

function DeltaFormatFloatFixed(const AValue: Double; AMaxDecimals: Integer = 8): string;
var
  S: string;
  I, EffDecimals, IntDigits: Integer;
  AbsVal: Double;
begin
  if IsNan(AValue) or IsInfinite(AValue) then
    Exit('0');
  if AValue = 0.0 then
    Exit('0');

  if AMaxDecimals < 0 then AMaxDecimals := 2;
  if AMaxDecimals > 18 then AMaxDecimals := 18;

  EffDecimals := AMaxDecimals;
  AbsVal := Abs(AValue);
  if AbsVal >= 1.0 then
  begin
    { Clamp fractional digits to IEEE 754 binary64 reliable 15 significant digits }
    IntDigits := Trunc(Log10(AbsVal)) + 1;
    if IntDigits >= 15 then
      EffDecimals := 0
    else if (15 - IntDigits) < EffDecimals then
      EffDecimals := 15 - IntDigits;
  end;

  S := FloatToStrF(AValue, ffFixed, 15, EffDecimals, GlobalDeltaFS);

  { Guarantee pure fixed-point output even if RTL falls back to scientific notation }
  if (Pos('E', S) > 0) or (Pos('e', S) > 0) then
    S := ExpandScientificToFixed(S);

  { Strip superfluous trailing zeros after the decimal separator }
  if Pos('.', S) > 0 then
  begin
    I := Length(S);
    while (I > 0) and (S[I] = '0') do
      Dec(I);
    if (I > 0) and (S[I] = '.') then
      Dec(I);
    SetLength(S, I);
  end;

  if (S = '') or (S = '-0') then
    S := '0';

  Result := S;
end;

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

function TDeltaField.GetFieldName: string;
begin
  if (FFieldName = '') and (FSchemaField <> nil) then
    Result := FSchemaField.FieldName
  else
    Result := FFieldName;
end;

procedure TDeltaField.SetFieldName(AValue: string);
begin
  if FFieldName = AValue then Exit;
  FFieldName := AValue;
  if FSchemaField <> nil then
    FSchemaField.Column(AValue);
end;

function TDeltaField.GetIsRequired: Boolean;
begin
  if FSchemaField <> nil then
    Result := FSchemaField.IsRequired
  else
    Result := FIsRequired;
end;

procedure TDeltaField.SetIsRequired(AValue: Boolean);
begin
  FIsRequired := AValue;
  if FSchemaField <> nil then
    FSchemaField.Required(AValue);
end;

function TDeltaField.GetDBOptions: TDBOptionsSet;
begin
  { Sem opções próprias, vale o schema fluente. Com opções próprias, o campo
    decide as permissões (dboInsert/dboUpdate) e o schema só acrescenta marcas
    estruturais (ex.: Field('id').AsPK.AutoInc), como em ExtractMetadata. }
  Result := FDBOptions;
  if FSchemaField <> nil then
  begin
    if Result = [] then
      Result := FSchemaField.DBOptions
    else
      Result := Result + (FSchemaField.DBOptions * DELTA_STRUCTURAL_DB_OPTIONS);
  end;
end;

procedure TDeltaField.SetDBOptions(AValue: TDBOptionsSet);
begin
  FDBOptions := AValue;
  if FSchemaField <> nil then
    FSchemaField.DBOptions := AValue;
end;

function TDeltaField.GetForeignKey: TForeignKey;
begin
  if FForeignKey = nil then
    FForeignKey := TForeignKey.Create;
  Result := FForeignKey;
end;

function TDeltaField.GetIsUnique: Boolean;
begin
  Result := dboUnique in FDBOptions;
end;

procedure TDeltaField.SetIsUnique(AValue: Boolean);
begin
  if AValue then
    Include(FDBOptions, dboUnique)
  else
    Exclude(FDBOptions, dboUnique);
end;

function TDeltaField.GetIsIndexed: Boolean;
begin
  Result := dboIndex in FDBOptions;
end;

procedure TDeltaField.SetIsIndexed(AValue: Boolean);
begin
  if AValue then
    Include(FDBOptions, dboIndex)
  else
    Exclude(FDBOptions, dboIndex);
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
  FDirty := False;
  FFieldKind := dfkString;
  FDBOptions := [dboInsert, dboUpdate];
end;

destructor TDeltaField.Destroy;
begin
  FForeignKey.Free;
  inherited Destroy;
end;

function TDeltaField.GetAsInteger: Integer;
begin
  if IsNull then Result := 0
  else Result := Integer(FValue);
end;

procedure TDeltaField.SetAsInteger(const AValue: Integer);
begin
  SetValue(AValue);
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

function TDeltaField.GetAsCurrency: Currency;
begin
  if IsNull then Result := 0
  else Result := Currency(FValue);
end;

procedure TDeltaField.SetAsCurrency(const AValue: Currency);
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

procedure TDeltaField.SetAsString(const AValue: string);
begin
  SetValue(AValue);
end;

function TDeltaField.Eq(const AValue: Variant): TDeltaCondition;
begin
  Result.Field := Self;
  Result.&Operator := doEq;
  Result.Value := AValue;
  Result.SecondaryValue := Null;
  SetLength(Result.Values, 0);
end;

function TDeltaField.NotEq(const AValue: Variant): TDeltaCondition;
begin
  Result.Field := Self;
  Result.&Operator := doNotEq;
  Result.Value := AValue;
  Result.SecondaryValue := Null;
  SetLength(Result.Values, 0);
end;

function TDeltaField.Gt(const AValue: Variant): TDeltaCondition;
begin
  Result.Field := Self;
  Result.&Operator := doGt;
  Result.Value := AValue;
  Result.SecondaryValue := Null;
  SetLength(Result.Values, 0);
end;

function TDeltaField.Gte(const AValue: Variant): TDeltaCondition;
begin
  Result.Field := Self;
  Result.&Operator := doGte;
  Result.Value := AValue;
  Result.SecondaryValue := Null;
  SetLength(Result.Values, 0);
end;

function TDeltaField.Lt(const AValue: Variant): TDeltaCondition;
begin
  Result.Field := Self;
  Result.&Operator := doLt;
  Result.Value := AValue;
  Result.SecondaryValue := Null;
  SetLength(Result.Values, 0);
end;

function TDeltaField.Lte(const AValue: Variant): TDeltaCondition;
begin
  Result.Field := Self;
  Result.&Operator := doLte;
  Result.Value := AValue;
  Result.SecondaryValue := Null;
  SetLength(Result.Values, 0);
end;

function TDeltaField.Like(const AValue: Variant): TDeltaCondition;
begin
  Result.Field := Self;
  Result.&Operator := doLike;
  Result.Value := AValue;
  Result.SecondaryValue := Null;
  SetLength(Result.Values, 0);
end;

function TDeltaField.Between(const AVal1, AVal2: Variant): TDeltaCondition;
begin
  Result.Field := Self;
  Result.&Operator := doBetween;
  Result.Value := AVal1;
  Result.SecondaryValue := AVal2;
  SetLength(Result.Values, 0);
end;

function TDeltaField.InList(const AValues: array of Variant): TDeltaCondition;
var
  I: Integer;
begin
  Result.Field := Self;
  Result.&Operator := doIn;
  Result.Value := Null;
  Result.SecondaryValue := Null;
  SetLength(Result.Values, Length(AValues));
  for I := Low(AValues) to High(AValues) do
    Result.Values[I - Low(AValues)] := AValues[I];
end;

function TDeltaField.InList(const AValues: array of string): TDeltaCondition;
var
  I: Integer;
begin
  Result.Field := Self;
  Result.&Operator := doIn;
  Result.Value := Null;
  Result.SecondaryValue := Null;
  SetLength(Result.Values, Length(AValues));
  for I := Low(AValues) to High(AValues) do
    Result.Values[I - Low(AValues)] := AValues[I];
end;

function TDeltaField.InList(const AValues: array of Int64): TDeltaCondition;
var
  I: Integer;
begin
  Result.Field := Self;
  Result.&Operator := doIn;
  Result.Value := Null;
  Result.SecondaryValue := Null;
  SetLength(Result.Values, Length(AValues));
  for I := Low(AValues) to High(AValues) do
    Result.Values[I - Low(AValues)] := AValues[I];
end;

function TDeltaField.IsDbNull: TDeltaCondition;
begin
  Result.Field := Self;
  Result.&Operator := doIsNull;
  Result.Value := Null;
  Result.SecondaryValue := Null;
  SetLength(Result.Values, 0);
end;

function TDeltaField.IsNotDbNull: TDeltaCondition;
begin
  Result.Field := Self;
  Result.&Operator := doIsNotNull;
  Result.Value := Null;
  Result.SecondaryValue := Null;
  SetLength(Result.Values, 0);
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
  else Result := DeltaFormatFloatFixed(Double(FValue), 8);
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
  else Result := DeltaFormatCurrencyFixed(Currency(FValue));
end;

function TDFCurrencyNull.SwaggerDataType: string;
begin
  Result := 'number';
end;

{ TDFStringNull }

function TDFStringNull.GetSize: Integer;
begin
  if (FSize = 0) or (FSize = DEFAULT_STR_SIZE) then
  begin
    if (FSchemaField <> nil) and (FSchemaField.FieldSize > 0) then
      Exit(FSchemaField.FieldSize);
  end;
  Result := FSize;
end;

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

procedure TDFStringNull.SetAsString(const AValue: string);
var
  TmpStr: string;
begin
  TmpStr := AValue;
  if (Self.FSize > 0) and (Length(TmpStr) > Self.FSize) then
    TmpStr := Copy(Trim(TmpStr), 1, Self.FSize);
  Self.FValue := TmpStr;
end;

procedure TDFStringNull.AfterConstruction;
begin
  inherited AfterConstruction;
  Size := DEFAULT_STR_SIZE;
end;

{ TDFTextNull }

constructor TDFTextNull.Create;
begin
  inherited Create;
  FFieldKind := dfkString;
  FSize := 0;
end;

procedure TDFTextNull.AfterConstruction;
begin
  inherited AfterConstruction;
  Size := 0;
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
  else Result := DeltaFormatFloatFixed(Double(FValue), 8);
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
  else Result := DeltaFormatCurrencyFixed(Currency(FValue));
end;

function TDFCurrencyRequired.SwaggerDataType: string;
begin
  Result := 'number';
end;

{ TDFStringRequired }

function TDFStringRequired.GetSize: Integer;
begin
  if (FSize = 0) or (FSize = DEFAULT_STR_SIZE) then
  begin
    if (FSchemaField <> nil) and (FSchemaField.FieldSize > 0) then
      Exit(FSchemaField.FieldSize);
  end;
  Result := FSize;
end;

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

procedure TDFStringRequired.SetAsString(const AValue: string);
var
  TmpStr: string;
begin
  TmpStr := AValue;
  if (Self.FSize > 0) and (Length(TmpStr) > Self.FSize) then
    TmpStr := Copy(Trim(TmpStr), 1, Self.FSize);
  Self.FValue := TmpStr;
end;

procedure TDFStringRequired.AfterConstruction;
begin
  inherited AfterConstruction;
  Size := DEFAULT_STR_SIZE;
end;

{ TDFTextRequired }

constructor TDFTextRequired.Create;
begin
  inherited Create;
  FFieldKind := dfkString;
  FSize := 0;
end;

procedure TDFTextRequired.AfterConstruction;
begin
  inherited AfterConstruction;
  Size := 0;
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
  FItems := TDFHasManyList.Create(True);
  FReferencesField := 'id';
  FForeignKeyField := '';
end;

destructor TDFHasMany.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TDFHasMany.References(ARelationClass: TClass; const AForeignKey: string; const AReferencesField: string);
begin
  FRelationClass := ARelationClass;
  FForeignKeyField := AForeignKey;
  if not AReferencesField.IsEmpty then
    FReferencesField := AReferencesField;
end;

procedure TDFHasMany.Add(AItem: TObject);
begin
  FItems.Add(AItem);
end;

function TDFHasMany.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TDFHasMany.GetItem(AIndex: Integer): TObject;
begin
  Result := FItems[AIndex];
end;

function TDFHasMany.GetValue: Variant;
begin
  Result := Null;
end;

procedure TDFHasMany.SetValue(AValue: Variant);
begin
  // Virtual fields do not store a scalar value
end;

function TDFHasMany.IsNull: Boolean;
begin
  Result := (FItems.Count = 0);
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
  FItems.Clear;
end;

{ TDFHasOne }

constructor TDFHasOne.Create;
begin
  inherited Create;
  FFieldKind := dfkVirtual;
  FIsVirtual := True;
  FOwnsModel := True;
  FModel := nil;
  FReferencesField := 'id';
end;

destructor TDFHasOne.Destroy;
begin
  if FOwnsModel and Assigned(FModel) then
    FreeAndNil(FModel);
  inherited Destroy;
end;

procedure TDFHasOne.References(ARelationClass: TClass; const AForeignKey: string;
  const AReferencesField: string);
begin
  FRelationClass := ARelationClass;
  FForeignKeyField := AForeignKey;
  if not AReferencesField.IsEmpty then
    FReferencesField := AReferencesField;
end;

function TDFHasOne.GetModel: TObject;
begin
  Result := FModel;
end;

procedure TDFHasOne.SetModel(AValue: TObject);
begin
  if FModel = AValue then Exit;
  if FOwnsModel and Assigned(FModel) then
    FModel.Free;
  FModel := AValue;
end;

function TDFHasOne.GetValue: Variant;
begin
  Result := Null;
end;

procedure TDFHasOne.SetValue(AValue: Variant);
begin
  // Virtual fields do not store a scalar value directly
end;

function TDFHasOne.IsNull: Boolean;
begin
  Result := (FModel = nil);
end;

function TDFHasOne.IsValid: Boolean;
begin
  Result := True;
end;

function TDFHasOne.SwaggerDataType: string;
begin
  Result := 'object';
end;

procedure TDFHasOne.Clear;
begin
  if FOwnsModel and Assigned(FModel) then
    FreeAndNil(FModel)
  else
    FModel := nil;
end;

initialization
  GlobalDeltaFS := DefaultFormatSettings;
  GlobalDeltaFS.DecimalSeparator := '.';
  GlobalDeltaFS.ThousandSeparator := ',';

end.
