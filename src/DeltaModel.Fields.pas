unit DeltaModel.Fields;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Variants, DeltaModelMessages;

type

  TDBOptions = (dboPrimaryKey, dboUpdate);
  TDBOptionsSet = set of TDBOptions;

  { TDeltaField }

  TDeltaField = class
  private
    FDBOptions: TDBOptionsSet;
    FFieldName: string;
    procedure SetFieldName(AValue: string);
  protected
    function GetValue: Variant; virtual; abstract;
    procedure SetValue(AValue: Variant); virtual; abstract;
    function FS: TFormatSettings;
  public
    property Value: Variant read GetValue write SetValue;

    ///to be used with ORM
    property FieldName: string read FFieldName write SetFieldName;
    property DBOptions: TDBOptionsSet read FDBOptions write FDBOptions;

    procedure Clear; virtual; abstract;
    procedure AfterConstruction; override;
    function IsNull: Boolean; virtual; abstract;
    function ToString: string; virtual;
    function IsValid: Boolean; virtual; abstract;
    function SwaggerDataType: string; virtual; abstract;
    constructor Create;
  end;

  { TDeltaFieldNullable }

  TDeltaFieldNullable = class(TDeltaField)
  private
    FValue: Variant;
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
  private
    FValue: Variant;
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
    function ToString: string; override;
  end;

  { TDFDoubleNull }

  TDFDoubleNull = class(TDeltaFieldNullable)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    function ToString: string; override;
  end;

  { TDFCurrencyNull }

  TDFCurrencyNull = class(TDeltaFieldNullable)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    function ToString: string; override;
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
    function ToString: string; override;
  end;

  { TDFDoubleRequired }

  TDFDoubleRequired = class(TDeltaFieldRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    function ToString: string; override;
  end;

  { TDFCurrencyRequired }

  TDFCurrencyRequired = class(TDeltaFieldRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    function ToString: string; override;
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

implementation

const
  DEFAULT_STR_SIZE = 20;

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
end;

function TDeltaField.ToString: string;
begin
  Result := VarToStrDef(Self.Value, EmptyStr);
end;

constructor TDeltaField.Create;
begin
  inherited;
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
    raise Exception.Create(ValueCannotBeEmpty);

  FValue := AValue;
end;

procedure TDeltaFieldRequired.Clear;
begin
  raise Exception.Create(ValueCannotBeEmpty);
end;

function TDeltaFieldRequired.IsNull: Boolean;
begin
  Result := False;
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

function TDFIntNull.ToString: string;
begin
  Result := StrToIntDef(inherited ToString, 0).ToString;
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

function TDFDoubleNull.ToString: string;
begin
  Result := FloatToStr(StrToFloatDef(inherited ToString, 0), FS);
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

function TDFCurrencyNull.ToString: string;
begin
  Result := FloatToStr(StrToFloatDef(inherited ToString, 0), FS);
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

function TDFIntRequired.ToString: string;
begin
  Result := StrToIntDef(inherited ToString, 0).ToString;
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

function TDFDoubleRequired.ToString: string;
begin
  Result := FloatToStr(StrToFloatDef(inherited ToString, 0), FS);
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

function TDFCurrencyRequired.ToString: string;
begin
  Result := FloatToStr(StrToFloatDef(inherited ToString, 0), FS);
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

end.


