unit DeltaModel.Types;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Variants, DeltaModelMessages;

type

  { TDeltaField }

  TDeltaField = class
  private
    FFieldName: string;
    procedure SetFieldName(AValue: string);
  protected
    function GetValue: Variant; virtual; abstract;
    procedure SetValue(AValue: Variant); virtual; abstract;
  public
    property Value: Variant read GetValue write SetValue;
    ///to be used with SQL Builder
    property FieldName: string read FFieldName write SetFieldName;
    procedure Clear; virtual; abstract;
    function IsNull: Boolean; virtual; abstract;
    function IsValid: Boolean; virtual; abstract;
    function SwaggerDataType: string; virtual; abstract;
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
    function IsNull: Boolean; override;
    function IsValid: Boolean; override;
    function SwaggerDataType: string; override;
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
  end;

  { TDFIntNull }

  TDFIntNull = class(TDeltaFieldNullable)
  protected
    procedure SetValue(AValue: Variant); override;
  end;

  { TDFDoubleNull }

  TDFDoubleNull = class(TDeltaFieldNullable)
  protected
    procedure SetValue(AValue: Variant); override;
  end;

  { TDFCurrencyNull }

  TDFCurrencyNull = class(TDeltaFieldNullable)
  protected
    procedure SetValue(AValue: Variant); override;
  end;

  { TDFStringNull }

  TDFStringNull = class(TDeltaFieldNullable)
  protected
    procedure SetValue(AValue: Variant); override;
  end;

  { TDFIntRequired }

  TDFIntRequired = class(TDeltaFieldRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  end;

  { TDFDoubleRequired }

  TDFDoubleRequired = class(TDeltaFieldRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  end;

  { TDFCurrencyRequired }

  TDFCurrencyRequired = class(TDeltaFieldRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  end;

  { TDFStringRequired }

  TDFStringRequired = class(TDeltaFieldRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  end;

implementation

{ TDeltaField }

procedure TDeltaField.SetFieldName(AValue: string);
begin
  if FFieldName = AValue then Exit;

  FFieldName := AValue;
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

{ TDFIntNull }

procedure TDFIntNull.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
  if not VarIsNull(AValue) then
  begin
    Self.FValue := Integer(AValue);
  end;
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

{ TDFCurrencyNull }

procedure TDFCurrencyNull.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
  if not VarIsNull(AValue) then
  begin
    Self.FValue := Currency(AValue);
  end;
end;

{ TDFStringNull }

procedure TDFStringNull.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
  if not VarIsNull(AValue) then
  begin
    Self.FValue := string(AValue);
  end;
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

{ TDFDoubleRequired }

procedure TDFDoubleRequired.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
  if not VarIsNull(AValue) then
  begin
    Self.FValue := Double(AValue);
  end;
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

{ TDFStringRequired }

procedure TDFStringRequired.SetValue(AValue: Variant);
begin
  inherited SetValue(AValue);
  if not VarIsNull(AValue) then
  begin
    Self.FValue := string(AValue);
  end;
end;

end.

