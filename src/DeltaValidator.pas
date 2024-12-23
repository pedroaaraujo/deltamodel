unit DeltaValidator;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF UNIX}
  LazUTF8,
  {$ENDIF}
  Classes, SysUtils, fgl, Variants, RegExpr, fpjson,
  DeltaModelMessages;

type
  TValid = record
    OK: Boolean;
    Message: UTF8String;
  end;

  IDeltaValidatorItem = interface ['{B89C6AA3-6762-4E24-8217-9240F162702F}']
    function Validate(Value: Variant): TValid;
  end;

  { TDeltaField }

  TDeltaField = class(specialize TFPGInterfacedObjectList<IDeltaValidatorItem>)
  private
    FName: string;
    FValue: Variant;
  public
    property Name: string read FName write FName;
    property Value: Variant read FValue write FValue;
    function AddValidator(Item: IDeltaValidatorItem): TDeltaField;
    constructor Create(_Name: string; _Value: Variant);
    function Validate: TValid;
    function ValidateToJson: TValid;
  end;

  { TValidator }

  TValidator = class
  private
    FFields: specialize TFPGObjectList<TDeltaField>;
  public
    property Fields: specialize TFPGObjectList<TDeltaField> read FFields;
    function AddField(Name: string; Value: Variant): TDeltaField;
    function Validate: TValid;
    function ValidateToJson: TValid;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;

  { Text Validations }

  TValidatorItemNotEmpty = class(TInterfacedObject, IDeltaValidatorItem)
  public
    function Validate(Value: Variant): TValid;
  end;

  TValidatorItemMinLength = class(TInterfacedObject, IDeltaValidatorItem)
  private
    FMinLength: Integer;
  public
    constructor Create(MinLength: Integer);
    function Validate(Value: Variant): TValid;
  end;

  TValidatorItemMaxLength = class(TInterfacedObject, IDeltaValidatorItem)
  private
    FMaxLength: Integer;
  public
    constructor Create(MaxLength: Integer);
    function Validate(Value: Variant): TValid;
  end;

  TValidatorItemRegex = class(TInterfacedObject, IDeltaValidatorItem)
  private
    FPattern: string;
  public
    constructor Create(Pattern: string);
    function Validate(Value: Variant): TValid;
  end;

  { TValidatorEmail }

  TValidatorEmail = class(TInterfacedObject, IDeltaValidatorItem)
  public
    function Validate(Value: Variant): TValid;
  end;

  { Numeric Validations }

  TValidatorItemMinValue = class(TInterfacedObject, IDeltaValidatorItem)
  private
    FMinValue: Double;
  public
    constructor Create(MinValue: Double);
    function Validate(Value: Variant): TValid;
  end;

  { TValidatorItemGreaterThanZero }

  TValidatorItemGreaterThanZero = class(TInterfacedObject, IDeltaValidatorItem)
  public
    constructor Create;
    function Validate(Value: Variant): TValid;
  end;

  TValidatorItemMaxValue = class(TInterfacedObject, IDeltaValidatorItem)
  private
    FMaxValue: Double;
  public
    constructor Create(MaxValue: Double);
    function Validate(Value: Variant): TValid;
  end;

  { Date Validations }

  TValidatorItemPeriod = class(TInterfacedObject, IDeltaValidatorItem)
  private
    FStartDate: TDateTime;
    FEndDate: TDateTime;
  public
    constructor Create(StartDate, EndDate: TDateTime);
    function Validate(Value: Variant): TValid;
  end;

implementation

{ TDeltaField }

function TDeltaField.AddValidator(Item: IDeltaValidatorItem): TDeltaField;
begin
  Self.Add(Item);
  Result := Self;
end;

constructor TDeltaField.Create(_Name: string; _Value: Variant);
begin
  inherited Create;
  FName := _Name;
  FValue := _Value;
end;

function TDeltaField.Validate: TValid;
var
  i: Integer;
  ValidationResult: TValid;
begin
  Result.OK := True;
  Result.Message := '';

  for i := 0 to Count - 1 do
  begin
    ValidationResult := Items[i].Validate(FValue);
    if not ValidationResult.OK then
    begin
      Result := ValidationResult;
      Exit;
    end;
  end;
end;

function TDeltaField.ValidateToJson: TValid;
var
  Json: TJSONArray;
  I: Integer;
  ValidationResult: TValid;
begin
  Json := TJSONArray.Create();
  try
    for I := 0 to Count - 1 do
    begin
      ValidationResult := Items[i].Validate(FValue);
      if not ValidationResult.OK then
      begin
        Json.Add(ValidationResult.Message);
      end;
    end;

    Result.Message := Json.AsJSON;
    Result.OK := Json.Count = 0;
  finally
    Json.Free;
  end;
end;

{ TValidator }

function TValidator.AddField(Name: string; Value: Variant): TDeltaField;
var
  Field: TDeltaField;
begin
  Field := TDeltaField.Create(Name, Value);
  FFields.Add(Field);
  Result := Field;
end;

function TValidator.Validate: TValid;
var
  i: Integer;
  ValidationResult: TValid;
begin
  Result.OK := True;
  Result.Message := '';

  for i := 0 to FFields.Count - 1 do
  begin
    ValidationResult := FFields[i].Validate;
    if not ValidationResult.OK then
    begin
      Result.Ok := False;
      Result.Message := Format(
        ValidationFailedForField,
        [FFields[I].Name, ValidationResult.Message]
      );
      Exit;
    end;
  end;
end;

function TValidator.ValidateToJson: TValid;
var
  I: Integer;
  ValidationResult: TValid;
  JsonItem: TJSONObject;
  JsonArr: TJSONArray;
begin
  JsonArr := TJSONArray.Create();
  try
    for I := 0 to FFields.Count - 1 do
    begin
      ValidationResult := FFields[I].ValidateToJson;
      if not ValidationResult.OK then
      begin
        JsonItem := TJSONObject.Create;
        JsonItem.Add('field', FFields[I].Name);
        JsonItem.Add('issues', GetJson(ValidationResult.Message, False));
        JsonArr.Add(JsonItem)
      end;
    end;

    Result.Message := JsonArr.AsJSON;
    Result.OK := JsonArr.Count = 0;
  finally
    JsonArr.Free;
  end;
end;

procedure TValidator.Clear;
begin
  FFields.Clear;
end;

constructor TValidator.Create;
begin
  FFields := specialize TFPGObjectList<TDeltaField>.Create(True);
end;

destructor TValidator.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;

{ Text Validations }

function TValidatorItemNotEmpty.Validate(Value: Variant): TValid;
begin
  Result.OK := not VarIsEmpty(Value) and (VarToStr(Value) <> '');
  if not Result.OK then
    Result.Message := ValueCannotBeEmpty;
end;

constructor TValidatorItemMinLength.Create(MinLength: Integer);
begin
  FMinLength := MinLength;
end;

function TValidatorItemMinLength.Validate(Value: Variant): TValid;
var
  Str: string;
  ActualLenght: Integer;
begin
  Str := VarToStr(Value);
  ActualLenght := {$IFDEF UNIX} Utf8Length(Str) {$ELSE} Length(Str) {$ENDIF};

  Result.OK := ActualLenght >= FMinLength;
  if not Result.OK then
    Result.Message := Format(MinimumLenght, [FMinLength]);
end;

constructor TValidatorItemMaxLength.Create(MaxLength: Integer);
begin
  FMaxLength := MaxLength;
end;

function TValidatorItemMaxLength.Validate(Value: Variant): TValid;
var
  Str: string;
  ActualLenght: Integer;
begin
  Str := VarToStr(Value);
  ActualLenght := {$IFDEF UNIX} Utf8Length(Str) {$ELSE} Length(Str) {$ENDIF};

  Result.OK := ActualLenght <= FMaxLength;
  if not Result.OK then
    Result.Message := Format(MaximumLenght, [FMaxLength]);
end;

constructor TValidatorItemRegex.Create(Pattern: string);
begin
  FPattern := Pattern;
end;

function TValidatorItemRegex.Validate(Value: Variant): TValid;
var
  RegEx: TRegExpr;
begin
  RegEx := TRegExpr.Create(FPattern);
  try
    Result.OK := RegEx.Exec(VarToStr(Value));
    if not Result.OK then
      Result.Message := ValueDoesNotMatchREGEX;
  finally
    RegEx.Free;
  end;
end;

{ TValidatorEmail }

function TValidatorEmail.Validate(Value: Variant): TValid;
const
  EmailPattern =
    '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$';
var
  Regex: TRegExpr;
begin
  Regex := TRegExpr.Create(EmailPattern);
  try
    Result.OK := Regex.Exec(VarToStr(Value));
    if not Result.OK then
      Result.Message := InvalidEmail;
  finally
    Regex.Free;
  end;
end;

{ Numeric Validations }

constructor TValidatorItemMinValue.Create(MinValue: Double);
begin
  FMinValue := MinValue;
end;

function TValidatorItemMinValue.Validate(Value: Variant): TValid;
begin
  Result.OK := Value >= FMinValue;
  if not Result.OK then
    Result.Message := Format(MinimumAllowedValue, [FMinValue]);
end;

constructor TValidatorItemGreaterThanZero.Create;
begin
  inherited Create;
end;

function TValidatorItemGreaterThanZero.Validate(Value: Variant): TValid;
begin
  Result.OK := Value > 0;
  if not Result.OK then
    Result.Message := ValueMustBeGreaterThanZero;
end;

constructor TValidatorItemMaxValue.Create(MaxValue: Double);
begin
  FMaxValue := MaxValue;
end;

function TValidatorItemMaxValue.Validate(Value: Variant): TValid;
begin
  Result.OK := Value <= FMaxValue;
  if not Result.OK then
    Result.Message := Format(MaximumAllowedValue, [FMaxValue]);
end;

constructor TValidatorItemPeriod.Create(StartDate, EndDate: TDateTime);
begin
  FStartDate := StartDate;
  FEndDate := EndDate;
end;

function TValidatorItemPeriod.Validate(Value: Variant): TValid;
var
  ValueDate: TDateTime;
begin
  Result.OK := False;
  if VarIsStr(Value) then
  begin
    ValueDate := StrToDate(Value);
    if (ValueDate >= FStartDate) and (ValueDate <= FEndDate) then
    begin
      Result.OK := True;
    end;
  end;

  if not Result.OK then
    Result.Message := Format(
      AllowedRange,
      [DateToStr(FStartDate), DateToStr(FEndDate)]
    );
end;

end.
