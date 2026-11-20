unit DeltaModel.Exceptions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type
  { Validation rule violation associated with a model field }
  TDeltaFieldViolation = record
    FieldName: string;
    Rule: string;
    Message: string;
  end;

  TDeltaFieldViolationArray = array of TDeltaFieldViolation;

  { ========================================================================= }
  { EDeltaModelException: Base exception for the domain and ORM layer         }
  { ========================================================================= }
  EDeltaModelException = class(Exception)
  private
    FErrorCode: string;
    FModelName: string;
  public
    constructor Create(const AMessage: string;
      const AErrorCode: string = '';
      const AModelName: string = '');
    constructor CreateFmt(const AFormat: string; const AArgs: array of const;
      const AErrorCode: string = '';
      const AModelName: string = '');

    property ErrorCode: string read FErrorCode write FErrorCode;
    property ModelName: string read FModelName write FModelName;
  end;

  { ========================================================================= }
  { EDeltaValidationException: Field-grouped validation violations            }
  { ========================================================================= }
  EDeltaValidationException = class(EDeltaModelException)
  private
    FViolations: TDeltaFieldViolationArray;
    function GetViolationCount: Integer; inline;
  public
    constructor Create(const AMessage: string = '';
      const AModelName: string = ''); overload;
    constructor Create(const AMessage: string;
      const AViolations: array of TDeltaFieldViolation;
      const AModelName: string = ''); overload;

    function AddViolation(const AFieldName, AMessage: string;
      const ARule: string = ''): EDeltaValidationException;
    function HasFieldViolation(const AFieldName: string): Boolean;
    function GetFieldViolations(const AFieldName: string): TDeltaFieldViolationArray;
    function ToJson: string;

    property Violations: TDeltaFieldViolationArray read FViolations;
    property ViolationCount: Integer read GetViolationCount;
  end;

  { ========================================================================= }
  { EDeltaNotFoundException: Entity or record not found                       }
  { ========================================================================= }
  EDeltaNotFoundException = class(EDeltaModelException)
  private
    FEntityId: string;
  public
    constructor Create(const AModelName, AEntityId: string;
      const AMessage: string = '');

    property EntityId: string read FEntityId;
  end;

  { ========================================================================= }
  { EDeltaConcurrencyException: Optimistic concurrency conflict (version)     }
  { ========================================================================= }
  EDeltaConcurrencyException = class(EDeltaModelException)
  private
    FEntityId: string;
    FExpectedVersion: string;
    FActualVersion: string;
  public
    constructor Create(const AModelName, AEntityId,
      AExpectedVersion, AActualVersion: string;
      const AMessage: string = '');

    property EntityId: string read FEntityId;
    property ExpectedVersion: string read FExpectedVersion;
    property ActualVersion: string read FActualVersion;
  end;

  { ========================================================================= }
  { EDeltaSerializationException: Serialization or deserialization failure    }
  { ========================================================================= }
  EDeltaSerializationException = class(EDeltaModelException)
  private
    FFormatName: string;
    FFieldName: string;
  public
    constructor Create(const AMessage: string;
      const AFormatName: string = '';
      const AFieldName: string = '';
      const AModelName: string = '');

    property FormatName: string read FFormatName;
    property FieldName: string read FFieldName;
  end;

  { ========================================================================= }
  { EDeltaQueryException: SQL query construction or execution error           }
  { ========================================================================= }
  EDeltaQueryException = class(EDeltaModelException)
  private
    FSqlStatement: string;
    FDialectName: string;
  public
    constructor Create(const AMessage: string;
      const ASqlStatement: string = '';
      const ADialectName: string = '';
      const AModelName: string = '');

    property SqlStatement: string read FSqlStatement;
    property DialectName: string read FDialectName;
  end;

{ Helper for ergonomic creation of field violations }
function MakeDeltaFieldViolation(const AFieldName, AMessage: string;
  const ARule: string = ''): TDeltaFieldViolation;

implementation

function MakeDeltaFieldViolation(const AFieldName, AMessage: string;
  const ARule: string): TDeltaFieldViolation;
begin
  Result.FieldName := AFieldName;
  Result.Rule := ARule;
  Result.Message := AMessage;
end;

{ =========================================================================== }
{ EDeltaModelException                                                        }
{ =========================================================================== }

constructor EDeltaModelException.Create(const AMessage, AErrorCode,
  AModelName: string);
begin
  inherited Create(AMessage);
  FErrorCode := AErrorCode;
  FModelName := AModelName;
end;

constructor EDeltaModelException.CreateFmt(const AFormat: string;
  const AArgs: array of const; const AErrorCode, AModelName: string);
begin
  Create(Format(AFormat, AArgs), AErrorCode, AModelName);
end;

{ =========================================================================== }
{ EDeltaValidationException                                                   }
{ =========================================================================== }

constructor EDeltaValidationException.Create(const AMessage, AModelName: string);
var
  Msg: string;
begin
  Msg := AMessage;
  if Msg = '' then
    Msg := 'Validation failed.';
  inherited Create(Msg, 'DELTA_VALIDATION_ERROR', AModelName);
  SetLength(FViolations, 0);
end;

constructor EDeltaValidationException.Create(const AMessage: string;
  const AViolations: array of TDeltaFieldViolation; const AModelName: string);
var
  I: Integer;
begin
  Create(AMessage, AModelName);
  SetLength(FViolations, Length(AViolations));
  for I := 0 to High(AViolations) do
    FViolations[I] := AViolations[I];
end;

function EDeltaValidationException.GetViolationCount: Integer;
begin
  Result := Length(FViolations);
end;

function EDeltaValidationException.AddViolation(const AFieldName, AMessage,
  ARule: string): EDeltaValidationException;
var
  Idx: Integer;
begin
  Idx := Length(FViolations);
  SetLength(FViolations, Idx + 1);
  FViolations[Idx].FieldName := AFieldName;
  FViolations[Idx].Rule := ARule;
  FViolations[Idx].Message := AMessage;
  Result := Self;
end;

function EDeltaValidationException.HasFieldViolation(
  const AFieldName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(FViolations) do
    if SameText(FViolations[I].FieldName, AFieldName) then
      Exit(True);
end;

function EDeltaValidationException.GetFieldViolations(
  const AFieldName: string): TDeltaFieldViolationArray;
var
  I, Count: Integer;
begin
  Result := nil;
  Count := 0;
  for I := 0 to High(FViolations) do
  begin
    if SameText(FViolations[I].FieldName, AFieldName) then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FViolations[I];
      Inc(Count);
    end;
  end;
end;

function EDeltaValidationException.ToJson: string;
var
  Root, Item: TJSONObject;
  Arr: TJSONArray;
  I: Integer;
begin
  Root := TJSONObject.Create;
  try
    Root.Add('errorCode', ErrorCode);
    Root.Add('message', Message);
    if ModelName <> '' then
      Root.Add('model', ModelName);

    Arr := TJSONArray.Create;
    for I := 0 to High(FViolations) do
    begin
      Item := TJSONObject.Create;
      Item.Add('field', FViolations[I].FieldName);
      if FViolations[I].Rule <> '' then
        Item.Add('rule', FViolations[I].Rule);
      Item.Add('message', FViolations[I].Message);
      Arr.Add(Item);
    end;
    Root.Add('violations', Arr);

    Result := Root.AsJSON;
  finally
    Root.Free;
  end;
end;

{ =========================================================================== }
{ EDeltaNotFoundException                                                     }
{ =========================================================================== }

constructor EDeltaNotFoundException.Create(const AModelName, AEntityId,
  AMessage: string);
var
  Msg: string;
begin
  Msg := AMessage;
  if Msg = '' then
  begin
    if (AModelName <> '') and (AEntityId <> '') then
      Msg := Format('%s with identifier "%s" was not found.', [AModelName, AEntityId])
    else if AModelName <> '' then
      Msg := Format('%s was not found.', [AModelName])
    else
      Msg := 'Requested record was not found.';
  end;
  inherited Create(Msg, 'DELTA_NOT_FOUND', AModelName);
  FEntityId := AEntityId;
end;

{ =========================================================================== }
{ EDeltaConcurrencyException                                                  }
{ =========================================================================== }

constructor EDeltaConcurrencyException.Create(const AModelName, AEntityId,
  AExpectedVersion, AActualVersion, AMessage: string);
var
  Msg: string;
begin
  Msg := AMessage;
  if Msg = '' then
    Msg := Format(
      'Optimistic concurrency conflict on %s "%s" (expected version "%s", actual "%s").',
      [AModelName, AEntityId, AExpectedVersion, AActualVersion]
    );
  inherited Create(Msg, 'DELTA_CONCURRENCY_CONFLICT', AModelName);
  FEntityId := AEntityId;
  FExpectedVersion := AExpectedVersion;
  FActualVersion := AActualVersion;
end;

{ =========================================================================== }
{ EDeltaSerializationException                                                }
{ =========================================================================== }

constructor EDeltaSerializationException.Create(const AMessage, AFormatName,
  AFieldName, AModelName: string);
begin
  inherited Create(AMessage, 'DELTA_SERIALIZATION_ERROR', AModelName);
  FFormatName := AFormatName;
  FFieldName := AFieldName;
end;

{ =========================================================================== }
{ EDeltaQueryException                                                        }
{ =========================================================================== }

constructor EDeltaQueryException.Create(const AMessage, ASqlStatement,
  ADialectName, AModelName: string);
begin
  inherited Create(AMessage, 'DELTA_QUERY_ERROR', AModelName);
  FSqlStatement := ASqlStatement;
  FDialectName := ADialectName;
end;

end.
