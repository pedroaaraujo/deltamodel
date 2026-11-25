unit DeltaModel.ORM.Types;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Variants, fgl;

type
  TComparisonOp = (
    opEqual,              // =
    opNotEqual,           // <>
    opGreaterThan,        // >
    opGreaterThanOrEqual, // >=
    opLessThan,           // <
    opLessThanOrEqual,    // <=
    opLike,               // LIKE
    opILike,              // ILIKE (PostgreSQL) / LIKE
    opIn,                 // IN
    opNotIn,              // NOT IN
    opIsNull,             // IS NULL
    opIsNotNull,          // IS NOT NULL
    opBetween             // BETWEEN
  );

  TJoinType = (
    jtInner,
    jtLeft,
    jtRight,
    jtFull,
    jtCross
  );

  TDeltaParamType = (
    dptUnknown,
    dptInteger,
    dptLargeInt,
    dptFloat,
    dptBoolean,
    dptDateTime,
    dptString,
    dptMemo
  );

  TDeltaServerInfo = record
    MajorVersion: Integer;
    MinorVersion: Integer;
    VersionString: string;
  end;

  { TDeltaParam: driver-agnostic SQL parameter }
  TDeltaParam = class
  private
    FName: string;
    FValue: Variant;
    FParamType: TDeltaParamType;
    FIsNull: Boolean;
    function GetAsBoolean: Boolean;
    function GetAsDateTime: TDateTime;
    function GetAsFloat: Double;
    function GetAsInteger: Integer;
    function GetAsLargeInt: Int64;
    function GetAsString: string;
    procedure SetAsBoolean(const AValue: Boolean);
    procedure SetAsDateTime(const AValue: TDateTime);
    procedure SetAsFloat(const AValue: Double);
    procedure SetAsInteger(const AValue: Integer);
    procedure SetAsLargeInt(const AValue: Int64);
    procedure SetAsMemo(const AValue: string);
    procedure SetAsString(const AValue: string);
  public
    constructor Create(const AName: string = '');
    procedure Clear;
    procedure AssignVariant(const AValue: Variant);
    procedure Assign(ASource: TDeltaParam);

    property Name: string read FName write FName;
    property Value: Variant read FValue write AssignVariant;
    property ParamType: TDeltaParamType read FParamType write FParamType;
    property IsNull: Boolean read FIsNull;

    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsLargeInt: Int64 read GetAsLargeInt write SetAsLargeInt;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsString: string read GetAsString write SetAsString;
    property AsMemo: string read GetAsString write SetAsMemo;
  end;

  TDeltaParamList = specialize TFPGObjectList<TDeltaParam>;

  { TDeltaParams: driver-agnostic parameter collection }
  TDeltaParams = class
  private
    FItems: TDeltaParamList;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TDeltaParam;
  public
    constructor Create;
    destructor Destroy; override;

    function FindParam(const AName: string): TDeltaParam;
    function AddParam(const AName: string; AType: TDeltaParamType = dptUnknown): TDeltaParam;
    function SetParam(const AName: string; const AValue: Variant): TDeltaParam;
    function AddAnonymous(const AValue: Variant): string;
    procedure Assign(ASource: TDeltaParams);
    procedure MergeFrom(ASource: TDeltaParams);
    procedure Clear;

    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TDeltaParam read GetItem; default;
  end;

function DefaultServerInfo: TDeltaServerInfo;

implementation

function DefaultServerInfo: TDeltaServerInfo;
begin
  Result.MajorVersion := 0;
  Result.MinorVersion := 0;
  Result.VersionString := '';
end;

{ TDeltaParam }

constructor TDeltaParam.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  Clear;
end;

procedure TDeltaParam.Clear;
begin
  FValue := Null;
  FParamType := dptUnknown;
  FIsNull := True;
end;

procedure TDeltaParam.AssignVariant(const AValue: Variant);
var
  VType: TVarType;
begin
  if VarIsNull(AValue) or VarIsEmpty(AValue) then
  begin
    Clear;
    Exit;
  end;

  FIsNull := False;
  FValue := AValue;
  VType := VarType(AValue);

  if VType = varBoolean then
    FParamType := dptBoolean
  else if VType in [varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord] then
    FParamType := dptInteger
  else if VType in [varInt64, varQWord] then
    FParamType := dptLargeInt
  else if VType in [varSingle, varDouble, varCurrency] then
    FParamType := dptFloat
  else if VType = varDate then
    FParamType := dptDateTime
  else if FParamType <> dptMemo then
    FParamType := dptString;
end;

procedure TDeltaParam.Assign(ASource: TDeltaParam);
begin
  if ASource = nil then
    Exit;
  FName := ASource.Name;
  FValue := ASource.Value;
  FParamType := ASource.ParamType;
  FIsNull := ASource.IsNull;
end;

function TDeltaParam.GetAsBoolean: Boolean;
begin
  if FIsNull then Exit(False);
  Result := Boolean(FValue);
end;

function TDeltaParam.GetAsDateTime: TDateTime;
begin
  if FIsNull then Exit(0);
  Result := VarToDateTime(FValue);
end;

function TDeltaParam.GetAsFloat: Double;
begin
  if FIsNull then Exit(0);
  Result := Double(FValue);
end;

function TDeltaParam.GetAsInteger: Integer;
begin
  if FIsNull then Exit(0);
  Result := Integer(FValue);
end;

function TDeltaParam.GetAsLargeInt: Int64;
begin
  if FIsNull then Exit(0);
  Result := Int64(FValue);
end;

function TDeltaParam.GetAsString: string;
begin
  if FIsNull then Exit('');
  Result := VarToStr(FValue);
end;

procedure TDeltaParam.SetAsBoolean(const AValue: Boolean);
begin
  FIsNull := False;
  FParamType := dptBoolean;
  FValue := AValue;
end;

procedure TDeltaParam.SetAsDateTime(const AValue: TDateTime);
begin
  FIsNull := False;
  FParamType := dptDateTime;
  FValue := AValue;
end;

procedure TDeltaParam.SetAsFloat(const AValue: Double);
begin
  FIsNull := False;
  FParamType := dptFloat;
  FValue := AValue;
end;

procedure TDeltaParam.SetAsInteger(const AValue: Integer);
begin
  FIsNull := False;
  FParamType := dptInteger;
  FValue := AValue;
end;

procedure TDeltaParam.SetAsLargeInt(const AValue: Int64);
begin
  FIsNull := False;
  FParamType := dptLargeInt;
  FValue := AValue;
end;

procedure TDeltaParam.SetAsMemo(const AValue: string);
begin
  FIsNull := False;
  FParamType := dptMemo;
  FValue := AValue;
end;

procedure TDeltaParam.SetAsString(const AValue: string);
begin
  FIsNull := False;
  FParamType := dptString;
  FValue := AValue;
end;

{ TDeltaParams }

constructor TDeltaParams.Create;
begin
  inherited Create;
  FItems := TDeltaParamList.Create(True);
end;

destructor TDeltaParams.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TDeltaParams.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TDeltaParams.GetItem(AIndex: Integer): TDeltaParam;
begin
  Result := FItems[AIndex];
end;

function TDeltaParams.FindParam(const AName: string): TDeltaParam;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FItems.Count - 1 do
    if SameText(FItems[I].Name, AName) then
      Exit(FItems[I]);
end;

function TDeltaParams.AddParam(const AName: string; AType: TDeltaParamType): TDeltaParam;
begin
  Result := FindParam(AName);
  if Result <> nil then
  begin
    if AType <> dptUnknown then
      Result.ParamType := AType;
    Exit;
  end;
  Result := TDeltaParam.Create(AName);
  Result.ParamType := AType;
  FItems.Add(Result);
end;

function TDeltaParams.SetParam(const AName: string; const AValue: Variant): TDeltaParam;
begin
  Result := AddParam(AName);
  Result.AssignVariant(AValue);
end;

function TDeltaParams.AddAnonymous(const AValue: Variant): string;
var
  Idx: Integer;
begin
  Idx := FItems.Count + 1;
  while FindParam('p' + IntToStr(Idx)) <> nil do
    Inc(Idx);
  Result := 'p' + IntToStr(Idx);
  SetParam(Result, AValue);
end;

procedure TDeltaParams.Assign(ASource: TDeltaParams);
begin
  Clear;
  MergeFrom(ASource);
end;

procedure TDeltaParams.MergeFrom(ASource: TDeltaParams);
var
  I: Integer;
  Src, Dst: TDeltaParam;
begin
  if ASource = nil then
    Exit;
  for I := 0 to ASource.Count - 1 do
  begin
    Src := ASource[I];
    Dst := AddParam(Src.Name, Src.ParamType);
    Dst.Assign(Src);
  end;
end;

procedure TDeltaParams.Clear;
begin
  FItems.Clear;
end;

end.
