unit DeltaModel.Types;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Variants;

type

  { INullableValue }

  INullableValue = interface
  ['{B08C21DB-13E1-43D8-B40E-D3375CCCE912}']
    procedure SetValue(AValue: Variant);
    function GetValue: Variant;
    procedure Clear;
  end;

  { TIntNull }

  TIntNull = class(TInterfacedObject, INullableValue)
  private
    FValue: Variant;
    function GetValue: Variant;
    procedure SetValue(AValue: Variant);
  public
    property Value: Variant read GetValue write SetValue;
    procedure Clear;
  end;

implementation

{ TIntNull }

function TIntNull.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TIntNull.SetValue(AValue: Variant);
begin
  if VarIsNull(AValue) then
  begin
    FValue := Null;
    Exit;
  end;

  if VarIsNumeric(AValue) and
     (Trunc(AValue) = AValue) then
  begin
    FValue := AValue;
    Exit;
  end;

  raise Exception.Create('Only Integer and Null values are accepteds.');
end;

procedure TIntNull.Clear;
begin
  SetValue(Null);
end;

end.

