unit DeltaModel.DataSetConverter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, DeltaModel, DeltaModel.Fields, DB;

procedure FromDataSet(AModel: TDeltaModel; DS: TDataSet);

implementation

procedure FromDataSet(AModel: TDeltaModel; DS: TDataSet);
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropType: PTypeInfo;
  I, PropCount: integer;
  PropObj: TObject;
begin
  PropCount := GetPropList(AModel.ClassInfo, tkProperties, nil);
  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropList(AModel.ClassInfo, tkProperties, PropList);
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];
      PropType := PropInfo^.PropType;
      if (PropType^.Kind = tkClass) then
      begin
        PropObj := GetObjectProp(AModel, PropInfo^.Name);
        if (PropObj is TDeltaField) then
        begin
          (PropObj as TDeltaField).Value := DS.FieldByName(PropInfo^.Name).Value;
        end;
      end
      else
      begin
        SetPropValue(AModel, PropInfo^.Name, DS.FieldByName(PropInfo^.Name).Value);
      end;
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;
end;

end.

