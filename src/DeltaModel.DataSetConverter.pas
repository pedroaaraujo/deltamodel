unit DeltaModel.DataSetConverter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, DeltaModel, DeltaModel.Fields, DB, SQLDB;

procedure FromDataSet(AModel: TDeltaModel; DS: TDataSet);
procedure ToDatasetParams(AModel: TDeltaModel; DS: TSQLQuery);

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
          if DS.FieldByName(PropInfo^.Name) is TStringField then
            (PropObj as TDeltaField).Value := DS.FieldByName(PropInfo^.Name).AsUTF8String
          else
            (PropObj as TDeltaField).Value := DS.FieldByName(PropInfo^.Name).Value;
        end;
      end
      else
      begin
        if DS.FieldByName(PropInfo^.Name) is TStringField then
          SetPropValue(AModel, PropInfo^.Name, DS.FieldByName(PropInfo^.Name).AsUTF8String)
        else
          SetPropValue(AModel, PropInfo^.Name, DS.FieldByName(PropInfo^.Name).Value);
      end;
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;
end;

procedure ToDatasetParams(AModel: TDeltaModel; DS: TSQLQuery);
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropType: PTypeInfo;
  I, PropCount: Integer;
  PropObj: TObject;
  FieldName: string;
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
          FieldName := (PropObj as TDeltaField).FieldName;
          if (FieldName <> '') and (DS.ParamByName(FieldName) <> nil) then
            DS.ParamByName(FieldName).Value := (PropObj as TDeltaField).Value;
        end;
      end
      else
      begin
        if DS.ParamByName(PropInfo^.Name) <> nil then
          DS.ParamByName(PropInfo^.Name).Value := GetPropValue(AModel, PropInfo^.Name);
      end;
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;
end;

end.

