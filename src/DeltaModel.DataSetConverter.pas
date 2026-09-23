unit DeltaModel.DataSetConverter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Variants, DeltaModel, DeltaModel.Fields, DB, SQLDB;

procedure FromDataSet(AModel: TDeltaModel; DS: TDataSet);
procedure ToDatasetParams(AModel: TDeltaModel; DS: TSQLQuery);
procedure ToDatasetParamsIndexed(AModel: TDeltaModel; DS: TSQLQuery; AIndex: Integer);

implementation

procedure FromDataSet(AModel: TDeltaModel; DS: TDataSet);
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropType: TTypeInfo;
  I, PropCount: integer;
  PropObj: TObject;
  DeltaField: TDeltaField;
  DSField: TField;
begin
  PropCount := GetPropList(AModel.ClassInfo, tkProperties, nil);
  if PropCount = 0 then Exit;

  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropList(AModel.ClassInfo, tkProperties, PropList, False);
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];
      if PropInfo^.GetProc = nil then Continue;

      PropType := PropInfo^.PropType^;

      if (PropType.Kind = tkClass) then
      begin
        PropObj := GetObjectProp(AModel, PropInfo);
        if PropObj is TDeltaField then
        begin
          DeltaField := TDeltaField(PropObj);

          if DeltaField.IsVirtual then Continue;

          DSField := DS.FindField(DeltaField.FieldName);
          if Assigned(DSField) then
            DeltaField.Value := DSField.Value;
        end;
      end
      else
      begin
        if PropInfo^.SetProc = nil then Continue;

        DSField := DS.FindField(PropInfo^.Name);
        if Assigned(DSField) then
        begin
          try
            case PropType.Kind of
              tkInteger, tkInt64, tkEnumeration, tkBool:
                SetOrdProp(AModel, PropInfo, DSField.AsLargeInt);
              tkString, tkLString, tkAString, tkWString, tkUString:
                SetStrProp(AModel, PropInfo, DSField.AsString);
              tkFloat:
                SetFloatProp(AModel, PropInfo, DSField.AsFloat);
            else
              SetPropValue(AModel, PropInfo^.Name, DSField.Value);
            end;
          except
            // ignora conversões incompatíveis de forma segura
          end;
        end;
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
  PropType: TTypeInfo;
  I, PropCount: Integer;
  PropObj: TObject;
  DeltaField: TDeltaField;
  Param: TParam;
begin
  PropCount := GetPropList(AModel.ClassInfo, tkProperties, nil);
  if PropCount = 0 then Exit;

  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropList(AModel.ClassInfo, tkProperties, PropList, False);
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];
      if PropInfo^.GetProc = nil then Continue;

      PropType := PropInfo^.PropType^;

      if (PropType.Kind = tkClass) then
      begin
        PropObj := GetObjectProp(AModel, PropInfo);
        if PropObj is TDeltaField then
        begin
          DeltaField := TDeltaField(PropObj);

          if DeltaField.IsVirtual then Continue;
          if DeltaField.FieldName.IsEmpty then Continue;

          Param := DS.Params.FindParam(DeltaField.FieldName);
          if Assigned(Param) then
          begin
            if DeltaField.IsNull then
              Param.Clear
            else
              Param.Value := DeltaField.Value;
          end;
        end;
      end
      else
      begin
        Param := DS.Params.FindParam(PropInfo^.Name);
        if Assigned(Param) then
        begin
          try
            case PropType.Kind of
              tkInteger, tkInt64, tkEnumeration, tkBool:
                Param.AsLargeInt := GetOrdProp(AModel, PropInfo);
              tkString, tkLString, tkAString, tkWString, tkUString:
                Param.AsString := GetStrProp(AModel, PropInfo);
              tkFloat:
                Param.AsFloat := GetFloatProp(AModel, PropInfo);
            else
              Param.Value := GetPropValue(AModel, PropInfo^.Name);
            end;
          except
            // ignora propriedades não mapeáveis
          end;
        end;
      end;
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;
end;

procedure ToDatasetParamsIndexed(AModel: TDeltaModel; DS: TSQLQuery; AIndex: Integer);
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropType: TTypeInfo;
  I, PropCount: Integer;
  PropObj: TObject;
  DeltaField: TDeltaField;
  Param: TParam;
  ParamName: string;
begin
  PropCount := GetPropList(AModel.ClassInfo, tkProperties, nil);
  if PropCount = 0 then Exit;

  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropList(AModel.ClassInfo, tkProperties, PropList, False);
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];
      if PropInfo^.GetProc = nil then Continue;

      PropType := PropInfo^.PropType^;

      if (PropType.Kind = tkClass) then
      begin
        PropObj := GetObjectProp(AModel, PropInfo);
        if PropObj is TDeltaField then
        begin
          DeltaField := TDeltaField(PropObj);

          if DeltaField.IsVirtual then Continue;
          if DeltaField.FieldName.IsEmpty then Continue;

          ParamName := DeltaField.FieldName + '_' + IntToStr(AIndex);
          Param := DS.Params.FindParam(ParamName);
          if Assigned(Param) then
          begin
            if DeltaField.IsNull then
              Param.Clear
            else
              Param.Value := DeltaField.Value;
          end;
        end;
      end
      else
      begin
        ParamName := PropInfo^.Name + '_' + IntToStr(AIndex);
        Param := DS.Params.FindParam(ParamName);
        if Assigned(Param) then
        begin
          try
            case PropType.Kind of
              tkInteger, tkInt64, tkEnumeration, tkBool:
                Param.AsLargeInt := GetOrdProp(AModel, PropInfo);
              tkString, tkLString, tkAString, tkWString, tkUString:
                Param.AsString := GetStrProp(AModel, PropInfo);
              tkFloat:
                Param.AsFloat := GetFloatProp(AModel, PropInfo);
            else
              Param.Value := GetPropValue(AModel, PropInfo^.Name);
            end;
          except
            // ignora propriedades não mapeáveis
          end;
        end;
      end;
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;
end;

end.
