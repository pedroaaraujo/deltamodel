unit DeltaModel.List;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type
  TCustomDeltaModelList = class
  public
    procedure FromJson(JsonStr: string); virtual; abstract;
    function ToJson: RawByteString; virtual; abstract;
    function ToJsonObj: TJSONArray; virtual; abstract;
    function SwaggerSchema(AddExamples: Boolean): TJSONObject; virtual abstract;
    function Count: Integer; virtual; abstract;

    function GetItemObj(AIndex: Integer): TObject; virtual; abstract;
    procedure ClearList; virtual; abstract;
    procedure AddObj(AObj: TObject); virtual; abstract;
    function GetModelClass: TClass; virtual; abstract;
    function NewItem: TObject; virtual; abstract;
  end;

implementation

end.

