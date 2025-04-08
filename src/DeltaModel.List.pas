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
  end;

implementation

end.

