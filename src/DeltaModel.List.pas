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
    function ToPaginatedJsonObj: TJSONObject; virtual; abstract;
    function SwaggerSchema(AddExamples: Boolean = False): TJSONObject; virtual; abstract; deprecated 'Use SwaggerSchemaArray or SwaggerSchemaPaginated instead';
    function SwaggerSchemaArray(AddExamples: Boolean = False): TJSONObject; virtual; abstract;
    function SwaggerSchemaPaginated(AddExamples: Boolean = False): TJSONObject; virtual; abstract;
    function Count: Integer; virtual; abstract;

    function GetPage: Integer; virtual; abstract;
    procedure SetPage(AValue: Integer); virtual; abstract;
    function GetPageSize: Integer; virtual; abstract;
    procedure SetPageSize(AValue: Integer); virtual; abstract;
    function GetTotalRecords: Int64; virtual; abstract;
    procedure SetTotalRecords(AValue: Int64); virtual; abstract;
    function GetTotalPages: Integer; virtual; abstract;
    procedure SetTotalPages(AValue: Integer); virtual; abstract;

    property page: Integer read GetPage write SetPage;
    property page_size: Integer read GetPageSize write SetPageSize;
    property total_records: Int64 read GetTotalRecords write SetTotalRecords;
    property total_pages: Integer read GetTotalPages write SetTotalPages;

    function GetItemObj(AIndex: Integer): TObject; virtual; abstract;
    procedure ClearList; virtual; abstract;
    procedure AddObj(AObj: TObject); virtual; abstract;
    function GetModelClass: TClass; virtual; abstract;
    function NewItem: TObject; virtual; abstract;
  end;

implementation

end.

