unit DeltaModel.Constants;

{$mode Delphi}{$H+}

interface

const
  { DeltaModel Serialization Format Identifiers }
  FORMAT_JSON = 'json';
  FORMAT_CBOR = 'cbor';
  FORMAT_BSON = 'bson';
  FORMAT_XML  = 'xml';
  FORMAT_CSV  = 'csv';

  { MIME Types }
  MIME_JSON = 'application/json';
  MIME_CBOR = 'application/cbor';
  MIME_BSON = 'application/bson';
  MIME_XML  = 'application/xml';
  MIME_CSV  = 'text/csv';

implementation

end.
