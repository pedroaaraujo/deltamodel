unit DatabaseURLParser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDatabaseConfig = record
    Protocol: string;
    Username: string;
    Password: string;
    Host: string;
    Port: Integer;
    Database: string;
    Charset: string; // Adicionado para suportar charset
  end;

function ParseDatabaseURL(const ADatabaseURL: string): TDatabaseConfig;

implementation

function NormalizeProtocol(const AProtocol: string): string;
begin
  case LowerCase(AProtocol) of
    'sqlite', 'sqlite3': Result := 'SQLite3';
    'firebird', 'fdb': Result := 'Firebird';
    'postgres', 'postgresql': Result := 'PostgreSQL';
    'mysql': Result := 'MySQL 5.7';
    'mysql8', 'mysql80', 'mysql8.0': Result := 'MySQL 8.0';
    'oracle': Result := 'Oracle';
    'odbc': Result := 'ODBC';
    'sybase': Result := 'Sybase';
  else
    raise Exception.CreateFmt('Unsupported protocol: %s', [AProtocol]);
  end;
end;

function ExtractURLParams(const AURL: string; Params: TStringList): string;
var
  QuestionPos: Integer;
  ParamsStr: string;
begin
  QuestionPos := Pos('?', AURL);
  if QuestionPos > 0 then
  begin
    ParamsStr := Copy(AURL, QuestionPos + 1, Length(AURL));
    Params.DelimitedText := ParamsStr;
    Result := Copy(AURL, 1, QuestionPos - 1);
  end
  else
    Result := AURL;
end;

function ParseDatabaseURL(const ADatabaseURL: string): TDatabaseConfig;
var
  URI: string;
  Credentials: string;
  AtPos, ColonPos, SlashPos: Integer;
  Params: TStringList;
begin
  URI := ADatabaseURL;

  // Extract protocol
  ColonPos := Pos('://', URI);
  if ColonPos = 0 then
    raise Exception.Create('Invalid Database URL: Protocol not found');
  Result.Protocol := NormalizeProtocol(Copy(URI, 1, ColonPos - 1));
  Delete(URI, 1, ColonPos + 2);

  // Extract credentials (if any)
  AtPos := Pos('@', URI);
  if AtPos > 0 then
  begin
    Credentials := Copy(URI, 1, AtPos - 1);
    Delete(URI, 1, AtPos);

    ColonPos := Pos(':', Credentials);
    if ColonPos > 0 then
    begin
      Result.Username := Copy(Credentials, 1, ColonPos - 1);
      Result.Password := Copy(Credentials, ColonPos + 1, Length(Credentials));
    end
    else
      Result.Username := Credentials;
  end;

  if URI.Equals(':memory:') then
  begin
    Result.Database := URI;
    Exit;
  end;

  // Extract host and port
  ColonPos := Pos(':', URI);
  SlashPos := Pos('/', URI);
  if ColonPos > 0 then
  begin
    Result.Host := Copy(URI, 1, ColonPos - 1);
    if SlashPos > 0 then
    begin
      Result.Port := StrToIntDef(Copy(URI, ColonPos + 1, SlashPos - ColonPos - 1), 0);
      Delete(URI, 1, SlashPos);
    end
    else
    begin
      Result.Port := StrToIntDef(Copy(URI, ColonPos + 1, Length(URI)), 0);
      URI := '';
    end;
  end
  else
  if SlashPos > 0 then
  begin
    Result.Host := Copy(URI, 1, SlashPos - 1);
    Delete(URI, 1, SlashPos);
  end
  else
  begin
    Result.Host := URI;
    URI := '';
  end;

  Params := TStringList.Create;
  try
    URI := ExtractURLParams(URI, Params);
    Result.Charset := Params.Values['charset']; // Extraindo charset
  finally
    Params.Free;
  end;

  Result.Database := URI;
end;

end.

