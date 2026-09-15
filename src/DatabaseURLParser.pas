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
    Charset: string;
    Params: TStringList;
  end;

function ParseDatabaseURL(const ADatabaseURL: string): TDatabaseConfig;

implementation

function NormalizeProtocol(const AProtocol: string): string;
begin
  case LowerCase(Trim(AProtocol)) of
    'sqlite', 'sqlite3':
      Result := 'SQLite3';
    'firebird', 'fdb', 'interbase':
      Result := 'Firebird';
    'postgres', 'postgresql', 'pgsql':
      Result := 'PostgreSQL';
    'mysql':
      Result := 'MySQL 5.7';
    'mysql8', 'mysql80', 'mysql8.0':
      Result := 'MySQL 8.0';
    'mariadb':
      Result := 'MariaDB';
    'mssql', 'mssqlserver', 'sqlserver':
      Result := 'MSSQLServer';
    'oracle', 'ora':
      Result := 'Oracle';
    'odbc':
      Result := 'ODBC';
    'sybase':
      Result := 'Sybase';
  else
    raise Exception.CreateFmt('Unsupported database protocol: %s', [AProtocol]);
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
    Params.Delimiter := '&';
    Params.StrictDelimiter := True;
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
  UrlParams: TStringList;
  IsTripleSlash: Boolean;
begin
  Result.Protocol := '';
  Result.Username := '';
  Result.Password := '';
  Result.Host     := '';
  Result.Port     := 0;
  Result.Database := '';
  Result.Charset  := '';
  Result.Params   := TStringList.Create;

  URI := ADatabaseURL;

  // Extract protocol
  ColonPos := Pos('://', URI);
  if ColonPos = 0 then
    raise Exception.Create('Invalid Database URL: Protocol not found');

  Result.Protocol := NormalizeProtocol(Copy(URI, 1, ColonPos - 1));

  IsTripleSlash := Pos(':///', URI) > 0;

  Delete(URI, 1, ColonPos + 2); // remove "://"

  // SQLite special cases: in-memory or direct file path
  if (Result.Protocol = 'SQLite3') then
  begin
    if (URI = ':memory:') or (URI = '/:memory:') then
    begin
      Result.Database := ':memory:';
      Exit;
    end;

    // sqlite:////path/to/db.sqlite or sqlite:///path/to/db.sqlite or sqlite://db.sqlite
    UrlParams := TStringList.Create;
    try
      URI := ExtractURLParams(URI, UrlParams);
      Result.Charset := UrlParams.Values['charset'];
      Result.Params.Assign(UrlParams);
    finally
      UrlParams.Free;
    end;

    if IsTripleSlash then
    begin
      if (Length(URI) > 1) and (URI[1] = '/') and (URI[2] = '/') then
        Result.Database := Copy(URI, 2, MaxInt)
      else if (Length(URI) > 0) and (URI[1] = '/') then
        Result.Database := Copy(URI, 2, MaxInt)
      else
        Result.Database := URI;
    end
    else
      Result.Database := URI;

    Exit;
  end;

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

  // Extract query params (?param=value&...)
  UrlParams := TStringList.Create;
  try
    URI := ExtractURLParams(URI, UrlParams);
    Result.Charset := UrlParams.Values['charset'];
    Result.Params.Assign(UrlParams);
  finally
    UrlParams.Free;
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
      Result.Database := URI;
    end
    else
    begin
      Result.Port := StrToIntDef(Copy(URI, ColonPos + 1, Length(URI)), 0);
      Result.Database := '';
    end;
  end
  else
  if SlashPos > 0 then
  begin
    Result.Host := Copy(URI, 1, SlashPos - 1);
    Delete(URI, 1, SlashPos);
    Result.Database := URI;
  end
  else
  begin
    Result.Host := URI;
    Result.Database := '';
  end;
end;

end.