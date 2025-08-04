unit OptiADOHelper;

interface

uses
  System.SysUtils, System.Classes, System.Variants, Data.DB, Data.Win.ADODB;

type
  TOptiADOHelper = class
  private
    // CHANGED: These are now class methods
    class function VarRecToVariant(const V: TVarRec): Variant;
    class procedure BindParams(qry: TADOQuery; const Params: array of const);
  public
    // REMOVED: The constructor is no longer needed.

    // CHANGED: All public methods are now class methods and take a TADOConnection parameter.
    class function QueryInt(AConn: TADOConnection; const SQL: string; const Params: array of const): Integer;
    class function QueryFloat(AConn: TADOConnection; const SQL: string; const Params: array of const): Double;
    class function QueryString(AConn: TADOConnection; const SQL: string; const Params: array of const): string;
    class function QueryPairList(AConn: TADOConnection; const SQL: string; const Params: array of const): TStringList;
    class function ExecSQL(AConn: TADOConnection; const SQL: string; const Params: array of const): Integer;
    class function QueryDataSet(AConn: TADOConnection; const SQL: string; const Params: array of const): TADOQuery; // Caller must Free
  end;

implementation

{ TOptiADOHelper }

class procedure TOptiADOHelper.BindParams(qry: TADOQuery; const Params: array of const);
var
  i: Integer;
begin
  // This logic assumes parameters are named in the SQL (e.g., :pManagerID)
  // and passed as name-value pairs in the Params array.
  if Length(Params) > 0 then
    for i := 0 to (Length(Params) div 2) - 1 do
      qry.Parameters.ParamByName(string(VarRecToVariant(Params[i * 2]))).Value := VarRecToVariant(Params[i * 2 + 1]);
end;

class function TOptiADOHelper.QueryDataSet(AConn: TADOConnection; const SQL: string; const Params: array of const): TADOQuery;
begin
  Result := TADOQuery.Create(nil);
  Result.Connection := AConn;
  Result.SQL.Text := SQL;
  BindParams(Result, Params);
  Result.Open;
end;

class function TOptiADOHelper.ExecSQL(AConn: TADOConnection; const SQL: string; const Params: array of const): Integer;
var
  qry: TADOQuery;
begin
  qry := TADOQuery.Create(nil);
  try
    qry.Connection := AConn;
    qry.SQL.Text := SQL;
    BindParams(qry, Params);
    Result := qry.ExecSQL;
  finally
    qry.Free;
  end;
end;

class function TOptiADOHelper.QueryFloat(AConn: TADOConnection; const SQL: string; const Params: array of const): Double;
var
  qry: TADOQuery;
begin
  qry := TADOQuery.Create(nil);
  try
    qry.Connection := AConn;
    qry.SQL.Text := SQL;
    BindParams(qry, Params);
    qry.Open;
    if not qry.Eof then
      Result := qry.Fields[0].AsFloat
    else
      Result := 0;
  finally
    qry.Free;
  end;
end;

class function TOptiADOHelper.QueryInt(AConn: TADOConnection; const SQL: string; const Params: array of const): Integer;
var
  qry: TADOQuery;
begin
  qry := TADOQuery.Create(nil);
  try
    qry.Connection := AConn;
    qry.SQL.Text := SQL;
    BindParams(qry, Params);
    qry.Open;
    if not qry.Eof then
      Result := qry.Fields[0].AsInteger
    else
      Result := 0;
  finally
    qry.Free;
  end;
end;

class function TOptiADOHelper.QueryPairList(AConn: TADOConnection; const SQL: string; const Params: array of const): TStringList;
var
  qry: TADOQuery;
begin
  Result := TStringList.Create;
  qry := TADOQuery.Create(nil);
  try
    qry.Connection := AConn;
    qry.SQL.Text := SQL;
    BindParams(qry, Params);
    qry.Open;
    while not qry.Eof do
    begin
      Result.Add(qry.Fields[0].AsString + '=' + qry.Fields[1].AsString);
      qry.Next;
    end;
  finally
    qry.Free;
  end;
end;

class function TOptiADOHelper.QueryString(AConn: TADOConnection; const SQL: string; const Params: array of const): string;
var
  qry: TADOQuery;
begin
  qry := TADOQuery.Create(nil);
  try
    qry.Connection := AConn;
    qry.SQL.Text := SQL;
    BindParams(qry, Params);
    qry.Open;
    if not qry.Eof then
      Result := qry.Fields[0].AsString
    else
      Result := '';
  finally
    qry.Free;
  end;
end;

class function TOptiADOHelper.VarRecToVariant(const V: TVarRec): Variant;
begin
  case V.VType of
    vtInteger:      Result := V.VInteger;
    vtBoolean:      Result := V.VBoolean;
    vtChar:         Result := V.VChar;
    vtExtended:     Result := V.VExtended^;
    vtString:       Result := string(V.VString^);
    vtPointer:      Result := Integer(V.VPointer);
    vtPChar:        Result := string(V.VPChar);
    vtObject:       Result := Integer(V.VObject);
    vtClass:        Result := Integer(V.VClass);
    vtWideChar:     Result := V.VWideChar;
    vtPWideChar:    Result := string(V.VPWideChar);
    vtAnsiString:   Result := AnsiString(V.VAnsiString);
    vtCurrency:     Result := V.VCurrency^;
    vtVariant:      Result := V.VVariant^;
    vtInt64:        Result := V.VInt64^;
    vtUnicodeString:Result := UnicodeString(V.VUnicodeString);
  else
    Result := Null;
  end;
end;

end.
