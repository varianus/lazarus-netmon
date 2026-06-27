unit NetInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  NetBytes = record
    BytesIn: int64;
    BytesOut: int64;
  end;

const
  NetId: string = '  ppp0: ';
  //  NetId :string = 'enp2s0: ';

function GetConnectionTime: TDateTime;
function GetNetByte: NetBytes;

implementation

uses
  BaseUnix, unix, DateUtils, strutils;

const
  PPP0PID: string = '/var/run/ppp0.pid';
  ProcFile: string = '/proc/net/dev';

function GetConnectionTime: TDateTime;
var
  Info: Stat;
  xTime: TDateTime;
begin
  if fpstat(pointer(PPP0PID), Info) = 0 then
  begin
    XTime := UnixtoDateTime(Info.st_mtime);
    if tzdaylight then
      Xtime := Xtime + 2 / 24
    else
      Xtime := Xtime + 1 / 24;
    Result := Now - Xtime;
  end
  else
    Result := -1;
end;

function GetNetByte: NetBytes;
var
  f: TStringList;
  Row: string;
  tmp: string;
  i, j: integer;
  tmpbytes: NetBytes;
begin

  f := TStringList.Create;
  try

    f.LoadFromFile(ProcFile);
    Result := Default(NetBytes);

    for i := 2 to f.Count - 1 do
      // if pos(NetId, f[i]) <> 0 then
    begin
      Row := f[i];
      //   Delete(Row, 1, Length(NetId));
      Delete(Row, 1, pos(':', row) + 1);
      Row := DelSpace1(Trim(row));
      Tmp := Copy2SpaceDel(Row);

      if not TryStrToInt64(tmp, tmpbytes.BytesIn) then
        tmpbytes.BytesIn := 0;

      for j := 0 to 6 do
        Tmp := Copy2SpaceDel(Row);

      Tmp := Copy2SpaceDel(Row);
      if not TryStrToInt64(tmp, tmpbytes.BytesOut) then
        tmpbytes.BytesOut := 0;
      Inc(Result.BytesIn, tmpbytes.BytesIn);
      Inc(Result.BytesOut, tmpbytes.BytesOut);
    end;
  finally
    FreeAndNil(f);
  end;

end;


end.
