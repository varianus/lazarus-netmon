unit NetInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  NetBytes = record
    BytesIn :Int64;
    BytesOut:Int64;
  end;


Function GetConnectionTime:TDateTime;
Function GetNetByte:NetBytes;

implementation
uses
  BaseUnix, unix, DateUtils, strutils;

Const
  PPP0PID:string  = '/var/run/ppp0.pid';
  ProcFile:string  = '/proc/net/dev';

Function GetConnectionTime:TDateTime;
Var
  Info : Stat;
  xTime : TDateTime;
begin
  If fpstat (pointer(PPP0PID),Info) = 0 then
      begin
        XTime  := UnixtoDateTime(Info.st_mtime);
        if tzdaylight then
           Xtime:= Xtime + 2/24
        else
           Xtime:= Xtime + 1/24 ;
        Result := Now - Xtime;
      end
  else
     result := -1;
end;

Function GetNetByte:NetBytes;
Var
  f: TStringList;
  Row : String;
  tmp : string;
  i,j:Integer;
Const
  NetId = '  ppp0: ';
begin

  f:= TStringList.Create;
  try

  f.LoadFromFile(ProcFile);

  result.BytesIn := 0;
  result.BytesOut := 0;
  for i := 0 to f.count - 1 do
    if pos(NetId, f[i]) <> 0 then
     begin
      Row := f[i];
      Delete(Row, 1, Length(NetId));
      Row := DelSpace1(Trim(row));
      Tmp := Copy2SpaceDel(Row);

      if not TryStrToInt64(tmp,result.BytesIn) then
         result.BytesIn := 0;

      for j:= 0 to 6 do
        Tmp := Copy2SpaceDel(Row);

      Tmp := Copy2SpaceDel(Row);
        if not TryStrToInt64(tmp,result.BytesOut) then
           result.BytesOut := 0
     end;
  finally
     FreeAndNil(f);
  end;

end;


end.

