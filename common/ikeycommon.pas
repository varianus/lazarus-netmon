unit ikeycommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  RIkeyData = record
    ActiveTime: TDateTime;
    InBytes: int64;
    OutBytes: int64;
    InSpeed: int64;
    outSpeed: int64;
    cnt: int64;
  end;

type
  TByteStringFormat = (bsfDefault, bsfBytes, bsfKB, bsfMB, bsfGB, bsfTB);

function FormatByteString(Bytes: uint64; Format: TByteStringFormat = bsfDefault): string;

implementation

const
  OneKB = 1024;
  OneMB = OneKB * OneKB;
  OneGB = OneKB * OneMB;
  OneTB = OneKB * OneGB;

  OneHour = MinsPerHour * SecsPerMin * MSecsPerSec;

function FormatByteString(Bytes: uint64; Format: TByteStringFormat = bsfDefault): string;
begin
  if Format = bsfDefault then
  begin
    if Bytes < OneKB then
    begin
      Format := bsfBytes;
    end
    else if Bytes < OneMB then
    begin
      Format := bsfKB;
    end
    else if Bytes < OneGB then
    begin
      Format := bsfMB;
    end
    else if Bytes < OneTB then
    begin
      Format := bsfGB;
    end
    else
    begin
      Format := bsfTB;
    end;
  end;

  case Format of
    bsfKB:
      Result := SysUtils.Format('%6.2f K', [Bytes / OneKB]);
    bsfMB:
      Result := SysUtils.Format('%6.2f M', [Bytes / OneMB]);
    bsfGB:
      Result := SysUtils.Format('%6.2f G', [Bytes / OneGB]);
    bsfTB:
      Result := SysUtils.Format('%6.2f T', [Bytes / OneTB]);
    else  // bsfBytes:
      Result := SysUtils.Format('%6.0f b', [Bytes/1]);

  end;
end;

end.
