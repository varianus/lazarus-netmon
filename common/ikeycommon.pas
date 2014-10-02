unit ikeycommon;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils;

type

  RIkeyData = record
    ActiveTime : TDateTime;
    InBytes    : Int64;
    OutBytes   : int64;
    InSpeed    : single;
    outSpeed   : single;
    cnt        : int64;
  end;

implementation


end.

