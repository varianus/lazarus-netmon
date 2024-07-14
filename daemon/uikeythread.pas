unit uikeythread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLIntf, DaemonApp, StrUtils, ikeycommon;

type

  { TikeyThread }

  TikeyThread = class(TThread)
  private
    { Private declarations }
    LastScan : TDateTime;
    fComputeTime: boolean;
  protected
    Procedure ConnectionStarted;
    procedure ConnectionStopped;
    procedure Execute; override;
  public
    Owner : TDaemon;
    ikeyData : RikeyData;
    Procedure Initialize(const AInterface: string; ComputeTime: Boolean);
  end;

implementation
uses
  netinfo, eventlog;
{ TikeyThread }

procedure TikeyThread.ConnectionStarted;
begin

end;

procedure TikeyThread.ConnectionStopped;
begin

end;

procedure TikeyThread.Execute;
var
  bLast, bNow: NetBytes;
  currTime: TdateTime;
  Coeff : double;
begin

   ikeyData.ActiveTime:= -1;
   LastScan := -1;
   ikeyData.cnt := 0;

   while not Terminated do
    begin
      inc(ikeyData.cnt);
      if fComputeTime then
        ikeyData.ActiveTime:= GetConnectionTime
      else
        ikeyData.ActiveTime:= 0;

      if ikeyData.ActiveTime <> -1 then
         begin
           bNow := GetNetByte;
           CurrTime := now;
           if lastScan <> -1 then
             begin
               coeff := MSecsPerDay * (currTime - LastScan);
               ikeyData.InSpeed:= (bNow.BytesIn - bLast.BytesIn) * ((1000 / coeff) / 1024) ;
               if ikeyData.InSpeed < 0 then ikeyData.InSpeed := 0;
               ikeyData.OutSpeed:= (bNow.BytesOut - bLast.BytesOut) * ((1000 / coeff) / 1024) ;
               if ikeyData.OutSpeed < 0 then ikeyData.OutSpeed := 0;

               ikeyData.InBytes:= bNow.BytesIn;
               ikeyData.OutBytes:= bNow.BytesOut;
               bLast := bNow;
             end
           else
             begin
               ConnectionStarted;
               bLast := bNow;
               ikeyData.InSpeed:= 0;
               ikeyData.OutSpeed:= 0;
               ikeyData.InBytes:= bNow.BytesIn;
               ikeyData.OutBytes:= bNow.BytesOut;
             end;
           LastScan := CurrTime
         end
       else
         begin
            if lastScan <> -1 then
              ConnectionStopped;
         end;

       Sleep(1000);
    end;

end;

procedure TikeyThread.Initialize(const AInterface: string; ComputeTime: Boolean
  );
begin
  NetId :=  PadLeft(Copy(trim(AInterface),1,6),6) +': ';
end;

end.

