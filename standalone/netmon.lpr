program netmon;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this },
  DateUtils, Strutils, NetInfo;

type
  { TNetMonitor }

  TNetMonitor = class(TCustomApplication)
  private
    procedure ShowData;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TNetMonitor }



procedure TNetMonitor.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;


  ShowData;
  Terminate;

end;

procedure TNetMonitor.ShowData;
var

  R1, r2 : NetBytes;
  tmp    : String;
  CurrTime : TDateTime;
  x: longint;
begin
  try
  CurrTime := GetConnectionTime;
  if CurrTime = -1 then
     begin
//       writeln('<img>/usr/share/icons/elementary-xfce/actions/24/sleep.png</img>');
//       writeln('<img>/usr/share/icons/elementary-xfce/actions/24/sleep.png</img>');

       writeln('<txt> Not Connected</txt>');
       terminate;
       exit;
     end
  else
    begin
      tmp:=  TimeToStr(CurrTime);
      r1:= GetNetByte;
      Sleep(500);
      r2:= GetNetByte;
      writeln('<click></click>');
      if MinuteOf(CurrTime) in [14,29,44,59] then
         begin
           if odd(SecondOf(CurrTime)) then
              writeln('<img>/usr/share/icons/hicolor/24x24/status/xfpm-brightness-lcd-invalid.png</img>')
           else
             writeln('<img>/usr/share/icons/hicolor/24x24/status/xfpm-brightness-lcd.png</img>');
         end
      else
         writeln('<img>/usr/share/icons/elementary-xfce/actions/24/help-info.png</img>');

      WriteLn (format('<txt>%S '+
                      '↓%3.2f ↑%3.2f '+
                      '</txt>', [
                      tmp,
                      (r2.Bytesin - r1.bytesIn) * (2 / 1024),
                      (r2.BytesOut - r1.bytesout) * (2 / 1024)
                      ]));

    end;

  // stop program loop
  except
    writeln('<img>/usr/share/icons/elementary-xfce/actions/24/sleep.png</img>');
    writeln('<txt>Not Connected</txt>');
    terminate;
    exit;
  end;

end;

constructor TNetMonitor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TNetMonitor.Destroy;
begin
  inherited Destroy;
end;

procedure TNetMonitor.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TNetMonitor;

{$R *.res}

begin
  Application:=TNetMonitor.Create(nil);
  Application.Run;
  Application.Free;
end.

