program netmon;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this },
  DateUtils, Strutils, dbus,
  ikeycommon;

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
  tmp    : String;
  Connection : PDBusConnection;
  ret: Hresult;
  reply, msg : PDBusMessage;
  args: DBusMessageIter;
  Data: RIkeyData;
  x_val: int64;
  d_Val: double;
  error: DBusError;

const
  BUS_NAME =  'org.marcocaselli.ikeymonitor';
  path1 = '/org/marcocaselli/ikeymonitor';

begin
  try
    dbus_error_init(@error);
    Connection := dbus_bus_get(DBUS_BUS_Session, @error);
    ret := dbus_bus_request_name(Connection, pchar('test.ikey.caller'), DBUS_NAME_FLAG_REPLACE_EXISTING, @error);
    if dbus_error_is_set(@error) <> 0 then
      begin
        WriteLn('<txt> Not Connected '+ error.message+ '</txt>');
        dbus_error_free(@error);
        exit;
      end;

      if ret <> DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER then Exit;
      msg := dbus_message_new_method_call(BUS_NAME, // target for the method call
                                          path1, // object to call on
                                          pchar(BUS_NAME+'.method'), // interface to call on
                                          'GetData'); // method name

      Reply := dbus_connection_send_with_reply_and_block(Connection, msg, -1, @Error);

      if dbus_error_is_set(@error) <> 0 then
        begin
          WriteLn('<txt> No Service </txt>');
          dbus_error_free(@error);
          exit;
        end;

      if (dbus_message_iter_init(Reply, @args) > 0) then
         begin
            dbus_message_iter_get_basic(@args, @d_val);
            data.ActiveTime:= d_Val;
            dbus_message_iter_next(@args);
            dbus_message_iter_get_basic(@args, @x_val);
            data.InBytes:= x_Val;
            dbus_message_iter_next(@args);
            dbus_message_iter_get_basic(@args, @x_val);
            data.OutBytes:= x_Val;
            dbus_message_iter_next(@args);
            dbus_message_iter_get_basic(@args, @d_val);
            data.InSpeed:= d_Val;
            dbus_message_iter_next(@args);
            dbus_message_iter_get_basic(@args, @d_val);
            data.outSpeed:= d_Val;
            dbus_message_iter_next(@args);
            dbus_message_iter_get_basic(@args, @x_val);
            data.cnt:= x_Val;
         end;

      dbus_connection_flush(Connection);

      dbus_message_unref(msg);
      dbus_message_unref(Reply);

      dbus_connection_unref(Connection);

      if Data.ActiveTime = -1 then
         begin
           writeln('<txt> Not Connected</txt>');
           terminate;
           exit;
         end
      else
        begin
          if MinuteOf(Data.ActiveTime) in [14,29,44,59] then
            begin
              if odd(data.cnt) then
                writeln('<img>/usr/share/icons/hicolor/24x24/status/xfpm-brightness-lcd-invalid.png</img>')
             else
                writeln('<img>/usr/share/icons/hicolor/24x24/status/xfpm-brightness-lcd.png</img>');
            end
          else
             writeln('<img>/usr/share/icons/elementary-xfce/actions/24/help-info.png</img>');
        write('<click>');
        write('/home/varianus/source/netmon/gui/guinetmon');
        writeln('</click>');
        WriteLn (format('<txt>%S '+
                      '↓%3.2f ↑%3.2f '+
                      '</txt>', [
                      TimeToStr(Data.ActiveTime),
                      Data.InSpeed,
                      Data.outSpeed
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

