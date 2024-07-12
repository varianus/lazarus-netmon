unit uikeydaemon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, uikeythread, DaemonApp, eventlog, dbus,
  dbusextension, ctypes;

type

  { TikeyDaemon }

  TikeyDaemon = class(TDaemon)
    procedure DataModuleStart(Sender: TCustomDaemon; var OK: boolean);
    procedure DataModuleStop(Sender: TCustomDaemon; var OK: boolean);
  private
    ikeyThread: TikeyThread;
    tm: TDBUSThread;
    Connection: PDBusConnection;
    procedure ActivateDBUS;
    procedure OnThreadTerminate(Aobject: TObject);
  public
    { public declarations }
  end;

var
  ikeyDaemon: TikeyDaemon;

implementation

uses
  ikeyCommon;

const
  BUS_NAME = 'org.marcocaselli.ikeymonitor';

const
  INTROSPECT_XML = '<!DOCTYPE node PUBLIC "-//freedesktop//DTD D-BUS Object Introspection 1.0//EN" '
                 + ' "http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd">               '
                 + '<node>                                                                         '
                 + '<interface name="org.freedesktop.DBus.Introspectable">                         '
                 + '  <method name="Introspect">                                                   '
                 + '   <arg name="data" direction="out" type="s"/>                                 '
                 + ' </method>                                                                     '
                 + '</interface>                                                                   '
                 + '  <interface name="org.marcocaselli.ikeymonitor">                             '
                 + '    <method name="GetData">                                                    '
                 + '      <arg name="ActiveTime" type="d" direction="out"/>                        '
                 + '      <arg name="InBytes" type="x" direction="out"/>                           '
                 + '      <arg name="OutBytes" type="x" direction="out"/>                          '
                 + '      <arg name="InSpeed" type="d" direction="out"/>                           '
                 + '      <arg name="outSpeed" type="d" direction="out"/>                          '
                 + '      <arg name="Count" type="x" direction="out"/>                             '
                 + '    </method>                                                                  '
                 + '  </interface>                                                                 '
                 + '</node>                                                                        ';

procedure RegisterDaemon;
begin
  RegisterDaemonClass(TikeyDaemon);
end;

{$R *.lfm}


function DBUS_filter_func(connection: PDBusConnection; message_: PDBusMessage; user_data: Pointer): DBusHandlerResult; cdecl;
const
  path1 = 'org.marcocaselli.ikeymonitor';
var
  property_: PChar;
  s_val: PChar;
  x_val: int64;
  b_val: dword;
  d_Val: double;

  Error: PDBusError;
  reply_message: PDBusMessage;
  sub0, sub1, sub2, sub3, sub4: DBusMessageIter;
  message_type: cint;
  Data: RIkeyData;
  xml: string;
begin
  message_type := dbus_message_get_type(message_);

  if (dbus_message_is_method_call(message_, 'org.freedesktop.DBus.Introspectable', 'Introspect')) > 0 then
  begin
    xml := INTROSPECT_XML;
    s_val := PChar(xml);
    reply_message := dbus_message_new_method_return(message_);
    dbus_message_append_args(reply_message,
      DBUS_TYPE_STRING, [@s_val, DBUS_TYPE_INVALID]);
    dbus_connection_send(connection, reply_message, nil);
    dbus_message_unref(reply_message);
    Result := DBUS_HANDLER_RESULT_HANDLED;
    exit;
  end;

  if (strcomp(dbus_message_get_path(message_), path1) = 0) or
     (strcomp(dbus_message_get_path(message_), '/') = 0) then
  begin

    if (dbus_message_is_method_call(message_, PChar(BUS_NAME), 'GetData')) > 0 then
    begin
      Data := TikeyThread(user_data).ikeyData;
      reply_message := dbus_message_new_method_return(message_);
      dbus_message_iter_init_append(reply_message, @sub0);
      d_val := Data.ActiveTime;
      dbus_message_iter_append_basic(@sub0, DBUS_TYPE_DOUBLE, @d_val);
      x_val := Data.InBytes;
      dbus_message_iter_append_basic(@sub0, DBUS_TYPE_INT64, @x_val);
      x_val := Data.OutBytes;
      dbus_message_iter_append_basic(@sub0, DBUS_TYPE_INT64, @x_val);
      d_val := Data.InSpeed;
      dbus_message_iter_append_basic(@sub0, DBUS_TYPE_DOUBLE, @d_val);
      d_val := Data.outSpeed;
      dbus_message_iter_append_basic(@sub0, DBUS_TYPE_DOUBLE, @d_val);
      x_val := Data.cnt;
      dbus_message_iter_append_basic(@sub0, DBUS_TYPE_INT64, @x_val);
      dbus_connection_send(connection, reply_message, nil);
      dbus_message_unref(reply_message);
      Result := DBUS_HANDLER_RESULT_HANDLED;
      exit;

    end;

  end;
  Result := DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
end;

{ TikeyDaemon }

procedure TikeyDaemon.ActivateDBUS;
var
  Error: DBusError;
  Match, Path: PChar;
  ret: HRESULT;
begin

  dbus_error_init(@error);
  Connection := dbus_bus_get(DBUS_BUS_SESSION, @error);
  //  dbus_connection_setup_with_g_main(Connection, nil);
  tm := TDBUSThread.Create(True);

  if (Connection = nil) then
  begin
    logger.Log(etError, format('Failed to open connection to %s message bus: %s', ['DBUS_BUS_SESSION', error.message]));
    dbus_error_free(@error);
    exit;
  end;

  path := BUS_NAME;
  ret := dbus_bus_request_name(Connection, path, DBUS_NAME_FLAG_REPLACE_EXISTING, @error);
  if dbus_error_is_set(@error) <> 0 then
  begin
    logger.Log(etError, format('Failed to open connection to %s message bus: %s', ['DBUS_BUS_SESSION', error.message]));
    dbus_error_free(@error);
    exit;
  end;


  if ret <> DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER then
  begin
    logger.Log(etError, format('OWNER ERROR %d message bus: %s', [ret, error.message]));
    Exit;
  end;

  dbus_connection_add_filter(Connection, @DBUS_filter_func, ikeyThread, nil);
  dbus_connection_flush(Connection);
  tm.fBus := Connection;
  tm.Start;

end;

procedure TikeyDaemon.DataModuleStart(Sender: TCustomDaemon; var OK: boolean);
begin
  Logger.Active := False;
  Logger.LogType := ltFile;
  Logger.Active := True;
  Logger.Log(etInfo, 'Start');

  ikeyThread := TikeyThread.Create(True);
  ikeyThread.FreeOnTerminate := True;
  ikeyThread.OnTerminate := @OnThreadTerminate;
  ikeyThread.Owner := self;
  ikeyThread.Start;
  ActivateDBUS;
  ok := True;
  Logger.Log(etInfo, 'Resuming');

end;

procedure TikeyDaemon.DataModuleStop(Sender: TCustomDaemon; var OK: boolean);
begin
  if Assigned(Connection) then
  begin
    tm.Terminate;
    dbus_connection_unref(Connection);
    Connection := nil;
    ikeyThread.Terminate;
    ikeyThread.WaitFor;
    FreeAndNil(ikeyThread);
  end;

end;

procedure TikeyDaemon.OnThreadTerminate(Aobject: TObject);
begin
  ikeyThread := nil;
end;


initialization
  RegisterDaemon;
end.
