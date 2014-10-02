unit uikeydaemon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, uikeythread, DaemonApp, eventlog, dbus, sqldblib,
  sqlite3conn, dbusextension, ctypes;

type

  { TikeyDaemon }

  TikeyDaemon = class(TDaemon)
    SQLite3Connection1: TSQLite3Connection;
    procedure DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
    procedure DataModuleStop(Sender: TCustomDaemon; var OK: Boolean);
  private
    ikeyThread : TikeyThread;
    tm: TDBUSThread;
    Connection : PDBusConnection;
    procedure ActivateDBUS;
    Procedure OnThreadTerminate(Aobject: TObject);
  public
    { public declarations }
  end;

var
  ikeyDaemon: TikeyDaemon;

implementation
uses
  ikeyCommon;

const
  BUS_NAME =  'org.marcocaselli.ikeymonitor';

procedure RegisterDaemon;
begin
  RegisterDaemonClass(TikeyDaemon)
end;

{$R *.lfm}


function DBUS_filter_func(connection: PDBusConnection; message_: PDBusMessage; user_data: Pointer): DBusHandlerResult; cdecl;
const
  path1 = '/org/marcocaselli/ikeymonitor';
var
  property_ : PChar;
  s_val: PChar;
  x_val: int64;
  b_val: dword;
  d_Val: double;

  Error: PDBusError;
  reply_message: PDBusMessage;
  sub0, sub1, sub2, sub3, sub4: DBusMessageIter;
  message_type: cint;
  Data : RIkeyData;
begin
  message_type := dbus_message_get_type(message_);

  s_val := dbus_message_get_path(message_);
  if (strcomp(dbus_message_get_path(message_), path1) = 0) then
    begin
       if (dbus_message_is_method_call(message_, pchar(BUS_NAME+'.method'), 'GetData')) > 0 then
          begin
          Data := TikeyThread(user_data).ikeyData;
          reply_message := dbus_message_new_method_return(message_);
          dbus_message_iter_init_append(reply_message, @sub0);
          d_val := data.ActiveTime;
          dbus_message_iter_append_basic(@sub0, DBUS_TYPE_DOUBLE, @d_val );
          x_val := data.InBytes;
          dbus_message_iter_append_basic(@sub0, DBUS_TYPE_INT64, @x_val );
          x_val := data.OutBytes;
          dbus_message_iter_append_basic(@sub0, DBUS_TYPE_INT64, @x_val );
          d_val := data.InSpeed;
          dbus_message_iter_append_basic(@sub0, DBUS_TYPE_DOUBLE, @d_val );
          d_val := data.outSpeed;
          dbus_message_iter_append_basic(@sub0, DBUS_TYPE_DOUBLE, @d_val );
          x_val := data.cnt;
          dbus_message_iter_append_basic(@sub0, DBUS_TYPE_INT64, @x_val );
          dbus_connection_send(connection, reply_message, nil);
          dbus_message_unref(reply_message);
          Result := DBUS_HANDLER_RESULT_HANDLED;
          exit;

      end;

    end;


end;

{ TikeyDaemon }

procedure TikeyDaemon.ActivateDBUS;
var
  Error: DBusError;
  Match, Path: PChar;
  ret: HRESULT;
begin

  dbus_error_init(@error);
  Connection := dbus_bus_get(DBUS_BUS_Session, @error);
//  dbus_connection_setup_with_g_main(Connection, nil);
  tm := TDBUSThread.Create(true);

  if (Connection = nil) then
    begin
      logger.Log(format('Failed to open connection to %s message bus: %s', ['DBUS_BUS_SESSION', error.message]));
      dbus_error_free(@error);
      exit;
    end;

  path := BUS_NAME;
  ret := dbus_bus_request_name(Connection, path, DBUS_NAME_FLAG_REPLACE_EXISTING, @error);
  if dbus_error_is_set(@error) <> 0 then
  begin
    logger.Log(format('Failed to open connection to %s message bus: %s', ['DBUS_BUS_SESSION', error.message]));
    dbus_error_free(@error);
    exit;
  end;


  if ret <> DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER then Exit;

  dbus_connection_add_filter(Connection, @DBUS_filter_func, ikeyThread, nil);
  dbus_connection_flush(Connection);
  tm.fBus:= Connection;
  tm.Start;

end;

procedure TikeyDaemon.DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
begin
  Logger.Active:= false;
  Logger.LogType:=ltFile;
  Logger.Active:= True;
  Logger.Log('Start');

  ikeyThread := TikeyThread.Create(True);
  ikeyThread.FreeOnTerminate:= true;
  ikeyThread.OnTerminate:=@OnThreadTerminate;
  ikeyThread.Owner := self;
  ikeyThread.Start;
  ActivateDBUS;
  ok := true;
  Logger.Log('Resuming');

end;

procedure TikeyDaemon.DataModuleStop(Sender: TCustomDaemon; var OK: Boolean);
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

