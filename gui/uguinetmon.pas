unit uguinetmon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASources, TASeries, TAFuncSeries,
  TAChartExtentLink, TATransformations, TAStyles, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, ikeycommon, dbus, ctypes,
  TACustomSource, SMS;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart1LineSeries2: TLineSeries;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    stConnTime: TStaticText;
    stSpeedin: TStaticText;
    stDataIn: TStaticText;
    stSpeedOut: TStaticText;
    stDataOut: TStaticText;
    Stats: TTabSheet;
    TabSheet1: TTabSheet;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
    procedure Chart1FuncSeries2Calculate(const AX: Double; out AY: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure stDataInClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure UserDefinedChartSource1GetChartDataItem(
      ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
  private
    Connection : PDBusConnection;
    Data: RIkeyData;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
const
  BUS_NAME =  'org.marcocaselli.ikeymonitor';
  path1 = '/org/marcocaselli/ikeymonitor';

{ TForm1 }

procedure TForm1.Timer1Timer(Sender: TObject);
var
  tmp    : String;
  ret: Hresult;
  reply, msg : PDBusMessage;
  args: DBusMessageIter;
  x_val: int64;
  d_Val: double;
  error: DBusError;

begin
  try
      dbus_error_init(@error);
      msg := dbus_message_new_method_call(BUS_NAME, // target for the method call
                                          '/', // object to call on
                                          pchar(BUS_NAME), // interface to call on
                                          'GetData'); // method name

      Reply := dbus_connection_send_with_reply_and_block(Connection, msg, -1, @Error);

      if dbus_error_is_set(@error) <> 0 then
        begin
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

      if Data.ActiveTime = -1 then
         begin
           Chart1LineSeries1.Add(0);
           Chart1LineSeries2.Add(0);
           if Chart1LineSeries1.Count > 100 then
              begin
                Chart1LineSeries1.Delete(0);
                Chart1LineSeries2.Delete(0);
              end;

         end
      else
        begin
           stConnTime.Caption := TimeToStr(Data.ActiveTime);
           stSpeedin.Caption := format('%3.2f kbps',[Data.inSpeed]);
           stSpeedOut.Caption := format('%3.2f kbps',[Data.OutSpeed]);
           stDataOut.Caption := format('%3.2f Mb',[Data.OutBytes / (1024 *1024)]);
           stDataIn.Caption := format('%3.2f Mb',[Data.InBytes / (1024 *1024)]);
           Chart1LineSeries1.Add(Data.inSpeed);
           Chart1LineSeries2.Add(Data.outSpeed);
           if Chart1LineSeries1.Count > 300 then
              begin
                Chart1LineSeries1.Delete(0);
                Chart1LineSeries2.Delete(0);
              end;
           Panel1.Invalidate;
       end;
      dbus_error_free(@error);
  // stop program loop
  except
    exit;
  end;


end;

procedure TForm1.UserDefinedChartSource1GetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin

end;

procedure TForm1.stDataInClick(Sender: TObject);
begin

end;

procedure TForm1.FormCreate(Sender: TObject);
var
  error: DBusError;
  ret : Hresult;
  i: Integer;
begin

  for i := 0 to 100 do
    begin

      Chart1LineSeries1.Add(0);
      Chart1LineSeries2.Add(0);
    end;


    dbus_error_init(@error);
    Connection := dbus_bus_get(DBUS_BUS_Session, @error);
    ret := dbus_bus_request_name(Connection, pchar('test.ikeygui.caller'), DBUS_NAME_FLAG_REPLACE_EXISTING, @error);
    if dbus_error_is_set(@error) <> 0 then
      begin
        WriteLn('<txt> Not Connected '+ error.message+ '</txt>');
        dbus_error_free(@error);
        exit;
      end;

      if ret <> DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER then Exit;

end;

procedure TForm1.Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
begin
end;

procedure TForm1.Button1Click(Sender: TObject);
var

begin

end;

procedure TForm1.Chart1FuncSeries2Calculate(const AX: Double; out AY: Double);
begin
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  dbus_connection_unref(Connection);
end;

end.

