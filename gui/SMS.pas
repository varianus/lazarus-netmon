unit sms;

(*
   TModemEngine v0.1
   created 28/05/2012
   by
     I Made Purnama Yasa

   Features:
   * Send SMS
   * Read SMS

   History
   -
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Registry, Forms, synaser, gsmsms;

type

  TModemEngine = class(TObject)
  private
    FSerial: TBlockSerial;

    // Hardware
    FPort: String;
    FBaudrate: Integer;
    FOpen: Boolean;

    function CleanATString(ATString: AnsiString): AnsiString;
    function SendAndWait(ATCommand, WaitStr: AnsiString; const TimeOut: Integer = 1000): AnsiString;
  protected
  public
    constructor Create; overload;

    // Hardware
    procedure SetOpen(Value: Boolean);
    function GetOpen: boolean;
    function GetManufacturedID: String;
    function GetModelID: String;
    function GetModemVersion: String;
    function GetIMEI: String;

    function SendSMS(Number, Message: string): boolean;
    function ReadSMS: TStringList;
    function ReadSMSIndex(Index: Integer): AnsiString;
    function ListPorts: TStringList;

    function ListSMSIndex: TStringList;
  published

    // Hardware
    property Port: String read FPort write FPort;
    property Baudrate: Integer read FBaudrate write FBaudrate;
    property Open: Boolean read GetOpen write SetOpen;
    property ManufacturedID: String read GetManufacturedID;
    property ModelID: String read GetModelID;
    property ModemVersion: String read GetModemVersion;
    property IMEI: String read GetIMEI;
  end;

implementation

constructor TModemEngine.Create;
begin
  inherited;
  FPort := 'COM1';
  FBaudrate := 9600;
  FOpen := False;
end;

function TModemEngine.CleanATString(ATString: AnsiString): AnsiString;
begin
  ATString := StringReplace(ATString, #10, EmptyStr, [rfReplaceAll, rfIgnoreCase]);
  ATString := StringReplace(ATString, #13, EmptyStr, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(ATString, 'OK', EmptyStr, [rfReplaceAll, rfIgnoreCase]);
end;

function TModemEngine.SendAndWait(ATCommand, WaitStr: AnsiString; const TimeOut: Integer = 1000): AnsiString;
var
  TmpStr: AnsiString;
begin
  try
    FSerial.SendString(ATCommand + #$0D);
    repeat
      Application.ProcessMessages;
      TmpStr := FSerial.RecvString(TimeOut);
      if LeftStr(TmpStr,Length(ATCommand)) <> ATCommand then
        Result := Result + TmpStr;

      if TmpStr = WaitStr then
        Break
      else if TmpStr = 'ERROR' then
        Break;
    until FSerial.LastError <> sOK;

    Result := CleanATString(Result);
  except
    Result := EmptyStr;
  end;
end;

function TModemEngine.ListPorts: TStringList;
var
  reg: TRegistry;
  st: TStrings;
  i: integer;
begin
  Result := TStringList.Create;
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.OpenKey('hardware\devicemap\serialcomm', False);
    st := TStringList.Create;
    try
      reg.GetValueNames(st);
      for i := 0 to st.Count - 1 do
        Result.Add(reg.Readstring(st.strings[i]));
    finally
      st.Free;
    end;
    reg.CloseKey;
  finally
    reg.Free;
  end;
end;

procedure TModemEngine.SetOpen(Value: Boolean);
begin
  try
    if Value then
    begin
      FSerial := TBlockSerial.Create;
      FSerial.Config(FBaudrate, 8, 'N', 1, False, False);
      FSerial.Connect(FPort);
      FOpen := True;
    end
    else
    begin
      FSerial.CloseSocket;
      FSerial.Free;
      FOpen := False;
    end;
  except
    FOpen := False;
  end;
end;

function TModemEngine.GetOpen: boolean;
begin
  Result := FOpen;
end;

function TModemEngine.GetManufacturedID: String;
begin
  try
    Result := SendAndWait('AT+CGMI','OK');
  except
    Result := 'N/A';
  end;
end;

function TModemEngine.GetModelID: String;
begin
  try
    Result := SendAndWait('AT+CGMM','OK');
  except
    Result := 'N/A';
  end;
end;

function TModemEngine.GetModemVersion: String;
begin
  try
    Result := SendAndWait('AT+CGMR','OK');
  except
    Result := 'N/A';
  end;
end;

function TModemEngine.GetIMEI: String;
begin
  try
    Result := SendAndWait('AT+CGSN','OK');
  except
    Result := 'N/A';
  end;
end;

function TModemEngine.SendSMS(Number, Message: string): boolean;
var
  SMS: TSMS;
  PDU: string;
begin
  Result := False;
  SMS := TSMS.Create;
  try
    SMS.Text := Message;
    SMS.Number := Number;
    SMS.RequestReply := False;
    SMS.FlashSMS := False;
    SMS.StatusRequest := False;
    SMS.UDHI := '';
    SMS.dcs := -1;
    PDU := SMS.PDU;

    FSerial.ATCommand('AT+CMGF=0');
    FSerial.ATCommand('AT+CMGS=' + IntToStr(SMS.TPLength));
    FSerial.ATCommand(PDU + #$1A);
    Result := True;
  finally
    SMS.Free;
  end;
end;

function TModemEngine.ReadSMS: TStringList;
var
  sl: TStringList;
  tgl, s: ansistring;
  SMS: TSMS;
  i: integer;
begin
  Result := TStringList.Create;
  sl := TStringList.Create;
  SMS := TSMS.Create;
  FSerial.ATCommand('AT+CPMS=MT');
  i := 0;
  try
    FSerial.SendString('AT+CMGL=4' + #$0D);
    repeat
      Application.ProcessMessages;
      s := FSerial.RecvString(FSerial.AtTimeout);
      sl.Add(s);
      if s = 'OK' then
        Break
      else if s = 'ERROR' then
        Break;
    until FSerial.LastError <> sOK;

    while i < sl.Count do
    begin
      Application.ProcessMessages;
      s := sl.Strings[i];
      if copy(s, 1, 7) = '+CMGL: ' then
      begin
        Inc(i);
        s := sl.Strings[i];
        sms.PDU := s;
        if sms.TimeStamp > 0 then
          tgl := DateTimeToStr(sms.TimeStamp)
        else
          tgl := '-';
        Result.Add(tgl + ':' + SMS.Number + ':' + SMS.Text);
      end;
      Inc(i);
    end;
  finally
    sl.Free;
    SMS.Free;
  end;
end;

function TModemEngine.ReadSMSIndex(Index: Integer): AnsiString;
var
  sl: TStringList;
  tgl, s: ansistring;
  SMS: tSMS;
  i: integer;
begin
  sl := TStringList.Create;
  SMS := TSMS.Create;
  FSerial.ATCommand('AT+CMGF=0');
  FSerial.ATCommand('AT+CPMS=MT');
  i := 0;
  try
    FSerial.SendString('AT+CMGR=' + IntToStr(Index) + #$0D);
    repeat
      Application.ProcessMessages;
      s := FSerial.RecvString(FSerial.AtTimeout);
      sl.Add(s);
      if s = 'OK' then
        Break
      else if s = 'ERROR' then
        Break;
    until FSerial.LastError <> sOK;

    while i < sl.Count do
    begin
      Application.ProcessMessages;
      s := sl.Strings[i];
      if copy(s, 1, 7) = '+CMGR: ' then
      begin
        Inc(i);
        s := sl.Strings[i];
        sms.PDU := s;
        if sms.TimeStamp > 0 then
          tgl := DateTimeToStr(sms.TimeStamp)
        else
          tgl := '-';
        Result := tgl + ':' + SMS.Number + ':' + SMS.Text;
      end;
      Inc(i);
    end;
  finally
    sl.Free;
    SMS.Free;
  end;
end;

function TModemEngine.ListSMSIndex: TStringList;
var
  tmpStr: ansistring;
begin
  Result := TStringList.Create;
  SendAndWait('AT+CMGF=0','OK');
  SendAndWait('AT+CPMS=MT','OK');
  try
    FSerial.SendString('AT+CMGL=4' + #$0D);
    repeat
      Application.ProcessMessages;
      tmpStr := FSerial.RecvString(FSerial.AtTimeout);
      if copy(tmpStr, 1, 7) = '+CMGL: ' then
        Result.Add(copy(tmpStr, 8, pos(',', tmpStr) - 8));
      if tmpStr = 'OK' then
        Break
      else if tmpStr = 'ERROR' then
        Break;
    until FSerial.LastError <> sOK;
  finally
  end;
end;

end.