unit uikeymapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DaemonApp;

type
  TikeyMapper = class(TDaemonMapper)
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ikeyMapper: TikeyMapper;

implementation

procedure RegisterMapper;
begin
  RegisterDaemonMapper(TikeyMapper)
end;

{$R *.lfm}


initialization
  RegisterMapper;
end.

