Program ikeymonitor;

{$DEFINE UseCThreads}
Uses

{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  DaemonApp, lazdaemonapp, uikeymapper, uikeydaemon, uikeythread, netinfo,
  ikeycommon;

begin
  Application.Title:='Daemon application';
  Application.Initialize;
  Application.Run;
end.
