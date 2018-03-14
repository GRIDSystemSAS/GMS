program GMSConsole;

{$APPTYPE CONSOLE}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}


uses

  {$IFDEF FPC}
  {$IFDEF UNIX}
  cthreads,
  cmem,
  {$ENDIF}
  SysUtils, classes,
  {$ELSE}
  {$IFDEF WIN32}
  FastMM4,
  {$ENDIF }
  System.SysUtils,
  System.Classes,
  {$ENDIF }
  GS.Threads,
  GS.Bus,
  GS.Task,
  GMS.Server in '..\..\..\..\Sources\Pascal\Server\GMS.Server.pas',
  GMS.Server.Types.Core in '..\..\..\..\Sources\Pascal\Server\GMS.Server.Types.Core.pas',
  GMS.Server.Central.DataModel in '..\..\..\..\Sources\Pascal\Server\Services\Central\GMS.Server.Central.DataModel.pas',
  GMS.Server.Central in '..\..\..\..\Sources\Pascal\Server\Services\Central\GMS.Server.Central.pas',
  GMS.Server.DataModel.Central.UserManagement.BO in '..\..\..\..\Sources\Pascal\Server\Services\Central\GMS.Server.DataModel.Central.UserManagement.BO.pas',
  GMS.Server.Services.Persists in '..\..\..\..\Sources\Pascal\Server\Services\Persistence\GMS.Server.Services.Persists.pas',
  GMS.Server.Services.Persists.types in '..\..\..\..\Sources\Pascal\Server\Services\Persistence\GMS.Server.Services.Persists.types.pas',
  GMS.ClientServer.DataModel.Central.BO in '..\..\..\..\Sources\Pascal\Common\Protocol\Central\GMS.ClientServer.DataModel.Central.BO.pas',
  GMS.ClientServer.Protocol.Central.BO in '..\..\..\..\Sources\Pascal\Common\Protocol\Central\GMS.ClientServer.Protocol.Central.BO.pas',
  GMS.ClientServer.DB.Protocol.BO in '..\..\..\..\Sources\Pascal\Common\Protocol\Persistence\GMS.ClientServer.DB.Protocol.BO.pas',
  GMS.ClientServer.Types.ChannelDefinitions in '..\..\..\..\Sources\Pascal\Common\GMS.ClientServer.Types.ChannelDefinitions.pas',
  GMS.ClientServer.Protocol.Helper in '..\..\..\..\Sources\Pascal\Common\Protocol\GMS.ClientServer.Protocol.Helper.pas',
  GMS.ClientServer.Protocol.MessageService.BO in '..\..\..\..\Sources\Pascal\Common\Protocol\GMS.ClientServer.Protocol.MessageService.BO.pas',
  GMS.ClientServer.Protocol.RPC.BO in '..\..\..\..\Sources\Pascal\Common\Protocol\GMS.ClientServer.Protocol.RPC.BO.pas',
  GMS.ClientServer.Protocol.Transport.BO in '..\..\..\..\Sources\Pascal\Common\Protocol\GMS.ClientServer.Protocol.Transport.BO.pas',
  GMS.Server.Services.IndyTCPServer in '..\..\..\..\Sources\Pascal\Server\Services\TCPServer\GMS.Server.Services.IndyTCPServer.pas',
  GMS.Server.Services.SSDPLikeServer.DataModel.BO in '..\..\..\..\Sources\Pascal\Server\Services\SSDPLike\GMS.Server.Services.SSDPLikeServer.DataModel.BO.pas',
  GMS.Server.Services.SSDPLikeServer.DataModel in '..\..\..\..\Sources\Pascal\Server\Services\SSDPLike\GMS.Server.Services.SSDPLikeServer.DataModel.pas',
  GMS.Server.Services.IndySSDPLikeServer in '..\..\..\..\Sources\Pascal\Server\Services\SSDPLike\IndyImpl\GMS.Server.Services.IndySSDPLikeServer.pas',
  GMS.ClientServer.PersistsConst in '..\..\..\..\Sources\Pascal\Common\Persistence\GMS.ClientServer.PersistsConst.pas';

Type

TConsoleApp = Class
private
  Flogfile : TFileStream;
Public
  Constructor Create; Virtual;
  Destructor Destroy; Override;
  Procedure LogIn(Sender : TObject; aText : string);
End;


{ TConsoleApp }

constructor TConsoleApp.Create;
begin
  Flogfile := TFileStream.Create('GMSAll.log',fmCreate or fmShareDenyNone);
end;

destructor TConsoleApp.Destroy;
begin
  FreeAndNil(Flogfile);
  inherited;
end;

procedure TConsoleApp.LogIn(Sender: TObject; aText: string);
var a : TStringStream;
    l : String;
begin
  l := IntToStr(GetThreadID)+'| '+aText;
  Writeln(l);
  l := l + #13#10;

  a := TStringStream.Create(l,TEncoding.UTF8);
  try
    a.Position := 0;
    {$IFNDEF FPC}
    Flogfile.WriteData(a.Bytes,a.Size);
    {$ELSE}
    Flogfile.Write(a.Memory^,a.Size);
    {$ENDIF}

  finally
    freeAndNil(a);
  end;
end;

Var GRIDServer : TopGRIDServer;
    console : TConsoleApp;
begin
  try
    {$IFNDEF FPC}
    {$IFDEF DEBUG}
    ReportMemoryLeaksOnShutdown := true;
    {$ENDIF}
    {$ENDIF}

    Console := TConsoleApp.Create;
    StartStandartBus;

    GRIDServer := TopGRIDServer.Create;
    GRIDServer.OnLog := Console.LogIn;
    GRIDServer.Activate;

    while (GRIDServer.GRID.TaskStatus = optLoop) do
    begin
      GRIDServer.Process;
      Sleep(50);
    end;

    GRIDServer.Desactivate;
    FreeAndnil(GRIDServer);
    FreeAndNil(Console);
    ReleaseStandartBus;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
