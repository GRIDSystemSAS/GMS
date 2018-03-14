library gmscl;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  System.SysUtils,
  System.Classes,
  GMS.Client.Connection in '..\..\..\..\Sources\Pascal\Client\GMS.Client.Connection.pas',
  GMS.Client in '..\..\..\..\Sources\Pascal\Client\GMS.Client.pas',
  GMS.Client.Resolver in '..\..\..\..\Sources\Pascal\Client\GMS.Client.Resolver.pas',
  GMS.Client.SSDPLikeServer.DataModel.BO in '..\..\..\..\Sources\Pascal\Client\GMS.Client.SSDPLikeServer.DataModel.BO.pas',
  GMS.Client.SSDPLikeServer.DataModel in '..\..\..\..\Sources\Pascal\Client\GMS.Client.SSDPLikeServer.DataModel.pas',
  GMS.Client.Transporter in '..\..\..\..\Sources\Pascal\Client\GMS.Client.Transporter.pas',
  GMS.Client.Transporter.Indy10 in '..\..\..\..\Sources\Pascal\Client\IndyImpl\GMS.Client.Transporter.Indy10.pas',
  GMS.ClientServer.Types.ChannelDefinitions in '..\..\..\..\Sources\Pascal\Common\GMS.ClientServer.Types.ChannelDefinitions.pas',
  GMS.ClientServer.Protocol.Helper in '..\..\..\..\Sources\Pascal\Common\Protocol\GMS.ClientServer.Protocol.Helper.pas',
  GMS.ClientServer.Protocol.MessageService.BO in '..\..\..\..\Sources\Pascal\Common\Protocol\GMS.ClientServer.Protocol.MessageService.BO.pas',
  GMS.ClientServer.Protocol.RPC.BO in '..\..\..\..\Sources\Pascal\Common\Protocol\GMS.ClientServer.Protocol.RPC.BO.pas',
  GMS.ClientServer.Protocol.Transport.BO in '..\..\..\..\Sources\Pascal\Common\Protocol\GMS.ClientServer.Protocol.Transport.BO.pas',
  GMS.ClientServer.DataModel.Central.BO in '..\..\..\..\Sources\Pascal\Common\Protocol\Central\GMS.ClientServer.DataModel.Central.BO.pas',
  GMS.ClientServer.Protocol.Central.BO in '..\..\..\..\Sources\Pascal\Common\Protocol\Central\GMS.ClientServer.Protocol.Central.BO.pas',
  gmscl.Message in 'gmscl.Message.pas',
  gmscl.MemStream in 'gmscl.MemStream.pas',
  gmscl.Connection in 'gmscl.Connection.pas',
  gmsclConst in 'gmsclConst.pas';

{$R *.res}

var CurrentResolver :  TGridClientGRIDResolver;

//Hello juste for descrpite how to pass string and get back in a cdecl mode.
//this is ok from usueal cpp compiler. It is the same recipe for buffer.
//Rules : Caller own the memory.
function hello(name, buffer : PWideChar; buflen: Integer): Integer; cdecl;
var
  rs: UnicodeString;
begin
  rs := 'Hello '+UnicodeString(name);
  if buffer = nil then
  begin
    Result := Length(rs) + 1;
  end
  else
  begin
    Result := buflen;
    if Length(rs)<Result then
      Result := Length(rs);
    Move(rs[1], buffer^, Result * SizeOf(WideChar));
  end;
end;

function GMSResolve : Integer; stdcall;
begin
  if Not(assigned(CurrentResolver)) then
  begin
    FreeAndNil(CurrentResolver);
  end;
  CurrentResolver := TGridClientGRIDResolver.Create;
  CurrentResolver.Resolve;
  Result:= CurrentResolver.GRIDServerCount;
end;

function GMSResolve_ServerInfo(Index : Cardinal; ServerInfoCode : Integer; ServerInfo : PWideChar; buflen : Integer) : integer; cdecl;
var rs : UnicodeString;
begin
  if Not(assigned(CurrentResolver)) then
  begin
    Result := -3;
    Exit;
  end;

  if CurrentResolver.GRIDServerCount<=Index then
  begin
    Result := -2;
    Exit;
  end
  else
  begin
    case  ServerInfoCode of
    0: rs := CurrentResolver.GRIDServers[Index].GridServerIP;
    1: rs := CurrentResolver.GRIDServers[Index].GridServerPort;
    2: rs := CurrentResolver.GRIDServers[Index].GridServerName;
    3: rs := CurrentResolver.GRIDServers[Index].GridServerDesc;
    4: rs := CurrentResolver.GRIDServers[Index].GridServerHost;
    else
    begin
      rs := 'Unknown code.';
    end;
    end;
  end;

  if ServerInfo = nil then
  begin
    Result := Length(rs) + 1;
  end
  else
  begin
    Result := buflen;
    if Length(rs)<Result then
      Result := Length(rs);
    Move(rs[1], ServerInfo^, Result * SizeOf(WideChar));
  end;
end;




exports
  hello,
  GMSResolve,
  GMSResolve_ServerInfo,
  MemStreamCreate,
  MemStreamRelease,
  MemStreamSize,
  MemStreamAddInteger,
  MemStreamAddString,
  MemStremAddDouble,
  GMSConnection,
  GMSConnection_Auto,
  GMSConnection_Release,
  GMSSendMessage,
  GMSSendMessageString,
  GMSSubscribt,
  GMSUnSubscript,
  GMSReceiveMessage,
  GMSMessageBoxCount,
  GMSMessageBox_MessageInfo;
  //GMSMessageBox_MessageStream;


begin
  CurrentResolver := Nil;

end.
