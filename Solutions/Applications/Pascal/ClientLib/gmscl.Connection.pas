unit gmscl.Connection;

interface

uses SysUtils,
     Classes,
     Generics.Collections,
     GS.Stream,
     gmsclConst,
     GMS.Client.Connection,
     GMS.Client,
     GMS.Client.Resolver,
     GMS.Client.SSDPLikeServer.DataModel.BO,
     GMS.Client.SSDPLikeServer.DataModel,
     GMS.Client.Transporter,
     GMS.Client.Transporter.Indy10,
     GMS.ClientServer.Types.ChannelDefinitions,
     GMS.ClientServer.Protocol.Helper,
     GMS.ClientServer.Protocol.MessageService.BO,
     GMS.ClientServer.Protocol.RPC.BO,
     GMS.ClientServer.Protocol.Transport.BO,
     GMS.ClientServer.DataModel.Central.BO,
     GMS.ClientServer.Protocol.Central.BO;

Type
TgmsCLConnectionManagement = class(TDictionary<Integer,TGridClient>)
Protected
Public
  Function AddActiveConnection(aReadyConnection : TGridClient) : Uint64;
  Function GetConnection(aHandle : Integer) : TGridClient;
end;

TgmsCLMailBoxes = class(TDictionary<Integer, TGridClientMessagesBox>)
Protected
Public
  //TGridclientMessageBoy is a maanged object, which owned its item. We keep one boxes per connection.
  // We release it each time it query a new mail box, and at the end of process.
  Function GetMessagesBox(aConnHandle : Integer) : TGridClientMessagesBox;
end;

Var
  glbConnectionManagement : TgmsCLConnectionManagement;
  glbConnectionMailBoxes : tgmsCLMailBoxes;

//create connection with a grid server with specified argument.
// out : Handle of connection.
function GMSConnection(IP : PWideChar; Port : UInt32; User : PWideChar; PassWord : PWideChar) : Integer; cdecl;
/// Create connection with a argument filled by a privat call to GMSResolve :
/// The first GRID server found is used for connection. Usefull API for standalone used as an Agent or interprocess pivot.
/// out : Handle of connection.
function GMSConnection_Auto(User : PWideChar; PassWord : PWideChar) : Integer; cdecl;
/// Releaser and disconnect all.
/// Out : 1 = success (gmsclConst)
function GMSConnection_Release(aHandle : Integer) : Integer; cdecl;

implementation

function GMSConnection(IP : PWideChar; Port : UInt32; User : PWideChar; PassWord : PWideChar) : Integer;
var lac : TGridClient;
begin
  Result := CST_ERROR;
  lac :=  TGridClient.Create;
  If lac.Connect( UnicodeString(IP),
                  Port,
                  UnicodeString(user),
                  UnicodeString(PassWord)
                ) then
  begin
    Result := glbConnectionManagement.AddActiveConnection(lac);
  end;
end;

function GMSConnection_Auto(User : PWideChar; PassWord : PWideChar) : Integer;
var lac : TGridClient;
    lr :  TGridClientGRIDResolver;
begin
  Result := CST_ERROR;
  lr := TGridClientGRIDResolver.Create;
  try
    lr.Resolve;
    if lr.GRIDServerCount > 0 then
    begin
      lac :=  TGridClient.Create;
      If lac.Connect( lr.GRIDServers[0].GridServerIP,
                      StrToInt(lr.GRIDServers[0].GridServerPort),
                      UnicodeString(user),
                      UnicodeString(PassWord)
                    ) then
      begin
        Result := glbConnectionManagement.AddActiveConnection(lac);
      end;
    end
  finally
    FreeAndNil(lr);
  end;
end;

function GMSConnection_Release(aHandle : Integer) : Integer;
var lac : TGridClient;
begin
  Result := CST_Error;
  glbConnectionManagement.TryGetValue(aHandle,lac);
  if Assigned(lac) then
  begin
    lac.Disconnect;
    FreeAndnil(Lac);
    glbConnectionManagement.Remove(aHandle);
  end;
end;


{ TgmsCLConnectionManagement }

function TgmsCLConnectionManagement.AddActiveConnection(
  aReadyConnection: TGridClient): Uint64;
begin
  Assert(Assigned(aReadyConnection));
  Result := Count+1;
  Add(Result,aReadyConnection);
end;

function TgmsCLConnectionManagement.GetConnection(
  aHandle: Integer): TGridClient;
begin
  Result := Nil;
  TryGetValue(aHandle,Result);
end;

{ tgmsCLMailBoxes }

function TgmsCLMailBoxes.GetMessagesBox(
  aConnHandle: Integer): TGridClientMessagesBox;
begin
  Result := Nil;
  if not TryGetValue(aConnHandle,Result) then
  begin
    Result := TGridClientMessagesBox.Create;
    Add(aConnHandle,Result);
  end;
end;


Initialization

glbConnectionManagement := TgmsCLConnectionManagement.Create;
glbConnectionMailBoxes := tgmsCLMailBoxes.Create;



finalization

FreeAndNil(glbConnectionManagement);
FreeAndNil(glbConnectionMailBoxes);


end.
