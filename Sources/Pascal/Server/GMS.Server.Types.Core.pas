unit GMS.Server.Types.Core;
interface

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

{$IFDEF FPC}
  {$IFDEF WINDOWS}
    {$IFDEF CPU386 OR CPUi386 OR CPUX86_64 OR CPUAMD64}
      {$DEFINE FORGE_ENCRYPTION}
   {$ENDIF}
  {$ENDIF}
{$ELSE}
  {$IFDEF WINDOWS}
    {$DEFINE FORGE_ENCRYPTION}
  {$ENDIF}
{$ENDIF}

Uses
{$IFDEF FPC}
  {$IFDEF UNIX}
  cthreads,
  cmem, // the c memory manager is on some systems much faster for multi-threading
  {$ENDIF}
  Classes, SysUtils, Generics.Collections,
  SyncObjs,
{$ELSE}
  System.Classes, System.SysUtils, System.Generics.Collections,
  System.SyncObjs, System.Threading,
{$ENDIF}
  GS.CPUUsage,
  GS.Threads,
  GS.Stream,
  GS.Task,
  GS.Bus,
  GS.LocalMemCached,
  GMS.Server.Central.DataModel,
  GMS.ClientServer.Types.ChannelDefinitions,
  GMS.ClientServer.Protocol.RPC.BO, //Hard highway to internal RPC service (Avoid to pass by TCP server to resole an RPC service.
  GMS.ClientServer.Protocol.Transport.BO;

Const
  CST_SERVICETHREADTIMEOUTINMS = 250;
  CST_CPUMESSAGEGENERATIONINTERVALINMS = 1000;

  CST_SERVER_GLOBAL_LOG_CHANNEL = 'GlobalServerLog';
  CST_SERVER_GLOBAL_COMUNICATION_CHANNEL = 'GlobalCentral';
  CST_SERVER_AUTH_CHANNEL = CST_SERVER_GLOBAL_COMUNICATION_CHANNEL+'/Auth';
  CST_SERVER_GLOBAL_SERVICEDECLARATION = CST_SERVER_GLOBAL_COMUNICATION_CHANNEL+'/ServiceDeclaration';
  CST_SERVER_GLOBAL_APIDECLARATION = CST_SERVER_GLOBAL_COMUNICATION_CHANNEL+'/API';

  CST_ERROR_PROTOCOL = 'PROTOCOL FAILURE';
  CST_GRID_PACKET_SIGNATURE = 8865421;
  CST_PB = 0;   //<Protocol base.
  CST_PBs = 10; //<Protocol base step.
  CST_SEPARATOR = '_'; //<Protocol base step.

  //Sharedate via TLocalMemCached
  CST_SHAREDDATA_CPULEVEL = 'Global.CPULevel';

  //Error
  CST_EXCEPTION_RPC_INTERNALBUS_WAIT_RESPONSE_TIMEOUT = 'Exception : RPC process with no response : Performance issue or Bus misfonction.';
  CST_EXCEPTION_ASK_FOR_RECEIVE_MSG_BUT_NO_SUBSCRIPTION = 'No channel''s subscriptions : No message delivery available';

  //Error Authentification
  CST_EXCEPTION_AUTH_INTERNALBUS_WAIT_RESPONSE_TIMEOUT = 'Exception : Message''s wait time out in Auth process.';

{$ifdef fpc}
  CST_InstanceTypes : Array[0..7] of string = ('Logger','CoreManagement','ComServer','API','Messager','ServiceProvider','MainTask','FileSystem');
  CST_Platform : Array[0..5] of string = ('Windows','Mac OS','iOS','Android','WinRT','Linux');
  CST_Archi : Array[0..2] of string = ('Intel X86','Intel X64','Arm32');
{$else}
  CST_InstanceTypes : Array of string = ['Logger','CoreManagement','ComServer','API','Messager','ServiceProvider','MainTask','FileSystem'];
  CST_Platform : Array of string = ['Windows','Mac OS','iOS','Android','WinRT','Linux'];
  CST_Archi : Array of string = ['Intel X86','Intel X64','Arm32'];
{$endif}


type
  //Message send to CST_SERVER_GLOBAL_COMUNICATION_CHANNEL in order to controlling the service.
  TofServerComChannelInternalOrder = (  cioNone,
                                        cioTerminate, //Thread terminaison. (USe mainly for .so/.dll to cummincate with, without dealing with shared object hassle.)
                                        cioTCPConnectionManagement_AddUser,
                                        cioTCPConnectionManagement_UserDisconnect //Use to make homework in service.
                                        );

  TGRIDData = Class
    Class Var MemCached : TLocalMemcached;
  End;


Type
TopGRIDServerServiceInstanceType = (gtUnknown = CST_PB + CST_PBs * 0, gtLogger,
                                                                      gtComServer,
                                                                      gtCentralManagementService,
                                                                      gtProcessManagementService,
                                                                      gtBus,
                                                                      gtProcess);

TopGridServerItem = Class
End;

TopGRIDServerServices = Class; //Wrap TaskManager.
TopGRIDServerServiceInstance = Class;

{TopGRIDServerServiceInstance is a TopTask heritated class : Basically, a managed thread.
This is basically all resident service for GRID (Log, Server TCP/HTTP, Central Service and so on)
}
TopGRIDServerServiceInstance = Class(TopTask)
private
protected
  FReaderObject: TBusClientReader;
  FRPC : TBusClientReader;
  FRPC_Stream : TMemoryStream;

  FGUID: String;
  function GetInstanceType: TopGRIDServerServiceInstanceType; Virtual; Abstract;
  function GetDescription : String; Virtual;
  Procedure SendServiceDeclarationToCentral; virtual;
  procedure LoadConfiguration; Virtual;

Public
  {Standart constructor : Generate the GUID}
  Constructor Create; Override; //WARNING : do not Reintroduce, because service re build via injection. Create must stay as is.
  Destructor Destroy; Override;


  Procedure OnServerMessage_FromCentral(Sender : TBus;Var Packet : TBusEnvelop); Virtual;

  {Override from TopTask : This is the infinite loop of the thread. It will just wait for message from bus.}
  Procedure BeforeRun; Override;
  Procedure RunLoop; Override;
  Procedure AfterRun; Override;

  //Generic RPC call. Allow to the service to call all channel responder via RPC process.
  //Based upen ServerSendMessageRPC, this method is rougthly the same, but with exclusive used of FRPC provate Client reader.
  Function InternalMessageRPCProcess(aMessage : TMemoryStream; aTargetChannel : String) : TMemoryStream;
  Procedure OnServerMessage_RPCReply(Sender : TBus;Var Packet : TBusEnvelop); //Here is where response is processed.

  {This GUID wil be used in communication header to determine the source
  and destination service of the packet.}
  Property InstanceGUID : String read FGUID;
  Property InstanceType : TopGRIDServerServiceInstanceType read GetInstanceType;

  Property ReaderObject : TBusClientReader read FReaderObject Write FReaderObject;
End;


TopGRIDServerServiceInstanceClass = Class of TopGRIDServerServiceInstance;

//Wrapper TaskManager.
TopGRIDServerServices = Class
private
Protected
  FTaskStatusBuffer : TStringList;
Public
  Manager : TTaskManager;

  Function AddService(aService : TopGRIDServerServiceInstanceClass) : TopGRIDServerServiceInstance; Overload;
  Function AddService(aServiceInstance : TopGRIDServerServiceInstance) : TopGRIDServerServiceInstance; Overload;

  Procedure StartServices;
  Procedure StopServices;

  constructor Create; Virtual;
  Destructor Destroy; Override;

  Procedure BeforeDestroy;

  Procedure Process;

  Function GetTasksStatusAsCSVString : String;

  Procedure SendMessageToAllTask(aMessage : TMemoryStream);

End;

TopGRIDServerServiceLogCategory = (gssllAll, gssllException, gssllWarning, gssllInfo, gssllTip);

{Log : An external service will take this to display to the main thread.}
procedure ServerLog(Sender : TObject; astr: String; Const LogCategory : TopGRIDServerServiceLogCategory = gssllAll);

//Send an information ask about User/Session detail. (info Owned by central).
//Synchrone mode.
Procedure ServiceAuthorization( Sender : TopGRIDServerServiceInstance;
                                aExclusiveServiceClientReader : TBusClientReader;
                                UserName, Password : String; InfoData : String);

//Send a generaliste Message.
Procedure ServerMessageSend( Sender : TObject;
                             Channel : String; Content : TMemoryStream;
                             Const ResponseChannel : String = '';
                             Const AdditionalData : string = '';
                             Const PersistentMessage : Boolean = False);

Procedure ServerMessageSendRPC( Sender : TObject;
                                aChannel : String;
                                aContent : TMemoryStream;
                                aClient : TBusClientReader;
                                Const AdditionalData : string = '';
                                Const PersistentMessage : Boolean = False);

Function ServerGetGUID : String;

//Simple AES stream crypt. { TODO : Make better throught a DLL. At least on main target. }
Procedure StreamEncrypt(aStreamToEncrypt : TMemoryStream);
Procedure StreamDecrypt(aStreamToDecrypt : TMemoryStream);

Procedure ServerMessageSendBroadCast(AOrder : TofServerComChannelInternalOrder; JSONData : String);

implementation

uses GMS.ClientServer.DataModel.Central.BO //For service decl.
     {$IFDEF FORGE_ENCRYPTION}
     ,
     tfTypes,
     tfBytes,
     tfCiphers
     {$ENDIF};

Function ServiceInstanceTypeToDatamodelCentralBo(aServiceType : TopGRIDServerServiceInstanceType) : TSOGRIDCentralServiceCategory;
begin
  Result := scComServer; //Most secured
  case aServiceType of
    gtLogger:                   Result := scLogger;
    gtComServer:                Result := scComServer;
    gtCentralManagementService: Result := scCentralManagementService;
    gtProcessManagementService: Result := scProcessManagementService;
    gtBus:                      Result := scBus;
    gtProcess:                  Result := scProcess;
  end;
end;

Function DataModelCentralBOServiceTypeToServiceInstanceType(aServiceType : TSOGRIDCentralServiceCategory) : TopGRIDServerServiceInstanceType;
begin
  Result := gtComServer; //Most secured
  case aServiceType of
    scLogger:                   Result := gtLogger;
    scComServer:                Result := gtComServer;
    scCentralManagementService: Result := gtCentralManagementService;
    scProcessManagementService: Result := gtProcessManagementService;
    scBus:                      Result := gtBus;
    scProcess:                  Result := gtProcess;
  end;
end;


Procedure StreamEncrypt(aStreamToEncrypt : TMemoryStream);
{$IFDEF FORGE_ENCRYPTION}
const
  HexKey = '000102030405060708090A0B0C0D0E0F';
  Nonce = 42;
var
  lOutStream: TMemoryStream;
  lKey : ByteArray;
{$ENDIF}
begin
  {$IFDEF FORGE_ENCRYPTION}
  lOutStream:= TMemoryStream.Create;
  aStreamToEncrypt.Position := 0;
  try
    //OutStream.WriteBuffer(Nonce, SizeOf(Nonce));
    lKey:= ByteArray.ParseHex(HexKey);
    TStreamCipher.AES.ExpandKey(lKey,Nonce).ApplyToStream(aStreamToEncrypt, lOutStream);
    aStreamToEncrypt.Clear;
    lOutStream.Position := 0;
    aStreamToEncrypt.CopyFrom(lOutStream,lOutStream.Size);
  finally
    lOutStream.Free;
  end;
  {$ENDIF}
end;

Procedure StreamDecrypt(aStreamToDecrypt : TMemoryStream);
{$IFDEF FORGE_ENCRYPTION}
const
  HexKey = '000102030405060708090A0B0C0D0E0F';
  Nonce = 42;
var
  lOutStream: TMemoryStream;
  lKey : ByteArray;
{$ENDIF}
begin
  {$IFDEF FORGE_ENCRYPTION}
  lOutStream:= TMemoryStream.Create;
  aStreamToDecrypt.Position := 0;
  try
    //OutStream.WriteBuffer(Nonce, SizeOf(Nonce));
    lKey:= ByteArray.ParseHex(HexKey);
    TStreamCipher.AES.ExpandKey(lKey,Nonce).ApplyToStream(aStreamToDecrypt,lOutStream);
    aStreamToDecrypt.Clear;
    lOutStream.Position := 0;
    aStreamToDecrypt.CopyFrom(lOutStream,lOutStream.Size);
  finally
    lOutStream.Free;
  end;
  {$ENDIF}
end;

Function StringEncryptSymetric(aStringToEncrypt : String) : String;
var ltmp: RawByteString;
begin
  if aStringToEncrypt='' then begin
    Result := EmptyStr;
  end
  else
  begin
    SetString(ltmp,PChar(aStringToEncrypt),Length(aStringToEncrypt)); // private copy
    //SymmetricEncrypt(66667784,ltmp);
    //Result := BinToBase64(ltmp);
  end;
end;

Function StringDecryptSymetric(aStringToDecrypt : String) : String;
var ltmp: RawByteString;
begin
  if aStringToDecrypt='' then begin
    Result := EmptyStr;
  end
  else
  begin
    SetString(ltmp,PChar(aStringToDecrypt),Length(aStringToDecrypt)); // private copy
    //SymmetricEncrypt(66667784,ltmp);
    //Result := BinToBase64(ltmp);
  end;
end;

Procedure ServerMessageSendBroadCast(AOrder : TofServerComChannelInternalOrder; JSONData : String);
var lm : TBusMessage;
    ls : TMemoryStream;
begin
  ls := TMemoryStream.Create;
  try
    WriteByte(ls,Byte(AOrder));
    WriteString(ls,JSONData);
    lm.FromStream(ls);
    Bus.Send(lm,CST_SERVER_GLOBAL_COMUNICATION_CHANNEL,EmptyStr);
  finally
    FreeAndNil(ls);
  end;
end;

procedure ServerLog(Sender : TObject; astr: String; Const LogCategory : TopGRIDServerServiceLogCategory);
var s : TMemoryStream;
    b : TBusMessage;
begin
  s := TMemoryStream.Create;
  try
    WriteInt64(s,GetThreadID);
    if Assigned(Sender) then
    begin
      WriteInt64(s,Int64(Sender));
      WriteString(s,Sender.ClassName);
    end
    else
    begin
      WriteInt64(s,Int64(0));
      WriteString(s,EmptyStr);
    end;
    WriteString(s,astr);
    WriteByte(s,Byte(LogCategory));
    b.FromStream(s);
    Bus.Send(b,CST_SERVER_GLOBAL_LOG_CHANNEL);
  finally
    s.Free;
  end;
end;


Procedure ServerMessageSend( Sender : TObject;
                             Channel : String;
                             Content : TMemoryStream;
                             Const ResponseChannel : String;
                             Const AdditionalData : string;
                             Const PersistentMessage : Boolean);
var b : TBusMessage;
begin
  Assert(Length(Channel)>0);
  b.FromStream(Content);
  Bus.Send(b,Channel,AdditionalData,ResponseChannel,PersistentMessage);
end;


Procedure ServerMessageSendRPC( Sender : TObject;
                                aChannel : String;
                                aContent : TMemoryStream;
                                aClient : TBusClientReader;
                                Const AdditionalData : string = '';
                                Const PersistentMessage : Boolean = False);
begin
  Assert(Assigned(aContent));
  Assert(Assigned(aClient));

  ServerMessageSend(Sender,aChannel,aContent,aclient.ChannelListening,AdditionalData,PersistentMessage);

  while true do
  begin
    case aclient.Event.WaitFor(CST_SERVICETHREADTIMEOUTINMS) of
      wrSignaled :
      begin
        Bus.ProcessMessages(aClient);
        break;
      end;
      wrTimeOut : ServerLog(Sender, achannel+' waiting...');
      else
      begin
        ServerLog(Sender, CST_EXCEPTION_RPC_INTERNALBUS_WAIT_RESPONSE_TIMEOUT);
        raise Exception.Create(CST_EXCEPTION_RPC_INTERNALBUS_WAIT_RESPONSE_TIMEOUT);
      end;
    end;
  end;
end;

Function ServerGetGUID : String;
  {$IFDEF FPC}
var l : TGUID;
  {$ENDIF}
begin
  {$IFDEF FPC}
  if CreateGUID(l) = s_OK then
    Result := GuidToString(l)
  else
    RaiseLastOSError;
  {$ELSE}
  Result := TGUID.NewGuid.ToString;
  {$ENDIF }
end;

procedure ServiceAuthorization( Sender: TopGRIDServerServiceInstance;
                                aExclusiveServiceClientReader: TBusClientReader;
                                UserName, Password : String;
                                InfoData : String);
var s : TMemoryStream;
begin
  Assert(Assigned(Sender));
  Assert(Assigned(aExclusiveServiceClientReader));
  s := TMemoryStream.Create;
  try
    //Simple proto ever : 3 strings. (Internal use only)
    WriteString(s,UserName);
    WriteString(s,Password);
    WriteString(s,Sender.InstanceGUID); //Service.
    ServerMessageSendRPC(Sender,CST_SERVER_AUTH_CHANNEL,s,aExclusiveServiceClientReader,InfoData);
  finally
    s.Free;
  end;
end;


{ TopGRIDService }

Function TopGRIDServerServices.AddService(aService: TopGRIDServerServiceInstanceClass) : TopGRIDServerServiceInstance;
var aTask : TopTask;
begin
  aTask := Manager.CreateTask(aService);
  Result := TopGRIDServerServiceInstance(aTask);
  //An event is pluged is Central service which will receive a message of self description sended in TopGRIDServerServiceInstance constructor. (See SendServiceDeclarationToCentral)
end;

function TopGRIDServerServices.AddService(
  aServiceInstance: TopGRIDServerServiceInstance): TopGRIDServerServiceInstance;
var aTask : TopTask;
begin
  aTask := Manager.AddTask(aServiceInstance);
  Result := TopGRIDServerServiceInstance(aTask);
end;

procedure TopGRIDServerServices.BeforeDestroy;
begin
end;

constructor TopGRIDServerServices.Create;
begin
  Inherited Create;
  Manager := TTaskManager.Create;
  FTaskStatusBuffer := TStringList.Create;
end;

destructor TopGRIDServerServices.Destroy;
begin
  FreeAndNil(Manager);
  FreeAndNil(FTaskStatusBuffer);
  inherited;
end;


function TopGRIDServerServices.GetTasksStatusAsCSVString: String;
begin
  Manager.TaskReport(FTaskStatusBuffer);
  Result := FTaskStatusBuffer.Text;
end;

procedure TopGRIDServerServices.Process;
begin
  Manager.Log(True).Free;
end;

procedure TopGRIDServerServices.SendMessageToAllTask(aMessage: TMemoryStream);
var a : TopTaskMessage;
begin
  a.BuildStreamMessage(aMessage);
  Manager.SendMessage(a);
end;

procedure TopGRIDServerServices.StartServices;
begin
  Manager.StartAll;
end;

procedure TopGRIDServerServices.StopServices;
begin
  Manager.KillAll;
end;

{ TopGRIDServerServiceInstance }

procedure TopGRIDServerServiceInstance.AfterRun;
begin
  inherited;
  ServerLog(Self,ClassName+' exiting main thread.');
end;

procedure TopGRIDServerServiceInstance.BeforeRun;
begin
  ServerLog(Self,ClassName);
  inherited;
  SendServiceDeclarationToCentral;
end;

constructor TopGRIDServerServiceInstance.Create;
begin
  inherited Create;
  FReaderObject := Bus.Subscribe(CST_SERVER_GLOBAL_COMUNICATION_CHANNEL,OnServerMessage_FromCentral);
  FReaderObject.Event := Bus.GetNewEvent;

  FRPC_Stream := TMemoryStream.Create;
  FRPC := Bus.Subscribe(ClassName+'_RPCInternalReply_'+IntToStr(NativeInt(Self)),OnServerMessage_RPCReply);
  FRPC.Event := Bus.GetNewEvent; //Indep.

  FGUID := ServerGetGUID;
  ServerLog(Self,'Create instance');
  LoadConfiguration;
end;


destructor TopGRIDServerServiceInstance.Destroy;
begin
  FreeAndNil(FReaderObject);
  FreeAndNil(FRPC);
  FreeAndNil(FRPC_Stream);
  inherited;
end;

function TopGRIDServerServiceInstance.GetDescription: String;
begin
  Result := 'None';
end;

function TopGRIDServerServiceInstance.InternalMessageRPCProcess( aMessage : TMemoryStream;
                                                                 aTargetChannel : String) : TMemoryStream;

begin
  Assert(Assigned(aMessage));
  //Buffer must be loaded with target service ask protocol buffer (GRID.ClientServer.protocol.Whatever)
  //aRPC.Channel is the original FunctionName ask by the user.
  //The incoming message will be processed by FRPC linked method, OnServerMessage_RPCReply in our case.
  //1) Send the message to the service. ----------------------------------------

  ServerMessageSendRPC(Self,aTargetChannel, aMessage,FRPC); //Send the message to the service.
  Result := FRPC_Stream; //this stream has been populated by FRPC linked methods. (OnServerMessage_RPCReply)
end;

procedure TopGRIDServerServiceInstance.LoadConfiguration;
begin
  //Nothing here : redefine to build configuration process.
end;

procedure TopGRIDServerServiceInstance.OnServerMessage_FromCentral(Sender: TBus;
  var Packet: TBusEnvelop);
begin
  //Not a mandatory for descendant to implement control.
end;

procedure TopGRIDServerServiceInstance.OnServerMessage_RPCReply(Sender: TBus;
  var Packet: TBusEnvelop);
var ls : TMemoryStream;
begin
  //When this service make an RPC call, reponse arrive here by classical message.

  ls := Packet.ContentMessage.AsStream; //Deep copy.
  try
    FRPC_Stream.Clear;
    FRPC_Stream.LoadFromStream(ls);
    FRPC_Stream.Position := 0;
  finally
    FreeAndNil(ls);
  end;
end;

procedure TopGRIDServerServiceInstance.RunLoop;
begin
{ TODO 1 -oVGS -cExceptionManagement : Manage exception : in function of Exception type; try to send back generic error to client ? }
  case FReaderObject.Event.WaitFor(CST_SERVICETHREADTIMEOUTINMS) of
    wrSignaled :
    begin
      Bus.ProcessMessages(FReaderObject);
    end;
  end;
end;




procedure TopGRIDServerServiceInstance.SendServiceDeclarationToCentral;
var lstream : TMemoryStream;
    ldata : TGRIDClientServerDataModelCentralBOManager;
    litem : TSOGRIDCentralServiceItem;
begin
  lstream := TMemoryStream.Create;
  ldata := TGRIDClientServerDataModelCentralBOManager.Create;
  try
    litem := TSOGRIDCentralServiceItem.Create(ldata);
    litem.ServiceClass := ClassName;
    litem.ServiceName := '';
    litem.ServiceDesc := GetDescription;
    litem.ServiceInstanceID := FGUID;
    litem.ServiceStarted := Started;
    litem.ServiceStartedOn := 0;
    if Started then
      litem.ServiceStartedOn := DateTimeBegin;
    litem.ServiceThreadID := ThreadID;
    litem.ServiceCategory := ServiceInstanceTypeToDatamodelCentralBo(InstanceType);
    ldata.SaveToStream(lstream);
    ServerMessageSend(Self,CST_SERVER_GLOBAL_SERVICEDECLARATION,lstream);
  finally
    FreeAndNil(lstream);
    FreeAndNil(ldata);
  end;
end;


Initialization

TGRIDData.MemCached := TLocalMemcached.Create;
TGRIDData.MemCached.FileName := EmptyStr; //Make it in memory only.
TGRIDData.MemCached.Open;

Finalization

TGRIDData.MemCached.Free;

end.
