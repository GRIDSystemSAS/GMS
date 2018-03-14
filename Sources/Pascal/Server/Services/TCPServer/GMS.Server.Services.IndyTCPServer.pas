unit GMS.Server.Services.IndyTCPServer;

interface
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

Uses
{$IFDEF FPC}
  SysUtils, Classes, Generics.Collections, SyncObjs,
{$ELSE}
  System.Classes, System.SysUtils, System.Generics.Collections, System.SyncObjs,
{$ENDIF}
  IdContext, IdTCPConnection, IdTCPClient, IdBaseComponent, IdComponent, IdCustomTCPServer, IdTCPServer,
  idYarn, idThread, idSocketHandle, idGlobal, idIOHandler, idStack,
  GMS.Server.Types.Core,
  GMS.ClientServer.Protocol.Transport.BO,
  GMS.ClientServer.Protocol.Helper,
  GMS.ClientServer.Protocol.MessageService.BO,
  GMS.ClientServer.Protocol.RPC.BO,
  GS.Task,
  GS.Bus,
  GS.Threads,
  GS.Stream,
  GS.JSON;

Const
  //Connection count Limit IP over TCP. Used in IPFilter function.
  CST_IPFILTER_LIMIT = 20;
  CST_CRLF           = #13#10;

Type

TGRIDService_IndyTCPServer = Class;
TGridContext = Class;

{TGridContext is an Indy lib sugar to avoid us to keep a list as
         <GRID Client> --><--- <Indy Context>
all the Indy Context will be built as TGridContext (Indy's OnContextCreateEvent).}
{$IFDEF FPC}
  TIdServerContext = class(TIdContext)
  protected
    FServer: TIdCustomTCPServer;
  public
    property Server: TIdCustomTCPServer read FServer Write FServer;
  end;

  TIdServerContextClass = class of TIdServerContext;
{$ENDIF}

TGRIDJSONContext = Class
private
  FC: String;
  FP: String;
  FPP: Word;
Published
  Property PeerIP : String read FP write FP;
  Property PeerPort : Word read FPP write FPP;
  Property ContextGUID : String read FC write FC;
End;

TGridJSONInfoData = Class
private
  FO: String;
  FCC: TGRIDJSONContext;
Public
Published
  Property Origin : String read FO Write FO;
  Property ContextualData : TGRIDJSONContext read FCC Write FCC; //Pointer only.
End;

TGridContext = Class(TidServerContext)
Public
  ContextGUID : String;
  IsAccredited : Boolean;
  SessionID : String;
  ChannelsSubscription : TStringList;
  GeneralMessageEvent : TEvent;

  //Messageing system.
  GeneralMessageReply : TGRIDClientServerProtocolMessageServiceManager;
  GeneralMessageReplyProcess : Boolean;

  //RPC system.
  GeneralMessageReplyRPC : TGRIDClientServerProtocolRPCBOManager;

  AuthReader : TBusClientReader;
  RPCReader : TBusClientReader;

  //Event used foar auth process. (AuthReader);
  Procedure OnServerMessage_AuthResponse(Sender : TBus;Var Packet : TBusEnvelop);

  //Event used for pure messaging task (not rpc) :
  //Internally, Each Channel will have its reader in Channel's list (ChannelSubscription.Objects).
  procedure OnServerMessage_General(Sender : TBus;Var Packet : TBusEnvelop);

  procedure OnServerMessage_RPCType(Sender : TBus;Var Packet : TBusEnvelop);

  Procedure BuildACKForNoMessage;
  Procedure BuildACKSoftFail(InfoReason : String);
  //Procedure BuildACKStillMessageOnServer; //TODO : Put in Info string how many message pending.

  Procedure Subscription(aChannelName : string);
  Procedure Unsubscription(aChannelName : string);
  Procedure UnsubscriptionFull;

{$IFDEF FPC}
  constructor Create(
    AConnection: TIdTCPConnection;
    AYarn: TIdYarn;
    AList: Classes.TThreadList = nil
    ); Override;
{$ELSE}
  constructor Create(
    AConnection: TIdTCPConnection;
    AYarn: TIdYarn;
    AList: TIdContextThreadList = nil
    ); Override;
{$ENDIF}

  Destructor Destroy; Override;

  //Give contextual data from origin of ctx.
  Function JsonSignature : String;

End;


{TGRIDService_IndyTCPServer is a GRID service which manage a MultiThreaded TCPServer}
TGRIDService_IndyTCPServer = Class(TopGRIDServerServiceInstance)
Private
  FFellowServerClientProtocolBehaviourInstruction: Boolean;
  function GetLogActive: Boolean;
  procedure SetLogActive(const Value: Boolean);
  function GetActive: Boolean;
  function GetPort: Word;
  procedure SetPort(const Value: Word);
  function GetBindString: String;
  function GetExecuteCount: Int64;


Protected
  FServer : TIdTCPServer;
  FLogActive : TProtectedBoolean;
  FBindString : TProtectedString;
  FExecuteCount : TProtectedInt64;
  FDesiredPort : Word;


  //Warning, those event is all in thread. Semaphore required.
  Procedure OnIdServerExecute(AContext: TIdContext);
    Procedure InternalAccreditationProcess(aReceivedStream : TMemoryStream; aContext : TGridContext; Var ErrorString : String);
    Procedure InternalHubProcess(aReceivedStream : TMemoryStream; aContext : TGridContext; Var ErrorString : String);
      Procedure InternalMessagingManagement(aReceivedStream : TMemoryStream; aContext : TGridContext; Var ErrorString : String);
        Procedure InternalMessageSubscription(aContext : TGridContext; aChannel : String);
        Procedure InternalMessageUnsubscription(aContext : TGridContext; aChannel : String);
        Procedure InternalMessageReply(aContext : TGridContext; WaitDelayInMS : Integer);
      Procedure InternalRPCManagement(aReceivedStream : TMemoryStream; aContext : TGridContext; Var ErrorString : String);
        Procedure InternalMessagingRPCProcess(aContext : TGridContext; aRPCMessage : TGRIDMessageRPCAsk);

        //Utility.
        Procedure InternalFullUnsubscribtion(aContext : TGridContext);

  procedure OnIdServerAfterBind(Sender: TObject);
  procedure OnServerBeforeListenerRun(AThread: TIdThread);

  procedure OnIdServerContextCreated(AContext: TIdContext);
  Procedure OnIdServerConnect(AContext: TIdContext);
  procedure OnIdServerDisconnect(AContext: TIdContext);
  procedure OnIdServerException(AContext: TIdContext; AException: Exception);
  procedure OnIdServerListenException(AThread: TIdListenerThread;  AException: Exception);
  procedure OnIdServerStatus(ASender: TObject; const AStatus: TIdStatus;  const AStatusText: string);

  Procedure BeforeDestroy;

  Function IPFilter(const StrIP: string): Boolean;
  Function IPClient(aContext : TidContext) : String;

  //StartupConfiguration.
  procedure LoadConfiguration; Override;

Public
  Constructor Create; Override;
  Destructor Destroy; Override;

  //Messaging bus
  Procedure OnServerMessage_FromCentral(Sender : TBus;Var Packet : TBusEnvelop); Override;

  Function GetContext(CtxId : String; out aContext : TGridContext) : Boolean;

  function GetInstanceType: TopGRIDServerServiceInstanceType; Override;
  function GetDescription : String; Override;

  Procedure Enable;
  Procedure Disable;

  Property EnableLogVerbose : Boolean read GetLogActive Write SetLogActive;

  Property Active : Boolean read GetActive;
  Property Port : Word Read GetPort Write SetPort;

  Property Binding : String read GetBindString;
  Property ExecuteCount : Int64 read GetExecuteCount;

  Property FellowServerClientProtocolBehaviourInstruction : Boolean read FFellowServerClientProtocolBehaviourInstruction
                                                                    write FFellowServerClientProtocolBehaviourInstruction;
end;


implementation


{ TGRIDService_IndyTCPServer }

procedure TGRIDService_IndyTCPServer.BeforeDestroy;
begin
  inherited;
  FLogActive.Value := False;
  FServer.OnExecute := Nil;
  FServer.OnAfterBind := Nil;
  FServer.OnConnect := Nil;
  FServer.OnContextCreated := Nil;
  FServer.OnDisconnect := Nil;
  FServer.OnException := Nil;
  FServer.OnListenException := Nil;
  FServer.Active := False;
end;

constructor TGRIDService_IndyTCPServer.Create;
{$IFDEF FPC}
var a : TIdSocketHandle;
{$ENDIF}
begin
  inherited Create;
  FFellowServerClientProtocolBehaviourInstruction := True;
  FServer := TIdTCPServer.Create(Nil);
  FServer.ContextClass := TGridContext;
  FServer.Bindings.Clear; //One binding only.
{$IFDEF FPC} //Linux seems to not support same binding on 2 interface (ipv6 and ipv4)
  a := FServer.Bindings.Add;
  a.IPVersion := TIdIPVersion.Id_IPv4;
{$ENDIF}
  FServer.OnExecute := OnIdServerExecute;
  FServer.OnAfterBind := OnIdServerAfterBind; //IN MAIN THREAD.
  FServer.OnConnect := OnIdServerConnect;
  FServer.OnContextCreated := OnIdServerContextCreated;
  FServer.OnDisconnect := OnIdServerDisconnect;
  FServer.OnException := OnIdServerException;
  FServer.OnListenException := OnIdServerListenException;
//  FServer.OnStatus := OnIdServerStatus;

  FLogActive := TProtectedBoolean.Create(False);
  FBindString := TProtectedString.Create(EmptyStr);
  FExecuteCount := TProtectedInt64.Create(0);
end;


destructor TGRIDService_IndyTCPServer.Destroy;
begin
  BeforeDestroy;
  FreeAndNil(FServer);
  FreeAndNil(FLogActive);
  FreeAndNil(FBindString);
  FreeAndNil(FExecuteCount);
  inherited;
end;


procedure TGRIDService_IndyTCPServer.Disable;
begin
  FServer.Active := False;
end;

procedure TGRIDService_IndyTCPServer.Enable;
begin
  Port := FDesiredPort;
  FServer.Active := True;
end;

function TGRIDService_IndyTCPServer.GetActive: Boolean;
begin
  Result := FServer.Active;
end;

function TGRIDService_IndyTCPServer.GetBindString: String;
begin
  Result := FBindString.Value;
end;

function TGRIDService_IndyTCPServer.GetContext(CtxId: String;
  out aContext: TGridContext): Boolean;
var i : integer;
{$IFDEF FPC}
    list: Classes.TList;
{$ELSE}
{$IFDEF NEXTGEN}
    list: TList<TIdContext>;
{$ELSE}
    list: TList;
{$ENDIF}
{$ENDIF}
begin
  aContext := nil;
  Result := False;
  list := FServer.Contexts.LockList;
  try
    for i := 0 to list.Count-1 do
    begin
      if TGridContext(list[i]).ContextGUID = CtxId  then
      begin
        aContext := TGridContext(list[i]);
        Result := true;
        Break;
      end;
    end;
  finally
    FServer.Contexts.UnlockList;
  end;
end;

function TGRIDService_IndyTCPServer.GetDescription: String;
begin
  Result := 'GRID TCP Raw protocol buffer Communication service';
end;

function TGRIDService_IndyTCPServer.GetExecuteCount: Int64;
begin
  Result := FExecuteCount.Value;
end;

function TGRIDService_IndyTCPServer.GetInstanceType: TopGRIDServerServiceInstanceType;
begin
  Result := gtComServer;
end;

function TGRIDService_IndyTCPServer.GetLogActive: Boolean;
begin
  Result := FLogActive.Value;
end;

function TGRIDService_IndyTCPServer.GetPort: Word;
begin
  result := FServer.DefaultPort;
end;

procedure TGRIDService_IndyTCPServer.InternalAccreditationProcess(
  aReceivedStream: TMemoryStream; aContext: TGridContext; Var ErrorString : String);
var aB : TGRIDClientServerProtocolTransportBOManager;
    aA : TGRIDC2SAuth;

    rR : TGRIDS2CProtocolEntry;
    rA : TGRIDS2CAuth;
    responseStream : TMemoryStream;
begin
  Assert(Assigned(aReceivedStream));
  Assert(Assigned(aContext));

  aReceivedStream.Position := 0;
  //If we are not locally (thread level) accredited, we decode the incoming C2S object to obtain data,
  //and then build a serverProtocol compliant object to ask credential

  ab := TGRIDClientServerProtocolTransportBOManager.Create;
  try
    try
      aB.LoadFromStream(aReceivedStream);

      If TGCPUtils.GetFirstC2SAuth(aB,aA) then
      begin
        //Send Auth message (Manage in Grid.Client.Core - By central service.)
        ServiceAuthorization(Self,aContext.AuthReader,aA.UserName,aA.Password,aContext.JsonSignature); //response shall be received by aContext's methods OnServerMessage_AuthResponse

        aB.StoredObject.ClearAllData(False);
        rR := TGCPUtils.BuildS2CProtocolObject(aB);
        rA := TGRIDS2CAuth.Create(aB);
        rR.AddContent(rA);
        rA.Approved := aContext.IsAccredited;
        rA.SessionID := aContext.SessionID;
        rA.DesapproveReaseon := EmptyStr; //Will perhaps use in a less aggressive version : Here, we kick out the non accredited client. To see.
        if Not(aContext.IsAccredited) then
        begin
          ServerMessageSendBroadCast(TofServerComChannelInternalOrder.cioTCPConnectionManagement_AddUser,TGridContext(AContext).JSONSignature);
          ErrorString := 'InternalAccreditationProcess - Context accreditation fail : kick.';
          Exit;
        end
        else
        begin
          responseStream := TMemoryStream.Create;
          try
            aB.SaveToStream(responseStream);
            responseStream.Position := 0;
            aContext.Connection.IOHandler.Write(responseStream,responseStream.Size,True);
          finally
            FreeAndNil(responseStream);
          end;
        end;
      end
      else
      begin
        ErrorString := 'InternalAccreditationProcess - Client message fail';
      end;

    Except
      On E : Exception do
      begin
        ErrorString := 'InternalAccreditationProcess - Protocol failure - ('+E.Message+')';
      end;
    end;


  finally
    FreeAndNil(aB); //Clean all bo message.
  end;

end;

procedure TGRIDService_IndyTCPServer.InternalFullUnsubscribtion(
  aContext: TGridContext);
var i : Integer;
begin
  for I := 0 to aContext.ChannelsSubscription.Count-1 do
  begin
    InternalMessageUnsubscription(aContext,aContext.ChannelsSubscription[i]);
  end;
end;

procedure TGRIDService_IndyTCPServer.InternalHubProcess(
  aReceivedStream: TMemoryStream; aContext: TGridContext;
  var ErrorString: String);
var aB : TGRIDClientServerProtocolTransportBOManager;
    aA : TGRIDC2SMessaging;
    aRPC : TGRIDC2SRPC;
    lTemp : TMemoryStream;

    aProtoEntry : TGRIDC2SProtocolEntry;
    responseStream : TMemoryStream;
    i : Integer;
    aR : TGRIDS2CProtocolEntry;
    aM : TGRIDS2CMessaging;

    Procedure ProcessToInternalMessagingManagement;
    begin
      if Assigned(lTemp) then
      begin
        if ltemp.Size>0 then
        begin
          ltemp.Position := 0;
          InternalMessagingManagement(ltemp, aContext, ErrorString);
        end;
      end;
    end;

    Procedure ProcessToInternalRPCManagement;
    begin
      if Assigned(lTemp) then
      begin
        if ltemp.Size>0 then
        begin
          ltemp.Position := 0;
          InternalRPCManagement(ltemp, aContext, ErrorString);
        end;
      end;
    end;


begin
  Assert(Assigned(aReceivedStream));
  Assert(Assigned(aContext));

  aReceivedStream.Position := 0;

  //Message Response nest.
  aContext.GeneralMessageReply.StoredObject.ClearAllData(false);

  ab := TGRIDClientServerProtocolTransportBOManager.Create;
  try
    aB.LoadFromStream(aReceivedStream);  //Client stream read.

    if TGCPUtils.GetFirstC2SProtocolEntry(ab,aProtoEntry) then
    begin

      for I := 0 to aProtoEntry.ContentCount-1 do
      begin
        if aProtoEntry.Content[i] is TGRIDC2SMessaging then  //It is a classic message.
        begin
          aA := TGRIDC2SMessaging(aProtoEntry.Content[i]);
          lTemp := TMemoryStream(aA.Content); //Deep copy.
          try
            ProcessToInternalMessagingManagement;
          finally
            FreeAndNil(lTemp);
          end;
        end
        else
        if aProtoEntry.Content[i] is TGRIDC2SRPC then  //It is a RPC message.
        begin
          aRPC := TGRIDC2SRPC(aProtoEntry.Content[i]);
          lTemp := TMemoryStream(aRPC.Content); //Deep copy.
          try
            ProcessToInternalRPCManagement;
          finally
            FreeAndNil(lTemp);
          end;
        end
        else
        begin
          ServerLog(Self,'InternalHubProcess - Warning : '+aProtoEntry.Content[i].ClassName+' not handled.');
        end;
      end;
    end
    else
    begin
      ErrorString := 'InternalHubProcess - No message to process';
    end;

    //aContext.GeneralMessageReplyProcess is a boolean which indicate if we need a reply to the client.
    //This var is setting by all the above process of InternalMessagingManagement.
    if aContext.GeneralMessageReplyProcess then
    begin
      //Protocol (or client) asked to send something back.

      //Nothing here, we inform there are nothing. :-/
      if aContext.GeneralMessageReply.StoredObject.ObjectCount=0 then
      begin
        aContext.BuildACKForNoMessage;
      end;

      //We reply all the stuff in GeneralMessage Reply as a response : Compile all and send to client.
      ResponseStream := TMemoryStream.Create;
      try
        aContext.GeneralMessageReply.SaveToStream(ResponseStream);
        responseStream.Position := 0;
        ServerLog(Self,'InternalHubProcess - Sending transport with '+IntToStr(aContext.GeneralMessageReply.StoredObject.BusinessObjectsCount)+' message(s)');

        //TODO, when it is a send message, we should send an ACK / Parameter ?.

        //TODO : Clarify.
        //Current Protocol :
        //TGRIDClientServerProtocolBOManager
        //  TGRIDS2CProtocolEntry
        //    TGRIDS2CSendMessage
        //       <List of TGRIDMessage in stream>
        //          Note : Depending the app, you can have in TGRIDMessage streams which is another unknow type of message.>


        ab.StoredObject.ClearAllData(False);
        ar := TGCPUtils.BuildS2CProtocolObject(ab);
        am := TGRIDS2CMessaging.Create(ab);
        ar.AddContent(am);
        am.Content := responseStream; //Response stream could be what you want. Potentialy not known by GRID.
        responseStream.Clear;
        ab.SaveToStream(responseStream); //We reuse ResponseStream to save the whole stuff (Protocol + data)
        responseStream.Position := 0;
        aContext.Connection.IOHandler.Write(responseStream,responseStream.Size,True); //Put on the wire. -> Take time !

        ServerLog(Self, 'InternalHubProcess - Sended : '+IntToStr(responseStream.Size)+
                        ' byte(s) transfered, '+
                        IntToStr(aContext.GeneralMessageReply.StoredObject.ObjectCount)+' messages replied.');

      finally
        FreeAndNil(responseStream);
        aContext.GeneralMessageReply.StoredObject.ClearAllData(false);
      end;
    end;

  finally
    FreeAndNil(ab);
  end;

end;

procedure TGRIDService_IndyTCPServer.InternalMessageReply(
  aContext: TGridContext; WaitDelayInMS : Integer);
var i : integer;
    aAr : Array of TBusClientReader;
begin
  Assert(Assigned(aContext));
  SetLength(aAr, aContext.ChannelsSubscription.Count);

  if aContext.ChannelsSubscription.Count >0 then //Only if there are channels to survey.
  begin
    //Populating Client array...
    for I := 0 to aContext.ChannelsSubscription.Count - 1 do
    begin
      aAr[I] := TBusClientReader(aContext.ChannelsSubscription.Objects[I]);
    end;

    //Immediate call (no delay)
    Bus.ProcessMessages(aAr); //That trig events, and fill GeneralMessageReply with messages.

    //If the Message queue is empty here : In this case, here is the scenario :
    //- Client may asked to wait (lock client side) an amount of time until message arrive.
    //- Else, or if there are still no message when wait time is consumed, we put a ACK message of type "NO MESSAGE" to the client.
    if aContext.GeneralMessageReply.StoredObject.ObjectCount = 0 then
    begin
      if WaitDelayInMS>0 then
      begin
        if aContext.GeneralMessageEvent.WaitFor(WaitDelayInMS) = wrSignaled then
        begin
          Bus.ProcessMessages(aAr); //That trig events, and fill GeneralMessageReply with messages.
        End;
      end;
    end;
  end
  else
  begin
    //We ask for message reception, but we have NO subscription : It is a fail case.
    aContext.BuildACKSoftFail(CST_EXCEPTION_ASK_FOR_RECEIVE_MSG_BUT_NO_SUBSCRIPTION); //This will add a message in reception stream.
  end;

  //In all case, we will send back something to the client.
  aContext.GeneralMessageReplyProcess := True;
end;

procedure TGRIDService_IndyTCPServer.InternalMessagingRPCProcess(
  aContext : TGridContext; aRPCMessage : TGRIDMessageRPCAsk);
var lm, ResponseStream : TMemoryStream;
    aB : TGRIDClientServerProtocolTransportBOManager;
    aR : TGRIDS2CProtocolEntry;
    am : TGRIDS2CRPC;
begin
  //aRPC.Channel is the original FunctionName ask by the user.
  lm := TMemoryStream(aRPCMessage.Buffer); //Copy.
  try
    aContext.GeneralMessageReply.StoredObject.ClearAllData(False);
    ServerMessageSendRPC(Self,aRPCMessage.FonctionName,lm,aContext.RPCReader,aContext.JsonSignature);
  finally
    FreeAndNil(lm);
  end;

  //The response is in Context.GeneralMessageReplyRPC.
  if (aContext.GeneralMessageReplyRPC.StoredObject.ObjectCount=0) then
  begin
    raise Exception.Create('Error : No Message. Code 995.');
    //aContext.BuildRPCFailNoMessage('No result');;
  end;

  //3 Directly send reponse in right format.
  //We reply all the stuff in GeneralMessageRPCReply as a response : Compile all and send to client.
  ResponseStream := TMemoryStream.Create;
  try
    aContext.GeneralMessageReplyRPC.SaveToStream(ResponseStream);
    ResponseStream.Position := 0;
    ServerLog(Self,'InternalMessageRPCProcess - Sending transport with '+IntToStr(aContext.GeneralMessageReplyRPC.StoredObject.BusinessObjectsCount)+' RPC message(s)');

    //TODO, when it is a send message, we should send an ACK / Parameter ?.

    //TODO : Clarify.
    //Current Protocol :
    //TGRIDClientServerProtocolBOManager
    //  TGRIDS2CProtocolEntry
    //    TGRIDS2CRPC
    //       TGRIDMessageRPCResponse (one object which contain an stream, with RPC private embeded protocol)

    aB := TGRIDClientServerProtocolTransportBOManager.Create;
    ar := TGCPUtils.BuildS2CProtocolObject(aB);
    am := TGRIDS2CRPC.Create(aB);
    ar.AddContent(am);
    am.Content := ResponseStream; //Response stream could be what you want. Not known by GRID.
    responseStream.Clear;
    ab.SaveToStream(responseStream); //We reuse ResponseStream to save the whole stuff (Protocol + data)
    responseStream.Position := 0;
    aContext.Connection.IOHandler.Write(responseStream,responseStream.Size,True); //Put on the wire.

    ServerLog(Self, 'InternalMessageRPCProcess - Sended : '+IntToStr(responseStream.Size)+
                    ' byte(s) transfered, '+
                    IntToStr(aContext.GeneralMessageReplyRPC.StoredObject.ObjectCount)+' messages replied.');

  finally
    FreeAndNil(ResponseStream);
    FreeAndNil(aB);
    aContext.GeneralMessageReplyRPC.StoredObject.ClearAllData(false);
  end;
end;

procedure TGRIDService_IndyTCPServer.InternalMessageSubscription(
  aContext: TGridContext; aChannel: String);
begin
  Assert(Assigned(aContext));
  aContext.Subscription(aChannel);
end;

procedure TGRIDService_IndyTCPServer.InternalMessageUnsubscription(
  aContext: TGridContext; aChannel: String);
begin
  Assert(Assigned(aContext));
  aContext.Unsubscription(aChannel);
end;

procedure TGRIDService_IndyTCPServer.InternalMessagingManagement(
  aReceivedStream: TMemoryStream; aContext: TGridContext;
  var ErrorString: String);
var aMS : TGRIDClientServerProtocolMessageServiceManager;
    aMessage : TGRIDMessage;
    lTempMem : TMemoryStream;
    i : integer;
begin
  Assert(Assigned(aReceivedStream));
  Assert(aReceivedStream.Size>0);

  aMS := TGRIDClientServerProtocolMessageServiceManager.Create;
  try
    aMS.LoadFromStream(aReceivedStream);
    ServerLog(Self,'InternalMessagingManagement : '+IntToStr(aMS.StoredObject.ObjectCount)+' Message(s) to process.');
    for i := 0 to aMS.StoredObject.ObjectCount-1 do
    begin
      if aMS.StoredObject.Objects[i] is TGRIDMessage then //a client has SENT a message.
      begin
        aMessage := TGRIDMessage(aMS.StoredObject.Objects[i]);
        if length(aMessage.Channel)>0 then
        begin
          lTempMem := TMemoryStream(aMessage.Buffer); //Buffer : copy.
          ServerLog(Self,'InternalMessagingManagement : Processing classic SEND Message '+IntToStr(i)+' id "'+aMessage.Id+'" (Size:'+IntToStr(lTempMem.Size)+')');
          try
            ServerMessageSend(Self,aMessage.Channel, lTempMem,EmptyStr,aContext.JsonSignature); //   <---- lTempMem is the original stream from the "origin" client (client which send the orig. message.)
          finally
            FreeAndNil(lTempMem);
          end;
        end
        else
        begin
          ServerLog(Self,'InternalMessagingManagement : Message '+IntToStr(i)+' id "'+aMessage.Id+'" have no target channel. Aborting.');
        end;
      end
      else
      if aMS.StoredObject.Objects[i] is TGRIDMessageSystem then //a client calling special function (sub, unsub) or ask to GET message.
      begin
        if aMS.StoredObject.Objects[i] is TGRIDMessageGetMsg then
        begin
          //Will Reply to client.
          ServerLog(Self,'InternalMessagingManagement : GETMESSAGE GetMessage ('+IntToStr(i)+'-');
          InternalMessageReply(aContext, TGRIDMessageGetMsg(aMS.StoredObject.Objects[i]).WaitDelay); //Trig event Context.OnServerMessage_General in a synchrone way. - In this thread.
        end
        else
        if aMS.StoredObject.Objects[i] is TGRIDMessageSubscription then
        begin
          //Will No reply to client.
          ServerLog(Self,'InternalMessagingManagement : SUBSCRIPTION Message '+IntToStr(i)+' for Channel "'+TGRIDMessageSubscription(aMS.StoredObject.Objects[i]).TargetChannel+'"');
          InternalMessageSubscription(aContext, TGRIDMessageSubscription(aMS.StoredObject.Objects[i]).TargetChannel);
        end
        else
        if aMS.StoredObject.Objects[i] is TGRIDMessageUnsubscription then
        begin
          //Will No reply to client.
          ServerLog(Self,'InternalMessagingManagement : UNSUBSCRIPTION Message '+IntToStr(i)+' for Channel "'+TGRIDMessageUnsubscription(aMS.StoredObject.Objects[i]).TargetChannel+'"');
          InternalMessageUnsubscription(aContext, TGRIDMessageUnsubscription(aMS.StoredObject.Objects[i]).TargetChannel);
        end
        else
        begin
          ServerLog(Self,'InternalMessagingManagement : Message class unknown in System subdivision : '+aMS.StoredObject.Objects[i].ClassName);
        end;
      end
      else
      begin
        ServerLog(Self,'InternalMessagingManagement : Message class unknown : '+aMS.StoredObject.Objects[i].ClassName);
      end;
    end;
  finally
    FreeAndNil(aMS);
  end;
end;

procedure TGRIDService_IndyTCPServer.InternalRPCManagement(
  aReceivedStream: TMemoryStream; aContext: TGridContext;
  var ErrorString: String);
var aMS : TGRIDClientServerProtocolRPCBOManager;
    aRPC : TGRIDMessageRPCAsk;
    i : integer;
begin
  Assert(Assigned(aReceivedStream));
  Assert(aReceivedStream.Size>0);

  aMS := TGRIDClientServerProtocolRPCBOManager.Create;
  try
    aMS.LoadFromStream(aReceivedStream);
    ServerLog(Self,'InternalRPCManagement : '+IntToStr(aMS.StoredObject.ObjectCount)+' Message(s) to process.');
    for i := 0 to aMS.StoredObject.ObjectCount-1 do
    begin
      if aMS.StoredObject.Objects[i] is TGRIDMessageRPCAsk then
      begin
        aRPC := TGRIDMessageRPCAsk(aMS.StoredObject.Objects[i]);
        if length(aRPC.FonctionName)>0 then
        begin
          ServerLog(Self,'InternalRPCManagement : Processing RPC '+IntToStr(i)+' "'+aRPC.FonctionName+'"');
          InternalMessagingRPCProcess(aContext, aRPC); //This RPCPRocess do the receive and the send...

          aContext.GeneralMessageReplyRPC.StoredObject.ClearAllData(false); //...and all is finished yet.
          aContext.GeneralMessageReply.StoredObject.ClearAllData(false);
          aContext.GeneralMessageReplyProcess := False; //We do not want standart reply in this case. It is not a message.
        end
        else
        begin
          ServerLog(Self,'InternalRPCManagement : RPC Message '+IntToStr(i)+' FonctionName :  "'+aRPC.FonctionName+'"');
        end;
      end;
    end;
  finally
    FreeAndNil(aMS);
  end;
end;

function TGRIDService_IndyTCPServer.IPClient(aContext: TidContext): String;
begin
  Result := ' ['+AContext.Binding.PeerIP+':'+IntToStr(AContext.Binding.PeerPort)+'] Thread# '+IntTostr(ThreadID);
end;

function TGRIDService_IndyTCPServer.IPFilter(const StrIP: string): Boolean;
var i, j: integer;
{$IFDEF FPC}
    list: Classes.TList;
{$ELSE}
{$IFDEF NEXTGEN}
    list: TList<TIdContext>;
{$ELSE}
    list: TList;
{$ENDIF}
{$ENDIF}
begin
  j := 0;
  list := FServer.Contexts.LockList;
  try
    for i := 0 to list.Count-1 do
    begin
      if TIdContext(list[i]).Binding.PeerIP = StrIP then
        Inc(j);
    end;
    Result := j <= CST_IPFILTER_LIMIT;
  finally
    FServer.Contexts.UnlockList;
  end;
end;

procedure TGRIDService_IndyTCPServer.LoadConfiguration;
var lFile : TStringList;
    lFileName : String;
    i : integer;
    ls : string;
begin
  FDesiredPort := 0;
  lFileName := ExtractFileDir(ParamStr(0))+'\GRIDServer.configuration';
  if FileExists(lFileName) then
  begin
    lFile := TStringList.Create;
    try
      lFile.LoadFromFile(lFileName);
      ls := lFile.Values['PORT'];
      if length(ls)>0 then
        FDesiredPort := StrToIntDef(ls,0);
    finally
      FreeAndNil(lFile);
    end;
    ServerLog(Self,'GRID Server configuration loaded and applyed.');
  end;
end;

procedure TGRIDService_IndyTCPServer.OnIdServerAfterBind(Sender: TObject);
var
   n:Integer;
   s : string;
begin
  //This run in main thread.
  //s := FServer.Bindings[0].PeerIP + #13#10;
  s := GStack.LocalAddresses.Text; // + #13#10;
  for n:= 0 to FServer.Bindings.Count-1 do
  begin
    with FServer.Bindings[n] do
    begin
      if (n=0) then
      begin
        FServer.DefaultPort := FServer.Bindings[n].Port;
      end;
      s := s + ( ip+':'+IntToStr(Port) ) + ' / ' + PeerIP + #13#10;
    end;
  end;
  FBindString.Value := s;
end;

procedure TGRIDService_IndyTCPServer.OnIdServerConnect(AContext: TIdContext);
//    Mes : TopTaskMessage;
begin
  ServerLog(Self,'OnIdServerConnect('+IntToStr(Integer(AContext))+') ');

//  s := TGridProtocole.ComServer.GenerateClientConnection(  InstanceGUID,
//                                                           cstTCP,
//                                                           TGridContext(AContext).ContextGUID,
//                                                           AContext.Binding.PeerIP,
//                                                           AContext.Binding.PeerPort);
  try
//    Mes.BuildStreamMessage(s);
//    DeliverMessage(Mes);
  finally
//    FreeAndNil(s);
  end;
end;

procedure TGRIDService_IndyTCPServer.OnIdServerContextCreated(
  AContext: TIdContext);
begin
  ServerLog(Self,'OnIdServerOnIdServerContextCreated('+IntToStr(Integer(AContext))+') ');
  //TODO Destroy ? Create ?
{$IFDEF FPC}
  TGridContext(AContext).Server := FServer;
{$ENDIF}
end;

procedure TGRIDService_IndyTCPServer.OnIdServerDisconnect(AContext: TIdContext);
//var s : TMemoryStream;
//    Mes : TopTaskMessage;
begin
  ServerLog(Self,'OnIdServerDisconnect('+IntToStr(Integer(AContext))+') ');
  ServerMessageSendBroadCast(TofServerComChannelInternalOrder.cioTCPConnectionManagement_UserDisconnect,TGridContext(AContext).JSONSignature);

//  s := TGridProtocole.ComServer.GenerateClientDisConnection( InstanceGUID,
//                                                             cstTCP,
//                                                             TGridContext(AContext).ContextGUID,
//                                                             AContext.Binding.PeerIP,
//                                                             AContext.Binding.PeerPort);
  try
//    Mes.BuildStreamMessage(s);
//    DeliverMessage(Mes);
  finally
//    FreeAndNil(s);
  end;
end;

procedure TGRIDService_IndyTCPServer.OnIdServerException(AContext: TIdContext;
  AException: Exception);
begin
  ServerLog(Self,'OnIdServerException('+IntToStr(Integer(AContext))+') '+AContext.Binding.PeerIP + '"'+AException.Message);
end;


procedure TGRIDService_IndyTCPServer.OnIdServerListenException(
  AThread: TIdListenerThread; AException: Exception);
begin
  ServerLog(Self,'OnIdServerListenException('+IntToStr(Integer(AThread))+','+IntToStr(Integer(AException))+')');
end;

procedure TGRIDService_IndyTCPServer.OnIdServerStatus(ASender: TObject;
  const AStatus: TIdStatus; const AStatusText: string);
begin
  ServerLog(Self,'OnIdServerStatus('+AStatusText+')');
end;

procedure TGRIDService_IndyTCPServer.OnServerBeforeListenerRun(
  AThread: TIdThread);
begin
  ServerLog(Self,'OnServerBeforeListenerRun('+IntToStr(Integer(AThread))+')');
end;


procedure TGRIDService_IndyTCPServer.OnServerMessage_FromCentral(Sender: TBus;
  var Packet: TBusEnvelop);
begin
 // :) Todo - stop, start and whatever.
end;


procedure TGRIDService_IndyTCPServer.SetLogActive(const Value: Boolean);
begin
  FLogActive.Value := Value;
end;

procedure TGRIDService_IndyTCPServer.SetPort(const Value: Word);
var i : Integer;
begin
  FServer.DefaultPort := Value;
  //Force yet existing binding.
  for I := 0 to FServer.Bindings.Count-1 do
  begin
    FServer.Bindings.Items[i].Port := Value;
  end;
end;


procedure TGRIDService_IndyTCPServer.OnIdServerExecute(AContext: TIdContext);
var
  ErrorStr : String;
  LocalStream : TMemoryStream;
  LocalContext : TGridContext;

  Procedure GenFatalError(anErrorTxt : string);
  begin
    ErrorStr := 'OnIdServerExecute( Context = '+IntToStr(Integer(AContext))+')'+IPClient(aContext)+CST_CRLF;
    ErrorStr := ErrorStr + 'OnIdServerExecute : '+anErrorTxt+' : Client Disconnecting.'+IPClient(aContext);
  end;

  Procedure StandartProtocolApply(var ErrorString : String);
  begin
    LocalStream := TMemoryStream.Create;
    try
      LocalContext.Connection.IOHandler.ReadStream(LocalStream);

      if LocalStream.Size>0 then
      begin

        //BUSINESS
        if Not(LocalContext.IsAccredited) then
        begin
          //Make accreditation.
          InternalAccreditationProcess(LocalStream, LocalContext, ErrorString);
        end
        else
        begin
          //Yet accedited - Message Hub
          InternalHubProcess(LocalStream, LocalContext, ErrorString);
        end;

        //END OF BUSINESS

      end
      else
      begin
        GenFatalError('Empty stream');
      end;

    Finally
      FreeAndNil(LocalStream);
    end;
  end;


begin
  LocalContext := TGridContext(AContext);

  ErrorStr := EmptyStr;
  FExecuteCount.Inc;
  LocalContext.GeneralMessageReplyProcess := False;

  // alternatively, if you call Disconnect(), make sure
  // the IOHandler's InputBuffer is empty, or else
  // AContext.Connection.Connected() will continue
  // returning True!...

  //Just for documentation
  //if not IPFilter(AContext.Binding.PeerIP) then
  //begin
  //  GenFatalError('IPFilter check negative (>'+IntToStr(CST_IPFILTER_LIMIT)+' connections at a time)');
  //end
  //else
  //begin
    //!!LocalContext.Connection.IOHandler.Readable //VGS : Lock sometimes. ??

  try
    StandartProtocolApply(ErrorStr);
  Finally
  end;

  if Length(ErrorStr)>0 then
  begin
    ServerLog(Self,ErrorStr);
    LocalContext.Connection.Disconnect;
    LocalContext.Connection.IOHandler.InputBuffer.Clear;
    Exit;
  end;

end;


procedure TGridContext.BuildACKForNoMessage;
var aMessEnv : TGRIDMessageGetMsgReply;
begin
  //Note : Entry point in server for this event is InternalMessageReply.
  Assert(Assigned(GeneralMessageReply));
  aMessEnv := TGRIDMessageGetMsgReply.Create(GeneralMessageReply);
  aMessEnv.Code := gmrNoMessageOnServer;
  aMessEnv.Information := EmptyStr;
end;

procedure TGridContext.BuildACKSoftFail(InfoReason: String);
var aMessEnv : TGRIDMessageGetMsgReply;
begin
  //Note : Entry point in server for this event is InternalMessageReply.
  Assert(Assigned(GeneralMessageReply));
  aMessEnv := TGRIDMessageGetMsgReply.Create(GeneralMessageReply);
  aMessEnv.Code := gmrFail;
  aMessEnv.Information := InfoReason;
end;


{ TGridContext }

{$IFDEF FPC}
constructor TGridContext.Create(
    AConnection: TIdTCPConnection;
    AYarn: TIdYarn;
    AList: Classes.TThreadList = nil
    );
var a : TGUID;
{$ELSE}
constructor TGridContext.Create(AConnection: TIdTCPConnection; AYarn: TIdYarn;
  AList: TIdContextThreadList);
{$ENDIF}
begin
  inherited;
  {$IFDEF FPC}
  if CreateGUID(a) = s_OK then
    ContextGUID := 'CxTcp'+GuidToString(a)
  else
    RaiseLastOSError;
  {$ELSE}
  ContextGUID := 'CxTcp'+TGUID.NewGuid.ToString;
  {$ENDIF }

  ServerLog(Self, 'New context connection : '+ContextGUID);
  IsAccredited := False;

  //Open a channel for auth for this context only.
  AuthReader := Bus.Subscribe('Auth_'+ContextGUID,OnServerMessage_AuthResponse);
  RPCReader :=  Bus.Subscribe('RPC_'+ContextGUID,OnServerMessage_RPCType);

  AuthReader.Event := Bus.GetNewEvent; //Private event for auth.
  RPCReader.Event :=  Bus.GetNewEvent; //Private event for RPC process.
  GeneralMessageEvent := Bus.GetNewEvent; //Event will be shared between all subscription.

  //Protocol wrapper.
  GeneralMessageReply := TGRIDClientServerProtocolMessageServiceManager.Create; //Pure messaging.
  GeneralMessageReplyRPC := TGRIDClientServerProtocolRPCBOManager.Create;         //RPC

  //List of listening channels.
  ChannelsSubscription := TStringList.Create;
end;

destructor TGridContext.Destroy;
begin
  ServerLog(Self,'----------------------------------');
  ServerLog(Self,'----------------------------------');
  ServerLog(Self,'----------------------------------');
  ServerLog(Self,'Destroy context : '+ContextGUID);
  ServerLog(Self,'----------------------------------');

  ServerLog(Self,'Removing ('+IntToStr(Integer(Self))+') with '+IntToStr(ChannelsSubscription.Count) +' subscription(s).');
  UnsubscriptionFull;
  ServerLog(Self,'----------------------------------');
  //Those channels belong to the context.
  Bus.ChannelDelete(AuthReader.ChannelListening);
  Bus.ChannelDelete(RPCReader.ChannelListening);

  FreeAndNil(ChannelsSubscription);
  FreeAndNil(GeneralMessageReply);
  FreeAndNil(GeneralMessageEvent);

  FreeAndNil(AuthReader);
  FreeAndNil(RPCReader);
  inherited;
end;

function TGridContext.JsonSignature: String;
var lj,ljj : Variant;

    a : TGRIDJSONContext;
    b : TGridJSONInfoData;
begin
  //Tech prob : We can use only CrossPlatform, due to target Mobil delphi. less powerfull that TDocVariant in SynCommon.
  //ljj := TJSODocVariant.NewObject(['PeerIP',Binding.PeerIP,'PeerPort',Binding.Port,'ContextGUID',ContextGUID]); :((

  {
  //And the nested object affectation does not work.
  TJSONVariant.Create;
  TJSONVariantData(ljj)['PeerID'] := Binding.PeerIP;
  TJSONVariantData(ljj)['PeerPort'] := Binding.PeerPort;
  TJSONVariantData(ljj)['ContextGUID'] := ContextGUID;
  TJSONVariantData(lj)['Origin'] := 'TCP';
  //TJSONVariantData(lj)['ContextualData'] := TJSONVariantData(ljj).Values; Does not work.
}
//Only solution, use rtti. :(
  b := TGridJSONInfoData.Create;
  a := TGRIDJSONContext.Create;
  b.ContextualData := a;
  try
    b.Origin := 'TCP';
    b.ContextualData.PeerIP := Binding.PeerIP;
    b.ContextualData.PeerPort := Binding.PeerPort;
    b.ContextualData.ContextGUID := ContextGUID;
    Result := TGSJson.ObjectToJson(b);
  finally
    FreeAndNil(a);
    FreeAndNil(b);
  end;
end;

procedure TGridContext.OnServerMessage_AuthResponse(Sender: TBus;
  var Packet: TBusEnvelop);
var s : TMemoryStream;
begin
  //Response from core central service in packet
  //Boolean, Session ID if boolean true.
  s := Packet.ContentMessage.AsStream;
  try
    IsAccredited := ReadBoolean(s);
    SessionID := ReadString(s);
  finally
    FreeAndNil(s);
  end;

  if IsAccredited then
  begin
    ServerLog(Self,'Connection Context '+ContextGUID+ ' accredited - Session '+SessionID);
  end
  else
  begin
    ServerLog(Self,'Connection Context '+ContextGUID+ ' NOT accredited');
  end;
end;

procedure TGridContext.OnServerMessage_General(Sender: TBus;
  var Packet: TBusEnvelop);
var s : TMemoryStream;
    aMessEnv : TGRIDMessage;
begin
  //Note : Entry point in server for this event is InternalMessageReply.
  Assert(Assigned(GeneralMessageReply));
  //Rise when message arriving on this bus. This event handler manage all the subscription ask by the client.
  s := Packet.ContentMessage.AsStream;
  try
    s.Position := 0;
    //Add Transport-level protocol message...
    aMessEnv := TGRIDMessage.Create(GeneralMessageReply);
    aMessEnv.Id := IntToStr(Packet.EnvelopId);
    aMessEnv.Channel := Packet.TargetChannel;
    aMessEnv.Buffer := s; //We write the packet in a binary form in aMessEnb TGridMessage.
    // --> Remember : .Create(GeneralMEssageReply) automaticaly add the object to its internal list.
    //                aMessEnv will be alive outside this call.
  finally
    FreeAndNil(s);
  end;
  //Here stats feeding ?
end;

procedure TGridContext.OnServerMessage_RPCType(Sender: TBus;
  var Packet: TBusEnvelop);
var s : TMemoryStream;
    aMessEnv : TGRIDMessageRPCResponse;
begin
  //Note : Entry point in server for this event is InternalMessageReplyRPC.
  Assert(Assigned(GeneralMessageReplyRPC));

  s := Packet.ContentMessage.AsStream;

  if s.Size = 0 then
  begin
    raise Exception.Create('Error Message');
  end;

  try
    s.Position := 0;
    //Add Transport-level protocol message...
    aMessEnv := TGRIDMessageRPCResponse.Create(GeneralMessageReplyRPC);
    aMessEnv.Buffer := s; //We write the packet in a binary form in aMessEnb TGridRPCMessage.
    // --> Remember : .Create(GeneralMEssageReplyRPC) automaticaly add the object to its internal list.
    //                aMessEnv will still be alive outside this call.
  finally
    FreeAndNil(s);
  end;
  //Here stats feeding ?
end;

procedure TGridContext.Subscription(aChannelName: string);
var li : Integer;
    bcr : TBusClientReader;
begin
  Assert(Length(aChannelName)>0);
  li := ChannelsSubscription.IndexOf(aChannelName);
  if li=-1 then
  begin
    bcr := Bus.Subscribe(aChannelName,OnServerMessage_General);
    bcr.Event := GeneralMessageEvent;
    ChannelsSubscription.AddObject(aChannelName,bcr);
    //Todo : Historize...
  end;
end;

procedure TGridContext.Unsubscription(aChannelName: string);
var li : Integer;
    bcr : TBusClientReader;
begin
  Assert(Length(aChannelName)>0);
  li := ChannelsSubscription.IndexOf(aChannelName);
  if li>-1 then
  begin
    bcr := TBusClientReader(ChannelsSubscription.Objects[li]);
    if Bus.UnSubscribe(bcr,aChannelName) then
      ChannelsSubscription.Delete(li);
    //Todo : Historize...
  end;
end;

procedure TGridContext.UnsubscriptionFull;
var i : Integer;
begin
  for I := ChannelsSubscription.Count-1 downto 0 do
  begin
    Unsubscription(ChannelsSubscription[i]);
  end;
end;

{ TGridJSONInfoData }


Initialization

Finalization
end.
