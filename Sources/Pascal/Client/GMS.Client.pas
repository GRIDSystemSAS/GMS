unit GMS.Client;

interface

Uses
{$IFDEF FPC}
{$mode Delphi}
SysUtils, Classes, Generics,
{$ELSE}
System.SysUtils, System.Classes, System.Generics.Collections,
{$ENDIF}
     GS.Stream,
     GS.StoredObject.Stream,
     GMS.Client.Transporter,
     GMS.Client.Connection,
     GMS.ClientServer.Protocol.Helper,                 //Utility
     GMS.ClientServer.Protocol.Transport.BO,           //Transport layer
     GMS.ClientServer.Protocol.MessageService.BO,      //Message service.
     GMS.ClientServer.Protocol.RPC.BO,                 //RPC service.
     GMS.ClientServer.DataModel.Central.BO,            //Central stuff (cpu, service, naming)
     GMS.ClientServer.Protocol.Central.BO,             //Central's Protocol wrapping.
     GMS.ClientServer.Types.ChannelDefinitions;        //Channel name.

Type

TGridClientGetMessageInfo = (gmiNone, gmiOk, gmiFail, gmiNoMessageOnServer, gmiMessagesStillPendingOnServer);
TGridClientGetMessageInfoStruct = Record
  ResultInfo : TGridClientGetMessageInfo;
  TxtInfo : String;
end;

TGridClientMessageRecept = Class
  MessageID : String;
  MessageChannel : String;
  MessageContent : TMemoryStream;

  Constructor Create; virtual;
  Destructor Destroy; Override;

End;

TGridClientMessagesBox = Class(TObjectList<TGridClientMessageRecept>)
  Procedure AddMessage(aID, aChannel : String; aContent : TMemoryStream);
End;

TGridClientRPCMessageRecept = Class
  FunctionName : String;
  Succes : Boolean;
  UnsuccessReason : String;
  ResultContent : TMemoryStream;

  Constructor Create; virtual;
  Destructor Destroy; Override;
End;

TGridClientRPCMessagesBox = Class(TObjectList<TGridClientMessageRecept>)
  Procedure AddMessage(aFunctionName : String; aSucces : Boolean; aUnSuccessReason : String; aContent : TMemoryStream);
End;


TGridClient = Class
Private
  FCX : TGridClientConnection;
  FBusiness : TGRIDClientServerProtocolTransportBOManager;                               //Transport protocol (common). Can include one of this (and one only) :
  FBusinessMsg : TGRIDClientServerProtocolMessageServiceManager;                //--> Messaging protocol
  FBusinessRPC : TGRIDClientServerProtocolRPCBOManager;                           //--> RPC protocol
  FSessionID: String;
  FLastInfo : string;
  FinTrans :Boolean;

  Procedure InternalCheck;

  Procedure Connect_InternalSendAuth(UserName, Password : String);
  Function Connect_InternalReceiveAuth : Boolean;

Protected
  Procedure SendMsg_InternalSendMsg(AChannel : String; aMessages : array of TMemoryStream);
  Function RecvMsg_InternalRecvMsg(Var aMessages : TGridClientMessagesBox; Const aTimeOutInMS : Integer= 0) : TGridClientGetMessageInfoStruct;

  Procedure SendRPC_InternalSendRPC(AChannel : String; aMessage : TMemoryStream);
  Function RecvRPC_InternalRecvRPC(Var aRPCMessage : TGridClientRPCMessageRecept) : Boolean; Overload;
{ TODO : WARNING / Methods above Never used : To test. }
  Procedure RecvRPC_InternalRecvRPC(Var aRPCMessages : TGridClientRPCMessagesBox); Overload;

  Function InternalSubUnSub(AChannel : String; Const Subscribte : Boolean = True) : Boolean; //TRUE = SUBSCRIPTION.
Public
  Constructor Create; Virtual;
  Destructor Destroy; Override;

  Function Connect(Host :String; Port : Word; Const UserName : String = 'Admin'; Password : String = 'admin') : Boolean;
  Function Disconnect : Boolean;

  //Basic API
  Function LastError_Transport : String;
  Function LastInfo : string;

  Procedure SendMsg(aTargetChannel : String; aMessages : array of TMemoryStream);
  Function RecvMsg(Out aMessages : TGridClientMessagesBox; Const aTimeOutInMS : Integer= 0) : TGridClientGetMessageInfoStruct;

  Function Subscription(Channel : string) : Boolean;
  Function Unsubscription(Channel : string) : Boolean;


  //Secondary (higher Level) API, based on TGRIDRPCMessage. All are RPC  (Synchrone call)
  Function SendAndReceiveMsg(aTargetChannel : String; aMessage : TMemoryStream; Out aReply : TGridClientRPCMessageRecept) : Boolean;
  { TODO : Make above methods. But before, see SendMsg_InternalSendMsg as a model, but add mechanism for message limitation count by stream sent on the wire. }
  //Procedure SendAndReceiveMsg(aTargetChannel : String; aMessages : Array of TMemoryStream; Out aReply : TGridClientRPCMessagesBox); OVerload;

  Function ServerDefinition(var aResultServerDefinition : TGRIDClientServerDataModelCentralBOManager) : Boolean; //RPC Call

  //After CPULoadService(True); Message CPU will come when wou'll call RecvMsg();
  //--> See expl "GridClientConsole" for the know-how to decode.
  Function CPULoadService(Subscribte : Boolean) : Boolean;



  Property SessionID : String read FSessionID;

End;

implementation


{ TGridClient }


function TGridClient.Connect(Host: String; Port: Word; const UserName: String;
  Password : String): Boolean;
begin
  if Not(Assigned(FCX)) then
  begin
    FCX := TGridClientConnectionFactory.CreateConnection;
    TGridClientTransporterNetwork(FCX.Transporter).RemoteIP := Host;
    TGridClientTransporterNetwork(FCX.Transporter).RemotePort := Port;

    try
      FCX.Transporter.Connect;
      Connect_InternalSendAuth(UserName, Password);
      //API Synchrone : Waiting for result.
      Result := Connect_InternalReceiveAuth;
    Except
      //FCX Last error should be populate here.
      Result := False;
    end;
  end
  else
  begin
    raise Exception.Create(ClassName+' : Connection already active');
  end;
end;

constructor TGridClient.Create;
begin
  Inherited;
  FCX := Nil;
  FSessionID := EmptyStr;
  FBusiness := TGRIDClientServerProtocolTransportBOManager.Create;
  FBusinessMsg := TGRIDClientServerProtocolMessageServiceManager.Create;
  FBusinessRPC := TGRIDClientServerProtocolRPCBOManager.Create;
  FinTrans := False;
end;

destructor TGridClient.Destroy;
begin
  if Assigned(FCX) then
  begin
    if FCX.Active then
    begin
      FCX.Transporter.Close;
    end;
    FreeAndNil(FCX);
  end;
  FreeAndNil(FBusiness);
  FreeAndNil(FBusinessMsg);
  FreeAndNil(FBusinessRPC);
  inherited;
end;

Function TGridClient.Disconnect : Boolean;
begin
  Result := True;
  if FCX.Active then
  begin
    Result := FCX.Disconnect;
    if Result then
    begin
      FreeAndNil(FCX);
    end;
  end;
end;

procedure TGridClient.InternalCheck;
begin
  if Not(FCX.Active) then
  begin
    raise Exception.Create(ClassName+' : Not connected : Use Connect() first.');
  end;
end;

function TGridClient.InternalSubUnSub(AChannel: String;
  const Subscribte: Boolean): Boolean;
var res,r : TMemoryStream;
    a : TGRIDC2SProtocolEntry;
    b : TGRIDC2SMessaging;

    c : TGRIDMessageSystemChannelRelated;
begin
  InternalCheck;
  Result := True;  //-(

  res := TMemoryStream.Create;
  r := TMemoryStream.Create;
  try
  //Message protocol setup.
  FBusinessMsg.StoredObject.ClearAllData(False);

  if Subscribte then
    c := TGRIDMessageSubscription.Create(FBusinessMsg)
  else
    c := TGRIDMessageUnsubscription.Create(FBusinessMsg);

  c.TargetChannel := AChannel;
  FBusinessMsg.SaveToStream(r);
  r.Position :=0;

  //Transport Protocol setup.
  FBusiness.StoredObject.ClearAllData(False);
  a := TGCPUtils.BuildC2SProtocolObject(FBusiness);
  b := TGRIDC2SMessaging.Create(FBusiness);
  a.AddContent(b);
  b.Content := r; //We encapsulate the Message protocol in transport one.

  //Send the mess ! :)
  res.Clear;
  FBusiness.SaveToStream(res);
  res.Position := 0;
  FCX.Transporter.WriteStream(res);

  finally
    FBusiness.StoredObject.ClearAllData(False);
    FBusinessMsg.StoredObject.ClearAllData(False);
    FreeAndNil(r);
    FreeAndNil(Res);
  end;
end;

Function TGridClient.Connect_InternalReceiveAuth : Boolean;
var res : TMemoryStream;
    b : TGRIDS2CAuth;
begin
  res := TMemoryStream.Create;
  FCX.Transporter.ReadStream(res);
  FBusiness.StoredObject.ClearAllData(False);
  Res.Position := 0;
  FBusiness.LoadFromStream(res);


  if TGCPUtils.GetFirstS2CAuth(FBusiness,b) then
  begin
    if b.Approved then
    begin
      FSessionID := b.SessionID;
      Result := true;
    end
    else
    begin
      raise Exception.Create(b.DesapproveReaseon);
    end;
  end
  else
  begin
    raise Exception.Create(ClassName + '.InternalReceiveAuth : No S2CAuth object received from server');
  end;

end;

procedure TGridClient.Connect_InternalSendAuth(UserName, Password: String);
var res : TMemoryStream;
    a : TGRIDC2SProtocolEntry;
    b : TGRIDC2SAuth;
begin
  res := TMemoryStream.Create;
  FBusiness.StoredObject.ClearAllData(False);
  a := TGCPUtils.BuildC2SProtocolObject(FBusiness);
  b := TGRIDC2SAuth.Create(FBusiness);
  a.AddContent(b);
  b.UserName := UserName;
  b.Password := Password;

  res.Clear;
  FBusiness.SaveToStream(res);
  res.Position := 0;
  FCX.Transporter.WriteStream(res);
end;

Function TGridClient.CPULoadService(Subscribte: Boolean) : Boolean;
begin
  if Subscribte then
    Result := Subscription(CST_CHANNELNAME_MES_CPULOAD)
  else
    Result := Unsubscription(CST_CHANNELNAME_MES_CPULOAD)
end;

Procedure TGridClient.SendMsg_InternalSendMsg(AChannel: String;
  aMessages: array of TMemoryStream);
var res,r : TMemoryStream;
    a : TGRIDC2SProtocolEntry;
    b : TGRIDC2SMessaging;
    c : TGRIDMessage;
    i : integer;
begin
  Assert(Length(aMessages)>0);

  res := TMemoryStream.Create;
  r := TMemoryStream.Create;
  try
    //Message protocol setup.
    FBusinessMsg.StoredObject.ClearAllData(False);
    for i := Low(aMessages) to High(aMessages) do
    begin
      c := TGRIDMessage.Create(FBusinessMsg);
      c.Id := IntToStr(i); //Todo GUID.
      c.Channel := AChannel;
      aMessages[i].Position := 0;
      c.Buffer := aMessages[i];
    end;
    FBusinessMsg.SaveToStream(r);
    r.Position :=0;

    //Transport Protocol setup.
    FBusiness.StoredObject.ClearAllData(False);
    a := TGCPUtils.BuildC2SProtocolObject(FBusiness);
    b := TGRIDC2SMessaging.Create(FBusiness);
    a.AddContent(b);
    b.Content := r; //We encapsulate the Message protocol in transport one.

    //Send the mess ! :)
    res.Clear;
    FBusiness.SaveToStream(res);
    res.Position := 0;
    FCX.Transporter.WriteStream(res);
  finally
    FBusiness.StoredObject.ClearAllData(False);
    FBusinessMsg.StoredObject.ClearAllData(False);
    FreeAndNil(r);
    FreeAndNil(Res);
  end;
end;



Procedure TGridClient.SendRPC_InternalSendRPC(AChannel: String;
  aMessage: TMemoryStream);
var res,r : TMemoryStream;
    a : TGRIDC2SProtocolEntry;
    b : TGRIDC2SRPC;

    c : TGRIDMessageRPCAsk;
begin
  res := TMemoryStream.Create;
  r := TMemoryStream.Create;
  try
    //Message protocol setup.
    FBusinessRPC.StoredObject.ClearAllData(False);
    c := TGRIDMessageRPCAsk.Create(FBusinessRPC);
    aMessage.Position := 0;
    c.FonctionName := AChannel;
    c.Buffer := aMessage;
    FBusinessRPC.SaveToStream(r);
    r.Position :=0;

    //Transport Protocol setup.
    FBusiness.StoredObject.ClearAllData(False);
    a := TGCPUtils.BuildC2SProtocolObject(FBusiness);
    b := TGRIDC2SRPC.Create(FBusiness);
    a.AddContent(b);
    b.Content := r; //We encapsulate the Message protocol in transport one.

    //Send the mess ! :)
    res.Clear;
    FBusiness.SaveToStream(res);
    res.Position := 0;
    FCX.Transporter.WriteStream(res);

  finally
    FBusiness.StoredObject.ClearAllData(False);
    FBusinessMsg.StoredObject.ClearAllData(False);
    FreeAndNil(r);
    FreeAndNil(Res);
  end;
end;

function TGridClient.ServerDefinition(
  var aResultServerDefinition: TGRIDClientServerDataModelCentralBOManager): Boolean;
var lData : TGRIDClientServerProtocolCentralBOManager;
    ls, temp : TMemoryStream;
    lRes : TGridClientRPCMessageRecept;
begin
  Result := False;
  Assert((Assigned(aResultServerDefinition)));
  InternalCheck;
  lData := TGRIDClientServerProtocolCentralBOManager.Create;
  ls := TMemoryStream.Create;
  lRes := nil;
  Try //Finally one

    Try //Except one.

      TGRIDCentralServiceAskDefinition.Create(lData);
      lData.SaveToStream(ls);
      lData.StoredObject.ClearAllData(False);

      If SendAndReceiveMsg(CST_CHANNELNAME_RPC_GETSERVERDEFINITION,ls,lRes) then
      begin
        LData.LoadFromStream(lres.ResultContent);
        if lData.StoredObject.ObjectCount=1 then
        begin
          if lData.StoredObject.Objects[0] is TGRIDCentralServiceDefinitionResponse then
          begin
            if TGRIDCentralServiceDefinitionResponse(lData.StoredObject.Objects[0]).Result then
            begin
              temp := TMemoryStream(TGRIDCentralServiceDefinitionResponse(lData.StoredObject.Objects[0]).buffer); //Deep copy.
              aResultServerDefinition.StoredObject.ClearAllData(False);
              temp.Position := 0;
              aResultServerDefinition.LoadFromStream(temp);
              Result := aResultServerDefinition.StoredObject.ObjectCount>0;
            end
            else
            begin
              raise Exception.Create('Fail in Definition report (Business level error)');
            end;
          end
          else
          begin
            raise Exception.Create('Response protocol Error Message : No awaited business class. Class return : '+lData.StoredObject.Objects[0].ClassName);
          end;
        end
        else
        begin
          raise Exception.Create(' Response protocol level Error Message : Not (or not only one) RPC Object return - Object count : '+IntToStr(lData.StoredObject.ObjectCount));
        end;
      end
      else
      begin
        raise Exception.Create(' RPC Level Error : '+LastInfo);
      end;

    Except
      On E : Exception do
      begin
        raise Exception.Create(ClassName+'.ServerDefinition : '+E.Message);
      end;
    end;


  finally
    if Assigned(Temp) then
      FreeAndNil(Temp);
    FreeAndNil(ls);
    FreeAndNil(lData);
    FreeAndNil(lRes);
  end;


end;

function TGridClient.Subscription(Channel: string): Boolean;
begin
  InternalCheck;
  Result := InternalSubUnSub(Channel);
end;

function TGridClient.Unsubscription(Channel: string): Boolean;
begin
  InternalCheck;
  Result := InternalSubUnSub(Channel,False);
end;

function TGridClient.LastInfo: string;
begin
  Result := FLastInfo;
end;

function TGridClient.LastError_Transport: String;
begin
  Result := EmptyStr;
  if Assigned(FCX) then
    Result := FCX.Transporter.LastError;
end;

function TGridClient.RecvMsg(Out aMessages : TGridClientMessagesBox; Const aTimeOutInMS : Integer= 0): TGridClientGetMessageInfoStruct;
begin
  InternalCheck;
  if Not(Assigned(aMessages)) then
    aMessages := TGridClientMessagesBox.Create(True);
  Result := RecvMsg_InternalRecvMsg(aMessages,aTimeOutInMS);
end;

function TGridClient.RecvMsg_InternalRecvMsg(
  Var aMessages: TGridClientMessagesBox; Const aTimeOutInMS : Integer= 0): TGridClientGetMessageInfoStruct;
var Re : TMemoryStream;
    i,j : integer;

//Transport.
    //ask
    aa : TGRIDC2SProtocolEntry;
    bb : TGRIDC2SMessaging;
    //recept.
    a : TGRIDS2CProtocolEntry;
//Message layer.
    cc : TGRIDMessageGetMsg;
    c : TGRIDMessage;
    r : TGRIDMessageGetMsgReply;

    tempMemoryStream : TMemoryStream;
begin
  Assert(Assigned(aMessages));
  Result.ResultInfo := gmiOk;
  InternalCheck;
  re := TMemoryStream.Create;
  //Ask for message
  FBusiness.StoredObject.ClearAllData(False);
  aa := TGCPUtils.BuildC2SProtocolObject(FBusiness);
  bb := TGRIDC2SMessaging.Create(FBusiness);
  aa.AddContent(bb);

  FBusinessMsg.StoredObject.ClearAllData(False);
  cc := TGRIDMessageGetMsg.Create(FBusinessMsg);
  cc.WaitDelay := aTimeOutInMS; //If 0, no wait, if >0, server will wait "WaitDelay" until send something (Message or ack.)
  FBusinessMsg.SaveToStream(re);
  re.Position := 0;

  bb.Content := re;

  re.Clear;
  FBusiness.SaveToStream(re);
  re.Position := 0;
  FCX.Transporter.WriteStream(re);

  //Reception.
  re.Clear;
  try
    FCX.Transporter.ReadStream(re);
    re.Position := 0;

    If re.Size>0 then
    begin
      //Decoding.
      FBusiness.StoredObject.ClearAllData(False);
      FBusiness.LoadFromStream(re);
      if TGCPUtils.GetFirstS2CProtocolEntry(FBusiness,a) then
      for I := 0 to a.ContentCount-1 do
      begin
        if a.Content[i] is TGRIDS2CMessaging then
        begin
          FBusinessMsg.StoredObject.ClearAllData(False);
          tempMemoryStream := TMemoryStream(TGRIDS2CMessaging(a.Content[i]).Content); //c.Buffer give a Copy.
          try
            FBusinessMsg.LoadFromStream(TMemoryStream(tempMemoryStream));
          finally
            FreeAndNil(tempMemoryStream);
          end;
          for J := 0 to FBusinessMsg.StoredObject.ObjectCount-1 do
          begin
            if FBusinessMsg.StoredObject.Objects[j] is TGRIDMessage then
            begin
              c := TGRIDMessage(FBusinessMsg.StoredObject.Objects[j]);
              tempMemoryStream := TmemoryStream(c.Buffer); //c.Buffer give a Copy.
              try
                aMessages.AddMessage(c.Id,c.Channel,tempMemoryStream);
              finally
                FreeAndNil(tempMemoryStream);
              end;
            end
            else
            if FBusinessMsg.StoredObject.Objects[j] is TGRIDMessageGetMsgReply then
            begin
              r := TGRIDMessageGetMsgReply(FBusinessMsg.StoredObject.Objects[j]);
              case r.Code of
                gmrNoMessageOnServer: Result.ResultInfo := gmiNoMessageOnServer;
                gmrStillMessagePendingOnServer: Result.ResultInfo := gmiMessagesStillPendingOnServer;
                gmrFail :
                begin
                  Result.ResultInfo := gmiFail;
                  Result.TxtInfo := r.Information;
                end;
              end;
            end
            else
            begin
              raise Exception.Create('Protocol Error : Unknown reply object : Class : '+FBusinessMsg.StoredObject.Objects[j].ClassName);
            end;
          end;
        end
        else
        begin
          raise Exception.Create('Protocol Error : No object in reply.');
        end;
      end;
    end;
  finally
    FreeAndNil(re);
    FBusiness.StoredObject.ClearAllData(False);
    FBusinessMsg.StoredObject.ClearAllData(False);
  end;
end;


Procedure TGridClient.RecvRPC_InternalRecvRPC(
  var aRPCMessages: TGridClientRPCMessagesBox);
var Re : TMemoryStream;
    i,j :  Integer;
//Transport.
    //recept.
    a : TGRIDS2CProtocolEntry;
//RPC layer.
    cc : TGRIDMessageRPCResponse;
    c : TGRIDMessageRPCFail;
    tempMemoryStream : TMemoryStream;
begin
  Assert(Assigned(aRPCMessages));
  //Pure RPC Reception.
  re := TMemoryStream.Create;
  try

  try
    FCX.Transporter.ReadStream(re);
    re.Position := 0;

    If re.Size>0 then
    begin
      //Decoding.
      FBusiness.StoredObject.ClearAllData(False);
      FBusiness.LoadFromStream(re);
      if TGCPUtils.GetFirstS2CProtocolEntry(FBusiness,a) then
      begin
        for I := 0 to a.ContentCount-1 do
        begin
          if a.Content[i] is TGRIDS2CRPC then
          begin
            FBusinessRPC.StoredObject.ClearAllData(False);
            tempMemoryStream := TMemoryStream(TGRIDS2CRPC(a.Content[i]).Content);
            try
              FBusinessRPC.LoadFromStream(tempMemoryStream);
            finally
              FreeAndNil(tempMemoryStream);
            end;

            if FBusinessRPC.StoredObject.ObjectCount>0 then
            begin
              for j := 0 to FBusinessRPC.StoredObject.ObjectCount-1 do
              begin
                if FBusinessRPC.StoredObject.Objects[j] is TGRIDMessageRPCResponse then
                begin
                  cc := TGRIDMessageRPCResponse(FBusinessRPC.StoredObject.Objects[j]);
                  tempMemoryStream := TmemoryStream(cc.Buffer); //cc.Buffer give a Copy.
                  try
                    aRPCMessages.AddMessage('Call.'+IntToStr(j),True,EmptyStr,tempMemoryStream);
                  finally
                    FreeAndNil(tempMemoryStream);
                  end;
                end
                else
                if FBusinessRPC.StoredObject.Objects[j] is TGRIDMessageRPCFail then
                begin
                  c := TGRIDMessageRPCFail(FBusinessRPC.StoredObject.Objects[j]);
                  aRPCMessages.AddMessage('Call.'+IntToStr(j),False,c.FailReason,nil);
                end
                else
                begin
                  raise Exception.Create('Unssuported message received : '+FBusinessRPC.StoredObject.Objects[j].ClassName);
                end;
              end;
            end;
          end
          else
          begin
            raise Exception.Create('No RPC message : Incorrect response message count : at least 1 excpected, 0 received.');
          end;
        end;
      end
      else
      begin
        raise Exception.Create('RPC Protocol Error : No object in reply.');
      end;
    end
    else
    begin
      raise Exception.Create('Protocol Error : Empty reply.');
    end;

  Except
    On E : Exception do
    begin
      raise Exception.Create(ClassName+'.RecvRPC_InternalRecvRPC : ' + E.Message);
    end;
  end;

  finally
    FreeAndNil(re);
    FBusiness.StoredObject.ClearAllData(False);
    FBusinessRPC.StoredObject.ClearAllData(False);
  end;
end;

Function TGridClient.RecvRPC_InternalRecvRPC(Var aRPCMessage : TGridClientRPCMessageRecept) : Boolean;
var Re : TMemoryStream;
//Transport.
    //recept.
    a : TGRIDS2CProtocolEntry;
//RPC layer.
    cc : TGRIDMessageRPCResponse;
    c : TGRIDMessageRPCFail;
    tempMemoryStream : TMemoryStream;
begin
  Assert(Assigned(aRPCMessage));
  Result := True;
  //Pure RPC Reception.
  re := TMemoryStream.Create;
  try

  try
    FCX.Transporter.ReadStream(re);
    re.Position := 0;

    If re.Size>0 then
    begin
      //Decoding.
      FBusiness.StoredObject.ClearAllData(False);
      FBusiness.LoadFromStream(re);
      if TGCPUtils.GetFirstS2CProtocolEntry(FBusiness,a) then
      begin
        if a.ContentCount=1 then
        begin
          if a.Content[0] is TGRIDS2CRPC then
          begin
            FBusinessRPC.StoredObject.ClearAllData(False);
            tempMemoryStream := TMemoryStream(TGRIDS2CRPC(a.Content[0]).Content);
            try
              FBusinessRPC.LoadFromStream(tempMemoryStream);
            finally
              FreeAndNil(tempMemoryStream);
            end;

            if FBusinessRPC.StoredObject.ObjectCount=1 then
            begin
              if FBusinessRPC.StoredObject.Objects[0] is TGRIDMessageRPCResponse then
              begin
                cc := TGRIDMessageRPCResponse(FBusinessRPC.StoredObject.Objects[0]);
                tempMemoryStream := TmemoryStream(cc.Buffer); //cc.Buffer give a Copy.
                try
                  aRPCMessage.ResultContent.LoadFromStream(tempMemoryStream);
                finally
                  FreeAndNil(tempMemoryStream);
                end;
              end
              else
              if FBusinessRPC.StoredObject.Objects[0] is TGRIDMessageRPCFail then
              begin
                c := TGRIDMessageRPCFail(FBusinessRPC.StoredObject.Objects[0]);
                FLastInfo := c.FailReason;
                Result := false;
              end
              else
              begin
                raise Exception.Create('Error Message');
              end;
            end
            else
            begin
              raise Exception.Create('Single send RPC message : Incorrect response message count : 1 excpected, '+IntToStr(FBusinessRPC.StoredObject.ObjectCount)+' receive.');
            end;
          end
          else
          begin
            raise Exception.Create('Unssuported message received : '+a.Content[0].ClassName);
          end;
        end
        else
        begin
          raise Exception.Create('Protocol error : 1 RCP response expected, '+IntToStr(a.ContentCount)+' received.');
        end;
      end
      else
      begin
        raise Exception.Create('RPC Protocol Error : No object in reply.');
      end;
    end
    else
    begin
      raise Exception.Create('Protocol Error : Empty reply.');
    end;

  Except
    On E : Exception do
    begin
      raise Exception.Create(ClassName+'.RecvRPC_InternalRecvRPC : ' + E.Message);
    end;
  end;

  finally
    FreeAndNil(re);
    FBusiness.StoredObject.ClearAllData(False);
    FBusinessRPC.StoredObject.ClearAllData(False);
  end;
end;



Function TGridClient.SendAndReceiveMsg(aTargetChannel: String;
  aMessage: TMemoryStream;
  out aReply: TGridClientRPCMessageRecept) : Boolean;
begin
  InternalCheck;
  Assert(Assigned(aMessage));
  Assert(Not(Assigned(aReply)));
  aMessage.Position := 0;
  Try //Except one.
    try
      SendRPC_InternalSendRPC(aTargetChannel,aMessage);
      aReply := TGridClientRPCMessageRecept.Create;
      Result := RecvRPC_InternalRecvRPC(aReply);
      aReply.ResultContent.Position := 0;
    Except
      On e : Exception do
      begin
        raise Exception.Create('Transport level Error Message : '+E.Message);
      end;
    end;
  Except
    On E : Exception do
    begin
      raise Exception.Create(ClassName + '.SendAndReceiveMsg : Send Failure : '+E.Message);
    end;
  end;
end;

Procedure TGridClient.SendMsg(aTargetChannel: String; aMessages: array of TMemoryStream);
begin
  InternalCheck;
  SendMsg_InternalSendMsg(aTargetChannel, aMessages);
end;


{ TGridClientMessagesBox }

procedure TGridClientMessagesBox.AddMessage(aID, aChannel: String;
  aContent: TMemoryStream);
var lMem : TGridClientMessageRecept;
begin
  lMem := TGridClientMessageRecept.Create;
  lMem.MessageID := aID;
  lMem.MessageChannel := aChannel;
  aContent.Position := 0;
  lMem.MessageContent.LoadFromStream(aContent);
  Add(lMem);
end;

{ TGridClientMessageRecept }

constructor TGridClientMessageRecept.Create;
begin
  Inherited;
  MessageID := EmptyStr;
  MessageChannel := EmptyStr;
  MessageContent := TMemoryStream.Create;
end;

destructor TGridClientMessageRecept.Destroy;
begin
  FreeAndNil(MessageContent);
  inherited;
end;

{ TGridClientRPCMessageRecept }

constructor TGridClientRPCMessageRecept.Create;
begin
  FunctionName := EmptyStr;
  Succes := True;
  UnsuccessReason := EmptyStr;
  ResultContent := TMemoryStream.Create;
end;

destructor TGridClientRPCMessageRecept.Destroy;
begin
  FreeAndNil(ResultContent);
  inherited;
end;

{ TGridClientRPCMessagesBox }

procedure TGridClientRPCMessagesBox.AddMessage(aFunctionName: String;
  aSucces: Boolean; aUnSuccessReason: String; aContent: TMemoryStream);
var aM :TGridClientRPCMessageRecept;
begin
  aM := TGridClientRPCMessageRecept.Create;
  aM.FunctionName := aFunctionName;
  aM.Succes := aSucces;
  aM.UnsuccessReason := aUnSuccessReason;
  aM.ResultContent.Clear;
  aM.ResultContent.LoadFromStream(aContent);
end;

end.
