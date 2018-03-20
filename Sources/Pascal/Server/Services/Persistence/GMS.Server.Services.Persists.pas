unit GMS.Server.Services.Persists;

interface

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

//------------------------------------------------------------------------------
// Create  : 20170510
// Author  : VGS
// Purpose : - Database and various peristance service.
// History
// - 20170710 - Replace topTask by thread - TopTask hnad on WaitFor(); -> No reason found for instance.
//            - Log Reporting when disconnection.
//------------------------------------------------------------------------------


Uses
{$IFDEF FPC}
  Classes, SysUtils, Generics.Collections,
  SyncObjs,
{$ELSE}
  System.Classes, System.SysUtils, System.Generics.Collections,
  System.SyncObjs, System.Threading,
{$ENDIF}
  GS.CPUUsage,
  GS.Threads,
  GS.Stream,
//  ObjectUp.Task, //VGS : topTask hang on "WaitFor();" --> Something lock in ObjectUp.task.
  GS.Bus,
  GMS.Server.Types.Core,
  GMS.ClientServer.PersistsConst,
  GMS.Server.Services.Persists.Types,
  GMS.ClientServer.DB.Protocol.BO;

Type



//TGRIDPersistenceConnection = Class(topTask)
//User connection (Thread - handle one database connection)
TGRIDPersistenceConnection = Class(TThread)
Private
  FComunication : TBusClientReader;
  FChannel : TProtectedValue<String>;
  FToken : TProtectedValue<String>;
  FSessionID : TProtectedValue<String>;
  FDatabase : TGRIDServerPersistenceDataBaseInstance;
  FPacketAddData : TProtectedValue<String>;
  function GetComChannel: String;
  function GetToken: String;
  function GetSessionID: String;
  procedure SetSessionID(const Value: String);
  function GetPacketAditionalData: String;
  procedure SetPacketAditionalData(const Value: String);
Protected
  Procedure OnDataIncoming(Sender : TBus;Var Packet : TBusEnvelop); Virtual;
Public
  Constructor Create; Reintroduce; //Override;
  Destructor Destroy; Override;

  Procedure Execute; Override;

//  Procedure BeforeRun; Override;
//  Procedure RunLoop; Override;
//  Procedure AfterRun; Override;

  Property ComChannel : String read GetComChannel;
  Property Token : String read GetToken;
  Property ClientSessionID : String read GetSessionID Write SetSessionID;
  property PacketAdditionalData : String read GetPacketAditionalData Write SetPacketAditionalData;
  Property DataBase : TGRIDServerPersistenceDataBaseInstance read FDatabase Write FDataBase; //This is a shared object ! Pointer only, not owned by this object.
End;

TGRIDServerPersistenceService = Class(TopGRIDServerServiceInstance)
Private
  //FPool : TTaskManager;
  FPool : TThreadList;
  FUserAskReader : TBusClientReader;

  FDataBaseServers : TObjectList<TGRIDServerPersistenceDataBaseInstance>;

  Function InternalTaskReport : String;
Protected
  Function GetAvailableDataBaseInstance(aDatabaseName : string; out anInstance : TGRIDServerPersistenceDataBaseInstance) : Boolean;
public
  Procedure OnServerMessage_FromUser(Sender : TBus; var Packet : TBusEnvelop);

  Procedure OnServerMessage_FromCentral(Sender : TBus;Var Packet : TBusEnvelop); Override;


  Constructor Create; Override;
  Destructor Destroy; Override;

  Procedure BeforeRun; Override;
  Procedure RunLoop; Override;

  function GetInstanceType: TopGRIDServerServiceInstanceType; Override;
  function GetDescription : String; Override;
  procedure LoadConfiguration; Override;

End;

implementation

    Procedure BuildAccessResponse( aMessage : TBusMessage;
                                   aToken,
                                   aChannelWhereSendDBOperation : String;
                                   aGranted : Boolean;
                                   anErrorReason : string;
                                   OriginalPacket : TBusEnvelop);
    var l : TGRIDClientServerDBProtocolBOManager;
        l1 : TGRIDdboResponseForAccess;
    begin
       l := TGRIDClientServerDBProtocolBOManager.Create;
       try
         l1 := TGRIDdboResponseForAccess.Create(l);
         l1.TokenID := aToken;
         l1.AskChannel := aChannelWhereSendDBOperation;
         l1.AccessGranted := aGranted;
         l1.AccessDeniedErrorDesc := anErrorReason;
         aMessage.FromStream(l.ToStream);
         Bus.Send(aMessage,OriginalPacket.ResponseChannel);
       finally
         FreeAndNil(l);
       end;
    end;

    Procedure BuildDataObjectResponse( aMessage : TBusMessage;
                                   aToken : String;
                                   aSuccess : Boolean;
                                   anErrorReason : string;
                                   aDBResult : TMemoryStream;
                                   OriginalPacket : TBusEnvelop);
    var l : TGRIDClientServerDBProtocolBOManager;
        l1 : TGRIDdboResponseForObjectData;
    begin
       l := TGRIDClientServerDBProtocolBOManager.Create;
       try
         l1 := TGRIDdboResponseForObjectData.Create(l);
         l1.TokenID := aToken;
         l1.OperationSuccess := aSuccess;
         l1.OperationFailErrorDesc := anErrorReason;
         if Assigned(ADBResult) then
           l1.OperationResultContent := aDBResult;
         aMessage.FromStream(l.ToStream);
         Bus.Send(aMessage,OriginalPacket.ResponseChannel);
       finally
         FreeAndNil(l);
       end;
    end;

    Procedure BuildGenericErrorResponse( aMessage : TBusMessage;
                                         aToken : String;
                                         anErrorCode : Integer;
                                         anErrorReason : string;
                                         OriginalPacket : TBusEnvelop);
    var l : TGRIDClientServerDBProtocolBOManager;
        l1 : TGRIDdboResponseGenericError;
    begin
       l := TGRIDClientServerDBProtocolBOManager.Create;
       try
         l1 := TGRIDdboResponseGenericError.Create(l);
         l1.TokenID := aToken;
         l1.ErrorCode := anErrorCode;
         l1.ErrorString := anErrorReason;
         aMessage.FromStream(l.ToStream);
         Bus.Send(aMessage,OriginalPacket.ResponseChannel);
       finally
         FreeAndNil(l);
       end;
    end;

{ TGRIDPersistenceConnection }

{
procedure TGRIDPersistenceConnection.AfterRun;
begin
  Bus.UnSubscribe(FComunication,FChannel.Value);
  Bus.ChannelDelete(FChannel.Value);
  inherited;
end;

procedure TGRIDPersistenceConnection.BeforeRun;
begin
  inherited;
end;
}
constructor TGRIDPersistenceConnection.Create;
begin
  Inherited Create(True);
  FSessionID := TProtectedValue<String>.Create(EmptyStr);
  FToken := TProtectedValue<String>.Create(TGUID.NewGuid.ToString);
  FChannel := TProtectedValue<String>.Create('DBCTX-Chan-'+FToken.Value);
  FPacketAddData := TProtectedValue<String>.create(EmptyStr);
  FComunication := Bus.Subscribe(ComChannel,OnDataIncoming);
  FComunication.Event := bus.GetNewEvent;
end;

destructor TGRIDPersistenceConnection.Destroy;
begin
  FreeAndNil(FSessionID);
  FreeAndNil(FToken);
  FreeAndNil(FChannel);
  FreeAndNil(FComunication);
  FreeAndNil(FPacketAddData);
  Inherited;
end;

procedure TGRIDPersistenceConnection.Execute;
begin
//  inherited;
  while not(Terminated) do
  begin
    case FComunication.Event.WaitFor(CST_SERVICETHREADTIMEOUTINMS) of
      wrSignaled :
      begin
        Bus.ProcessMessages([FComunication]); //Will trig OnDataIncomming in the thread's context if message available.
      end;
    end;
  end;
  Bus.UnSubscribe(FComunication,FChannel.Value);
  Bus.ChannelDelete(FChannel.Value);
end;

function TGRIDPersistenceConnection.GetComChannel: String;
begin
  result := FChannel.Value;
end;


function TGRIDPersistenceConnection.GetPacketAditionalData: String;
begin
  Result := FPacketAddData.Value;
end;

function TGRIDPersistenceConnection.GetSessionID: String;
begin
  Result := FSessionID.Value;
end;

function TGRIDPersistenceConnection.GetToken: String;
begin
  result := FToken.Value;
end;

procedure TGRIDPersistenceConnection.OnDataIncoming(Sender: TBus;
  var Packet: TBusEnvelop);
var lProtocol : TGRIDClientServerDBProtocolBOManager;
    lStream : TMemoryStream;
    i : integer;
    em : TBusMessage;

    Procedure ProcessRequests;
    var lr : TGRIDdboRequestObjectData;
        lrSave : TGRIDdboRequestSaveObjectData;
        lrRetr : TGRIDdboRequestRetrieveObjectData;
        lRep : TGRIDdboResponseForObjectData;
        lsIn, lsOut : TMemoryStream;
        lPotentialError : string;
        lok : Boolean;
    begin
      if lProtocol.StoredObject.Objects[i] is TGRIDdboRequestObjectData then
      begin
        lr := TGRIDdboRequestObjectData(lProtocol.StoredObject.Objects[i]);
        ServerLog(Self,'Database operation begin : Token : '+lr.TokenID);
        lsIn := TMemoryStream(lr.OperationContent); //Copy.
        lsIn.Position := 0;
        lok := false;
        lsOut := nil;
        try
          FDatabase.Lock;
          try
            if lProtocol.StoredObject.Objects[i] is TGRIDdboRequestRetrieveObjectData then
              lok := FDatabase.DataBaseProcessLoad(lsIn,lsOut,lPotentialError)
            else
            if lProtocol.StoredObject.Objects[i] is TGRIDdboRequestSaveObjectData then
            begin
              lok := FDatabase.DataBaseProcessSave(lsIn,lsOut,lPotentialError);
              if lok then
              begin
                { TODO : Send lsIn to all *others* clients, if subscribted to Mirroring Object function. }
              end;
            end
            else
            begin
              raise Exception.Create('Protocol Error : Unknown class "'+ lProtocol.StoredObject.Objects[i].ClassName+'"');
            end;
          finally
            FDatabase.Unlock;
          end;

          if lok then
          begin
            BuildDataObjectResponse(em,lr.TokenID,True,EmptyStr,lsOut,Packet);
          end
          else
          begin
            BuildDataObjectResponse(em,lr.TokenID,False,lPotentialError,lsOut,Packet);
          end;
        Finally
          FreeAndNil(lsIn);
          if Assigned(lsOut) then
            FreeAndNil(lsOut);
          ServerLog(Self,'Database operation finished : Token : '+lr.TokenID);
        end;
      end
      else
      begin
        raise Exception.Create('Protocol Error : Only "'+ lProtocol.StoredObject.Objects[i].ClassName+'" request allowed on this step');
      end;
    end;
begin
  //Here our thread receive data from outside :)
  em.Buffer := Nil;
  lStream := Packet.ContentMessage.AsStream; //Copy.
  lProtocol := TGRIDClientServerDBProtocolBOManager.Create;
  try
    try
      lProtocol.LoadFromStream(lStream);

      for I := 0 to lProtocol.StoredObject.ObjectCount-1 do
      begin
        if lProtocol.StoredObject.Objects[i] is TGRIDdboCustomRequest then
        begin
          ProcessRequests;
        end
        else
        begin
          raise Exception.Create('Protocol error : No request allowed here');
        end;
      end;

    Except
      On E : Exception do
      begin
        //Last change to send error server side.
        ServerLog(self,ClassName+'.OnDataIncoming  : Exception : '+E.Message);
        BuildGenericErrorResponse(em,EmptyStr,-1,E.Message,Packet);
      end;
    end;
  finally
    FreeAndNil(lStream);
    FreeAndNil(lProtocol);
  end;
end;

{
procedure TGRIDPersistenceConnection.RunLoop;
begin
  case FComunication.Event.WaitFor(CST_SERVICETHREADTIMEOUTINMS) of
    wrSignaled :
    begin
      Bus.ProcessMessages([FComunication]); //Will trig OnDataIncomming in the thread's context if message available.
    end;
  end;
end;
}
procedure TGRIDPersistenceConnection.SetPacketAditionalData(
  const Value: String);
begin
  FPacketAddData.Value := Value;
end;

procedure TGRIDPersistenceConnection.SetSessionID(const Value: String);
begin
  FSessionID.Value := Value;
end;

{ TGRIDServerExecuteAgent }

procedure TGRIDServerPersistenceService.BeforeRun;
begin
  LoadConfiguration;
  inherited;
end;

constructor TGRIDServerPersistenceService.Create;
begin
  inherited Create;
//  FPool := TTaskManager.Create;
  FPool := TThreadList.Create;
  FUserAskReader := Bus.Subscribe(CST_SERVER_Persistance_SERVICE_CONTROLER,OnServerMessage_FromUser);
  FUserAskReader.Event := FReaderObject.Event; //Given by father. And shared.
  FDataBaseServers := TObjectList<TGRIDServerPersistenceDataBaseInstance>.Create;
end;

destructor TGRIDServerPersistenceService.Destroy;
begin
  Bus.UnSubscribe(FUserAskReader, CST_SERVER_Persistance_SERVICE_CONTROLER);
  FreeAndNil(FUserAskReader);
  FreeAndNil(FDataBaseServers);
  FreeAndNil(FPool);
  inherited;
end;

function TGRIDServerPersistenceService.GetAvailableDataBaseInstance(
  aDatabaseName: string;
  out anInstance: TGRIDServerPersistenceDataBaseInstance): Boolean;
var i : integer;
begin
  Assert(Not(Assigned(anInstance)));
  Result := False;
  for I := 0 to FDataBaseServers.Count-1 do
  begin
    if FDataBaseServers[i].DatabaseName = aDatabaseName then
    begin
      anInstance := FDataBaseServers[i];
      Result := True;
      Break;
    end;
  end;
end;

function TGRIDServerPersistenceService.GetDescription: String;
var i : integer;
    l : TStringList;
    ls : String;
begin
  //GetDesc is called by heritated object : In our case, we will call this manually later.
  Result := EmptyStr;
  if Not(Assigned(FDataBaseServers)) then
    Exit;

  l := TStringList.Create;
  try
    l.Add(EmptyStr);
    l.Add('----------------------------------------------');
    l.Add('GRID Persitence Service');
    l.Add('----------------------------------------------');
    if TGRIDServerPersistenceFactory.Drivers.Count>0 then
      l.Add('-->  '+IntToStr(TGRIDServerPersistenceFactory.Drivers.Count)+' driver(s) available.');
    l.Add('-----------');
    for I := 0 to TGRIDServerPersistenceFactory.Drivers.Count-1 do
    begin
      ls  := '  ('+ IntTostr(i+1) + ') ' +
               TGRIDServerPersistenceFactory.Drivers[i].DriverName + ' V.' +
               TGRIDServerPersistenceFactory.Drivers[i].Version;
      l.Add(ls);
    end;
    l.Add(EmptyStr);
    if FDataBaseServers.Count>0 then
      l.Add('-->  '+IntToStr(FDataBaseServers.Count)+' database(s) online available.');
    l.Add('------------');
    for I := 0 to FDataBaseServers.Count-1 do
    begin
      ls  := '  ('+ IntTostr(i+1) + ') ' +
               FDataBaseServers[i].DatabaseName + ' - (' +
               FDataBaseServers[i].Driver.DriverName+')';
      l.Add(ls);
    end;
    l.Add('----------------------------------------------');
    Result := l.Text;
  finally
    FreeAndNil(l);
  end;
end;

function TGRIDServerPersistenceService.GetInstanceType: TopGRIDServerServiceInstanceType;
begin
  Result := gtProcessManagementService;
end;


function TGRIDServerPersistenceService.InternalTaskReport: String;
var FT : TList;
    lft : TGRIDPersistenceConnection;
    I : Integer;
    lt : TStringList;
begin
  lt := TStringList.Create;
  FT := FPool.LockList;
  try
    lt.Add('Channel;Token;Session;DBName;MultiInstanceAllowed;DriverName');
    for I := 0 to FT.Count-1 do
    begin
      if TObject(FT[i]) is TGRIDPersistenceConnection then
      begin
        lft := TGRIDPersistenceConnection(FT[i]);
        lt.Add(lft.ComChannel+';'+
        lft.Token+';'+
        lft.ClientSessionID+';'+
        lft.DataBase.DatabaseName+';'+
        BoolToStr(lft.DataBase.AllowMultiInstance)+';'+
        lft.DataBase.Driver.DriverName)
      end
      else
      begin
        ServerLog(Self,'ALERT : Unknown class : '+TObject(FT[i]).ClassName);
      end;
    end;
  finally
    Result := lt.Text;
    lt.Free;
    FPool.UnlockList;
  end;
end;

procedure TGRIDServerPersistenceService.LoadConfiguration;
var lFile : TStringList;
    lFileName : String;
    i : integer;
    ls, ls1, ls2 : string;
    lDB : TGRIDServerPersistenceDataBaseInstance;
    lDR : TGRIDServerPersistenceDataBaseDriver;
begin
  //Load configuration is called by heritated object : In our case, we will call this manually later, when
  if Not(Assigned(FDataBaseServers)) then
    Exit;

  ServerLog(Self,'GRID Persistence Service : Loading configuration file...');
  lFileName := ExtractFileDir(ParamStr(0))+'\GRIDPersistsService.configuration';
  if FileExists(lFileName) then
  begin
    lFile := TStringList.Create;
    try
      lFile.LoadFromFile(lFileName);
      for I := 0 to lFile.Count-1 do
      begin
        ls := lFile.Values['DBNAME_'+IntToStr(i)];
        ls1 := lFile.Values['DRIVERNAME_'+IntToStr(i)];
        ls2 := lFile.Values['DRIVERVERSION_'+IntToStr(i)];
        if (length(ls)>0) And (Length(ls1)>0) And (Length(ls2)>0) then
        begin
          if TGRIDServerPersistenceFactory.GetDriver(ls1,lDR,ls2) then
          begin
            ServerLog(Self,'  GRID Persistence Service : Creating new database : "'+ls+'" with "'+ls1+' v'+ls2+' " drivers..."');
            try
              lDB := LDR.GetNewInstance(ls);
              FDataBaseServers.Add(lDB);
            Except
              On e : Exception do
              begin
                ServerLog(Self,'    WARNING : GRID Persistence Service : Creating new database failure : "'+ls+'" with "'+ls1+'" drivers..." : '+e.Message);
              end;
            end;
          end;
        end
        else
        begin
          Break;
        end;
      end;
    finally
      FreeAndNil(lFile);
    end;
    ServerLog(Self,'GRID Persistence service HTTP Server configuration loaded.');
    ServerLog(Self,'--------------------------------------------------');
    for I := 0 to FDataBaseServers.Count-1 do
    begin
      ServerLog(Self,'  GRID Persistence Service : Database available : "'+FDataBaseServers[i].DatabaseName+
                     '" with "'+FDataBaseServers[i].Driver.DriverName+'" drivers"');
    end;
    ServerLog(Self,'--------------------------------------------------');
    ServerLog(Self,EmptyStr);
  end;
end;


procedure TGRIDServerPersistenceService.OnServerMessage_FromCentral(
  Sender: TBus; var Packet: TBusEnvelop);
var ls : TMemoryStream;
    lb : TofServerComChannelInternalOrder;
    ljs : String;
    i : integer;

    FT : TList;
    lft : TGRIDPersistenceConnection;

begin
  ls := Packet.ContentMessage.AsStream;
  try
    lb := TofServerComChannelInternalOrder(ReadByte(ls));
    ljs := ReadString(ls);
    if lb = cioTCPConnectionManagement_UserDisconnect then
    begin
      //Look if db is used by this client which just disconnect.

      FT := FPool.LockList;
      try
        for I := FT.Count-1 downto 0 do
        begin
          if TObject(FT[i]) is TGRIDPersistenceConnection then
          begin
            if TGRIDPersistenceConnection(FT[i]).PacketAdditionalData = ljs then
            begin
              Serverlog(Self, 'Persistence service Task terminate : '+TObject(FT[i]).ClassName + ' --- '+IntToStr(TThread(FT[i]).ThreadID));
              TThread(FT[i]).Terminate;
              TThread(FT[i]).WaitFor;
              TThread(FT[i]).Free;
              FT.Delete(i);
            end;
          end;
        end;
      finally
        FPool.UnlockList;
      end;

      ServerLog(Self,InternalTaskReport);
    end;
  finally
    FreeAndNil(ls);
  end;
end;

procedure TGRIDServerPersistenceService.OnServerMessage_FromUser(Sender: TBus;
  var Packet: TBusEnvelop);
var lProtocol : TGRIDClientServerDBProtocolBOManager;
    lStream : TMemoryStream;
    i : integer;
    em : TBusMessage;

    Procedure ProcessRequests;
    var aR : TGRIDdboRequestObjectData;
        aA : TGRIDdboRequestForAccess;
        m : TBusMessage;
        lCTX : TGRIDPersistenceConnection;
        lDBInstance : TGRIDServerPersistenceDataBaseInstance;
    begin
      if lProtocol.StoredObject.Objects[i] is TGRIDdboRequestObjectData then
      begin
       aR := TGRIDdboRequestObjectData(lProtocol.StoredObject.Objects[i]);
       ServerLog(Self, 'ObjectData for '+aR.TokenID);
       raise Exception.Create('Protocol Error : No ObjectData request allowed here.');
      end
      else
      if lProtocol.StoredObject.Objects[i] is TGRIDdboRequestForAccess then
      begin
       aA := TGRIDdboRequestForAccess(lProtocol.StoredObject.Objects[i]);
       ServerLog(Self,'Request Access for '+aA.DatabaseName);

         { TODO :
Here : Check if user have the right (GRID security) to access to this service.
And to this DB (double check) }
       //Check if the service is available.
//       ServiceAuthorization(Self,FAuthorisation, aA.SpecificUserName, aA.SpecificPassword);
       //Check if user have the right to access database.
//       ServiceAuthorization(Self,FAuthorisation, aA.SpecificUserName, aA.SpecificPassword);
       //aA.DatabaseName;
       //aA.SpecificUserName;
       //aA.SpecificPassword;
       //aA.ClientSessionID;

       //IF ALL OK, And if asked database is available...
       if GetAvailableDataBaseInstance(aA.DataBaseName, lDBInstance) then
       begin
         //Database available

         //In this case New internal client cts (ie thread), but always sharing an existing DB connection.
         //lCTX := TGRIDPersistenceConnection(FPool.CreateTask(TGRIDPersistenceConnection,False,False));
         FPool.LockList;
         try
           lCTX := TGRIDPersistenceConnection.Create;
           FPool.Add(LCTX);
           lCTX.ClientSessionID := aA.ClientSessionID;
           lCTX.DataBase := lDBInstance;
           lCTX.PacketAdditionalData := Packet.AdditionalData; //SAve connection data as json string.
           BuildAccessResponse(m,lCTX.Token,lCTX.ComChannel, True, EmptyStr,Packet);
         finally
           FPool.UnlockList;
         end;
         lCTX.Start;
       end
       else
       begin
         //Here, a whole new Database connection, with a new grid connection (ie thread) ???? Or, just an error ? Could a client started a DB ?
         BuildGenericErrorResponse(m,EmptyStr,-1,'No database "'+aA.DatabaseName+'" available',Packet);
       end;
      end;
    end;

begin
  lStream := Packet.ContentMessage.AsStream;
  lProtocol := TGRIDClientServerDBProtocolBOManager.Create;
  try
    try
      lProtocol.LoadFromStream(lStream);

      for I := 0 to lProtocol.StoredObject.ObjectCount-1 do
      begin
        if lProtocol.StoredObject.Objects[i] is TGRIDdboCustomRequest then
        begin
          ProcessRequests;
        end
        else
        begin
          raise Exception.Create('Protocol Error : No response in a incomming query allowed.');
        end;
      end;

    Except
      On E : Exception do
      begin
        //Last change to send error server side.
        ServerLog(self,ClassName+'.OnServerMessage_FromUser  : Exception : '+E.Message);
        BuildGenericErrorResponse(em,EmptyStr,-1,E.Message,Packet);
      end;
    end;
  finally
    FreeAndNil(lStream);
    FreeAndNil(lProtocol);
  end;

end;

procedure TGRIDServerPersistenceService.RunLoop;
begin
  case FReaderObject.Event.WaitFor(CST_SERVICETHREADTIMEOUTINMS) of
    wrSignaled :
    begin
      Bus.ProcessMessages([FUserAskReader, FReaderObject]);
    end;
  end;
end;



end.
