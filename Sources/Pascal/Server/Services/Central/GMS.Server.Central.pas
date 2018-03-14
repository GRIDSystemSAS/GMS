unit GMS.Server.Central;

interface

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}


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
  GS.Task,
  GS.Bus,
  GS.LocalMemCached,
  GMS.Server.Central.DataModel,
  GMS.ClientServer.Types.ChannelDefinitions,
  GMS.Server.Types.Core,
  GMS.ClientServer.Protocol.Central.BO, //For Definition service. (OnServerMessage_AskForSystemDefinition)
  GMS.ClientServer.DataModel.Central.BO;


Const
  CST_SESSION_GUID_PREFIX = 'SessionId_';
Type

TopGRIDServerLogic = Class(TopGRIDServerServiceInstance)
Private
  FGenerateCPUMessageIntervalInMS: UInt32;
  FGenerateCPUMessageStatus : Int64;
  FLastCPULevel : Double;
  FCPURangeLimitForPersitantMessage : Double;
  FSharedDataClient : TLocalMemcachedClient;

  //This reader will respond to client who ask "Give your name and definition".
  FReaderSystemDef : TBusClientReader;

  FCPU : TopCPUUsage;
  Procedure CPUUpdate(aObj : TSOGRIDSystemProfile);
Public
  DataModel : TGRIDCentralDataModel;

  //Messaging bus
  LocalAuthentificator : TBusClientReader;
  LocalServiceNest : TBusClientReader;
  LocalServiceAPI : TBusClientReader;
  Procedure OnServerMessage_FromCentral(Sender : TBus;Var Packet : TBusEnvelop); Override;
  Procedure OnServerMessage_AuthAsk(Sender : TBus; Var Packet : TBusEnvelop);
  procedure OnServerMessage_ReportServiceFromGridServerInstance(Sender : TBus; Var Packet : TBusEnvelop);

  Procedure OnServerMessage_AskForSystemDefinition(Sender : TBus; Var Packet : TBusEnvelop);
  Procedure OnServerMessage_AskForAPI(Sender : TBus; Var Packet : TBusEnvelop);


  Constructor Create; Override;
  Destructor Destroy; Override;

  procedure LoadConfiguration; Override;

  Procedure RunLoop; Override;
  Procedure CPUMessageGenerationProcess;

  //Heritated stuff.
  function GetInstanceType: TopGRIDServerServiceInstanceType; Override;
  function GetDescription : String; Override;
  //End of.

  Function GetOSAndArchAsString : String;
  Function GetOSMajorMinorBuild : String;
  Function GetOSArchitecture : String;
  Function GetOSName : String;
  Function GetOSGenuineName : String;

  Function GetBusStateAsCSVString : string;

  Property GenerateCPUMessageIntervalInMilliSec : UInt32 read FGenerateCPUMessageIntervalInMS;
End;


implementation


{ TopGRIDServerLogic }

procedure TopGRIDServerLogic.CPUMessageGenerationProcess;
var lManager : TGRIDClientServerDataModelCentralBOManager;
    lsp : TSOGRIDSystemProfile;
    lStream : TMemoryStream;

begin
  FGenerateCPUMessageStatus := FGenerateCPUMessageStatus + TimeFromLastLoopInMilliSec;
  if FGenerateCPUMessageStatus>FGenerateCPUMessageIntervalInMS then
  begin
    FGenerateCPUMessageStatus := 0;
    lManager := TGRIDClientServerDataModelCentralBOManager.Create;
    lStream := TMemoryStream.Create;
    try
      lsp :=TSOGRIDSystemProfile.Create(lManager);
      CPUUpdate(lsp);
      lManager.SaveToStream(lStream);

      if Abs(lsp.SystemCPULoad - FLastCPULevel) > FCPURangeLimitForPersitantMessage then
      begin
        //If there are more than 5% range between 2 consecutive call, we send a persistant message, for keep history.
        ServerMessageSend(Self,CST_CHANNELNAME_MES_CPULOAD,lStream,EmptyStr,EmptyStr,True);
        FLastCPULevel := lsp.SystemCPULoad;

        //Parallely, we post in shared data current cpu level, in order to be rechable esay for everybody without Bus messageing hassle.
        FSharedDataClient.SetValue(CST_SHAREDDATA_CPULEVEL,FloatToStr(FLastCPULevel));
      end
      else
      begin
        //Else, generate normal message for hypothetical overwatch client.
        ServerMessageSend(Self,CST_CHANNELNAME_MES_CPULOAD,lStream);
      end;

    finally
      FreeAndNil(lStream);
      FreeAndNil(lManager);
    end;
  end;
end;

procedure TopGRIDServerLogic.CPUUpdate(aObj: TSOGRIDSystemProfile);
begin
  Assert(Assigned(aObj));
  FCPU.Update;
  aObj.SystemCPULoad := FCPU.UsageCPUPercent;
  aObj.SystemCPULoadKernel := FCPU.KernelUsageCPUPercent;
  aObj.SystemCPUPIdleTime := FCPU.PureIdleTimePercent;
  aObj.SystemCPUUserTime := FCPU.PureUserTimePercent;
  aObj.SystemCPUKernelTime := FCPU.PureKernelTimePercent;
  aObj.SystemCPUNiceTime := FCPU.PureNiceTimePercent;
  aObj.SystemCaptureDateTime := Now;
end;

constructor TopGRIDServerLogic.Create;
begin
  Inherited Create;
  FSharedDataClient := TLocalMemcachedClient.Create(TGRIDData.MemCached);

  FReaderSystemDef := Bus.Subscribe(CST_CHANNELNAME_RPC_GETSERVERDEFINITION, OnServerMessage_AskForSystemDefinition);
  LocalAuthentificator := Bus.Subscribe(CST_SERVER_AUTH_CHANNEL,OnServerMessage_AuthAsk);
  LocalServiceNest := Bus.Subscribe(CST_SERVER_GLOBAL_SERVICEDECLARATION, OnServerMessage_ReportServiceFromGridServerInstance);
  LocalServiceAPI := Bus.Subscribe(CST_SERVER_GLOBAL_APIDECLARATION, OnServerMessage_AskForAPI);

  LocalAuthentificator.Event := FReaderObject.Event; //Note : Shared the same Event. FReaderObject is given by heritated.
  LocalServiceNest.Event := FReaderObject.Event; //Note : Shared the same Event.
  FReaderSystemDef.Event := FReaderObject.Event; //Note : Shared the same Event.
  LocalServiceAPI.Event := FReaderObject.Event;

  FCPU := TopCPUUsage.Create;
  FGenerateCPUMessageIntervalInMS  := CST_CPUMESSAGEGENERATIONINTERVALINMS;
  FGenerateCPUMessageStatus := 0;
  FLastCPULevel := 0.0;
  FCPURangeLimitForPersitantMessage := 3.0;
end;

destructor TopGRIDServerLogic.Destroy;
begin
  FreeAndNil(DataModel);
  FreeAndNil(FReaderObject);
  FreeAndNil(LocalAuthentificator);
  FreeAndNil(LocalServiceAPI);
  FreeAndNil(FSharedDataClient);
  FreeAndNil(FReaderSystemDef);
  FreeAndNil(FCPU);
  inherited;
end;

function TopGRIDServerLogic.GetBusStateAsCSVString: string;
var s,ss : TStringList;
begin
  s := TStringList.Create;
  ss := TStringList.Create;
  try
    Bus.GetChannelsConfigurationAsCSV(s);
    Bus.GetSubscribtersConfigurationAsCSV(ss);
    s.Add('');
    s.Add(ss.Text);
    Result := s.Text;
  finally
    FreeAndNil(s);
    FreeAndNil(ss);
  end;
end;

function TopGRIDServerLogic.GetDescription: String;
begin
  Result := 'Central Service (Core) GRID Server.';
end;

function TopGRIDServerLogic.GetInstanceType: TopGRIDServerServiceInstanceType;
begin
  Result := TopGRIDServerServiceInstanceType.gtCentralManagementService;
end;

function TopGRIDServerLogic.GetOSAndArchAsString: String;
begin
  Result :=  GetOSGenuineName + ' ('+GetOSMajorMinorBuild+') - '+ GetOSName +' - '+ GetOSArchitecture;
end;

function TopGRIDServerLogic.GetOSArchitecture: String;
begin
  {$IFDEF FPC}
    {$IFDEF CPUARM}
    Result := CST_Archi[2];
    {$ELSE}
    Result := CST_Archi[0];
    {$ENDIF}
  {$ELSE}  Result := CST_Archi[integer(TOSVersion.Architecture)]
{$ENDIF}
end;

function TopGRIDServerLogic.GetOSGenuineName: String;
begin
  {$IFDEF FPC}
    {$IFDEF LINUX}
    Result := CST_Platform[5];
    {$ELSE}
    Result := CST_Platform[0];
    {$ENDIF}
  {$ELSE}
  Result := TOSVersion.Name;
  {$ENDIF}
end;

function TopGRIDServerLogic.GetOSMajorMinorBuild: String;
begin
{$IFDEF FPC}
  Result := '0.0';
{$ELSE}
  Result := IntToStr(TOSVersion.Major)+'.'+IntToStr(TOSVersion.Minor)+'.'+IntToStr(TOSVersion.Build);
{$ENDIF}
end;

function TopGRIDServerLogic.GetOSName: String;
begin
  {$IFDEF FPC}
    {$IFDEF LINUX}
    Result := CST_Platform[5];
    {$ELSE}
    Result := CST_Platform[0];
    {$ENDIF}
  {$ELSE}
  Result := CST_Platform[Integer(TOSVersion.Platform)]
{$ENDIF}
end;



procedure TopGRIDServerLogic.LoadConfiguration;
begin
  DataModel := TGRIDCentralDataModel.Create;
  DataModel.SystemManager.GRIDServerName := 'Unnamed GMS Server on '+GetOSName; //TODO Api RPC for rename (With persistence)
  DataModel.SystemManager.GRIDServerDesc := 'GMS GRIDServer';
  DataModel.SystemManager.GRIDServerVersionMajor := '1';
  DataModel.SystemManager.GRIDServerVersionMinor := '0';
  DataModel.SystemManager.GRIDServerVersionBuild := '0';
  DataModel.SystemManager.GRIDServerHost := GetOSAndArchAsString;
  DataModel.SystemManager.GRIDServerStartUp := Now;
  DataModel.SystemManager.GRIDSystemProfile := TSOGRIDSystemProfile.Create(DataModel.SystemManagerDataModel);
end;

procedure TopGRIDServerLogic.OnServerMessage_AskForAPI(Sender: TBus;
  var Packet: TBusEnvelop);
var ls : TMemoryStream;
    lAPIName : String;
    lpCount : Byte;
    lParam : Array of String;
    i : integer;
begin
  //respond to various API Call from ExecuteAgent or else where.
///Protocol is simple, because it must be fast, and it is a inner use only.
///  RECV Header :
///  1 : String : APIName
///  2 : byte : Params count.
///  3 : 1..n : String : Parameter As String.
///
///  ACK header :
///  1 : String : OK or <Error String>
///  2 : Anything else, depending to api


 ls := Packet.ContentMessage.AsStream;
 try
   lAPIName := ReadString(ls);
   lpCount := ReadByte(ls);
   SetLength(lParam,lpCount);
   for i := 0 to lpCount-1 do
   begin
     lParam[i] := ReadString(ls);
   end;

   ls.Clear;
   WriteString(ls, 'OK');  //be optimistic !
   try
     if LowerCase(lAPIName) = 'averagecpulevel' then
     begin
       WriteDouble(ls,FLastCPULevel);
       ls.Position := 0;
     end
     else
     begin
       raise Exception.Create('No such API');
     end;
   EXCEPT
     On E : Exception do
     begin
       ls.Clear;
       WriteString(ls,lAPIName + ' Error : ['+E.Message+']');
     end;
   end;
   //back info !
   ServerMessageSend(Self,Packet.ResponseChannel,ls);
 finally
   FreeAndNil(ls);
 end;

end;

procedure TopGRIDServerLogic.OnServerMessage_AskForSystemDefinition(
  Sender: TBus; var Packet: TBusEnvelop);
var lDefManager : TGRIDClientServerProtocolCentralBOManager; //TODO rename. (TSOGRID...)
    ls : TMemoryStream;
    {$IFDEF FPC}
    lList : Classes.TList;
    {$ELSE}
    lList : TList;
    {$ENDIF}
    lChannelToRespond : string;
    lMessage : TBusMessage;
    Response : TGRIDCentralServiceDefinitionResponse;
begin
  //respond to ServerDef(...)
  ls := Packet.ContentMessage.AsStream;
  ls.Position := 0;

  try

  try
    try
      lChannelToRespond := Packet.ResponseChannel;
      lDefManager := TGRIDClientServerProtocolCentralBOManager.Create;
      lDefManager.LoadFromStream(ls);
    Except
      On E : Exception do
      begin
        raise Exception.Create('AskForSystemDefinition - Protocol error : Stream load stage - ' +E.Message);
      end;
    end;

    try
    {$IFDEF FPC}
      lList := Classes.TList.Create;
    {$ELSE}
      lList := TList.Create;
    {$ENDIF}
      try
        lDefManager.GetEntryByClassName(TGRIDCentralServiceAskDefinition,lList);
        if lList.Count=0 then
        begin
          raise Exception.Create('No AskDefinition object found');
        end;

      finally
        FreeAndNil(lList);
      end;
    Except
      On E : Exception do
      begin
        raise Exception.Create('AskForSystemDefinition - Process failed : Stream decode stage - ' +E.Message);
      end;
    end;

    try
      //All is ok : We send directly a GRID.ClientServer.DataModel.Core.Bo object model embeded in a local protocol response.
      lDefManager.StoredObject.ClearAllData(false);
      response := TGRIDCentralServiceDefinitionResponse.Create(lDefManager);

      ls.Clear;
      { TODO -oVGS -cCRITICAL : Protect datamodel (Interprocess) - Event if Datamodel is a local proc, make it private else}
      CPUUpdate(DataModel.SystemManager.GRIDSystemProfile); //We update on the fly CPU level !
      DataModel.SystemManagerDataModel.SaveToStream(ls);
      ls.Position := 0;
      Response.Result := True;
      Response.buffer := ls;
      ls.Clear;
      lDefManager.SaveToStream(ls);
      lMessage.FromStream(ls);
      Bus.Send(lMessage,lChannelToRespond);
    Except
      On E : Exception do
      begin
        raise Exception.Create('AskForSystemDefinition - Process failed : Stream build state - ' +E.Message);
      end;
    end;

  Except
    On E : Exception do
    begin
      ServerLog(Self,E.Message);
      raise Exception.Create('OnServerMessage_AskForSystemDefinition failure : '+e.Message);
    end;
  end;

  finally
    FreeAndNil(ls);
  end;
end;

procedure TopGRIDServerLogic.OnServerMessage_AuthAsk(Sender: TBus;
  var Packet: TBusEnvelop);
var s : TMemoryStream;
    userName : String;
    Password : String;
    serviceInstanceID : string;
    b : TBusMessage;

    lService : TSOGRIDCentralServiceItem;
    lUserSession : String;
    ld : Boolean;

    Procedure ComposeResponse(aGranted : Boolean; Txt : string);
    begin
      WriteBoolean(s,aGranted);
      WriteString(s,Txt);
    end;

begin
  //respond to ServerAuth(...)
  s := TMemoryStream.Create;
  s.Write(Packet.ContentMessage.Buffer[0],Length(Packet.ContentMessage.Buffer));
  s.Position := 0;
  try
    try
      userName := ReadString(s);
      Password := ReadString(s);
      serviceInstanceID := ReadString(s);

      s.Clear;
      lService := nil;
      if Not DataModel.GetService(serviceInstanceID,lService) then
      begin
        ComposeResponse(False,'Failure - Internal Service not found');
        Exit
      end;

      if DataModel.UserManagement_IsUserAllowed(username,password,lService.ServiceClass) then
      begin
        lUserSession := CST_SESSION_GUID_PREFIX+ServerGetGUID;
        DataModel.UserManagement_RegisterSession(username, lUserSession,Packet.AdditionalData);
        ComposeResponse(True,lUserSession); //Ok, Accredited. We give a session : Shall use for general accreditation system.
      end
      else
      begin
        //If user test fail, it can be a service test. In this case, test is
        //more complicated : We cross test Session(public) with AdditionalData (Stricly private) and if it
        //match, well got as return a username and a password.
        //We retest it classicaly, and if user is allowed to access to the service, access is granted.
        ld := false;
        if lService.ServiceCategory <> scComServer then //Direct ComService is not allowed to bypass by session. ComServer need user/pass accreditation.
        begin
          lUserSession := userName; //In case of Auth proc from other service, the session is used to avoid to many user/pass traffic.
          if DataModel.UserManagement_IsUserAllowedBySessionAndCTX(userName,Packet.AdditionalData,lService.ServiceClass) then
          begin
            ComposeResponse(True,lUserSession); //Ok, Accredited. We response by same session.
          end
          else
          begin
            ComposeResponse(False,'User Session not allowed');
          end;
        end
        else
        begin
          ComposeResponse(False,'User not allowed');
        end;
      end;

      b.FromStream(s);
      //Respond to the thread via its private channel given before.
      Bus.Send(b,Packet.ResponseChannel);

    Except
      On E : Exception do
      begin
        ServerLog(Self,'Authentification process failed : Stream build stage - ' +E.Message);
      end;
    end;

  finally
    FreeAndNil(s);
  end;


end;

procedure TopGRIDServerLogic.OnServerMessage_FromCentral(Sender: TBus;
  var Packet: TBusEnvelop);
begin
  //Please keep this empty : TopGRIDServerLogic is the central service : It must not respond directly to management order.
end;


procedure TopGRIDServerLogic.OnServerMessage_ReportServiceFromGridServerInstance(
  Sender: TBus; var Packet: TBusEnvelop);
var lstream : TMemoryStream;
    lData : TGRIDClientServerDataModelCentralBOManager;
    lResult : TList;
    i : integer;
    litem : TSOGRIDCentralServiceItem;
begin
  //Other TopGRIDServerServiceInstance instance will send message here to declare their service.
  lResult := TList.Create;
  lstream := Packet.ContentMessage.AsStream;
  try
    lData := TGRIDClientServerDataModelCentralBOManager.Create;
    try
      ldata.LoadFromStream(lstream);
      ldata.GetEntryByClassName(TSOGRIDCentralServiceItem,lResult);
      for i := 0 to lResult.Count-1 do
      begin
       litem := TSOGRIDCentralServiceItem(lResult[i]);
       ServerLog(Self,ClassName+' : '+litem.ServiceClass+' Service reported. ('+litem.ServiceDesc+')');
       //{ TODO -oVGS -cCRITICAL : We should Protect Datamodel multithread acess. }
       DataModel.AddService(litem.ServiceInstanceID,litem.ServiceClass,litem.ServiceDesc,litem.ServiceThreadID);
      end;
    finally
      FreeAndNil(lData);
    end;
  finally
    FreeAndNil(lstream);
    FreeAndNil(lResult);
  end;
end;

procedure TopGRIDServerLogic.RunLoop;
begin
//  CPUMessageGenerationProcess;
  case FReaderObject.Event.WaitFor(CST_SERVICETHREADTIMEOUTINMS) of
    wrSignaled :
    begin
      try
        Bus.ProcessMessages([ FReaderObject,
                              LocalAuthentificator,
                              LocalServiceNest,
                              LocalServiceAPI,
                              FReaderSystemDef]); //All Reader in list shared the same Event.
      Except
        On E : Exception do
        begin
          ServerLog(Self,'Alert : Exception in Central module : '+E.Message);
          { TODO :
Make something ?
Analyse entry : If client error ignore ? }
        end;
      end;
    end;
  end;
end;

end.
