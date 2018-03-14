unit GMS.Server;

interface
{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}


Uses
  {$IFDEF FPC}
  SysUtils, Classes, Generics.Collections,
  {$ELSE}
  System.SysUtils, System.Classes, Types, System.Generics.Collections,
  {$ENDIF}
  GMS.Server.Types.Core,
  GMS.Server.Central,
  GS.Stream,
  GS.Task,
  GS.Threads,
  GS.Bus,
  GMS.Server.Central.DataModel,
  GMS.ClientServer.DataModel.Central.BO,
  GMS.Server.Services.IndyTCPServer,
  GMS.Server.Services.IndySSDPLikeServer
//  GMS.Server.Services.persists,
  {$IFDEF MSWINDOWS}
  ,
  {$IFDEF FPC}
  Windows;
  {$ELSE}
  Winapi.Windows;
  {$ENDIF}
  {$ELSE}
  ;
  {$ENDIF}

Type
  TopGRIDServerLogEvent = Procedure(Sender : TObject; aText : string) of Object;
  TopGRIDServer = Class
  Private
    FOnLog : TopGRIDServerLogEvent;
  Protected
  Public
    LocalLogReader : TBusClientReader; //Bus

    GRID : TopGRIDServerLogic;
    ServiceManager : TopGRIDServerServices;

    aServer : TGRIDService_IndyTCPServer;
    aSSDP : TGRIDService_IndySSDPLikeServer;
    { Déclarations publiques }

    Procedure OnServerLogMessage(Sender : TBus;Var Packet : TBusEnvelop);


    Procedure OnTaskBegin(Sender : TObject; Task : TopTask);
    Procedure OnTaskProgress(Sender : TObject; Task : TopTask; aProgressString : String);
    Procedure OnTaskProgressData(Sender : TObject; Task : TopTask; aProgressStream : TMemoryStream);
    //Procedure OnTaskInterrupt(Sender : TObject; Task : TopTask); //Shall not be used here : Could stop task such as CoreManager !
    Procedure OnTaskFinish(Sender : TObject; Task : TopTask);
    Procedure OnTaskException(Sender : TObject; Task : TopTask; E : Exception);

    Procedure DecodeStream(aStream : TMemoryStream);
      Procedure SendServerPresentationToComServer(aTargetInstanceId, aTargetCtxId : String);
      Procedure SendServerStatusToComServer(aTargetInstanceId, aTargetCtxId : String);

    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure Activate;
    Procedure Desactivate;
    Procedure StopServices;
    Procedure Process;

    Procedure LogIn(aText : String);

    Property OnLog : TopGRIDServerLogEvent read FOnLog Write FOnLog;
  End;


implementation

{ TopGRIDServer }

procedure TopGRIDServer.Activate;
begin
  try
    ServiceManager.StartServices;

    if Assigned(aServer) then
    begin
      aServer.Enable;
      if aServer.Active then
        ServerLog(Self, 'TCP server active.');

      LogIn(aServer.Binding);
      GRID.DataModel.SystemManager.GRIDServerInterface := aServer.Binding;

      //TODO : Activate by binding. In the case of automap port, Defaultport = 0, but binding is ok.
      //In fact, Default port is only available on single binding.
      if assigned(aSSDP) then
      begin
        aSSDP.LOCALPORT.Value := IntToStr(aServer.Port);
        ServerLog(Self, 'SSDP-Like server active.');
      end;

    end;

    LogIn(ClassName + ' Activated.');

  Except
    On E : Exception do
    begin
      raise Exception.Create(ClassName+'.Activate : '+E.Message);
    end;
  end;
end;

constructor TopGRIDServer.Create;
begin
  inherited;
  LocalLogReader := Bus.Subscribe(CST_SERVER_GLOBAL_LOG_CHANNEL,OnServerLogMessage);
  ServiceManager := TopGRIDServerServices.Create;

  ServiceManager.Manager.OnTaskProgress := OnTaskProgress;
  ServiceManager.Manager.OnTaskProgressData :=  OnTaskProgressData;
  ServiceManager.Manager.OnTaskException := OnTaskException;
  ServiceManager.Manager.OnTaskBegin := OnTaskBegin;
  ServiceManager.Manager.OnTaskFinish := OnTaskFinish;

  //---- Services.
  GRID := TopGRIDServerLogic(ServiceManager.AddService(TopGRIDServerLogic));
  GRID.Start;

  //This is a pure TCPServer. All incoming TCP network data on port [OS_Choosen] will be manager from here.
  aServer := TGRIDService_IndyTCPServer(ServiceManager.AddService(TGRIDService_IndyTCPServer));
  aServer.EnableLogVerbose := True;

  //This is a SSDP like server : It will announce our server on the network via UDP Protocol
  aSSDP := TGRIDService_IndySSDPLikeServer(ServiceManager.AddService(TGRIDService_IndySSDPLikeServer));

  //ServiceManager.AddService(TGRIDServerPersistenceService);
end;

procedure TopGRIDServer.DecodeStream(aStream: TMemoryStream);
//var a : TGPCEnvelop;
//    b : TGPCEnvelopeMainBusiness;
//    c : TGPCEnvelopeMainBusinessServerPresentation;
//    d : TGPCEnvelopeMainBusinessServerStatus;
begin
//  Assert(Assigned(aStream));
//  aStream.Position := 0;
//  a.Decode(aStream);
//  case a.SourceType of
//    gtLogger: ;
//    gtCoreManagement:
//    begin
//      case a.DestType of
//        gtLogger: ;
//        gtCoreManagement: ;
//        gtComServer: ;
//        gtAPICenter: ;
//        gtMessager: ;
//        gtServiceProvider: ;
//        gtMainTask:
//        begin
//          b.Decode(aStream);
//          case b.MessageType of
//            mbmtLowLevelTaksList: ;
//            mbmtServerPresentation:
//            begin
//              c.Decode(aStream);
//              LogIn('Sending data server presentation to comserver connection id : '+c.FinalCtxId);
//              SendServerPresentationToComServer(a.DestInstanceID,c.FinalCtxId);
//            end;
//            mbmtServerStatus:
//            begin
//              d.Decode(aStream);
//              LogIn('Sending data server status to comserver connection id : '+d.FinalServerStatusCtxId);
//              SendServerStatusToComServer(a.DestInstanceID,d.FinalServerStatusCtxId);
//            end;
//          end;
//        end;
//      end;
//    end;
//    gtComServer: ;
//    gtAPICenter: ;
//    gtMessager: ;
//    gtServiceProvider: ;
//    gtMainTask: ;
//  end;
end;

procedure TopGRIDServer.Desactivate;
begin
  aServer.Disable;
  aSSDP.Disable;
  StopServices;
end;

destructor TopGRIDServer.Destroy;
begin
  ServiceManager.BeforeDestroy;
  FreeAndNil(ServiceManager);
  FreeAndNil(LocalLogReader);
  inherited;
end;


procedure TopGRIDServer.LogIn(aText: String);
begin
  if Assigned(FOnLog) then
  begin
    FOnLog(Self, aText);
  end;
end;

procedure TopGRIDServer.OnServerLogMessage(Sender: TBus;
  var Packet: TBusEnvelop);
var s : TMemoryStream;
    LocalThreadID : Int64;
    localObjectAdress : Int64;
    LocalObjectClass : String;
    localMessage : string;
begin
  s := Packet.ContentMessage.AsStream;
  try
    try
      LocalThreadID := ReadInt64(s);
      localObjectAdress := ReadInt64(s);
      LocalObjectClass := ReadString(s);
      localMessage := ReadString(s);
      LogIn('['+LocalObjectClass+'/'+IntToStr(localObjectAdress)+'] '+localMessage +' ThreadID : '+ IntToStr(LocalThreadID));
    Except
     ///
    end;
  finally
    FreeAndNil(s);
  end;

end;

procedure TopGRIDServer.OnTaskBegin(Sender: TObject; Task: TopTask);
var service : TSOGRIDCentralServiceItem;
    //mem : TMemoryStream;
    //BusMessage : TBusMessage;
begin
  ServerLog(Self, 'TaskManager:OnTaskBegin: '+Task.ClassName +' TaskThreadID : '+ IntToStr(Task.ThreadID));
  //Notify existing task that a new task has been started.
  If GRID.DataModel.GetServiceByThreadId(Task.ThreadID,service) then
  begin
    Service.ServiceStarted := true;
  end;
end;

procedure TopGRIDServer.OnTaskException(Sender: TObject; Task: TopTask;
  E: Exception);
//var service : TGRIDCentralServiceData;
begin
  ServerLog(Self,'TaskManager:OnTaskExcept: '+Task.ClassName +' TaskThreadID : '+ IntToStr(Task.ThreadID));
//  If GRID.DataModel.GetServiceByThreadId(Task.ThreadID,service) then
//  begin
//    service.ServiceStarted := false;
//  end;
end;

procedure TopGRIDServer.OnTaskFinish(Sender: TObject; Task: TopTask);
var service : TSOGRIDCentralServiceItem;
begin
  ServerLog(Self, 'TaskManager:OnTaskFinish: '+Task.ClassName +' TaskThreadID : '+ IntToStr(Task.ThreadID));
  If GRID.DataModel.GetServiceByThreadId(Task.ThreadID,service) then
  begin
    service.ServiceStarted := false;
  end;
end;

procedure TopGRIDServer.OnTaskProgress(Sender: TObject; Task: TopTask;
  aProgressString: String);
begin
  ServerLog(Self, 'TaskManager:OnTaskProgress: '+Task.ClassName +'"'+aProgressString+'" TaskThreadID : '+ IntToStr(Task.ThreadID));
end;

procedure TopGRIDServer.OnTaskProgressData(Sender: TObject; Task: TopTask;
  aProgressStream: TMemoryStream);
begin
  ServerLog(Self, 'TaskManager:OnTaskProgressData: '+Task.ClassName +'"OnTaskProgress(Stream): Stream size = '+IntToStr(aProgressStream.Size)+'" TaskThreadID : '+ IntToStr(Task.ThreadID));
//  DecodeStream(aProgressStream);
end;

procedure TopGRIDServer.Process;
begin
  ServiceManager.Process; //Run TaskManager event.
  Bus.ProcessMessages(LocalLogReader); //Process Bus Message.
end;

procedure TopGRIDServer.SendServerPresentationToComServer(aTargetInstanceId,
  aTargetCtxId: String);
//    pres : TGRIDClientDataModelManager;
//    a : TGridServerDescription;
begin
  //Over network client response stream.
//  Pres := TGRIDClientDataModelManager.Create;

//  a := TGridServerDescription.Create(pres);
//  a.ServerName := GRID.DataModel.GRIDServerName;
//  a.ServerHost := GRID.DataModel.GRIDServerHost;
//  a.ServerDesc := GRID.DataModel.GRIDServerDesc;
//  a.HostGenuineName := GRID.GetOSGenuineName;
//  a.HostOSNameAsString := GRID.GetOSName;
//  a.HostOSName := GRID.GetOSName; TODO
//  a.HostOSVersion := GRID.GetOSMajorMinorBuild;
//  a.HostArchitectureAsString := GRID.GetOSArchitecture;
//  a.HostArchitecture := GRID.GetOSArchitecture; TODO

  //Desc ?
  //Startup ?
  //...
////  s := TGridClientProtocol.ServerSide_ResponseCallPresentation(Pres);
//  try
    ////GRID inner stream communication.
//    ss := TGridProtocole.MainTask.GenerateGRIDServiceServerPresentation
//                                                      (
//                                                       aTargetInstanceId,
//                                                       aTargetCtxId,
//                                                       s
//                                                      );
//    try
//      GRID.Services.SendMessageToAllTask(ss);
//    finally
//      FreeAndNil(ss);
//    end;
//  finally
//    FreeAndNil(s);
//    FreeAndNil(pres);
//  end;
end;

procedure TopGRIDServer.SendServerStatusToComServer(aTargetInstanceId,
  aTargetCtxId: String);
//    pres : TGRIDClientDataModelManager;
begin
//  Pres := GetServerStatusInfo;

//  s := TGridClientProtocol.ServerSide_ResponseCallStatus(Pres);
  try
    //GRID inner stream communication.
//    ss := TGridProtocole.MainTask.GenerateGRIDServiceServerStatus
//                                                      (
//                                                       aTargetInstanceId,
//                                                       aTargetCtxId,
//                                                       s
//                                                      );
    try
//      GRID.Services.SendMessageToAllTask(ss);
    finally
//      FreeAndNil(ss);
    end;
  finally
//    FreeAndNil(s);
//    FreeAndNil(pres);
  end;
end;

procedure TopGRIDServer.StopServices;
var i : integer;
    l : TList;
    ls : string;
begin
  ServerMessageSendBroadCast(cioTerminate,EmptyStr); //That will terminate service's thread, where supported. Mandatory for service in DLL or SO.
  l := ServiceManager.Manager.TaskListLock;
  try
    for I := 0 to l.Count-1 do
    begin
      Process; //For log.
      if Not(TObject(l[i]) is TopGRIDServerLogic) then
      begin
        ls :=  TopGRIDServerServiceInstance(l[i]).ClassName;
        ServiceManager.Manager.KillTask(i);          //Warning : Does not work with servive in dll : They not respond to Terminate. So WaitFor locked all.
        ServerLog(Self,ls + ' task finished.');
      end;
    end;
  finally
    ServiceManager.Manager.TaskListUnLock;
  end;
  ServiceManager.Manager.KillAll; //Kill GridServerLogic (on last, because
end;


end.
