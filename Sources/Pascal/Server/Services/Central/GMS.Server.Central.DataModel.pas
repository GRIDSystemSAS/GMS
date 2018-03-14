unit GMS.Server.Central.DataModel;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

Uses
{$IFDEF FPC}
  SysUtils, Classes,
{$ELSE}
  System.SysUtils, System.Classes,
{$ENDIF}
  GS.LocalMemCached,
  GMS.ClientServer.DataModel.Central.BO,
  GMS.Server.DataModel.Central.UserManagement.BO;

Type

TGRIDCentralDataModel = Class
Protected
  //(In Memory Only)
  FSystemManagerDatamodel: TGRIDClientServerDataModelCentralBOManager;
  FSystemManager : TSOGRIDCentralDataModel;

  //This is persistent (File - UserID)
  FUserManagerDataModel : TGRIDServerDataModelCentralUserManagementBOManager;
  FUserManager : TSOGRIDCentralUserManager;

  //In Mem only : Info of the users.

  //GRID Wide shared access data. (MemCached)
  FSharedMemAccess : TLocalMemcachedClient;
Public
  //SystemManager
  Function AddService(aServiceInstanceID, aServiceClass, aServiceDesc : String; aServiceThreadID : Int64) : TSOGRIDCentralServiceItem;
  Function GetService(InstanceId : String; Out Service : TSOGRIDCentralServiceItem) : Boolean;
  Function GetServiceByThreadId(ThreadId : Int64; Out Service : TSOGRIDCentralServiceItem) : Boolean;

  //UserManager
  Function UserManagement_IsUserAllowed(UserName, Password, Service : String) : Boolean;
  Function UserManagement_IsUserAllowedBySessionAndCTX(SessionId, CTXData, Service : String) : Boolean;

  Procedure UserManagement_RegisterSession(aUserName, aSession : string; aPhysicalConnectionData : String);

  Function UserManagement_IsUserExists(aUserName : String; out aUser : TSOGRIDCentralUser) : Boolean; //No case sensitive.
  Function UserManagement_AddOrGetUser(aUserName, aPassword : String) : TSOGRIDCentralUser;

  Function UserManagement_IsGroupExists(aAppFunctionID : String; out aGroup : TSOGRIDCentralGroup) : Boolean; //No case sensitive.
  Function UserManagement_AddGroup(aAppFunctionID, aGroupDesc : String) : TSOGRIDCentralGroup;
  Function UserManagement_GetGroup(aAppFunctionID : string) : TSOGRIDCentralGroup;  //Raise Exception if group not exists.

  Constructor Create; Virtual;
  Destructor Destroy; Override;

  Property SystemManagerDataModel : TGRIDClientServerDataModelCentralBOManager read FSystemManagerDatamodel;
  Property UserManagerDataModel : TGRIDServerDataModelCentralUserManagementBOManager read FUserManagerDataModel;

  Property SystemManager : TSOGRIDCentralDataModel read FSystemManager;
  Property UserManager : TSOGRIDCentralUserManager read FUserManager;
End;


implementation

Uses GMS.Server.Types.Core;
{ TopGRIDDataModel }

function TGRIDCentralDataModel.UserManagement_AddGroup(aAppFunctionID,
  aGroupDesc: String): TSOGRIDCentralGroup;
begin
  Assert(Length(aAppFunctionID)>0);
  Result := Nil;
  if Not(UserManagement_IsGroupExists(aAppFunctionID,Result)) then
  begin
    Result := TSOGRIDCentralGroup.Create(FUserManagerDataModel);
    Result.GroupDesc := aGroupDesc;
    Result.AppFunctionID := aAppFunctionID;
    FUserManager.AddGroupList(Result);
  end;
end;

function TGRIDCentralDataModel.UserManagement_AddOrGetUser(aUserName,
  aPassword: String): TSOGRIDCentralUser;
begin
  Result := Nil;
  if Not(UserManagement_IsUserExists(aUserName,Result)) then
  begin
    Result := TSOGRIDCentralUser.Create(FUserManagerDataModel);
    Result.UserName := aUserName;
    Result.Password := aPassword;
    Result.UserCreateDateTime := Now;
    FUserManager.AddUserList(Result);
  end;
end;

function TGRIDCentralDataModel.UserManagement_GetGroup(
  aAppFunctionID: string): TSOGRIDCentralGroup;
begin
  if Not(UserManagement_IsGroupExists(aAppFunctionID,Result)) then
  begin
    raise Exception.Create(ClassName+'.UserManagement_GetGroup : Group not found.');
  end;
end;

Function TGRIDCentralDataModel.AddService(aServiceInstanceID, aServiceClass,
  AserviceDesc: String; aServiceThreadID : Int64) : TSOGRIDCentralServiceItem;
begin
  Result := TSOGRIDCentralServiceItem.Create(FSystemManagerDatamodel);
  Result.ServiceClass := aServiceClass;
  Result.ServiceDesc := AserviceDesc;
  Result.ServiceInstanceID := aServiceInstanceID;
  Result.ServiceThreadID := aServiceThreadID;
  Result.ServiceStarted := False;
  FSystemManager.AddGRIDService(Result);
end;

constructor TGRIDCentralDataModel.Create;

  Procedure BuildNewStructure; //DEFAULT CONF if first launch an no file exists. Nota : Dev only ?
  var lstrUser : TSOGRIDCentralUser;
      lstrGroup : TSOGRIDCentralGroup;
      ls : TMEmoryStream;
  begin
    //NOTE : Please let this proc as is, and do not refactor it : It is a documentation of the data model technicaly available.
    //Data (persisted)
    FUserManager := TSOGRIDCentralUserManager.Create(FUserManagerDataModel);
    lstrUser := UserManagement_AddOrGetUser('Admin','admin');
    lstrGroup := UserManagement_AddGroup('ADMINISTRATOR','Default administrator (full right, for all services)');
    lstrUser.AddGroups(lstrGroup);
    lstrGroup.AddUsersAttached(lstrUser);

    ls := TMemoryStream.Create;
    try
      FUserManagerDataModel.SaveToStream(ls);
      StreamEncrypt(ls);
      ls.SaveToFile('GRIDCentral.configuration');
    finally
      FreeAndNil(ls);
    end;

    //Data (In memory, Shared)
    //USER<username> <---> List of Group.
    FSharedMemAccess.SetValue('USER'+lstrUser.UserName,lstrGroup.AppFunctionID);
  end;

  var tt : TMemoryStream;

begin
  Inherited Create;
  FSystemManagerDatamodel := TGRIDClientServerDataModelCentralBOManager.Create;
  FSystemManager := TSOGRIDCentralDataModel.Create(FSystemManagerDatamodel);
  FSharedMemAccess := TLocalMemcachedClient.Create(TGRIDData.MemCached);

  //By default; User manager is saved as a raw stream.
  FUserManagerDataModel := TGRIDServerDataModelCentralUserManagementBOManager.Create;
  if FileExists('GRIDCentral.configuration') then
  begin
    tt := TMemoryStream.Create;
    try
      tt.LoadFromFile('GRIDCentral.configuration');
      StreamDecrypt(tt);
      FUserManagerDataModel.LoadFromStream(tt);
    Finally
      FreeAndNil(tt);
    end;

    if FUserManagerDataModel.GetFirstEntryByClassName(TSOGRIDCentralUserManager,TObject(FUserManager)) then //Assignate FUserManager on the fly (if true).
    begin
      ServerLog(Self,'Central configuration loaded successfully : '+IntToStr(FUserManager.UserListCount)+' user registered.');
      if FUserManager.UserListCount=0 then
      begin
        ServerLog(Self,'ALERT : No user found ! Server potentialy unreachable.');
      end;
      if FUserManager.GroupListCount=0 then
      begin
        ServerLog(Self,'ALERT : No group found ! Server potentialy unreachable.');
      end;
    end
    else
    begin
      ServerLog(Self,'ALERT Central configuration empty : Build new structure.');
      BuildNewStructure;
    end;
  end
  else
  begin
    //No file, applying default conf. : create admin/admin
    BuildNewStructure;
  end;
end;

destructor TGRIDCentralDataModel.Destroy;
begin
  FreeAndNil(FSystemManagerDatamodel);
  FreeAndNil(FUserManagerDataModel);
  FreeAndNil(FSharedMemAccess);
  inherited;
end;

function TGRIDCentralDataModel.GetService(InstanceId: String;
  out Service: TSOGRIDCentralServiceItem): Boolean;
var i : integer;
begin
  Result := False;
  for I := 0 to FSystemManager.GRIDServiceCount-1 do
  begin
    if FSystemManager.GRIDService[i].ServiceInstanceID = InstanceId then
    begin
      Service := TSOGRIDCentralServiceItem(FSystemManager.GRIDService[i]);
      Result := true;
      Break;
    end;
  end;
end;

function TGRIDCentralDataModel.GetServiceByThreadId(ThreadId: Int64;
  out Service: TSOGRIDCentralServiceItem): Boolean;
var i : integer;
begin
  Result := False;
  for I := 0 to FSystemManager.GRIDServiceCount-1 do
  begin
    if FSystemManager.GRIDService[i].ServiceThreadID = ThreadId then
    begin
      Service := TSOGRIDCentralServiceItem(FSystemManager.GRIDService[i]);
      Result := true;
      Break;
    end;
  end;
end;



function TGRIDCentralDataModel.UserManagement_IsUserExists(aUserName: String;
  Out aUser: TSOGRIDCentralUser): Boolean;
var i : integer;
begin
  Assert(Not(Assigned(aUser)));
  Result := False;
  for i := 0 to FUserManager.UserListCount-1 do
  begin
    if (lowercase(FUserManager.UserList[i].UserName) = lowercase(aUserName)) then
    begin
      aUser := FUserManager.UserList[i];
      Result := True;
      Exit;
    end;
  end;
end;

procedure TGRIDCentralDataModel.UserManagement_RegisterSession(aUserName,
  aSession: string; aPhysicalConnectionData : String);
var luser : TSOGRIDCentralUser;
    lSe : TSOGRIDCentralSession;
begin
  Assert(length(aUserName)>0);
  Assert(length(aSession)>0);
  Assert(length(aPhysicalConnectionData)>0);
  luser := nil;
  if UserManagement_IsUserExists(aUserName,lUser) then
  begin
    lSe := TSOGRIDCentralSession.Create(FUserManagerDataModel);
    lSe.PhysicalConnectionID := aPhysicalConnectionData;
    LSe.SessionID := aSession;
    lSe.CreateDateTime := Now;
    lSe.UserAttached := luser;
    luser.AddCurrentSessions(lSe);
  end;
end;

function TGRIDCentralDataModel.UserManagement_IsGroupExists(
  aAppFunctionID: String; out aGroup: TSOGRIDCentralGroup): Boolean;
var i : integer;
begin
  Assert(Not(Assigned(aGroup)));
  Result := False;
  for i := 0 to FUserManager.GroupListCount-1 do
  begin
    if (lowercase(FUserManager.GroupList[i].AppFunctionID) = lowercase(aAppFunctionID)) then
    begin
      aGroup := FUserManager.GroupList[i];
      Result := True;
      Exit;
    end;
  end;
end;

function TGRIDCentralDataModel.UserManagement_IsUserAllowed(UserName,
  Password, Service: String): Boolean;
var i : integer;
    ltemp : TSOGRIDCentralUser;
begin
  Result := False;
  ltemp := nil;
  if UserManagement_IsUserExists(UserName,ltemp) then //no case sensitive
  begin
    if (ltemp.Password = Password) then //case sensitive.
    begin
      for i := 0 to ltemp.GroupsCount-1 do
      begin
        if (ltemp.Groups[i].AppFunctionID = Service) OR
           (ltemp.Groups[i].AppFunctionID = 'ADMINISTRATOR') Then
           begin
             Result := True;
             Exit;
           end;
      end;
    end;
  end;
end;

function TGRIDCentralDataModel.UserManagement_IsUserAllowedBySessionAndCTX(
  SessionId, CTXData, Service: String): Boolean;
var i,j : integer;
    ltemp, luser : TSOGRIDCentralUser;
begin
  Result := False;
  ltemp := nil;
  luser := nil;
  for i := 0 to FUserManager.UserListCount-1 do
  begin
    ltemp := FUserManager.UserList[i];
    for j := 0 to ltemp.CurrentSessionsCount-1 do
    begin
      if (ltemp.CurrentSessions[j].SessionID = SessionId) And
         (ltemp.CurrentSessions[j].PhysicalConnectionID = CTXData) then
      begin
        luser := ltemp; //Found.
        Break;
      end;
    end;
  end;

  if Assigned(luser) then
  begin
    Result :=  UserManagement_IsUserAllowed(luser.UserName,luser.Password,Service);
  end;

end;


end.
