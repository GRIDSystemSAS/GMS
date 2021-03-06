unit GMS.Server.Services.DB.SQLLite;

interface

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
  GMS.Server.Types.Core,
  GMS.ClientServer.PersistsConst,
  GMS.Server.Services.Persists.Types,
  GS.StoredObject.BaseTypes,
  GS.StoredObject.Persister,
  GS.StoredObject.Audit,
  GS.StoredObject.Persister.SQLLite3;


Type

TGRIDDBSQLLiteDrivers = Class(TGRIDServerPersistenceDataBaseDriver)
Protected
  Function GetDriverName : String; Override;
  Function GetVersion : String; Override;
Public
  Function GetNewInstance(aDatabaseName : string) : TGRIDServerPersistenceDataBaseInstance; Override;
End;

TGRIDDBSQLLiteAccess = Class(TGRIDServerPersistenceDataBaseInstance)
Private
Protected
  FDB : TofPersisterEngine_SQLLite;
  FAuditManager : TofSOAuditManager;
  function GetMultiInstance: Boolean; Override;
Public
  Constructor Create(aDatabaseName : String; aDriver : TGRIDServerPersistenceDataBaseDriver); Override;
  Destructor Destroy; Override;

  function DataBaseProcessSave(var aIn : TMemoryStream; Out aOut : TMemoryStream; Out ErrorDesc : String) : Boolean; Override;
  function DataBaseProcessLoad(var aIn : TMemoryStream; Out aOut : TMemoryStream; Out ErrorDesc : String) : Boolean; Override;
End;


implementation

{ TGRIDDBSQLLiteAccess }

constructor TGRIDDBSQLLiteAccess.Create(aDatabaseName: String;
  aDriver: TGRIDServerPersistenceDataBaseDriver);
begin
  Inherited;
  FAuditManager := TofSOAuditManager.Create;
  FDB := TofPersisterEngine_SQLLite.Create;
  FAuditManager.Persister := FDB;
  FDB.Connect(aDatabaseName,plmThroughtAudit);
end;

function TGRIDDBSQLLiteAccess.DataBaseProcessLoad(var aIn: TMemoryStream;
  out aOut: TMemoryStream; out ErrorDesc: String): Boolean;
var lReq : String;
    la : TofSOAuditManager;
begin
  Assert(Assigned(aIn));
  Result := False;
  //aIn is just a string :)
  aIn.Position := 0;
  try
    lReq := ReadString(aIn);
    la := nil;
    FDB.GetAudit(lReq,TofSOCustomAuditManager(la));
  Except
    On E : Exception do
    begin
      ErrorDesc := ClassName + ' Loading Server audit step : '+E.Message + ' | Query : "'+lReq+'"';
      ServerLog(Self,'EXCEPTION <SILENT> Database LOAD Process : Step GetAudit : '+ErrorDesc);
      Exit;
    end;
  end;
  ServerLog(Self,'Database LOAD Process : '+IntToStr(la.AuditCentral.StoredObject.ObjectCount)+' object(s) trace found in Server audit');

  try
    aOut := TMemoryStream.Create;
    aOut.LoadFromStream(la.AuditCentral.ToStream);

    FreeAndNil(la);
    Result := true;
  Except
    On E : Exception do
    begin
      ErrorDesc := ClassName + ' Building Audit Stream for client : '+E.Message;
      ServerLog(Self,'EXCEPTION <SILENT> Database LOAD Process : Step Audit.ToStream : '+ErrorDesc);
    end;
  end;
end;

function TGRIDDBSQLLiteAccess.DataBaseProcessSave(var aIn: TMemoryStream;
  out aOut: TMemoryStream; Out ErrorDesc : String): Boolean;
begin
  Assert(Assigned(aIn));
  Result := False;
  aIn.Position := 0;
  try
    FAuditManager.Audit_ClearAllData;
    FAuditManager.AuditCentral.LoadFromStream(aIn);
  Except
    On E : Exception do
    begin
      ErrorDesc := ClassName + ' Loading Client audit step : '+E.Message;
      ServerLog(Self,'EXCEPTION <SILENT> Database SAVE Process : Step Audit.LoadFromStream : '+ErrorDesc,gssllException);
      Exit;
    end;
  end;
  ServerLog(Self,'Database SAVE Process : '+IntToStr(FAuditManager.AuditCentral.StoredObject.ObjectCount)+' object(s) trace found in audit sent by the client',gssllInfo);
  try
    //DEBUG FAuditManager.Persister.Save(FAuditManager,True);
    FAuditManager.Persister.Save(FAuditManager); //We not call Save, because it need a linked TStoredObject... Which is client side :)    FAuditManager.Audit_ClearAllData;
    ErrorDesc := EmptyStr;
    Result := True;
  Except
    On E : Exception do
    begin
      ErrorDesc := ClassName + ' Save step : '+E.Message;
      ServerLog(Self,'EXCEPTION <SILENT> Database SAVE Process : Step Persiter.Save(Audit) '+ErrorDesc,gssllException);
    end;
  end;
end;

destructor TGRIDDBSQLLiteAccess.Destroy;
begin
  FDB.Disconnect;
  FreeAndNil(FDB);
  FreeAndNil(FAuditManager);
  inherited;
end;

function TGRIDDBSQLLiteAccess.GetMultiInstance: Boolean;
begin
  Result := False;
end;

{ TGRIDDBSQLLiteDrivers }

function TGRIDDBSQLLiteDrivers.GetDriverName: String;
begin
  Result := 'SQLLite';
end;

function TGRIDDBSQLLiteDrivers.GetNewInstance(aDatabaseName : string) : TGRIDServerPersistenceDataBaseInstance;
begin
  Result := TGRIDDBSQLLiteAccess.Create(aDatabaseName,Self);
end;

function TGRIDDBSQLLiteDrivers.GetVersion: String;
begin
  Result := '1.0';
end;

Initialization

TGRIDServerPersistenceFactory.Drivers.Add(TGRIDDBSQLLiteDrivers.Create);

Finalization

end.
