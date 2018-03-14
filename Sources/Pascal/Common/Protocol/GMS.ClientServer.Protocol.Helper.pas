//Utility unit for BO objects.
unit GMS.ClientServer.Protocol.Helper;

interface

Uses Classes, SysUtils,
     GMS.ClientServer.Protocol.Transport.BO;

Type

TGCPUtils = Class
  Class Function BuildC2SProtocolObject(aManager : TGRIDClientServerProtocolTransportBOManager) : TGRIDC2SProtocolEntry;
  Class Function GetFirstC2SProtocolEntry(aManager : TGRIDClientServerProtocolTransportBOManager; Out aC2SProtoEntry : TGRIDC2SProtocolEntry) : Boolean;
  Class Function GetFirstC2SAuth(aManager : TGRIDClientServerProtocolTransportBOManager; var aC2SCAuth :  TGRIDC2SAuth) : Boolean;

  Class Function BuildS2CProtocolObject(aManager : TGRIDClientServerProtocolTransportBOManager) : TGRIDS2CProtocolEntry;
  Class Function GetFirstS2CProtocolEntry(aManager : TGRIDClientServerProtocolTransportBOManager; Out aS2CProtoEntry : TGRIDS2CProtocolEntry) : Boolean;
  Class Function GetFirstS2CAuth(aManager : TGRIDClientServerProtocolTransportBOManager; var aS2CAuth :  TGRIDS2CAuth) : Boolean;

End;

implementation

{ TGCPUtils }

class function TGCPUtils.BuildC2SProtocolObject(aManager : TGRIDClientServerProtocolTransportBOManager): TGRIDC2SProtocolEntry;
begin
  Assert(Assigned(aManager));
  Result := TGRIDC2SProtocolEntry.Create(aManager);
  Result.ClientProtocolVersion := 1;
end;

class function TGCPUtils.BuildS2CProtocolObject(
  aManager: TGRIDClientServerProtocolTransportBOManager): TGRIDS2CProtocolEntry;
begin
  Assert(Assigned(aManager));
  Result := TGRIDS2CProtocolEntry.Create(aManager);
  Result.ServerProtocolVersion := 1;
end;

class function TGCPUtils.GetFirstC2SAuth(
  aManager: TGRIDClientServerProtocolTransportBOManager;
  var aC2SCAuth: TGRIDC2SAuth): Boolean;
var  i : integer;
begin
  aC2SCAuth := nil;
  Result := False;
  for I := 0 to aManager.StoredObject.BusinessObjectsCount-1 do
  begin
    if aManager.StoredObject.BusinessObjectsByIndex[i] is TGRIDC2SAuth then
    begin
      aC2SCAuth := TGRIDC2SAuth(aManager.StoredObject.BusinessObjectsByIndex[i]);
      Result := True;
      Break;
    end;
  end;
end;

class function TGCPUtils.GetFirstC2SProtocolEntry(
  aManager: TGRIDClientServerProtocolTransportBOManager;
  Out aC2SProtoEntry: TGRIDC2SProtocolEntry): Boolean;
var i : Integer;
begin
  aC2SProtoEntry := nil;
  Result := False;
  for I := 0 to aManager.StoredObject.BusinessObjectsCount-1 do
  begin
    if aManager.StoredObject.BusinessObjectsByIndex[i] is TGRIDC2SProtocolEntry then
    begin
      aC2SProtoEntry := TGRIDC2SProtocolEntry(aManager.StoredObject.BusinessObjectsByIndex[i]);
      Result := True;
      Break;
    end;
  end;
end;

class function TGCPUtils.GetFirstS2CAuth(
  aManager: TGRIDClientServerProtocolTransportBOManager;
  var aS2CAuth: TGRIDS2CAuth): Boolean;
var  i : integer;
begin
  aS2CAuth := nil;
  Result := False;
  for I := 0 to aManager.StoredObject.BusinessObjectsCount-1 do
  begin
    if aManager.StoredObject.BusinessObjectsByIndex[i] is TGRIDS2CAuth then
    begin
      aS2CAuth := TGRIDS2CAuth(aManager.StoredObject.BusinessObjectsByIndex[i]);
      Result := True;
      Break;
    end;
  end;
end;

class function TGCPUtils.GetFirstS2CProtocolEntry(
  aManager: TGRIDClientServerProtocolTransportBOManager;
  Out aS2CProtoEntry: TGRIDS2CProtocolEntry): Boolean;
var i : Integer;
begin
  aS2CProtoEntry := nil;
  Result := False;
  for I := 0 to aManager.StoredObject.BusinessObjectsCount-1 do
  begin
    if aManager.StoredObject.BusinessObjectsByIndex[i] is TGRIDS2CProtocolEntry then
    begin
      aS2CProtoEntry := TGRIDS2CProtocolEntry(aManager.StoredObject.BusinessObjectsByIndex[i]);
      Result := True;
      Break;
    end;
  end;
end;

end.
