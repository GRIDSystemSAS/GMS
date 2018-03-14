unit GMS.Client.Resolver;

interface


Uses
    {$IFDEF FPC}
    {$mode Delphi}
    SysUtils, Classes, Generics,
    {$ELSE}
    System.SysUtils, System.Classes, System.Generics.Collections,
    {$ENDIF}
    GMS.Client.Transporter.indy10,
    GMS.Client.SSDPLikeServer.DataModel,
    GMS.Client.SSDPLikeServer.DataModel.BO;


Const
  CST_SSDP_ROOTDEVICE     = 'upnp:rootdevice';
  CST_SSDP_HTTPOK         = 'HTTP/1.1 200 OK';

type

TGridClientResolver = Class
Public
  Procedure Resolve; Virtual; Abstract;
End;

TGridClientSSDPLikeResolver = Class(TGridClientResolver)
Private
  FImpl : TGridClientTransporterIndy10UDP;
  FDataM : TGRIDSSDPServerSideManager;

  Function TextStartsWith(S,SubS : String) : Boolean;
Public
  Data : TGRIDSSDPServerSideDatamodel;

  Constructor Create; Virtual;
  Destructor Destroy; Override;

  Procedure Resolve; Override;
End;


TGridServerInstanceItem = Class
  GridServerIP : String;
  GridServerPort : String;
  GridServerName : String;
  GridServerDesc : String;
  GridServerHost : String;
End;

TGridServerInstanceItems = Class(TObjectList<TGridServerInstanceItem>)
Public
End;

//Perform complete resolution, with named data.
TGridClientGRIDResolver = Class(TGridClientSSDPLikeResolver)
Private
  FInternalList : TGridServerInstanceItems;
    function GetServer(Index: UINT32): TGridServerInstanceItem;
    function GetServerCount: UINT32;
Public
  Constructor Create; Override;
  Destructor Destroy; Override;
  Procedure Resolve; Override;

  Property GRIDServers[Index : UINT32] : TGridServerInstanceItem read GetServer;
  Property GRIDServerCount : UINT32 read GetServerCount;
End;

implementation

Uses GMS.Client, GMS.ClientServer.DataModel.Central.BO;


{ TGridClientSSDPLikeResolver }

constructor TGridClientSSDPLikeResolver.Create;
begin
  Inherited;
  FDataM := TGRIDSSDPServerSideManager.Create;
  Data := TGRIDSSDPServerSideDatamodel.Create(FDataM);
  FImpl := TGridClientTransporterIndy10UDP.Create;
end;

destructor TGridClientSSDPLikeResolver.Destroy;
begin
  FImpl.Close;
  FreeAndNil(FImpl);
  FreeAndNil(FDataM);
  inherited;
end;

procedure TGridClientSSDPLikeResolver.Resolve;
var
    i : integer;
    s,obb : integer;
    strAn : TStringList;

    anServerName : String;
    anUID : String;
    anType : String;
    anLocation : String;
    aDevice : TGRIDSSDPServerSideDevice;

    Function ExtractValue(FromString : String) : String;
    var p : Integer;
    begin
      Result := EmptyStr;
      p := Pos(':',FromString);
      if p>0 then
        Result := trim(Copy(FromString,p+1,Length(FromString)-p));
    end;

    Procedure Eval;
    begin
      if (anType <> '') And (anserverName <>'') And (anUID<>'') and (anLocation<>'') then
      begin
        //if (Length(anUID)>0) then
        begin
          //We should have all the info here.
          if anType = CST_SSDP_ROOTDEVICE then
          begin
            if Not(Data.IsDeviceExistsByUID(anUID,obb)) then
            begin
              //log(' BuildSnapShotDataModel : New root device detected : '+anServerName);

              aDevice := TGRIDSSDPServerSideDevice.Create(FDataM);
              aDevice.ServerName := anServerName;
              aDevice.UNS := anUID;
              aDevice.Location := anLocation;
              Data.AddDeviceList(aDevice);
            end
            else
            begin
              //log(' BuildSnapShotDataModel : Root device "'+anServerName+'" exists yet.');
            end;
          end
          else
          begin
            //TODO OTHER TYPE. (SERVICE...)
          end;
        end;


        aDevice := nil;
        anServerName := EmptyStr;
        anUID := EmptyStr;
        anType := EmptyStr;
        anLocation := EmptyStr;
      end;
    end;

begin
  FDataM.StoredObject.ClearAllData(False);
  Data := TGRIDSSDPServerSideDatamodel.Create(FDataM);

  //Log('BuildSnapShotDataModel : Broadcasting M-SEARCH...');
  //Note / should be sent if we had a "real" server listen en 239.255.255.250:1900.
  //But currently, our server listen on other interface on port 1900.
  //This is mainly for 2 reason :
  //- Our SSDP server is just four our usage, it is not open.
  //- We do not want to do the job of the OS.
  FImpl.ReceiveTimeout := 2000;
  FImpl.SendMode := udpsmBroadcast;
  FImpl.RemotePort := 1900;
  FImpl.WriteAsString('M-SEARCH * HTTP/1.1' + #13#10 +
                      'HOST: 239.255.255.250:1900' + #13#10 +
                      'MAN: "ssdp:discover"'+ #13#10 +
                      'MX: 3'+ #13#10 +
                       // 'ST: urn:dial-multiscreen-org:service:dial:1'+ #13#10 +
                       // 'ST: urn:schemas-udap:service:smartText:1'+ #13#10 +
                       // 'ST: udap:all'+ #13#10 +
                       // 'ST: urn:all'+ #13#10 +
                      'ST: ssdp:all'+ #13#10);

  //Log('BuildSnapShotDataModel : Broadcast M-SEARCH done.');
  strAn := TStringList.Create;
  try
    stran.Text := FImpl.ReadAsString;
    //Log('BuildSnapShotDataModel : Analysing data...');

   aDevice := nil;
   anServerName := EmptyStr;
   anUID := EmptyStr;
   anType := EmptyStr;
   anLocation := EmptyStr;

    for i := 0 to strAn.Count-1 do
    begin
      if TextStartsWith(lowercase(strAn[i]),'st:') then
      begin
        anType := ExtractValue(strAn[i]);
      end
      else
      if TextStartsWith(lowercase(strAn[i]),'location') then
      begin
        anLocation := ExtractValue(strAn[i]);
      end
      else
      if TextStartsWith(lowercase(strAn[i]),'usn') then
      begin
        anUID := ExtractValue(strAn[i]);
      end
      else
      if TextStartsWith(lowercase(strAn[i]),'server') then
      begin
        anServerName := ExtractValue(strAn[i]);
      end;

      Eval;
    end;
  //Log('BuildSnapShotDataModel : Done.');
  finally
    FreeAndNil(strAn);
  end;
end;

function TGridClientSSDPLikeResolver.TextStartsWith(S,SubS : String): Boolean;
var LLen : Integer;
begin
  LLen := Length(SubS);
  Result := LLen <= Length(S);
  if Result then
    Result := System.String.Compare(S, 0, SubS, 0, LLen, True) = 0;
end;

{ TGridClientGRIDResolver }

constructor TGridClientGRIDResolver.Create;
begin
  inherited;
  FInternalList := TGridServerInstanceItems.Create;
end;

destructor TGridClientGRIDResolver.Destroy;
begin
  FreeAndNil(FInternalList); //Owned ObjectList
  inherited;
end;

function TGridClientGRIDResolver.GetServer(
  Index: UINT32): TGridServerInstanceItem;
begin
  result := FInternalList[Index];
end;

function TGridClientGRIDResolver.GetServerCount: UINT32;
begin
  Result := FInternalList.Count;
end;

procedure TGridClientGRIDResolver.Resolve;
Const CST_UNABLE_TO_RETRIEVE = 'Unknown';
var a : TGridClientSSDPLikeResolver;
    c : TGridClient;
    cc : TGRIDClientServerDataModelCentralBOManager;
    ccc : TSOGRIDCentralDataModel;
    i : integer;
    temp, ip,port : string;
    aItem : TGridServerInstanceItem;
begin
  FInternalList.Clear;
  a := TGridClientSSDPLikeResolver.Create;
  c := TGridClient.Create;
  cc := TGRIDClientServerDataModelCentralBOManager.Create;
  try
    a.Resolve;

    for I := 0 to a.Data.DeviceListCount-1 do
    begin
      temp :=a.Data.DeviceList[i].Location;
      if pos('grid',temp)>0 then
      begin
        temp := lowercase(temp);
        temp := trim(Stringreplace(temp,'grid://','',[]));
        port := Copy(temp,Pos(':',temp)+1,length(temp)-Pos(':',temp));
        Ip := Copy(temp,1,Pos(':',temp)-1);

        aItem := TGridServerInstanceItem.Create;
        aItem.GridServerIP := Ip;
        aItem.GridServerPort := port;
        aItem.GridServerName := CST_UNABLE_TO_RETRIEVE;
        aItem.GridServerDesc := CST_UNABLE_TO_RETRIEVE;
        aItem.GridServerHost := CST_UNABLE_TO_RETRIEVE;

        try
          c.Connect(IP,StrToInt(Port));
          c.ServerDefinition(cc);
          c.Disconnect;
          if cc.GetFirstEntryByClassName(TSOGRIDCentralDataModel,TObject(ccc)) then
          begin
            aItem.GridServerName := ccc.GRIDServerName;
            aItem.GridServerDesc := ccc.GRIDServerDesc;
            aItem.GridServerHost := ccc.GRIDServerHost;
          end;
          cc.StoredObject.ClearAllData(True);
        Except
          //Silent exception. Resolution should not be fatal if one of thos server is not up-to-date with the client protocol.
        end;

        FInternalList.Add(aItem);

      end;
    end;
  finally
    FreeAndNil(a);
    FreeAndNil(c);
    FreeAndNil(cc);
  end;
end;

end.
