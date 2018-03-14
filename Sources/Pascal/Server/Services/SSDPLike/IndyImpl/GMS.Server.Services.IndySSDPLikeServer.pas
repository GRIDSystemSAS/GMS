unit GMS.Server.Services.IndySSDPLikeServer;

{$IFDEF FPC}
  {$mode delphi}
  {$H+}
{$ENDIF}

interface

Uses
{$IFDEF FPC}
  Classes, SysUtils, Generics.Collections,
  SyncObjs,
{$ELSE}
  System.Classes, System.SysUtils, System.Generics.Collections,
  System.SyncObjs, System.Threading,
{$ENDIF}
  IdSocketHandle, IdUDPServer, IdGlobal,
  IdComponent, IdBaseComponent, IdUDPBase, IdUDPClient,
  GS.Task,
  GS.Bus,
  GS.Threads,
  GMS.Server.Types.Core,
  GMS.Server.Services.SSDPLikeServer.DataModel;

Const
  CST_SERVER_PRODUCT_NAME = 'ObjectUp (c) 2016 - GRID Server V1.0';
  CST_SSDP_ROOTDEVICE     = 'upnp:rootdevice';
  CST_SSDP_HTTPOK         = 'HTTP/1.1 200 OK';

Type

TGRIDService_IndySSDPLikeServer = Class(TopGRIDServerServiceInstance)
Private
  FUDPServer : TidUDPServer;
  FUDPClient : TIdUDPClient;
  FMasterData : TGRIDSSDPManager;
  FData : TGRIDSSDPDataModel;

  FGUID : TProtectedString; //Unique ID of a server. (Only RunTime valid, not saved.)
  FLOCALIP : TProtectedString;

  function GetGUID: String;
  function GetLocalIP: String;

  Procedure DoSendNetworkPresence;

  //Act as a client, and ask to the local network area all the ssdp service available.
  //It will clear and entirely refresh the FDATA model.
{  Procedure BuildSnapShotDataModel;
}

  {$IFDEF FPC}
  Procedure OnIdUDPServerUDPRead(AThread: TIdUDPListenerThread;
  AData: TIdBytes; ABinding: TIdSocketHandle);
  {$ELSE }
  Procedure OnIdUDPServerUDPRead(AThread: TIdUDPListenerThread;
  const AData: TIdBytes; ABinding: TIdSocketHandle);
  {$ENDIF }

Protected
  function GetInstanceType: TopGRIDServerServiceInstanceType; Override;
  function GetDescription : String; Override;


Public
  LOCALPORT : TProtectedString; //To set by the server.

  Constructor Create; Override;
  Destructor Destroy; Override;
  Procedure Disable;

  //Messaging bus (Global com. channel)
  Procedure OnServerMessage_FromCentral(Sender : TBus;Var Packet : TBusEnvelop); Override;


  Procedure BeforeRun; Override;

  Property GUID : String read GetGUID;
  Property LocalIpv4 : String read GetLocalIP;
End;


implementation

{ TGRIDService_IndySSDPLikeServer }

procedure TGRIDService_IndySSDPLikeServer.BeforeRun;
{$IFDEF FPC}
var a : TGUID;
{$ENDIF }
begin
  //1 AutoTest Server : Getting Local IP (LOCATION) by broadcast process.
  Try
    ServerLog(Self,'Local UDP Server activating...');
    FUDPServer.Active := True;
    ServerLog(Self,'Local UDP Server activated...');

    {$IFDEF FPC}
    if CreateGUID(a) = s_OK then
      FGUID.Value := GuidToString(a)
    else
      RaiseLastOSError;
    {$ELSE}
    FGUID.Value := TGUID.NewGuid.ToString;
    {$ENDIF }

    ServerLog(Self,'GUID is '+GUID);

    ServerLog(Self,'Broadcasting identity...');
    FUDPClient.Broadcast(
       CST_SERVER_PRODUCT_NAME + #13#10 +
       FGUID.Value,
       1900);

    ServerLog(Self,'Startup done..');

  Except
    On e : Exception do
    begin
      ServerLog(Self,'EXCEPTION : '+e.Message);
      //Terminate;
    end;
  End;
end;

{
procedure TGRIDService_IndySSDPLikeServer.BuildSnapShotDataModel;
var aUDPClient : TIdUDPClient;
    i : integer;
    rb : TIdBytes;
    s,obb : integer;
    strAn : TStringList;

    anServerName : String;
    anUID : String;
    anType : String;
    anLocation : String;
    aDevice : TGRIDSSDPDevice;

    Function ExtractValue(FromString : String) : String;
    var p : Integer;
    begin
      Result := EmptyStr;
      p := Pos('=',FromString);
      if p>0 then
        Result := trim(Copy(FromString,p,Length(FromString)-p));
    end;

begin
  try
    ServerLog(Self,'BuildSnapShotDataModel : Broadcasting M-SEARCH...');
    aUDPClient := TIdUDPClient.Create(nil);
    aUDPClient.IPVersion := TIdIPVersion.Id_IPv4;

    //Note / should be sent if we had a "real" server listen en 239.255.255.250:1900.
    //But currently, our server listen on other interface on port 1900.
    //This is mainly for 2 reason :
    //- Our SSDP server is just four our usage, it is not open.
    //- We do not want to do the job of the OS.
    aUDPClient.ReceiveTimeout := 1000;
    aUDPClient.Broadcast('M-SEARCH * HTTP/1.1' + #13#10 +
     'HOST: 239.255.255.250:1900' + #13#10 +
     'MAN: "ssdp:discover"'+ #13#10 +
     'MX: 3'+ #13#10 +
      // 'ST: urn:dial-multiscreen-org:service:dial:1'+ #13#10 +
      // 'ST: urn:schemas-udap:service:smartText:1'+ #13#10 +
      // 'ST: udap:all'+ #13#10 +
      // 'ST: urn:all'+ #13#10 +
     'ST: ssdp:all'+ #13#10
     ,1900);

    ServerLog(Self,'BuildSnapShotDataModel : Broadcast M-SEARCH done.');
  finally
    FreeAndNil(aUDPClient);
  end;


  strAn := TStringList.Create;
  try

    while True do
    begin
      setlength(rb,aUDPClient.BufferSize);
      s := aUDPClient.ReceiveBuffer(rb,1000);
      if s = 0  then
      begin
        ServerLog(Self,'BuildSnapShotDataModel : No More data : Finished');
        Break;
      end;
      setlength(rb,s);
      strAn.Add(BytesToString(rb));
      ServerLog(Self,'BuildSnapShotDataModel: After the ssdp:discover (M-SEARCH) we got Data : '+IntToStr(Length(rb))+' byte(s)');
    end;

    ServerLog(Self,'BuildSnapShotDataModel : Analysing data...');

     anServerName := EmptyStr;
     anUID := EmptyStr;
     anType := EmptyStr;
     anLocation := EmptyStr;

    for i := 0 to strAn.Count-1 do
    begin
      if TextStartsWith(CST_SSDP_HTTPOK,strAn[i]) then
      begin
        if (Length(anUID)>0) then
        begin
          //We should have all the info here.
          if anType = CST_SSDP_ROOTDEVICE then
          begin
            if Not(FData.IsDeviceExistsByUID(anUID,obb)) then
            begin
              ServerLog(Self,' BuildSnapShotDataModel : New root device detected : '+anServerName);
              aDevice := TGRIDSSDPDevice.Create(FMasterData);
              aDevice.ServerName := anServerName;
              aDevice.UNS := anUID;
              aDevice.Location := anLocation;
              FData.AddDeviceList(aDevice);
            end
            else
            begin
              ServerLog(Self,' BuildSnapShotDataModel : Root device "'+anServerName+'" exists yet.');
            end;
          end
          else
          begin
            //TODO OTHER TYPE. (SERVICE...)
          end;
        end;


        anServerName := EmptyStr;
        anUID := EmptyStr;
        anType := EmptyStr;
        anLocation := EmptyStr;
      end
      else
      if TextStartsWith(lowercase(strAn[i]),'nt :') then
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

    end;
  ServerLog(Self,'BuildSnapShotDataModel : Done.');
  finally
    FreeAndNil(strAn);
  end;
end;
}


constructor TGRIDService_IndySSDPLikeServer.Create;
begin
  inherited Create;
  FUDPServer := TidUDPServer.Create(Nil);
  FUDPServer.IPVersion := TIdIPVersion.Id_IPv4;
  FUDPServer.BroadcastEnabled := True;
  FUDPServer.DefaultPort := 1900;
  FUDPServer.ThreadedEvent := True;
  FUDPServer.OnUDPRead := OnIdUDPServerUDPRead;

  FUDPClient := TIdUDPClient.Create(Nil);
  FUDPClient.IPVersion := TIdIPVersion.Id_IPv4;

  FGUID := TProtectedString.Create('');
  FLOCALIP := TProtectedString.Create('');
  LOCALPORT := TProtectedString.Create('60000');

  FMasterData := TGRIDSSDPManager.Create;
  FData := TGRIDSSDPDataModel.Create(FMasterData);
end;

destructor TGRIDService_IndySSDPLikeServer.Destroy;
begin
  FreeAndNil(FUDPServer);
  FreeAndNil(FUDPClient);
  FreeAndNil(FGUID);
  FreeAndNil(FLOCALIP);
  FreeAndNil(LOCALPORT);
  FreeAndNil(FMasterData); //FData freed by FMasterData.
  inherited;
end;

procedure TGRIDService_IndySSDPLikeServer.Disable;
begin
  ServerLog(Self,'Local UDP Server desactivating...');
  FUDPServer.Active := False;
  ServerLog(Self,'Local UDP Server desactivated...');
end;

procedure TGRIDService_IndySSDPLikeServer.DoSendNetworkPresence;
var lUDPC : TIdUDPClient;
begin
  lUDPC := TIdUDPClient.Create(nil);
  lUDPC.IPVersion := TIdIPVersion.Id_IPv4;
  Try
    ServerLog(Self,'UDPServer Activity : Broadcasting NOTIFY');
    //2 SSDP-Like SERVER START : PAquet BROADCAST ssdp:alive.
    lUDPC.Broadcast('NOTIFY * HTTP/1.1' + #13#10 +
       'HOST: 239.255.255.250:1900' + #13#10 +
       'LOCATION: grid://'+LocalIpv4+ #13#10 + //Standart is http://
       'SERVER: '+CST_SERVER_PRODUCT_NAME + #13#10 +
       'NTS: ssdp:alive'+ #13#10 +
       'USN: uuid:'+GUID+'::upnp:rootdevice' + #13#10 +
       'CACHE-CONTROL: max-age=1800' + #13#10 +
       'NT: '+CST_SSDP_ROOTDEVICE + #13#10 +
       'Content-Length: 0' + #13#10
       ,1900);
  Finally
    FreeAndNil(lUDPC);
  End;
end;

function TGRIDService_IndySSDPLikeServer.GetDescription: String;
begin
  Result := 'SSDP-Like server for GRID discovering service';
end;

function TGRIDService_IndySSDPLikeServer.GetGUID: String;
begin
  Result := FGUID.Value;
end;

function TGRIDService_IndySSDPLikeServer.GetInstanceType: TopGRIDServerServiceInstanceType;
begin
  Result := gtProcess;
end;

function TGRIDService_IndySSDPLikeServer.GetLocalIP: String;
begin
  Result := FLOCALIP.Value+':'+LOCALPORT.Value;
end;

{$IFDEF FPC}
procedure TGRIDService_IndySSDPLikeServer.OnIdUDPServerUDPRead(
  AThread: TIdUDPListenerThread; AData: TIdBytes;
  ABinding: TIdSocketHandle);
{$ELSE }
procedure TGRIDService_IndySSDPLikeServer.OnIdUDPServerUDPRead(
  AThread: TIdUDPListenerThread; const AData: TIdBytes;
  ABinding: TIdSocketHandle);
{$ENDIF }
var i : integer;
    t : TStringList;
    localUDPC : TIdUDPClient;
begin
  if ABinding.IPVersion = TIdIPVersion.Id_IPv6 then
    Exit; //For instance; respond only on ipv4 interface.

  //UDP READ.
 ServerLog(Self,'UDPServer Activity : '+IntToStr(Length(aData))+' byte(s) incoming from '+ABinding.PeerIP+':'+IntToStr(ABinding.PeerPort));

  t := TStringList.Create;
  try
    t.Text := BytesToString(aData);

    if t.Count>0 then
    begin
      if Length(FLOCALIP.Value)=0 then
      begin
          if t.Count=2 then
          begin
            if (t[0] = CST_SERVER_PRODUCT_NAME) And
               (t[1] = FGUID.Value) then
            begin
              FLOCALIP.Value := ABinding.PeerIP;
              //FLOCALPORT.Value := IntToStr(ABinding.Port); //Cannot use it : it is 1900. We need the real port of GRIDServer.

              DoSendNetworkPresence;
            end;
          end;
      end
      else
      begin
        if TextStartsWith(t[0],'NOTIFY * HTTP') then //If M_NOTIFY : Asking to a M_NOTIFY QUERY.
        begin
          ServerLog(Self,'UDPServer Activity : Got NOTIFY from '+ABinding.PeerIP);

          //Check if it not myself. :)
          //if not in-memory registered yet, do it. (Record localy Name, product etc etc (image !)
          //HERE : RESEND ALL IN MEMORY OBJECT MAP AS JSON STRING.
        end
        else
        if TextStartsWith(t[0],'M-SEARCH * HTTP') then //If M_SEARCH : answer to a M_SEARCH QUERY (ssdp:discover).
        begin
          ServerLog(Self,'UDPServer Activity : Got M-SEARCH from '+ABinding.PeerIP);
          localUDPC := TIdUDPClient.Create(nil);
          try
            ServerLog(Self,'UDPServer Activity : Sending response...');
            for I := 0 to 1 do //2 times. (UDP protocol)
            begin
              localUDPC.Send(ABinding.PeerIP,ABinding.PeerPort,
                  CST_SSDP_HTTPOK + #13#10 +
                  'ST: '+CST_SSDP_ROOTDEVICE+ #13#10 +
                  'EXT:'+ #13#10 +
                  'LOCATION: grid://'+LocalIpv4+ #13#10 +
                  'SERVER: '+CST_SERVER_PRODUCT_NAME + #13#10 +
                   'USN: uuid:'+GUID+'::'+CST_SSDP_ROOTDEVICE+ #13#10 +
                  'CACHE-CONTROL: max-age=1800' + #13#10 +
                  'Content-Length: 0' +
                  #13#10);

              //Here LOOP on GRID functions.
            end;
            ServerLog(Self,'UDPServer Activity : Response sent.');
          finally
            FreeAndNil(localUDPC);
          end;
        end;
      end;
    end;

  finally
    FreeAndNil(t);
  end;
end;




procedure TGRIDService_IndySSDPLikeServer.OnServerMessage_FromCentral(
  Sender: TBus; var Packet: TBusEnvelop);
begin
 //:) Todo / Start; stop; analyse (This event is summon when central service ask for (thus, the user)
end;

end.
