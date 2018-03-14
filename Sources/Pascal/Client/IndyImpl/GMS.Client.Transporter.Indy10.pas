unit GMS.Client.Transporter.Indy10;

interface

Uses
  System.Classes, System.SysUtils,
  GMS.Client.Transporter, idTCPClient, idUDPClient, IdGlobal;

Type

TGridClientTransporterIndy10TCP = Class(TGridClientTransporterNetwork)
Private
  FClient : TIdTCPClient;
Public
  Constructor Create; Override;
  Destructor Destroy; Override;
  Function TransporterDef : String; Override;
  Function Connect : Boolean; Override;
  Function Connected : Boolean; Override;
  Function ReadStream(var aStream : TMemoryStream) : Boolean; Override;
  Function WriteStream(aStream : TMemoryStream) : Boolean; Override;
  Function Close : Boolean; Override;
End;

TGridClientTransporterIndy10UDPSendMode = (udpsmUnicast, udpsmBroadcast);
TGridClientTransporterIndy10UDP = Class(TGridClientTransporterNetwork)
Private
  FClient : TIdUDPClient;
  FSendMode: TGridClientTransporterIndy10UDPSendMode;
  function GetReceiveTimeout: Integer;
  procedure SetReceiveTimeout(const Value: Integer);
Public
  Constructor Create; Override;
  Destructor Destroy; Override;
  Function TransporterDef : String; Override;
  Function Connect : Boolean; Override;
  Function Connected : Boolean; Override;
  Function ReadStream(var aStream : TMemoryStream) : Boolean; Override;
  Function ReadAsString : String;
  Function WriteStream(aStream : TMemoryStream) : Boolean; Override;
  Procedure WriteAsString(aStr : String);
  Function Close : Boolean; Override;

  Property ReceiveTimeOut : Integer read GetReceiveTimeout Write SetReceiveTimeout;
  Property SendMode : TGridClientTransporterIndy10UDPSendMode read FSendMode Write FSendMode;

End;


implementation

{ TGridClientTransporterIndy10TCP }

function TGridClientTransporterIndy10TCP.Close: Boolean;
begin
  Result := True;
  FClient.Disconnect;
end;

function TGridClientTransporterIndy10TCP.Connect: Boolean;
begin
  Result := False;
  try
    FClient.Host := FIP;
    FClient.Port := FPort;
    FClient.Connect;
    Result := True;
  Except
    On E : Exception do
    begin
      FLastError := e.Message;
    end;
  end;
end;

function TGridClientTransporterIndy10TCP.Connected: Boolean;
begin
  Result := FClient.Connected;
end;

constructor TGridClientTransporterIndy10TCP.Create;
begin
  inherited;
  FClient := TIdTCPClient.Create(nil);
end;

destructor TGridClientTransporterIndy10TCP.Destroy;
begin
  FreeAndNil(FClient);
  inherited;
end;

function TGridClientTransporterIndy10TCP.ReadStream(
  var aStream: TMemoryStream): Boolean;
begin
  FClient.IOHandler.ReadStream(aStream);
  Result := true;
end;

function TGridClientTransporterIndy10TCP.TransporterDef: String;
begin
  Result := 'TCP Client - Indy10 Implementation';
end;

function TGridClientTransporterIndy10TCP.WriteStream(
  aStream: TMemoryStream): Boolean;
begin
  aStream.Position := 0;

  FClient.IOHandler.Write(aStream, aStream.Size,True);
  Result := true;
end;


{ TGridClientTransporterIndy10UDP }

function TGridClientTransporterIndy10UDP.Close: Boolean;
begin
  result := true;
  FClient.Disconnect;
end;

function TGridClientTransporterIndy10UDP.Connect: Boolean;
begin
  FClient.Host := FIP;
  FClient.Port := FPort;
  try
    FClient.Connect;
  Except
  end;
  result := Connected;
end;

function TGridClientTransporterIndy10UDP.Connected: Boolean;
begin
  Result := FClient.Connected;
end;

constructor TGridClientTransporterIndy10UDP.Create;
begin
  inherited;
  FClient := TIdUDPClient.Create(nil);
  FClient.Host := FIP;
  FClient.Port := FPort;
end;

destructor TGridClientTransporterIndy10UDP.Destroy;
begin
  FreeAndnil(FClient);
  inherited;
end;

function TGridClientTransporterIndy10UDP.GetReceiveTimeout: Integer;
begin
  Result := FClient.ReceiveTimeout;
end;

function TGridClientTransporterIndy10UDP.ReadAsString: String;
var rb : TidBytes;
    s : Integer;
begin
  Result := EmptyStr;

  while True do
  begin
    setlength(rb,FClient.BufferSize);
    s := FClient.ReceiveBuffer(rb,1000);
    if s = 0  then
    begin
      //Log('BuildSnapShotDataModel : No More data : Finished');
      Break;
    end;
    setlength(rb,s);
    Result := Result + BytesToString(rb);
  end;
end;

function TGridClientTransporterIndy10UDP.ReadStream(
  var aStream: TMemoryStream): Boolean;
var rb : TidBytes;
    s : Integer;
begin
  Assert(Assigned(aStream));
  while True do
  begin
    setlength(rb,FClient.BufferSize);
    s := FClient.ReceiveBuffer(rb,1000);
    if s = 0  then
    begin
      //Log('BuildSnapShotDataModel : No More data : Finished');
      Break;
    end;
    setlength(rb,s);
    //strAn.Add(BytesToString(rb));
  end;
  astream.Write(rb,LEngth(rb));
  Result := true;
end;

procedure TGridClientTransporterIndy10UDP.SetReceiveTimeout(
  const Value: Integer);
begin
  FClient.ReceiveTimeout := Value;
end;

function TGridClientTransporterIndy10UDP.TransporterDef: String;
begin
  Result := 'UDP Client - Indy10 Implementation';
end;

procedure TGridClientTransporterIndy10UDP.WriteAsString(aStr: String);
begin
  case FSendMode of
    udpsmUnicast: FClient.Send(aStr);
    udpsmBroadcast:
    begin
      FClient.BroadcastEnabled := True;
      FClient.Broadcast(aStr,FPort);
    end;
  end;
end;

function TGridClientTransporterIndy10UDP.WriteStream(
  aStream: TMemoryStream): Boolean;
var a : TIdBytes;
begin
  Result := true;
  SetLength(a,aStream.Size);
  aStream.Position := 0;
  aStream.Read(a,length(a));
  FClient.SendBuffer(a);
end;

Initialization

  GlobalClientTransporterImplementationClass.Add(TGridClientTransporterIndy10TCP);

end.
