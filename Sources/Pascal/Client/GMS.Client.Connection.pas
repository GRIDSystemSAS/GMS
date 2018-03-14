unit GMS.Client.Connection;

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
     GMS.Client.Transporter;
Type

//TGridClientFinder = Class
//Private
//  FCTXFinderInstance : TGridClientSSDPLikeResolver;
//Public
//End;

TGridClientConnection = Class
Private
  FCTXInstance : TGridClientTransporter;
  FLastError : String;

  function GetActive: Boolean;
  function GetTransporter: TGridClientTransporterNetwork;

Public
  Constructor Create(aTransporter : TGridClientTransporter); Virtual;
  Destructor Destroy; Override;

  Function  Disconnect : Boolean;
  Function LastError : String;

  Property Active : Boolean read GetActive;
  Property Transporter : TGridClientTransporterNetwork read GetTransporter;
End;

TGridClientConnectionFactory = Class
  Class Function CreateConnection : TGridClientConnection;
end;

implementation

{ TGridClientConnection }

constructor TGridClientConnection.Create(aTransporter: TGridClientTransporter);
begin
  Assert(Assigned(aTransporter));
  FCTXInstance := aTransporter;
  FLastError := EmptyStr;
end;

destructor TGridClientConnection.Destroy;
begin
  FCTXInstance.Close;
  FreeAndNil(FCTXInstance);
  inherited;
end;

Function TGridClientConnection.Disconnect : Boolean;
begin
  Result := true;
  if Active then
  begin
    Result := FCTXInstance.Close;
  end;
end;

function TGridClientConnection.GetActive: Boolean;
begin
  Result := FCTXInstance.Connected;
end;

function TGridClientConnection.GetTransporter: TGridClientTransporterNetwork;
begin
  Result := nil;
  if FCTXInstance is TGridClientTransporterNetwork then
    Result :=  TGridClientTransporterNetwork(FCTXInstance);

  if Not Assigned(Result) then
    raise Exception.Create('TGridClientConnection.GetTransporter : Transporter is not a NetWork transporter.');
end;

function TGridClientConnection.LastError: String;
begin
  Result := FLastError;
end;


{ TGridClientConnectionFactory }

class function TGridClientConnectionFactory.CreateConnection: TGridClientConnection;
begin
  Result := TGridClientConnection.Create(TGridClientTransporterFactory.GetDefaultTransporter)
end;



end.

