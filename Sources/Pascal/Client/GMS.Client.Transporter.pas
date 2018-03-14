unit GMS.Client.Transporter;

interface

Uses
{$IFDEF FPC}
{$mode Delphi}
SysUtils, Classes, Generics.Collections;
{$ELSE}
  System.Classes, System.SysUtils, System.Generics.Collections;
{$ENDIF}

Type

TGridClientTransporter = Class
Protected
  FLastError : String;
Public
  Constructor Create; Virtual;
  Destructor Destroy; Override;
  Function TransporterDef : String; Virtual; Abstract;
  Function Connect : Boolean; Virtual; Abstract;
  Function Connected : Boolean; Virtual; Abstract;
  Function ReadStream(var aStream : TMemoryStream) : Boolean; Virtual; Abstract;
  Function WriteStream(aStream : TMemoryStream) : Boolean; Virtual; Abstract;
  Function Close : Boolean; Virtual; Abstract;
  Function LastError : String; Virtual;
End;

TGridClientTransporterNetwork = Class(TGridClientTransporter)
private
Protected
  FPort: Word;
  FIP: String;
Public
  Constructor Create; Override;
  Property RemoteIP : String read FIP Write FIP;
  Property RemotePort : Word read FPort Write FPort;
End;



TGridClientTransporterClass = Class of TGridClientTransporter;

TGridClientTransporterFactory = Class
  Class Function GetDefaultTransporter : TGridClientTransporter;
End;

TGridClientTransporterImplementation = TList<TGridClientTransporterClass>;

Var
  GlobalClientTransporterImplementationClass : TGridClientTransporterImplementation;
implementation

{ TGridClientTransporterFactory }

class function TGridClientTransporterFactory.GetDefaultTransporter: TGridClientTransporter;
begin
  Result := Nil;

  if GlobalClientTransporterImplementationClass.Count>0 then
  begin
    Result := GlobalClientTransporterImplementationClass[0].Create;
  end;

  if Not(aSsigned(Result)) then
    raise Exception.Create('GridClientTransporterFactory.GetDefaultTransporter : No default transporter found : Please add uses clauses.');
end;

{ TGridClientTransporter }

constructor TGridClientTransporter.Create;
begin
  //Empty. Just here for Delphi inheritance system.
end;

destructor TGridClientTransporter.Destroy;
begin
  //Empty. Just here for Delphi inheritance system.
  inherited;
end;

function TGridClientTransporter.LastError: String;
begin
  Result := FLastError;
end;

{ TGRidClientTransporterNetwork }

constructor TGRidClientTransporterNetwork.Create;
begin
  inherited;
  FIP := '127.0.0.1';
  FPort := 60000;
end;

Initialization

  GlobalClientTransporterImplementationClass := TGridClientTransporterImplementation.Create;

Finalization

  FreeAndNil(GlobalClientTransporterImplementationClass);
end.
