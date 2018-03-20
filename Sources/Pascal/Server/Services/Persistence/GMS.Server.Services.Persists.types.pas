unit GMS.Server.Services.Persists.types;

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
  GS.Threads;

Type
//This is the class which describtes a database access.
TGRIDServerPersistenceDataBaseDriver = Class;
//This is the class where real access and users management must be implemented.
TGRIDServerPersistenceDataBaseInstance = Class;

TGRIDServerPersistenceDataBaseDriver = Class
private
Protected
  Function GetDriverName : String; Virtual; Abstract;
  Function GetVersion : String; Virtual; Abstract;
Public
  Property DriverName : string read GetDriverName;
  Property Version : String read GetVersion;

  Function GetNewInstance(aDatabaseName : string) : TGRIDServerPersistenceDataBaseInstance; Virtual; Abstract;
End;

TGRIDServerPersistenceDataBaseInstance = Class
Private
Protected
  FAccess : TCriticalSection;
  FDatabaseName : TProtectedValue<String>;
  FDriver: TGRIDServerPersistenceDataBaseDriver;
  function GetDatabaseName: String; Virtual;
  function GetMultiInstance: Boolean; Virtual;
Public
  //Call those 2 before use shared stuff inside this object.
  Procedure Lock;
  Procedure Unlock;

  Constructor Create(aDatabaseName : String; aDriver : TGRIDServerPersistenceDataBaseDriver); Reintroduce; Virtual;
  Destructor Destroy; Override;

  //All work on db pass throught this methods. (In/out langage : GRID.ClientServer.Protocol.DBO)

  function DataBaseProcessSave(var aIn : TMemoryStream; Out aOut : TMemoryStream; Out ErrorDesc : String) : Boolean; Virtual; Abstract;
  function DataBaseProcessLoad(var aIn : TMemoryStream; Out aOut : TMemoryStream; Out ErrorDesc : String) : Boolean; Virtual; Abstract;
  function CheckCredential(UserName,Password : string): Boolean; Virtual;


  Property DatabaseName : String read GetDatabaseName;

  //Data depending from drivers, if many client ask for the same DBName, an instance of this class could be build for each client.
  //As a result, all this instances will work stricly in a paralel manner (i.e. many ctx on the same database backend).
  //->True by default
  //->Put this to "false" to share this instance between client. All client queries will be sequenced. (!perf!)
  Property AllowMultiInstance : Boolean read GetMultiInstance;
  Property Driver : TGRIDServerPersistenceDataBaseDriver read FDriver;
End;


TGRIDServerPersistenceFactory = Class
Public
  Class var Drivers : TObjectList<TGRIDServerPersistenceDataBaseDriver>;

  Class Function GetAvailableDriver : TStringList;
  Class Function GetDriver( Const aDriverName : String;
                            Out aDriver : TGRIDServerPersistenceDataBaseDriver;
                            Const aDriverVersion : String = '1.0') : Boolean;
  Class Function CreateInstance(aDriverName : string) : TGRIDServerPersistenceDataBaseInstance;
End;

implementation

{ TGRIDServerPersistenceFactory }

class function TGRIDServerPersistenceFactory.CreateInstance(
  aDriverName: string): TGRIDServerPersistenceDataBaseInstance;
begin

end;


class function TGRIDServerPersistenceFactory.GetAvailableDriver: TStringList;
var i : integer;
begin
  Result := TStringList.Create;
  for I := 0 to Drivers.Count-1 do
  begin
    Result.Add(Drivers[i].DriverName)
  end;
end;


class function TGRIDServerPersistenceFactory.GetDriver(Const aDriverName: String;
  out aDriver: TGRIDServerPersistenceDataBaseDriver;
  const aDriverVersion: String): Boolean;
var i : integer;
begin
  Result := False;
  for I := 0 to Drivers.Count-1 do
  begin
    if (aDriverName = Drivers[i].DriverName) And (aDriverVersion = Drivers[i].Version) then
    begin
      aDriver := Drivers[i];
      Result := True;
    end;
  end;
end;

{ TGRIDServerPersistenceDataBaseInstance }

function TGRIDServerPersistenceDataBaseInstance.CheckCredential(UserName,
  Password: string): Boolean;
begin
  Result := true;
end;

constructor TGRIDServerPersistenceDataBaseInstance.Create(aDatabaseName : String;
                                                    aDriver : TGRIDServerPersistenceDataBaseDriver);
begin
  Inherited Create;
  Assert(Length(aDatabaseName)>0);
  Assert(Assigned(aDriver));
  FAccess := TCriticalSection.Create;
  FDatabaseName := TProtectedValue<String>.Create(aDatabaseName);
  FDriver := aDriver;
end;

destructor TGRIDServerPersistenceDataBaseInstance.Destroy;
begin
  FreeAndNil(FAccess);
  FreeAndNil(FDatabaseName);
  inherited;
end;

function TGRIDServerPersistenceDataBaseInstance.GetDatabaseName: String;
begin
  result := FDatabaseName.Value;
end;

function TGRIDServerPersistenceDataBaseInstance.GetMultiInstance: Boolean;
begin
  result := True; //By default.
end;

procedure TGRIDServerPersistenceDataBaseInstance.Lock;
begin
  FAccess.Acquire;
end;

procedure TGRIDServerPersistenceDataBaseInstance.Unlock;
begin
  FAccess.Release;
end;

Initialization

TGRIDServerPersistenceFactory.Drivers := TObjectList<TGRIDServerPersistenceDataBaseDriver>.Create;

Finalization

TGRIDServerPersistenceFactory.Drivers.Clear;
FreeAndNil(TGRIDServerPersistenceFactory.Drivers);

end.
