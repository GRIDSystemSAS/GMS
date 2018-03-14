unit GMS.Client.SSDPLikeServer.DataModel;

interface

Uses SysUtils, Classes,
     GMS.Client.SSDPLikeServer.DataModel.BO;

Type

TGRIDSSDPServerSideService = Class(TSOSSDPServerService)

End;

TGRIDSSDPServerSideDevice = Class(TSOSSDPServerDevice)

End;

TGRIDSSDPServerSideDataModel = class(TSOSSDPServerSideDatamodel)
Public
  function IsDeviceExistsByUID(Const aUSN : String; var Index: Integer): Boolean;
  function IsDeviceExistsByServerName(Const aServerName : String; var Index: Integer): Boolean;
End;


TGRIDSSDPServerSideManager = Class(TSOGRIDSSDPLikeServerSideDataModelManager)
Public
  constructor Create; Override;
End;


implementation

{ TGRIDSSDPManager }

constructor TGRIDSSDPServerSideManager.Create;
begin
  Inherited Create;
  RegisterClass;
  StoredObject.RegisterOutsideClass('TGRIDSSDPServerSideService','TSOSSDPServerService');
  StoredObject.RegisterOutsideClass('TGRIDSSDPServerSideDevice','TSOSSDPServerDevice');
  StoredObject.RegisterOutsideClass('TGRIDSSDPServerSideDataModel','TSOSSDPServerSideDatamodel');
end;

{ TGRIDSSDPDataModel }

function TGRIDSSDPServerSideDataModel.IsDeviceExistsByServerName(
  const aServerName: String; var Index: Integer): Boolean;
var a : TGRIDSSDPServerSideDevice;
    i : Integer;
begin
  result := False;
  for i := 0 to DeviceListCount-1 do
  begin
    a := TGRIDSSDPServerSideDevice(DeviceList[i]);
    if a.ServerName = aServerName then
    begin
      Result := True;
      Index := i;
      Break;
    end;
  end;
end;


function TGRIDSSDPServerSideDataModel.IsDeviceExistsByUID(const aUSN: String;
  var Index: Integer): Boolean;
var a : TGRIDSSDPServerSideDevice;
    i : Integer;
begin
  result := False;
  for i := 0 to DeviceListCount-1 do
  begin
    a := TGRIDSSDPServerSideDevice(DeviceList[i]);
    if a.UNS = aUSN then
    begin
      Result := True;
      Index := i;
      Break;
    end;
  end;
end;

end.
