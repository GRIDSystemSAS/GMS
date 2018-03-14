unit GMS.Server.Services.SSDPLikeServer.DataModel;

interface

Uses SysUtils, Classes,
    GMS.Server.Services.SSDPLikeServer.DataModel.BO;

Type

TGRIDSSDPService = Class(TSOSSDPService)

End;

TGRIDSSDPDevice = Class(TSOSSDPDevice)

End;

TGRIDSSDPDataModel = Class(TSOSSDPDatamodel)
Public
  function IsDeviceExistsByUID(Const aUSN : String; var Index: Integer): Boolean;
  function IsDeviceExistsByServerName(Const aServerName : String; var Index: Integer): Boolean;
End;


TGRIDSSDPManager = Class(TSOGRIDSSDPLikeDataModelManager)
Public
  constructor Create; Override;
End;


implementation

{ TGRIDSSDPManager }

constructor TGRIDSSDPManager.Create;
begin
  Inherited Create;
  RegisterClass;
  StoredObject.RegisterOutsideClass('TGRIDSSDPService','TSOSSDPService');
  StoredObject.RegisterOutsideClass('TGRIDSSDPDevice','TSOSSDPDevice');
  StoredObject.RegisterOutsideClass('TGRIDSSDPDataModel','TSOSSDPDatamodel');
end;

{ TGRIDSSDPDataModel }

function TGRIDSSDPDataModel.IsDeviceExistsByServerName(
  const aServerName: String; var Index: Integer): Boolean;
var a : TGRIDSSDPDevice;
    i : Integer;
begin
  result := False;
  for i := 0 to DeviceListCount-1 do
  begin
    a := TGRIDSSDPDevice(DeviceList[i]);
    if a.ServerName = aServerName then
    begin
      Result := True;
      Index := i;
      Break;
    end;
  end;
end;


function TGRIDSSDPDataModel.IsDeviceExistsByUID(const aUSN: String;
  var Index: Integer): Boolean;
var a : TGRIDSSDPDevice;
    i : Integer;
begin
  result := False;
  for i := 0 to DeviceListCount-1 do
  begin
    a := TGRIDSSDPDevice(DeviceList[i]);
    if a.UNS = aUSN then
    begin
      Result := True;
      Index := i;
      Break;
    end;
  end;
end;

end.
