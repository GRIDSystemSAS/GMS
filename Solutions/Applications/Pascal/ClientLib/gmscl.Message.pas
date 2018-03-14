unit gmscl.Message;

interface

uses SysUtils,
     Classes,
     GS.Stream,
     gmsclConst,
     GMS.Client.Connection,
     GMS.Client,
     GMS.Client.Resolver,
     GMS.Client.SSDPLikeServer.DataModel.BO,
     GMS.Client.SSDPLikeServer.DataModel,
     GMS.Client.Transporter,
     GMS.Client.Transporter.Indy10,
     GMS.ClientServer.Types.ChannelDefinitions,
     GMS.ClientServer.Protocol.Helper,
     GMS.ClientServer.Protocol.MessageService.BO,
     GMS.ClientServer.Protocol.RPC.BO,
     GMS.ClientServer.Protocol.Transport.BO,
     GMS.ClientServer.DataModel.Central.BO,
     GMS.ClientServer.Protocol.Central.BO;


function GMSSendMessage(aConnHandle : Integer; aChannel : PWideChar; aMessage : PByteArray; AMessageLength : Uint64) : Integer; cdecl;
function GMSSendMessageString(aConnHandle : Integer; aChannel, aMessage : PWideChar) : Integer; cdecl;
function GMSSubscribt(aConnHandle : Integer; aChannel : PWideChar) : Integer; cdecl;
function GMSUnSubscript(aConnHandle : Integer; aChannel : PWideChar) : Integer; cdecl;
function GMSReceiveMessage(aConnHandle : Integer; aTimeOutInMS : UINT64) : integer; cdecl;
function GMSMessageBoxCount(aConnHandle : Integer) : Integer; cdecl;
///  0 Id
///  1 Channel name.
function GMSMessageBox_MessageInfo(aConnHandle : Integer; aMessageIndex : Integer; aInfoCode : Integer; aInfo : PWideChar; aInfoLen : Integer) : Integer; cdecl;
// TODO ! function GMSMessageBox_MessageStream(aConnHandle : Integer; aMessageIndex : Integer; aMessageStream : PByteArray; aMessageStreamLen : Integer) : Integer; cdecl;


implementation

Uses gmscl.Connection;

function GMSSendMessage(aConnHandle : Integer; aChannel : PWideChar; aMessage : PByteArray; AMessageLength : Uint64) : Integer; cdecl;
var lac : TGridClient;
    ls : TMemoryStream;
    lp : PByte;
begin
  Result := CST_ERROR;
  lac := glbConnectionManagement.GetConnection(aConnHandle);
  if Assigned(lac) then
  begin
    ls := TMemoryStream.Create;
    try
      ls.SetSize(AMessageLength);
      lp := ls.Memory;
      Move(aMessage, lp^, AMessageLength);
      ls.Position := 0;
      lac.SendMsg(UnicodeString(aChannel),[ls]);
      Result := CST_ALL_FINE;
    finally
      FreeAndNil(ls);
    end;
  end;
end;

function GMSSendMessageString(aConnHandle : Integer; aChannel, aMessage : PWideChar) : Integer;
var lac : TGridClient;
    ls : TMemoryStream;
begin
  Result := CST_ERROR;
  lac := glbConnectionManagement.GetConnection(aConnHandle);
  if Assigned(lac) then
  begin
    ls := TMemoryStream.Create;
    try
      WriteString(ls,UnicodeString(aMessage));
      ls.Position := 0;
      lac.SendMsg(UnicodeString(aChannel),[ls]);
      Result := CST_ALL_FINE;
    finally
      FreeAndNil(ls);
    end;
  end;
end;

function GMSSubscribt(aConnHandle : Integer; aChannel : PWideChar) : Integer;
var lac : TGridClient;
begin
  Result := CST_ERROR;
  lac := glbConnectionManagement.GetConnection(aConnHandle);
  if Assigned(lac) then
  begin
    if lac.Subscription(UnicodeString(aChannel)) then
    begin
      Result := CST_ALL_FINE;
    end;
  end;
end;

function GMSUnSubscript(aConnHandle : Integer; aChannel : PWideChar) : Integer;
var lac : TGridClient;
begin
  Result := CST_ERROR;
  lac := glbConnectionManagement.GetConnection(aConnHandle);
  if Assigned(lac) then
  begin
    if lac.Unsubscription(UnicodeString(aChannel)) then
    begin
      Result := CST_ALL_FINE;
    end;
  end;
end;

function GMSReceiveMessage(aConnHandle : Integer; aTimeOutInMS : UINT64) : integer;
var lac : TGridClient;
    lmb : TGridClientMessagesBox;
begin
  Result := CST_ERROR;
  lac := glbConnectionManagement.GetConnection(aConnHandle);
  if Assigned(lac) then
  begin
    lmb := glbConnectionMailBoxes.GetMessagesBox(aConnHandle);
    if lmb.Count>0 then
    begin
      lmb.Clear; //Clear message. They are supposed to be read and process before the receive call.
    end;
    lac.RecvMsg(lmb);
    Result := lmb.Count;
  end;
end;

function GMSMessageBoxCount(aConnHandle : Integer) : Integer;
var lm : TGridClientMessagesBox;
begin
  result := 0;
  lm := glbConnectionMailBoxes.GetMessagesBox(aConnHandle);
  if Assigned(lm) then
  begin
    Result := lm.Count;
  end;
end;

function GMSMessageBox_MessageInfo(aConnHandle : Integer; aMessageIndex : Integer; aInfoCode : Integer; aInfo : PWideChar; aInfoLen : Integer) : Integer; cdecl;
var
  rs: UnicodeString;
  lMes : TGridClientMessageRecept;
  lm : TGridClientMessagesBox;
begin
  lm := glbConnectionMailBoxes.GetMessagesBox(aConnHandle);
  if Assigned(lm) then
  begin
    if (aMessageIndex>-1) And (aMessageIndex<lm.Count) then
    begin
      lMes := lm.Items[aMessageIndex];
      Case aInfoCode of
        0: //id
        begin
          rs := lmes.MessageID;
        end;
        1: //Channel
        begin
          rs := lmes.MessageChannel;
        end;
      end;
    end;
  end;

  if aInfo = nil then
  begin
    Result := Length(rs) + 1;
  end
  else
  begin
    Result := aInfoLen;
    if Length(rs)<Result then
      Result := Length(rs);
    Move(rs[1], aInfo^, Result * SizeOf(WideChar));
  end;
end;

end.
