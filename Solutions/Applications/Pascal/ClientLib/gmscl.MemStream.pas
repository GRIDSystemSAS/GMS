unit gmscl.MemStream;

interface

Uses SysUtils, Classes, Generics.Collections, GS.Stream, gmsclConst;


Type
TgmsCLStreamManagement = class(TDictionary<Integer,TMemoryStream>)
Protected
Public
  //Todo : Put all of this thread-safe.
  Function AddMemStream : UInt64;
  Function GetMemStream(Index : UInt64) : TMemoryStream;
  Function DelMemStream(Index : UInt64) : Boolean;
end;

Var
  GlbMemStreamManager : TgmsCLStreamManagement;

//dll accessible
function MemStreamCreate : UInt64; cdecl;
function MemStreamRelease(aStreamHandle : UInt64) : Integer; cdecl;

function MemStreamSize(aHandle : UInt64) : UInt64; cdecl;
function MemStreamAddInteger(aHandle : UInt64; aValue : Integer) : Integer; cdecl;
function MemStreamAddString(aHandle : UInt64; aValue : PWideChar) : Integer; cdecl;
function MemStremAddDouble(aHandle : UInt64; aValue : Double) : Integer; cdecl;


implementation

function MemStreamCreate : UInt64;
begin
  Result := GlbMemStreamManager.AddMemStream;
end;

function MemStreamRelease(aStreamHandle : UInt64) : Integer;
var ls : TMemoryStream;
begin
  Result := CST_ERROR;
  if GlbMemStreamManager.DelMemStream(aStreamHandle) then
  begin
   Result := CST_ALL_FINE;
  end;
end;

function MemStreamSize(aHandle : UInt64) : UInt64;
var ls : TMemoryStream;
begin
  Result := 0;
  ls := GlbMemStreamManager.GetMemStream(aHandle);
  if Assigned(ls) then
  begin
    Result := ls.Size;
  end;
end;

function MemStreamAddInteger(aHandle : UInt64; aValue : Integer) : Integer;
var ls : TMemoryStream;
begin
  Result := CST_ERROR;
  ls := GlbMemStreamManager.GetMemStream(aHandle);
  if Assigned(ls) then
  begin
    WriteInteger(ls,aValue);
    Result := CST_ALL_FINE;
  end;
end;

function MemStreamAddString(aHandle : UInt64; aValue : PWideChar) : Integer;
var
  ls : TMemoryStream;
begin
  ls := GlbMemStreamManager.GetMemStream(aHandle);
  if Assigned(ls) then
  begin
    WriteString(ls,UnicodeString(aValue));
    Result := CST_ALL_FINE;
  end;
end;

function MemStremAddDouble(aHandle : UInt64; aValue : Double) : Integer;
var ls : TMemoryStream;
begin
  Result := CST_ERROR;
  ls := GlbMemStreamManager.GetMemStream(aHandle);
  if Assigned(ls) then
  begin
    WriteDouble(ls,aValue);
    Result := CST_ALL_FINE;
  end;
end;



{ TgmsCLStreamManagement }

function TgmsCLStreamManagement.AddMemStream: UInt64;
begin
  Result := Count+1;
  Add(Result,TMemoryStream.Create);
end;

function TgmsCLStreamManagement.DelMemStream(Index: UInt64): Boolean;
var ls : TMemoryStream;
begin
  Result := False;
  ls := GetMemStream(Index);
  if Assigned(ls) then
  begin
    FreeandNil(ls);
  end;
  Remove(Index);
  Result := True;
end;

function TgmsCLStreamManagement.GetMemStream(Index : UInt64) : TMemoryStream;
var lS : TMemoryStream;
begin
  Result := Nil;
  if TryGetValue(Index,lS) then
  begin
    Result := lS;
  end;
end;

Initialization

GlbMemStreamManager := TgmsCLStreamManagement.Create;

Finalization

//FreeAndNil(GlbMemStreamManager);

end.
