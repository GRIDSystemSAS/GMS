unit gmsclh;

interface

Uses SysUtils, Classes;

{$IFDEF LINUX OR UNIX}
  Const cst_gmscl_libName = 'gmscl.so';
{$ELSE}
  {$IFDEF WIN32 or WIN64}
  Const cst_gmscl_libName = 'gmscl.dll';
  {$ENDIF}
{$ENDIF}

function hello(name, buffer: PWideChar; buflen: Integer): Integer; cdecl; external cst_gmscl_libName;
function GMSResolve: Integer; cdecl; external cst_gmscl_libName;
function GMSResolve_ServerName(Index : Integer; ServerName : PWideChar; buflen : Integer) : integer; cdecl; external cst_gmscl_libName;

/// 0: GridServerIP;
/// 1: GridServerPort;
/// 2: GridServerName;
/// 3: GridServerDesc;
/// 4: GridServerHost;
function GMSResolve_ServerInfo(Index : Integer; ServerInfoCode : Integer; ServerInfo : PWideChar; buflen : Integer) : integer; cdecl; external cst_gmscl_libName;


function MemStreamCreate : UInt64; cdecl; external cst_gmscl_libName;
function MemStreamRelease(aStreamHandle : UInt64) : Integer; cdecl; external cst_gmscl_libName;

function MemStreamSize(aHandle : UInt64) : UInt64; cdecl; external cst_gmscl_libName;
function MemStreamAddInteger(aHandle : UInt64; aValue : Integer) : Integer; cdecl; external cst_gmscl_libName;
function MemStreamAddString(aHandle : UInt64; aValue : PWideChar) : Integer; cdecl; external cst_gmscl_libName;
function MemStremAddDouble(aHandle : UInt64; aValue : Double) : Integer; cdecl; external cst_gmscl_libName;

function GMSConnection(IP : PWideChar; Port : UInt32; User : PWideChar; PassWord : PWideChar) : Integer; cdecl; external cst_gmscl_libName;
function GMSConnection_Auto(User : PWideChar; PassWord : PWideChar) : Integer; cdecl; external cst_gmscl_libName;
function GMSConnection_Release(aHandle : Integer) : Integer; cdecl; external cst_gmscl_libName;

function GMSSendMessageString(aConnHandle : Integer; aChannel, aMessage : PWideChar) : Integer; cdecl; external cst_gmscl_libName;
function GMSSendMessage(aConnHandle : Integer; aChannel : PWideChar; aMessage : PByteArray; AMessageLength : Uint64) : Integer; cdecl; external cst_gmscl_libName;

function GMSSubscribt(aConnHandle : Integer; aChannel : PWideChar) : Integer; cdecl; external cst_gmscl_libName;
function GMSUnSubscript(aConnHandle : Integer; aChannel : PWideChar) : Integer; cdecl; external cst_gmscl_libName;
function GMSReceiveMessage(aConnHandle : Integer; aTimeOutInMS : UINT64) : integer; cdecl; external cst_gmscl_libName;
function GMSMessageBoxCount(aConnHandle : Integer) : Integer; cdecl; external cst_gmscl_libName;

///  0 Id
///  1 Channel name.
function GMSMessageBox_MessageInfo(aConnHandle : Integer; aMessageIndex : Integer; aInfoCode : Integer; aInfo : PWideChar; aInfoLen : Integer) : Integer; cdecl; external cst_gmscl_libName;
// TODO ! function GMSMessageBox_MessageStream(aConnHandle : Integer; aMessageIndex : Integer; aMessageStream : PByteArray; aMessageStreamLen : Integer) : Integer; cdecl;



implementation

end.
