#pragma once


typedef int(*_FuncGMSResolve)();
typedef int(*_FuncHello)(wchar_t* name, wchar_t* buffer, int buflen);
typedef int(*_FuncGMSResolve_ServerInfo)(int Index, int ServerCodeInfo, wchar_t* buffer, int buflen);

typedef int(*_FuncGMSResolve_ServerInfo)(int Index, int ServerCodeInfo, wchar_t* buffer, int buflen);

//function GMSConnection_Auto(User : PWideChar; PassWord: PWideChar) : Integer; cdecl;
typedef int(*_FuncGMSConnection_Auto)(wchar_t* User, wchar_t* Password);

//function SendMessageString(aConnHandle : Integer; aChannel, aMessage : PWideChar) : Integer; cdecl;
typedef int(*_FuncGMSSendMessageString)(int aConnHandle, wchar_t* aChannel, wchar_t* aMessage);
//function SendMessage(aConnHandle : Integer; aChannel: PWideChar; aMessage: PByteArray; AMessageLength: Uint64) : Integer; cdecl;
typedef int(*_FuncGMSSendMessage)(int aConnHandle, wchar_t* aChannel, byte* aMessage, unsigned __int64 AMessageLength);


//function GMSSubscribt(aConnHandle : Integer; aChannel: PWideChar) : Integer; cdecl; external cst_gmscl_libName;
typedef int(*_FuncGMSSubscribt)(int aConnHandle, wchar_t* aChannel); 

//function GMSUnSubscript(aConnHandle : Integer; aChannel: PWideChar) : Integer; cdecl; external cst_gmscl_libName;
typedef int(*_FuncGMSUnSubscribt)(int aConnHandle, wchar_t* aChannel);

//function GMSReceiveMessage(aConnHandle : Integer; aTimeOutInMS: UINT64) : integer; cdecl; external cst_gmscl_libName;
typedef int(*_FuncGMSReceiveMessage)(int aConnHandle, unsigned __int64 aTimeOutInMS);

//function GMSMessageBoxCount(aConnHandle : Integer) : Integer; cdecl; external cst_gmscl_libName;
typedef int(*_FuncGMSMessageBoxCount)(int aConnHandle);

///  0 Id
///  1 Channel name.
//function GMSMessageBox_MessageInfo(aConnHandle : Integer; aMessageIndex: Integer; aInfoCode: Integer; aInfo: PWideChar; aInfoLen: Integer) : Integer; cdecl; external cst_gmscl_libName;
typedef int(*_FuncGMSMessageBox_MessageInfo)(int aConnHandle, int aMessageIndex, int aInfoCode, wchar_t* aInfo, int aInfoLen);



_FuncGMSResolve GMSResolve;
_FuncGMSResolve_ServerInfo GMSResolve_ServerInfo;
_FuncHello Hello;
_FuncGMSConnection_Auto GMSConnection_Auto;
_FuncGMSSendMessageString GMSSendMessageString;
_FuncGMSSendMessage GMSSendMessage;
_FuncGMSSubscribt GMSSubscribt;
_FuncGMSUnSubscribt GMSUnSubscript;
_FuncGMSReceiveMessage GMSReceiveMessage;
_FuncGMSMessageBoxCount GMSMessageBoxCount;
_FuncGMSMessageBox_MessageInfo GMSMessageBox_MessageInfo;

bool InitLibrary(void)
{
	HMODULE module;

module = LoadLibrary(L"gmscl.dll");


if (module == NULL)
{
	std::cout << "Loading gmscl.dll fail" << std::endl;
}
else
{
	std::cout << "loading ok" << std::endl;
}

Hello = (_FuncHello)GetProcAddress(module, "hello");

if (Hello)
{
	std::cout << "hello ok " << std::endl;
}
else
{
	std::cout << "GMSresolve non chargée: " << std::endl;
}


GMSResolve = (_FuncGMSResolve)GetProcAddress(module, "GMSResolve");
if (GMSResolve)
{
	std::cout << "GMSresolve ok " << std::endl;
}
else
{
	std::cout << "GMSresolve non chargée: " << std::endl;
}


GMSResolve_ServerInfo = (_FuncGMSResolve_ServerInfo)GetProcAddress(module, "GMSResolve_ServerInfo");
if (GMSResolve_ServerInfo)
{
	std::cout << "GMSResolve_ServerInfo ok " << std::endl;
}
else
{
	std::cout << "GMSResolve_ServerInfo non chargée " << std::endl;
}

GMSConnection_Auto = (_FuncGMSConnection_Auto)GetProcAddress(module, "GMSConnection_Auto");
if (GMSConnection_Auto)
{
	std::cout << "GMSConnection_Auto ok " << std::endl;
}
else
{
	std::cout << "GMSConnection_Auto non chargée " << std::endl;
}

GMSSendMessageString = (_FuncGMSSendMessageString)GetProcAddress(module, "GMSSendMessageString");
if (GMSSendMessageString)
{
	std::cout << "SendMessageString ok " << std::endl;
}
else
{
	std::cout << "SendMessageString non chargée " << std::endl;
}

GMSSendMessage = (_FuncGMSSendMessage)GetProcAddress(module, "GMSSendMessage");
if (GMSSendMessage)
{
	std::cout << "GMSSendMessage ok " << std::endl;
}
else
{
	std::cout << "GMSSendMessage non chargée " << std::endl;
}

GMSSubscribt = (_FuncGMSSubscribt)GetProcAddress(module, "GMSSubscribt");
if (GMSSubscribt)
{
	std::cout << "GMSSubscribt ok " << std::endl;
}
else
{
	std::cout << "GMSSubscribt non chargée " << std::endl;
}

GMSUnSubscript = (_FuncGMSUnSubscribt)GetProcAddress(module, "GMSUnSubscript");
if (GMSUnSubscript)
{
	std::cout << "GMSUnSubscript ok " << std::endl;
}
else
{
	std::cout << "GMSUnSubscript non chargée " << std::endl;
}

GMSReceiveMessage = (_FuncGMSReceiveMessage)GetProcAddress(module, "GMSReceiveMessage");
if (GMSReceiveMessage)
{
	std::cout << "GMSReceiveMessage ok " << std::endl;
}
else
{
	std::cout << "GMSReceiveMessage non chargée " << std::endl;
}

GMSMessageBoxCount = (_FuncGMSMessageBoxCount)GetProcAddress(module, "GMSMessageBoxCount");
if (GMSMessageBoxCount)
{
	std::cout << "GMSMessageBoxCount ok " << std::endl;
}
else
{
	std::cout << "GMSMessageBoxCount non chargée " << std::endl;
}

GMSMessageBox_MessageInfo = (_FuncGMSMessageBox_MessageInfo)GetProcAddress(module, "GMSMessageBox_MessageInfo");
if (GMSMessageBox_MessageInfo)
{
	std::cout << "GMSMessageBox_MessageInfo ok " << std::endl;
}
else
{
	std::cout << "GMSMessageBox_MessageInfo non chargée " << std::endl;
}






return true;
}