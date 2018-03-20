// ConsoleApplication1.cpp : Définit le point d'entrée pour l'application console.
//

#include "stdafx.h"
#include <iostream>
#include <windows.h>
#include "gmscl.h"

int main()
{
	InitLibrary();

	std::cout << std::endl << std::endl;
	std::cout << "*********************" << std::endl;
	std::cout << "test hello" << std::endl;

	wchar_t str[256];
	int len = Hello(L"Vincent", str, 255);
	str[len] = 0;
	std::wcout << str << std::endl;

	std::cout << std::endl << std::endl;
	std::cout << "*********************" << std::endl;
	std::cout << "test GMSResolve" << std::endl;
	int ServerCount = GMSResolve();
	std::wcout << "GMS Servers found number : " << ServerCount << std::endl;

	if (ServerCount > 0)
	{
	  std::cout << std::endl << std::endl;
	  std::cout << "*********************" << std::endl;
	  std::cout << "test GMSResolve_ServerInfo" << std::endl;
	  for(int i = 0;i < 6; ++i)
	  {
		  FillMemory(str, 255, 0);
		  len = GMSResolve_ServerInfo(0, i, str, 255);
		  str[len] = 0;
    	  std::wcout << "Server Name 0 / Info " << i << " : " << str << std::endl;
	  }
	}

	std::cout << std::endl << std::endl;
	std::cout << "*********************" << std::endl;
	std::cout << "test GMSConnection_Auto" << std::endl;
	int Conn = GMSConnection_Auto(L"admin", L"admin");

	if (Conn>-1)
	{
	  std::cout << std::endl << std::endl;
	  std::cout << "*********************" << std::endl;
	  std::cout << "test SendMessageString" << std::endl;
	  for (int i = 0; i<1000; i++)
	  {
		//wchar_t a = wchar_t("this is a test") + wchar_t(i);
  		GMSSendMessageString(Conn, L"Test", L"this is a test from c++");
	  }

	  std::cout << "test SendMessage binary" << std::endl;
	  for (int i = 0; i < 100; i++)
	  {
		  byte bstr[8191];
		  GMSSendMessage(Conn, L"Test", bstr, 8192);
	  }
	}
	 
		
	std::cout << "Finnished" << std::endl;
	std::cin.get();
	return 0;
}

