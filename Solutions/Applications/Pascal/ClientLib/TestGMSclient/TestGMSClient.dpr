program TestGMSClient;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  FastMM4,
  System.SysUtils,
  Classes,
  gmsclh in '..\gmsclh.pas';

Const
  CST_STRING_TEST = 9;
var
  buff: array [0..255] of WideChar;
  s,s2: string;
  ServerCount : Integer;
  i : integer;


  lhandle :  UInt64;
  ltemp : UInt64;

  lconn : Integer;

  lbytebuffer : array[0..4095] of Byte;
  lmem : TMemoryStream;
begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
  {$ENDIF}
  try
    Writeln('test GSM client Lib dll...');
    s := 'Vincent';
    hello(PWideChar(s),buff,256);
    s := Buff;
    Writeln(s);

    ServerCount := GMSResolve;
    Writeln(ServerCount);
    if  ServerCount>0 then
    begin
      //There are only 4 (0-4) options currently. the fifth is for show what happen when overflow.
      for i := 0 to 5 do
      begin
        FillChar(buff,255,0);
        GMSResolve_ServerInfo(0,i,buff,256);
        s := buff;
        Writeln('Info code '+IntToStr(i)+' : ' + s);
      end;

    end;

    Writeln('test MemStream client Lib dll...');
    lhandle := MemStreamCreate;
    Writeln('Memhandle '+IntToStr(lhandle)+ ' created');

    ltemp := MemStreamSize(lhandle);
    Writeln('Size of Memhandle : '+IntToStr(ltemp));
    Writeln('AddString '+IntToStr(CST_STRING_TEST+1)+' strings');
    for I := 0 to CST_STRING_TEST do
    begin
      s := 'Hello World '+IntToStr(i+1);
      MemStreamAddString(lhandle,pWideChar(s));
      s :=  EmptyStr;
    end;

    Writeln('Size of Memhandle : '+IntToStr(MemStreamSize(lhandle)));
    Writeln('release Memhandle '+IntToStr(lhandle));
    MemStreamRelease(lhandle);



    Writeln(EmptyStr);
    Writeln(EmptyStr);
    Writeln('Connection');
    lconn := GMSConnection_Auto('admin','admin');
    Writeln('Connection handle : '+IntTostr(lconn));

    if lconn>-1 then
    begin
      Writeln('SendMessageString');
      for I := 0 to 999 do
      begin
        Writeln('SendMessageString message '+IntToStr(i+1));
        GMSSendMessageString(lconn,PWideChar('Test'),PWideChar('a fancy message '+IntToStr(i)));
      end;

      Writeln('SendMessage binary message 1');
      //A whole array of byte
      GMSSendMessage(lconn,PWideChar('Test'),PByteArray(@(lByteBuffer[0])),Length(lByteBuffer));
        ///... Or a TMemoryStream.
      lmem := TMemoryStream.Create;
      try
        lmem.SetSize(8192); //or loadFromFile(...
        Writeln('SendMessage binary message 2');
        GMSSendMessage(lconn,PWideChar('Test'),PByteArray(@(lmem.Memory)),lmem.Size);
      Finally
        FreeAnDnil(lmem);
      end;

      Writeln(EmptyStr);
      Writeln('SUB');
      Writeln('Subscript to "test" : '+IntToStr(GMSSubscribt(lConn,'Test'))); //Warning ! Case sensitive !
      Writeln('Re-SendMessageString on "Test"');
      GMSSendMessageString(lconn,'Test','a fancy message ');
      Writeln('ReceiveMessage''s count: '+IntToStr(GMSReceiveMessage(lConn,0)));
      if GMSMessageBoxCount(lconn)>0 then
      begin
        for I := 0 to GMSMessageBoxCount(lconn)-1 do
        begin
          FillChar(buff,255,0);
          GMSMessageBox_MessageInfo(lconn,I,0,buff,256); //id.
          s := buff;

          FillChar(buff,255,0);
          GMSMessageBox_MessageInfo(lconn,I,1,buff,256); //channel.
          s2 := buff;

          Writeln(' --> Message '+IntToStr(i)+' Message Id : "'+s+'" / Channel : "'+s2+'"');
        end;
      end;

    end;


    readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
