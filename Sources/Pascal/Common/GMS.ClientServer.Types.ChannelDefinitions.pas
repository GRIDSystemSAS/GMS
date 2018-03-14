unit GMS.ClientServer.Types.ChannelDefinitions;

interface

//Protocol for Channel : naming const.
//  In the form of : CST_CHANNELNAME_<Proto>_<FunctionName>
//  CST : inalterable const.
//  <Proto> : possible values :
//  - RPC : Indicate that this channel drive to a service with an RPC processus call.
//        --> For the client, that means that the response will come by the same channel.
//        Client have to make a SendMsg(CST_RPC_XXX...,Stream_With_ProtocolObj) and then
//        immediately make a RecvMess on the same channel to get response.
//        Avantage of RPC mode is that you not really need a transport protocol.
//  - MES : Indicate that this channel drive to a service in a Messaging style call.
//          Usually, ask and answer will be not on the same channel, so client
//          can read or write on this channel, depending the app implementation.
//          Typically (as CPU_LOAD for exemple), server WRITE on this channel, and client is reading. 
//  CHANNELNAME : It is a channel name.
CONST
  CST_CHANNELNAME_RPC_GETSERVERDEFINITION = 'GRID/Core/Definition';
  CST_CHANNELNAME_MES_CPULOAD = 'GRID/Core/CPULoad';


implementation

end.
