include "console.iol"
include "ui/ui.iol"
include "ui/swing_ui.iol"
include "interface.iol"

outputPort PrintService {
	Location: "socket://localhost:2000"
	Protocol: sodep
	Interfaces: PrintInterface
}

main
{
	showInputDialog@SwingUI( "Insert your name" )( request.name );
	keepRunning = true;
	login@PrintService( request )( response );
	opMessage.sid = response.sid;
	println@Console( "Server Responded: " + response.message + "\t sid: "+opMessage.sid )();
	while( keepRunning ){
		showInputDialog@SwingUI( "Insert a message or type \"logout\" for logging out." )( opMessage.message );
		if( opMessage.message != "logout" ){
			print@PrintService( opMessage )
		} else {
			logout@PrintService( opMessage );
			keepRunning = false
		}
	}
}