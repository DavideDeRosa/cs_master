include "console.iol"
include "interface.iol"

inputPort PrintService {
	Location: "socket://localhost:2000"
	Protocol: sodep
	Interfaces: PrintInterface
}

cset {
	sid: OpMessage.sid
}

execution{ concurrent }

init {
	keepRunning = true
}

main
{
	login( request )( response ){
		username = request.name;
		response.sid = csets.sid = new;
		response.message = "You are logged in."
	};
	while( keepRunning ){
		[ print( request ) ]{
			println@Console( "User "+username+" writes: "+request.message )()
		}
		[ logout( request ) ] { 
			println@Console("User "+username+" logged out.")();
			keepRunning = false }
	}
}