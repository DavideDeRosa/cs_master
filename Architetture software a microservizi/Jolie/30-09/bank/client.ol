include "console.iol"
include "ui/ui.iol"
include "ui/swing_ui.iol"
include "interface.iol"

outputPort BankService {
	Location: "socket://localhost:2000"
	Protocol: sodep
	Interfaces: BankInterface
}

main
{
	showInputDialog@SwingUI("Insert your name" )(request.name)
	keepRunning = true
	login@BankService(request)(response)
	opMessage.sid = response.sid
    mySid = response.sid
	println@Console("Server Responded: " + response.message + "\t sid: " + opMessage.sid)()
    while(keepRunning){
		showInputDialog@SwingUI("Write a command (deposit, withdraw, report) or type \"logout\" for logging out.")(opMessage.message);
		if(opMessage.message == "logout"){
			logout@BankService(opMessage);
			keepRunning = false
		} else if(opMessage.message == "deposit") {
            showInputDialog@SwingUI("Amount to deposit")(amount);
            deposit.amount = int(amount)
            deposit.sid = mySid
			deposit@BankService(deposit)(res)
            println@Console(res.message)()
		} else if(opMessage.message == "withdraw") {
            showInputDialog@SwingUI("Amount to withdraw")(amount);
            withdraw.amount = int(amount)
            withdraw.sid = mySid
            withdraw@BankService(withdraw)(res)
            println@Console(res.message)()
        } else if(opMessage.message == "report") {
            report.sid = mySid
            report@BankService(report)(res)
            println@Console(res.message)()
        }
	}
}