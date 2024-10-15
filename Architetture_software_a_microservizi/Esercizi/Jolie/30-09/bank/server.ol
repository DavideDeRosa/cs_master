include "console.iol"
include "interface.iol"

inputPort BankService {
	Location: "socket://localhost:2000"
	Protocol: sodep
	Interfaces: BankInterface
}

cset {
	sid: OpMessage.sid DepositMessage.sid WithdrawMessage.sid ReportMessage.sid
}

execution{ concurrent }

init {
	keepRunning = true
}

main
{
	login(request)(response){
		username = request.name;
		response.sid = csets.sid = new;
		response.message = "You are logged in."
	};

    synchronized(username){
        if(!is_defined(global.username.(username))){
            global.username.(username) = 0
            println@Console(global.username.(username))()
        }
    }

    balance -> global.username.(username)

	while(keepRunning){
		[ deposit(request)(deposit_response) {
            synchronized(username){
                balance = balance + request.amount
            }

            deposit_response.message = "New balance: " + balance
        } ] {
			println@Console("deposit")()
		}

		[ withdraw(request)(withdraw_response) {
            synchronized(username){
                balance = balance - request.amount
            }

            withdraw_response.message = "New balance: " + balance
        } ] {
            println@Console("withdraw")()
        }

        [ report(request)(report_response) {
            report_response.message = "Balance: " + balance
        } ] {
            println@Console("report")()
        }

        [ logout(request) ]{
            println@Console("User " + username + " logged out.")()
			keepRunning = false 
        }
	}
}