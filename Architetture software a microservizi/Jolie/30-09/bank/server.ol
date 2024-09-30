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

    balance = 0

	while(keepRunning){
		[ deposit(request)(deposit_response) {
            balance = balance + request.amount
            deposit_response.message = "New balance: " + balance
        } ] {
			println@Console("deposit")(deposit_response)
		}

		[ withdraw(request)(withdraw_response) {
            balance = balance - request.amount
            withdraw_response.message = "New balance: " + balance
        } ] {
            println@Console("withdraw")(withdraw_response)
        }

        [ report(request)(report_response) {
            report_response.message = "Balance: " + balance
        } ] {
            println@Console("report")(report_response)
        }

        [ logout(request) ]{
            println@Console("User " + username + " logged out.")()
			keepRunning = false 
        }
	}
}