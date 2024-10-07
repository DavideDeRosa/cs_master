type LoginRequest: void {
	.name: string
}

type OpMessage: void{
	.sid: string
	.message?: string
}

type DepositMessage: void{
	.sid: string
	.amount: int
}

type WithdrawMessage: void{
	.sid: string
	.amount: int
}

type ReportMessage: void{
	.sid: string
}

type Response: void{
    .message?: string
}

interface BankInterface {
	RequestResponse: 
        login(LoginRequest)(OpMessage), 
        deposit(DepositMessage)(Response), 
        withdraw(WithdrawMessage)(Response), 
        report(ReportMessage)(Response)
    OneWay: logout(OpMessage)
}