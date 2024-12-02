include "interface_calc.iol"
include "console.iol"

outputPort calculator {
    Location: "socket://localhost:8000"
    Protocol: sodep
    Interfaces: Calculator
}

main {
    req.x = 20
    req.y = 0

    install(dividebyzero => println@Console("You can't divide by zero!")())
    install(IOException => println@Console("Crash IOException")())

    divide@calculator(req)(res)
    println@Console(res)()
}