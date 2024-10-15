include "interface_calc.iol"
include "console.iol"

outputPort calculator {
    Location: "socket://localhost:8000"
    Protocol: sodep
    Interfaces: Calculator
}

main {
    req.x = 20
    req.y = 10

    sub@calculator(req)(res)
    println@Console(res)()

    reqAvg.array[0] = 10
    reqAvg.array[1] = 12
    reqAvg.array[2] = 14
    average@calculator(reqAvg)(res)
    println@Console(res)()

    add@calculator(req)(res)
    println@Console(res)()
}