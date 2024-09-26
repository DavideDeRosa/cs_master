include "interface.iol"
include "console.iol"

inputPort MyInput {
    Location: "socket://localhost:8000"
    Protocol: sodep
    Interfaces: MyInterface
}

main {
    sendNumber(x)
    println@Console(x)()
}