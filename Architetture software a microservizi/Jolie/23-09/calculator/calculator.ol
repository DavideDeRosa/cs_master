include "interface_calc.iol"
include "console.iol"

inputPort Client {
    Location: "socket://localhost:8000"
    Protocol: sodep
    Interfaces: Calculator
}

main {
    [ add(request)(response)  {
        response = request.x + request.y
    } ] {main}

    [ sub(request)(response) {
        response = request.x - request.y
    } ] {main}

    [ average(request)(response) {
        response = 0

        for(i = 0, i < #request.array, i++) {
            response = response + request.array[i]
        }

        response = response / #request.array
    } ] {main}
}