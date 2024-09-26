type SumRequest:void {
    .x:int
    .y:int
}

type SubRequest:void {
    .x:int
    .y:int
}

type AverageRequest:void {
    .array*:int
}

interface Calculator {
    RequestResponse:
        add(SumRequest)(int),
        sub(SubRequest)(int),
        average(AverageRequest)(double)
}