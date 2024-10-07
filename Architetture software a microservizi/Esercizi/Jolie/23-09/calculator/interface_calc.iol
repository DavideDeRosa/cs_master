type SumRequest:void {
    .x:int
    .y:int
}

type SubRequest:void {
    .x:int
    .y:int
}

type AverageRequest:void {
    .array[1,*]:int
}

interface Calculator {
    RequestResponse:
        add(SumRequest)(int),
        sub(SubRequest)(int),
        average(AverageRequest)(double)
}