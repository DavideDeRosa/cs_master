/* Obiettivo: riprodurre il codice Haskell      \x -> \y -> x + y       in C*/

#include <stdio.h>
#include <stdlib.h>

typedef void* value;

typedef struct Closure {
    value (*func)(struct Closure*, value);
    value data[0];
} Closure;

/* Funzioni ausiliarie */
value to_value(intptr_t n) {
    return (value) n;
}

intptr_t to_int(value v) {
    return (intptr_t) v;
}

Closure* lambda(value (*func)(Closure*, value), int n) {
    Closure* cls = (Closure*) malloc(sizeof(Closure) + n * sizeof(value));
    cls->func = func;
    return cls;
}

value apply(value f, value arg) {
    Closure* cls = (Closure*) f;
    return cls->func(cls, arg);
}

/* Funzione interna (\y -> x + y) */
value f2(Closure* cls, value y) {
    return to_value(to_int(cls->data[0]) + to_int(y));
}

/* Funzione esterna (\x -> \y -> x + y) */
value f1(Closure* cls, value x) { 
    Closure* c = lambda(f2, 1);
    c->data[0] = x;
    return (value) c;
}

/* Vogliamo compilare ((f1 2) 3) ed ottenere il risultato 5 */
int main() {
    int res = to_int(apply(apply(lambda(f1, 0), to_value(2)), to_value(3)));
    printf("%d\n", res);
}