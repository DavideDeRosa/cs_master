## Si vuole rappresentare con una struttura dati gli alberi di sintassi astratta 
## delle espressioni di un linguaggio di programmazione.

from __future__ import annotations
from dataclasses import dataclass

@dataclass
class Num:
    n: int

@dataclass
class String:
    n: str

@dataclass
class Mult:
    e1: Expr
    e2: Expr

@dataclass
class Concat:
    e1: Expr
    e2: Expr

@dataclass
class Eq:
    e1: Expr
    e2: Expr

Expr = Num | String | Mult | Concat | Eq

def eval(e):
    match e:
        case Num(n):
            return n
        case String(s):
            return s
        case Mult(e1, e2):
            return eval(e1) * eval(e2)
        case Concat(e1, e2):
            return eval(e1) + eval(e2)
        case Eq(e1, e2):
            return eval(e1) == eval(e2)

good = Eq(Mult(Num(3), Num(4)), Num(5))
bad = Eq(Concat(String("ciao"), Num(2)), Num(3))

print(f"Good: {eval(good)}")
print(f"Bad: {eval(bad)}")