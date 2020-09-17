
sealed trait Nat
case object Zero extends Nat
case class Succ(pred : Nat) extends Nat

val one = Succ(Zero)
val two = Succ(one)
val three = Succ(two)
val four = Succ(three)
val five = Succ(four)

def plus(x : Nat, y : Nat) : Nat = x match {
    case Zero    => y
    case Succ(x) => Succ(plus(x, y))
}

def nat_to_int(x : Nat) : Int = x match {
    case Zero => 0
    case Succ( x ) => 1 + nat_to_int(x)
}

def print_nat(x : Nat) : String = nat_to_int(x).toString

print_nat(four)

def mult(x: Nat, y: Nat): Nat = y match {
  // add x to itself y times
  case Zero => Zero
  case Succ(y) => plus(mult(x, y), x)
}

print_nat(mult(five, four)) // Should be 20
print_nat(mult(five, five)) // Should be 25

sealed trait List
case object Empty extends List
case class Cons(head: Nat, tail: List) extends List

def ex1 = Cons(one, Cons(two, Cons(three, Cons(four, Cons(five, Empty)))))
def ex2 = Cons(one, Empty)
def ex3 = Empty

def print_list_helper(xs : List) : String = xs match {
    case Empty => ""
    case Cons(x, Empty) => print_nat(x)
    case Cons(x, xs) => print_nat(x) + ", " + print_list_helper(xs)
}

def print_list(xs : List) : String = "[" + print_list_helper(xs) + "]"

print_list(ex1)
print_list(ex2)
print_list(ex3)

def length(list: List): Nat = list match {
  case Empty => Zero
  case Cons(head, tail) => plus(one, length(tail))
}

print_nat(length(Empty))

print_nat(length(ex1))

def init(list: List): List = list match {
  case Empty => Empty
  case Cons(head, Empty) => Empty
  case Cons(head, Cons(last, Empty)) => Cons(head, Empty)
  case Cons(head, tail) => Cons(head, init(tail))
}

print_list(init(ex1)) // should be [1, 2, 3, 4]
print_list(init(ex2)) // should be [ ]

def last(list: List): List = list match {
  case Empty => Empty
  case Cons(last, Empty) => Cons(last, Empty)
  case Cons(head, tail) => last(tail)
}

print_list(last(ex1))
print_list(last(ex2))
print_list(last(ex3))

def append(a: List, b: List): List = a match {
  case Empty => b
  case Cons(head, tail) => Cons(head,append(tail, b))
}

print_list(append(ex1, Cons(Succ(five), Cons(three, Cons(Zero, Empty)))))
