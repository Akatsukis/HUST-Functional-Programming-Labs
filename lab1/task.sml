(* task 2 *)
(* mult: int list -> int *)
(* REQUIRES: true *)
(* ENSURES: mult(L) evaluates to the product of the integers in L. *)
fun mult([  ] : int list): int = 1
    | mult(x::L) = x * (mult L)

(* task 3 *)
(* Mult: int list list -> int *)
(* REQUIRES: true *)
(* ENSURES: mult(R) evaluates to the product of all the integers in the lists of R. *)
fun Mult([  ]: int list list): int = 1
    | Mult(x::L) = mult(x) * Mult(L)

(* task 4 *)
(* mult’: int list * int -> int *)
(* REQUIRES: true *)
(* ENSURES: mult’(L, a) evaluates to the product of the integers in L and a *)
fun mult'([  ], a) = a
    | mult'(x::L, a) = mult'(L, x*a)

fun Mult'([  ], a) = a
    | Mult'(x::L, a) = Mult'(L, mult(x)*a)

(* task 5 *)
(* double: int -> int *)
(* REQUIRES: n >= 0 *)
(* ENSURES: double n evaluates to 2 * n.*)
fun double(0 : int): int = 0
    | double n = 2+double(n-1)

(* square: int -> int *)
(* REQUIRES: n >= 0 *)
(* ENSURES: square n evaluates to n * n.*)
fun square(0 : int): int = 0
    | square n = double(n)-1+square(n-1)

(* task 6 *)
(* divisibleByThree: int -> bool *)
(* REQUIRES: n >= 0 *)
(* ENSURES: divisibleByThree n evaluates to true if n is a multiple of 3 and to false otherwise *)
fun divisibleByThree(0 : int): bool = true
    | divisibleByThree 1 = false
    | divisibleByThree 2 = false
    | divisibleByThree n = if n > 0 then divisibleByThree(n-3)
        else divisibleByThree(n+3)

(* task 7 *)
(* oddP: int -> bool *)
(* REQUIRES: n >= 0 *)
(* ENSURES: oddP n evaluates to true iff n is odd. *)
fun oddP(0 : int):bool = false
    | oddP 1 = true
    | oddP n = oddP(n-2)

(* homework 4 *)
(* zip: ing list * string list -> (string * int) list *)
(* REQUIRES: true *)
(* ENSURES: *)
fun zip([] : int list, [] : string list) = []
    | zip(x::xs, y::ys) = (x, y)::zip(xs, ys)

(* unzip: (string * int) list -> int list * string list *)
(* REQUIRES: true *)
(* ENSURES: *)
fun unzip([]) = ([], [])
    | unzip((x, y)::L) = 
    let val(xs, ys) = unzip L
    in (x::xs, y::ys) end
