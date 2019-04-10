fun add(x) = x+1

(* task 1 *)
(* thenAddOne: ((int -> int) * int -> int) *)
(* REQUIRES: true *)
fun thenAddOne(func: (int -> int), v) = func(v)+1

(* task 2 *)
(* mapList: ((‘a -> ‘b) * ‘a list) -> ‘b list *)
(* REQUIRES: true*)
fun mapList(func, []) = []
    | mapList(func, x::L) = func(x)::mapList(func, L)

(* task 3 *)
(* mapList': (‘a -> ‘b) -> (‘a list -> ‘b list) *)
(* REQUIRES: true *)
fun mapList'(func) = map func

(* task 4 *)
(* findOdd: int list -> int option *)
(* REQUIRES: true *)
fun findOdd([]) = NONE
    | findOdd(x::L) = if (x mod 2 = 1) then SOME x
      else findOdd(L)

(* task 5 *)
(* subsetSumOption: int list * int -> int list option *)
(* REQUIRES: true *)
fun subsetSumOption(L, 0) = SOME[]
    | subsetSumOption([], _) = NONE
    | subsetSumOption(x::L, s) = case subsetSumOption(L, s-x) of
          SOME R => SOME(x::R)
        | NONE => subsetSumOption(L, s)

(* task 6 *)
(* exists: (‘a -> bool) -> ‘a list -> bool *)
(* REQUIRES: true *)
fun exists func [] = false
    | exists func (x::L) = func(x) orelse exists func L

(* forall: (‘a -> bool) -> ‘a list -> bool *)
fun forall func ([]) = true
    | forall func (x::L) = func(x) andalso forall func L

datatype 'a tree = Empty
    | Node of 'a tree * 'a * 'a tree
(* task 7 *)
(* treeFilter: (‘a -> bool) -> ‘a tree -> ‘a option tree *)
(* REQUIRES: true *)
fun treeFilter func Empty = Empty
    | treeFilter func (Node(t1, x, t2)) = 
        if func(x)
        then Node(treeFilter func t1, SOME x, treeFilter func t2)
        else Node(treeFilter func t1, NONE, treeFilter func t2)

fun even(x) = (x mod 2 = 0)
