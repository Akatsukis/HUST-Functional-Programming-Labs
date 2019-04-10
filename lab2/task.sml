(* task 1 *)
(* reverse: a list -> a list *)
(* REQUIRES: true *)
fun reverse([]) = []
    | reverse(x::L) = reverse(L)@[x]

(* revAppend: a list * a list -> a list *)
(* REQUIRES: true *)
fun revAppend([], ys) = ys
    | revAppend(x::xs, ys) = revAppend(xs, x::ys)


(* reverse': a list -> a list *)
(* REQUIRES: true *)
fun reverse'(xs) = revAppend(xs, [])

(* task 2 *)
(* interleave: a list * a list -> a list *)
(* REQUIRES: true *)
fun interleave([], []) = []
    | interleave(xs, []) = xs
    | interleave([], ys) = ys
    | interleave(x::L, y::R) = x::y::interleave(L, R)

(* task 3 *)
datatype 'a tree = Empty
    | Node of 'a tree * 'a * 'a tree
(* split: int list -> int list * int * int list *)
(* REQUIRES: list2 is not empty *)
fun split(_, []) = raise Fail "list 2 cannot be empty"
    | split(L, x::R) = if abs(length(L)-length(R)) <= 1 then (L, x, R)
        else split(L@[x], R)

(* listToTree: int list -> tree *)
(* REQUIRES: true *)
fun listToTree([]: int list) = Empty
    | listToTree [x] = Node(Empty, x, Empty)
    | listToTree L = 
        let val k = length(L) div 2
            val L1 = List.take(L, k)
            val x::R1 = List.drop(L, k)
        in Node(listToTree(L1), x, listToTree(R1)) end

(* task 4 *)
(* trav: tree -> int list *)
(* REQUIRES: true *)
fun trav(Empty) = []
    | trav(Node(t1, x, t2)) = trav(t1)@x::trav(t2)

(* revT: tree -> tree *)
(* REQUIRES: true *)
fun revT(Empty) = Empty
    | revT(Node(t1, x, t2)) = Node(revT(t2), x, revT(t1))

(* task 5 *)
(* binarySearch: tree -> bool *)
(* REQUIRES: true *)
fun binarySearch(Empty, v) = false
    | binarySearch(Node(t1, x, t2), v) = 
        case Int.compare(v, x) of
          EQUAL => true
        | LESS => binarySearch(t1, v)
        | GREAT => binarySearch(t2, v)

