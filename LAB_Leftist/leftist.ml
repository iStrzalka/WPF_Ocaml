(*Twórca programu : Tomasz Nitsch*)
(*Recenzant programu : Bohdan Petraszczuk*)

(*========================NOWE TYPY ZMIENNYCH================================*)
(*Inicjacja wartosci - kolejka trzymająca wartość swoją, długość prawej ścieżki
                         lewego i prawego syna
                       lub liść*)
type 'a queue =
  | Node of {value: 'a; right_length: int; left: 'a queue; right: 'a queue}
  | Leaf

(*===========================KONSTRUKTORY====================================*)
(*Pomocniczy konstruktor z wszystkimi zmiennymi jako wartości Node'a*)
let node v rl l r = Node {value= v; right_length= rl; left= l; right= r}

(*Konstuktor pustej kolejki 'a queue*)
let empty = Leaf

(*=========================OPERACJE NA KOLEJKACH=============================*)
(*Funkcja join queue1 queue2 - łączy dwie kolejki w jedną i zwraca jako wynik*)
let rec join queue1 queue2 =
  match (queue1,queue2) with
  | (Leaf,q ) | (q,Leaf) -> q
  | (Node(q1),Node(q2)) -> 
      if q1.value > q2.value then join queue2 queue1
      else
        let queuepom = join q1.right queue2 in
        match (q1.left, queuepom) with
        | (Leaf,q3) | (q3,Leaf) -> node q1.value 0 q3 Leaf
        | (Node(q3), Node(q4)) -> 
            if q4.right_length < q3.right_length 
            then node q1.value (q4.right_length + 1) q1.left queuepom 
            else node q1.value (q3.right_length + 1) queuepom q1.left

(*Funkcja add element queue - łączy kolejkę początkową oraz nowy element
  w jedną kolejkę i zwraca jako wynik*)
let add element queue = join queue (node element 0 Leaf Leaf)

(*NOWY NIESTANDARDOWY WYJĄTEK - EMPTY = jeżeli kolejka jest pusta to ten wyjątek 
  zostaje podnisiony dla funkcji delete_min*)
exception Empty

(*Funckja delete_min queue - funkcja która zwraca najbardziej priorytetowy 
  element i złączenie ich dwóch synów lub podnosi wyjątek jeżeli pusta*)
let delete_min queue =
  match queue with
  | Leaf -> raise Empty
  | Node node -> (node.value, join node.left node.right)

(*Funkcja is_empty queue - funkcja które sprawdza czy kolejka jest pusta*)
let is_empty queue = queue = Leaf

(*

let test a b num msg =
  if a <> b then (print_int num; print_endline msg);; 

let rec zwin l q num msg =
  try
    match l with
    | [] -> test q empty num msg
    | h::t -> let (mn,r) = delete_min q in test mn h num msg; zwin t r (num+1) msg
  with Empty -> (print_int num; print_string "Empty"; print_endline msg);;

let a = add 0. empty;;        (* 0.*)
let b = add 1. empty;;        (* 1. *)
let c = add (-0.1) empty;;    (* -0.1 *)
let d = add 7. a;;            (* 0., 7. *)
let e = add (-3.) d;;         (* -3., 0., 7. *)
let f = add (-0.5) c;;        (* -0.5, -0.1 *)
let g = join b c;;            (* -0.1, 1.*)
let h = join d e;;            (* -3., 0., 0., 7., 7. *)
let i = join f e;;            (* -3., -0.5, -0.1, 0., 7. *)
let j = join h i;;            (* -3., -3., -0.5, -0.1, 0., 0., 0., 7., 7., 7. *)

let la = [0.];;
let lb = [1.];;
let lc = [-0.1];;
let ld = la @ [7.];;
let le = -3.::ld;;
let lf = -0.5::lc;;
let lg = lc @ lb;;
let lh = [-3.; 0.; 0.; 7.; 7.];;
let li = [-3.; -0.5; -0.1; 0.; 7.];;
let lj = [-3.; -3.; -0.5; -0.1; 0.; 0.; 0.; 7.; 7.; 7.];;

test (join empty empty) empty (-1) ": empty + empty";;
zwin la a 0 ": a";;
zwin lb b 0 ": b";;
zwin lc c 0 ": c";;
zwin ld d 0 ": d";;
zwin le e 0 ": e";;
zwin lf f 0 ": f";;
zwin lg g 0 ": g";;
zwin lh h 0 ": h";;
zwin li i 0 ": i";;
zwin lj j 0 ": j";;

print_string("Done\n");;
*)