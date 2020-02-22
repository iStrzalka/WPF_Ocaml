(*Twórca programu : Tomasz Nitsch*)
(*Recenzant programu : --*)

(** Wyjątek podnoszony gdy graf z wejścia jest cykliczny *)
exception Cykliczne

(** Funkcja tworząca graf z podanej na wejsciu listy graf jest tworzony 
    przy pomocy modułu PMap wartość trzymana w mapie to trójka:
    (lista sąsiedztwa, Czy był odwiedzony, Czy jest już w wyniku) *)
let make input_list =
    let pom acc (a, alist) =
        if PMap.mem a acc then 
            let (list, _, _) = PMap.find a acc in 
            PMap.add a (alist @ list, false, false) acc
        else 
            PMap.add a (alist, false, false) acc
    in
        List.fold_left pom (PMap.empty) input_list
        
(** Sprawdza czy klucz znajduje się na mapie jeśli tak to zwraca jego*)
let fnd key graph =
    if PMap.mem key !graph then PMap.find key !graph
    else ([], false, false) 

let topol input_list =
    let graph = ref (make input_list) in
    let wynik = ref [] in
    (** Funkcja przechodząca po grafie (DFS) *)
    let rec dfs x =
    (** Wartość w przeciwnym wypadku podaje trójkę (później doda go do mapy) *)
        match fnd x graph with
        (* Odwiedzony, niedodany do wyniku => cykl *)
        | (_, true, false) -> raise Cykliczne
        (* Odwiedzony, dodany do wyniku => koniec dfsa*)
        | (_, true, true) -> ()
        (* Nowo odwiedzony wierzchołek *)
        | (xlist, _, _) ->
        begin
            (** Wierzchołek jest rozpatrywany *)
            graph := PMap.add x (xlist, true, false) !graph;
            List.iter dfs xlist;
            (** Wierzchołek został rozpatrzony i dodany do wyniku *)
            graph := PMap.add x (xlist, true, true) !graph;
            wynik := x::!wynik;
        end
    in
        List.iter (fun (x, _) -> dfs x) input_list;
        !wynik

(*
TESTY
topol [(1, [2;3]); (2, [3]); (3, [])];;
topol [(1, [2;3]); (2, [3]); (3, [1])];;
topol [(1, [2]); (2, [3;4]); (3, [5;6;7]); (4, []); (5, []); (6, [8]); (7, []); (8, [9]); (9, [10]); (10, [])];;
topol [(1, [2]); (2, [3;4]); (3, [5;6;7]); (4, [3]); (5, []); (6, [8]); (7, []); (8, [9]); (9, [10]); (10, [7])];;
topol [(1, [2]); (2, [3;4]); (3, []); (4, [3]); (5, []); (6, [])];;
topol [(1, [2;3]); (2, [3;4;5;6;7]); (3, [])];;
topol [(1, [2;3]); (2, [3;4;5;6;7]); (3, [7])];;
topol [("t", ["te"; "tes"]); ("te", ["test"; "tes"])];;
*)