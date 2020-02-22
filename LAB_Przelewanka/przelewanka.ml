(** Twórca programu : Tomasz Nitsch*)
(** Recenzant programu : Karol Zagródka*)

(** Funkcja pomocnicza obliczająca NWD dwóch liczb *)
let rec nwd a b = if b = 0 then a else nwd b (a mod b)

(** Wyjątek pomocniczy dla funkcji sprawdzającej jeden z waruków koniecznych przelewanki.
    Podnoszony wtedy gdy znajdziemy szklankę, która docelowo ma być napełniona po brzeg.*)
exception Full_Found 

(** Dwa warunki konieczne dla przelewanki : 
    -- Wszystkie docelowe pojemnosci muszą być podzielne przez NWD pojemnosci szklanek
    -- Musi być conajmniej jedna szklanka, która jest albo pusta albo pełna *)
let warunek_konieczny_jest_spelniony input_array =
    if Array.length input_array = 0 then true else 
    let n = Array.length input_array in 
    let pojemnosci = Array.init n (fun i -> fst input_array.(i)) and 
        oczekiwane = Array.init n (fun i -> snd input_array.(i)) in 
    let warunek_nwd= 
        let nwd_pojemnosci = Array.fold_left nwd pojemnosci.(0) pojemnosci in 
        if nwd_pojemnosci = 0 then true
        else Array.for_all (fun ele -> ele mod nwd_pojemnosci = 0) oczekiwane
    and warunek_pelnego_pustej_szklanki= 
        if Array.exists (fun ele -> ele = 0) oczekiwane then true 
        else
            try 
                for i = 0 to n - 1 do 
                    if pojemnosci.(i) = oczekiwane.(i) then raise Full_Found 
                done;
                false;
            with Full_Found -> true 
    in (warunek_nwd && warunek_pelnego_pustej_szklanki)

(** Wyjątek pomocniczy dla funkcji same_array
    Podnoszony gdy funkcja znajdzie nie te same elementy w dwóch tablicach*)
exception Found_Not_Equal 

(** Funkcja pomocnicza, która sprawdza czy aktualnie rozpatrywany stan
    jest równy stanowi oczekiwanemu*)
let same_array array_of_state input_array=
    let n = Array.length array_of_state in 
    try 
        for i = 0 to n - 1 do 
            if array_of_state.(i) <> snd input_array.(i) then raise Found_Not_Equal
        done;
        true;
    with Found_Not_Equal -> false 

(** Wyjątek pomocniczy dla przelewanki i post_process_state
    Podnoszony gdy znajdziemy satysfakcjonujący stan i zwraca wynik*)
exception Found of int 

(** Typy zmian w szklance
    Spill - wylanie zawartosci szklanki [i] do zlewu 
    Fill - nalanie z kranu do szklanki [i]
    Transfer - Przelanie ze szklanki [i] do innych szklanek*)
type state_of_change=
    Spill of int | Fill of int | Transfer of int 

(** Funkcja pomocnicza, która sprawdza czy aktualny stan jeszcze nie wystąpił 
    oraz czy nie jest stanem oczekiwanym
    Jeżeli nie jest - jest on dodawany na kolejke
    Jeżeli jest - podnoszony jest wyjątek kończący przelewankę i zwracający wynik *)
let post_process_state (state, number) input_array kolejka visited=
    if not (Hashtbl.mem visited state)
    then if same_array state input_array then raise (Found number)
    else (Queue.add (state, number) kolejka; Hashtbl.add visited state true)

(** Funkcja pomocnicza, która generuje nowe stany w zaleznosci od [type_of_change]*)
let process type_of_change (state, number) input_array kolejka visited= 
    match type_of_change with 
        | Spill i -> 
            if state.(i) > 0 then 
            let temp = Array.copy state in 
            temp.(i) <- 0; 
            post_process_state (temp, number + 1) input_array kolejka visited;         
        | Fill i -> 
            if state.(i) < (fst input_array.(i)) then 
            let temp = Array.copy state in 
            temp.(i) <- (fst input_array.(i));
            post_process_state (temp, number + 1) input_array kolejka visited;
        | Transfer i -> 
            if state.(i) <> 0 then
            let n = Array.length input_array in  
            for j = 0 to (n - 1) do 
                if i <> j then 
                let temp = Array.copy state in 
                temp.(j) <- min (fst input_array.(j)) (temp.(i) + temp.(j));
                temp.(i) <- temp.(i) - (temp.(j) - state.(j));
                post_process_state (temp, number + 1) input_array kolejka visited;
            done;;

(** Główna funkcja zwracająca minimalną ruchów, żeby dojść do podanego stanu 
    napełnienia szklanek lub zwracająca -1, jesli nie jest to możliwe*)
let przelewanka input_array = 
    if warunek_konieczny_jest_spelniony input_array then 
        let kolejka = Queue.create ()
        and visited = Hashtbl.create 42 
        and n = Array.length input_array in 
        try 
            post_process_state (Array.make n 0, 0) input_array kolejka visited;
            while not (Queue.is_empty kolejka) do 
                let cur = Queue.pop kolejka in 
                for i = 0 to (n - 1) do 
                    process (Spill i) cur input_array kolejka visited;
                    process (Fill i) cur input_array kolejka visited;
                    process (Transfer i) cur input_array kolejka visited;
                done;
            done;
            -1;
        with 
            Found answer -> answer
    else 
        -1;;

(* TESTY
assert (przelewanka [| (10,2); (1,1) |] = 5);;
assert (przelewanka [| (0,0); (2,2); (2,2); (2,2); (0,0); (0,0); (1,0);
  (0,0); (1,0) |] = (3));;
assert (przelewanka [| (1,1); (2,1); (3,0); (4,2) |] = (3));
assert (przelewanka [| (0,0); (2,2); (1,0); (1,1); (1,0); (2,2); (1,0);
  (0,0); (0,0) |] = (3));
assert (przelewanka [| (11,11); (11,1) |] = (-1));;
assert (przelewanka [| (1,1); (0,0); (2,2); (0,0); (2,0); (0,0); (0,0);
  (1,0); (2,0); (1,0) |] = (2));;
assert (przelewanka [| (5,2); (0,0); (0,0); (2,0); (3,2) |] = (4));;
assert (przelewanka [| (1,1); (0,0); (4,4); (4,0); (4,4) |] = (3));;
assert (przelewanka [| (9,9); (13,12) |] = (10));;
assert (przelewanka [| (2,2); (1,0); (2,2); (0,0); (1,0); (0,0); (1,1);
  (1,0); (0,0) |] = (3));;
assert (przelewanka [| (5,2); (3,1); (0,0); (4,1); (0,0); (1,0) |] = (5));;
assert (przelewanka [| (310,76); (139,91) |] = (-1));;
assert (przelewanka [| (48,9); (12,0); (1,1); (65,64) |] = (10));;
assert (przelewanka [| (7,5); (3,3); (9,4); (10,4); (6,3); (5,3) |] =
  (8));;
assert (przelewanka [| (100000,50000); (1,1) |] = (100000));;
assert (przelewanka [| (0,0); (0,0); (0,0); (300000,151515);
  (1,0); (0,0) |] = (296971));;
assert (przelewanka [| (11,2); (11,10); (4,0); (10,8); (21,16) |] = (12));;
assert (przelewanka [| (50,1); (7,3); (78,64) |] = (-1));;
assert (przelewanka [| (85,23); (524,210) |] = (-1));;
assert (przelewanka [| (557,349); (73,49) |] = (-1));;
assert (przelewanka [| (62,3); (38,7) |] = (-1));;
assert (przelewanka [| (15,15); (6,3); (42,32); (33,20) |] = (-1));;
assert (przelewanka [| (39,12); (35,34); (21,7); (2,1) |] = (-1));;
assert (przelewanka [| (1,0); (2,1); (2,1); (0,0); (2,0); (0,0); (0,0);
  (0,0); (1,1); (0,0); (1,0) |] = (4));;
assert (przelewanka [| (2,0); (2,2); (2,1); (6,6); (0,0) |] = (-1));;
assert (przelewanka [| (2,0); (1,1); (1,1); (1,1); (0,0); (1,0); (3,2);
  (0,0) |] = (4));;
assert (przelewanka [| (1,1); (2,2); (4,1); (0,0); (1,0); (2,1) |] = (5));;
assert (przelewanka [| (1,0); (3,1); (2,2); (1,1); (1,0); (1,0) |] = (3));;
assert (przelewanka [| (20,7); (12,11) |] = (-1));;
assert (przelewanka [| (0,0); (21,21) |] = (1));;
assert (przelewanka [| (13,8); (11,11) |] = (14));;
assert (przelewanka [| (1,1); (3,2); (6,5) |] = (5));;
assert (przelewanka [| (4,4); (7,6); (2,2) |] = (6));;
assert (przelewanka [| (3,2); (3,3); (1,1); (2,0) |] = (3));;
assert (przelewanka [| (0,0); (2,0); (0,0); (2,0); (3,2); (2,1); (1,0) |] =
  (3));;
  *)