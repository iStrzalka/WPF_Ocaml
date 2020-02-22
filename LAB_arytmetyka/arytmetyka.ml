(*Twórca programu : Tomasz Nitsch*)
(*Recenzant programu : Hubert Badocha*)

(*========================NOWE TYPY ZMIENNYCH================================*)
(*Inicjacja wartosci - jeden przedzial (a, b) 
                       lub dwa przedzialy (-inf, a) u (b, +inf)*)
type wartosc =
  | OneBoundry of float * float
  | TwoBoundries of float * float * float * float

(*===========================KONSTRUKTORY====================================*)
(* wartosc_dokladnosc x p = x +/- p%, gdzie p > 0*)
let wartosc_dokladnosc x p =
  OneBoundry
    ( min (x *. (100.0 -. p) /. 100.0) (x *. (100.0 +. p) /. 100.0)
    , max (x *. (100.0 -. p) /. 100.0) (x *. (100.0 +. p) /. 100.0) )

(* wartosc_od_do x y = przedział (x, y) gdzie x <= y*)
let wartosc_od_do x y = OneBoundry (x, y)

(* wartosc_dokladna x = przedział (x, x) *)
let wartosc_dokladna x = OneBoundry (x, x)

(* Funkcja zwracająca czy dana liczba zawiera się w przedziale czy też nie*)
let in_wartosc boundry number =
  match boundry with
  | OneBoundry (left, right) ->
      if left <= number && number <= right then true else false
  | TwoBoundries (_, left, right, _) ->
      if number <= left || right <= number then true else false

(* min_wartosc w = najmniejsza możliwa dla podanego przedziału   *)
(* lub neg_infinity jeśli brak dolnego ograniczenia.             *)
let min_wartosc boundry =
  match boundry with
  | OneBoundry (left, _) -> left
  | TwoBoundries (_ , _, _, _) -> neg_infinity 

(* max_wartosc w = największa możliwa dla podanego przedziału    *)
(* lub infinity jeśli brak górnego ograniczenia.                 *)
let max_wartosc boundry =
  match boundry with
  | OneBoundry (_, right) -> right
  | TwoBoundries (_, _, _, _) -> infinity

(* środek przedziału od min_wartosc do max_wartosc, *)
(* lub nan jeśli min i max_wartosc nie są określone.*)
let sr_wartosc boundry = (min_wartosc boundry +. max_wartosc boundry) /. 2.0

(*===========================FUNKCJE POMOCNICZE==============================*)
(* Funkcja sprawdzająca czy któryś z przedziałów
   nie jest przedziałem (nan, nan) *)
let isnan boundry1 boundry2 =
  if
    classify_float (min_wartosc boundry1) = FP_nan
    || classify_float (min_wartosc boundry2) = FP_nan
  then true
  else false

(* Zamienia przedział na (-1) * przedział*)
let opposite boundry =
  match boundry with
  | OneBoundry (left1, right1) -> OneBoundry (-.right1, -.left1)
  | TwoBoundries (_, left1, right1, _) ->
      TwoBoundries (-.infinity, -.right1, -.left1, infinity)

(* Dodatkowa funkcja na mnożenie                             *)
(* Ponieważ według zadania inf * 0 ma się równać 0 a nie nan *)
let ( ** ) (x : float) (y : float) =
  if x = infinity && y = 0. then 0.
  else if y = infinity && x = 0. then 0.
  else x *. y

(* Zwraca minimalną wartość dla dwóch parametrów          *)
(* Jeżeli którykolwiek jest "nan'em" to zwraca drugą      *)
(* W przypadku obu parametrów będącymi nan'ami zwraca nan *)
let min_p x y =
  if classify_float x = FP_nan then y
  else if classify_float y = FP_nan then x
  else if x < y then x
  else y

(* Analogicznie do powyższej funkcji, lecz ta zwraca max dwóch parametrów *)
let max_p x y =
  if classify_float x = FP_nan then y
  else if classify_float y = FP_nan then x
  else if x < y then y
  else x

(* Funkcje pomocnicze dla mnożenia *)
(* Zwracają min/max_p z każdego możliwego mnożenia 
    z pierwszej pary liczb z drugą parą *)
let min_pom a b c d = min_p (min_p (a ** c) (a ** d)) (min_p (b ** c) (b ** d))
let max_pom a b c d = max_p (max_p (a ** c) (a ** d)) (max_p (b ** c) (b ** d))

(* Funkcja pomocnicza dla mnożenia *)
(* W przypadku gdy przedział jest podwójny 
   sprawdza czy pierwszy przedział nie nachodzi na drugi*)
(* Jeśli tak zwraca jeden przedział (-inf, inf)
         nie to zwraca początkowy przedział*)
let is_a_boundry boundry =
  match boundry with
  | OneBoundry (_, _) -> boundry
  | TwoBoundries (_, left1, right1, _) ->
      if right1 <= left1 then OneBoundry (-.infinity, infinity) else boundry

(* Funkcja łącząca dwa przedziały w jedno *)
let join_boundries boundry1 boundry2 =
  let pom boundry1 boundry2 = (*Pomocnicza tylko po to by przedziały
           były posortowane po mniejszej wartości minimalnej (-> 140linijka)*)
    match (boundry1, boundry2) with
    | OneBoundry (left1, right1), OneBoundry (left2, right2) -> (
      match min_wartosc boundry2 <= max_wartosc boundry1 with
      | true -> (*Jeden przedział nachodzi na drugi*)
          OneBoundry
            ( min_p(min_wartosc boundry1) (min_wartosc boundry2)
            , max_p (max_wartosc boundry1) (max_wartosc boundry2) )
      | false -> (*Oba przedziały rozłączne*)
          TwoBoundries
            (-.infinity, max_wartosc boundry1, min_wartosc boundry2, infinity)
      )
    | OneBoundry (left1, right1), TwoBoundries (_, left2, right2, _) ->
        if right1 <= left2 then boundry2      (*Jeden przedział w drugim*)
        else if right2 <= left1 then boundry2 (*Jeden przedział w drugim*)
        else if left2 <= right1 then (*Jeden przedział niecałkowicie w drugim*)
          is_a_boundry (TwoBoundries (-.infinity, right1, right2, infinity))
        else is_a_boundry (TwoBoundries (-.infinity, left2, left1, infinity))
    | TwoBoundries (_, left2, right2, _), OneBoundry (left1, right1) ->
        if right1 <= left2 then boundry2      (*Jeden przedział w drugim*)
        else if right2 <= left1 then boundry2 (*Jeden przedział w drugim*)
        else if left2 <= right1 then (*Jeden przedział niecałkowicie w drugim*)
          is_a_boundry (TwoBoundries (-.infinity, right1, right2, infinity))
        else is_a_boundry (TwoBoundries (-.infinity, left2, left1, infinity))
    | TwoBoundries (_, left1, right1, _), TwoBoundries (_, left2, right2, _)
      ->
        is_a_boundry
          (TwoBoundries
             (-.infinity, max_p left1 left2, min_p right1 right2, infinity)) in
  if min_wartosc boundry1 <= min_wartosc boundry2 then pom boundry1 boundry2
  else pom boundry2 boundry1 (*Sort po mniejszej min_wartosc w przedziale*)

(* Funkcja pomocnicza dla inverse_boundries                      *)
(* Funkcja odwracająca dokładnie JEDEN przedział przez siebie    *)
(* W tłumaczeniu matematycznym wynikiem funkcji jest 1/przedział *)
let inverse_boundry boundry =
  if isnan boundry (OneBoundry (0.0, 0.0)) then OneBoundry (nan, nan)
  else if boundry = OneBoundry (0.0, 0.0) then OneBoundry (nan, nan)
  else
    match (*Sprawdza czy jest ujemne i dodatnie zero w przedziale*)
      (in_wartosc boundry (-.epsilon_float), in_wartosc boundry epsilon_float)
    with (*Nie ma ani jednego ani drugiego*)
    | false, false ->
        OneBoundry
          ( min_p(1.0 /. min_wartosc boundry) (1.0 /. max_wartosc boundry)
          , max_p (1.0 /. min_wartosc boundry) (1.0 /. max_wartosc boundry) )
    (*Jest 'dodatnie' 0*)
    | false, true -> OneBoundry (1.0 /. max_wartosc boundry, infinity)
    (*Jest 'ujemne' 0*)
    | true, false -> OneBoundry (-.infinity, 1.0 /. min_wartosc boundry)
    (*Jest i 'ujemne' 0 i 'dodatnie' 0*)
    | true, true ->
        TwoBoundries
          ( -.infinity
          , min_p(1.0 /. min_wartosc boundry) (1.0 /. max_wartosc boundry)
          , max_p (1.0 /. min_wartosc boundry) (1.0 /. max_wartosc boundry)
          , infinity )

(* Funkcja odwracająca podany przedział                                   *)
(* Jeżeli przedział jest pojedyńczy to korzysta z funkcji inverse_boundry *)
(* Jeżeli podwójny, to odwraca pojedyńcze przedziały, odwraca je          *)
(* przy pomocy inverse_boundy i łączy w jedno                            *)
let inverse_boundries boundry =
  match boundry with
  | OneBoundry (_, _) -> inverse_boundry boundry
  | TwoBoundries (_, left1, right1, _) ->
      join_boundries
        (inverse_boundry (OneBoundry (-.infinity, left1)))
        (inverse_boundry (OneBoundry (right1, infinity)))

(*=========================OPERACJE NA PRZEDZIAŁACH=========================*)
(*Funkcja zwracająca dodanie dwóch przedziałów*)
let plus boundry1 boundry2 =
  if isnan boundry1 boundry2 then OneBoundry (nan, nan)
  else
    match (boundry1, boundry2) with (*Sprawdza który przypadek jest*)
    | OneBoundry (left1, right1), OneBoundry (left2, right2) ->
        (*(a, b) + (c, d) = (a + c, b + d)*)
        OneBoundry (left1 +. left2, right1 +. right2)
    | OneBoundry (left1, right1), TwoBoundries (_, left2, right2, _) ->
        (*(a, b) + (-inf, c, d, inf) = (-inf, a + c, b + d, inf)*)
        if right1 +. left2 >= left1 +. right2 then
          OneBoundry (-.infinity, infinity)
        else
          TwoBoundries (-.infinity, right1 +. left2, left1 +. right2, infinity)
    | TwoBoundries (_, left2, right2, _), OneBoundry (left1, right1) ->
        (*(-inf, a, b, inf) + (c, d) = (-inf, a + c, b + d, inf)*)
        if right1 +. left2 >= left1 +. right2 then
          OneBoundry (-.infinity, infinity)
        else
          TwoBoundries (-.infinity, right1 +. left2, left1 +. right2, infinity)
    | TwoBoundries (_, _, _, _), TwoBoundries (_, _, _, _) ->
        (*(-inf, a, b, inf) + (-inf, c, d, inf) = (-inf, inf)
          bo dla każdej małej liczby moge znaleźć taką dużą liczbe że 
          x + y c R*)
        OneBoundry (-.infinity, infinity)

(*Funkcja zwracająca odjęcie pierwszego przedziału od drugiego*)
let minus boundry1 boundry2 = plus boundry1 (opposite boundry2)

(*Funkcja rekurencyjna zwracająca przemnożenie dwóch przedziałów*)
let rec razy boundry1 boundry2 =
  if isnan boundry1 boundry2 then OneBoundry (nan, nan)
  else if boundry1 = OneBoundry (0.0, 0.0) || boundry2 = OneBoundry (0.0, 0.0)                                                           
  then OneBoundry (0.0, 0.0) (*Przemnożenie przez 0 daje 0*)
  else
    match (boundry1, boundry2) with
    | OneBoundry (left1, right1), OneBoundry (left2, right2) ->
        OneBoundry (*Przemnożenie każdej pary początku i końcu przedziału
                     przez drugą pare początku i końca i wyciągnięcie
                     najmniejszego i największego iloczynu jako 
                     końce przedziału wynikowego                          *)
          (min_pom left1 right1 left2 right2, max_pom left1 right1 left2 right2)
    | OneBoundry (left1, right1), TwoBoundries (_, left2, right2, _) ->
        join_boundries (*Rozbicie podwójnego przedziału na dwa małe*)
          (razy boundry1 (OneBoundry (-.infinity, left2)))
          (razy boundry1 (OneBoundry (right2, infinity)))
    | TwoBoundries (_, left2, right2, _), OneBoundry (left1, right1) ->
        join_boundries (*Rozbicie podwójnego przedziału na dwa małe*)
          (razy boundry2 (OneBoundry (-.infinity, left2)))
          (razy boundry2 (OneBoundry (right2, infinity)))
    | TwoBoundries (_, left1, right1, _), TwoBoundries (_, left2, right2, _)
      -> (*Rozbicie podwójnych przedziałów na dwa małe 
                       i przemnożenie ich przez siebie*)
        join_boundries 
          (join_boundries
             (razy
                (OneBoundry (-.infinity, left1))
                (OneBoundry (-.infinity, left2)))
             (razy
                (OneBoundry (-.infinity, left1))
                (OneBoundry (right2, infinity))))
          (join_boundries
             (razy
                (OneBoundry (right1, infinity))
                (OneBoundry (-.infinity, left2)))
             (razy
                (OneBoundry (right1, infinity))
                (OneBoundry (right2, infinity))))

(*Funkcja zwracająca podzielenie przedziału przez przedział*)
let podzielic boundry1 boundry2 = razy boundry1 (inverse_boundries boundry2)
