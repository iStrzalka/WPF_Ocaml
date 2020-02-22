(*Twórca programu : Tomasz Nitsch*)
(*Recenzant programu : --*)

(** Represents tree in form of either Leaf or Nodes in form 
  (left child, interval of (int, int), right child, height, 
  (numbers of #int in left child, right child) *)
type t =
  | Node of t * (int * int) * t * int * (int * int)
  | Leaf

(** Used for checking overflow during addition*)
let ( +$ ) a b = 
    if a + b < 0 && (a > 0) then max_int else a + b

(** Used for checking overflow during subtraction.*)
let ( -$ ) a b = 
    if a - b > 0 && (a < 0) then min_int else a - b

(** Returns height of given tree *)
let height = function Node (_, _, _, h, _) -> h | Leaf -> 0

(** Returns the size of given interval *)
let iVal_size (min, max) =
  if max - min >= 0 then max - min +$ 1 else max_int

(** Returns # of numbers in given tree *)
let ints_in_tree = function
  | Leaf -> 0
  | Node (l, k, _, _, (ints_in_l, ints_in_r)) ->
      ints_in_l +$ ints_in_r +$ iVal_size k

(** Creates tree *)
let make l k r =
    let h = max (height l) (height r) + 1
    and numbers_below = (ints_in_tree l, ints_in_tree r) 
    in Node (l, k, r, h, numbers_below)

(** Type of outcomes of interval comparison *)
type cmp_outcome =
  | Bigger
  | Smaller
  | Overlapping

(** Compares two intervals and returns whether it is bigger/smaller 
    than first interval or is overlapping *)
let cmp (x1, y1) (x2, y2) =
  if x2 > y1 then Bigger else 
  if x1 > y2 then Smaller
  else Overlapping

(** Creates balanced tree takes tree interval and tree *)
let bal l k r =
    let hl = height l in
    let hr = height r in
    if hl > hr + 2
    then
      match l with
        | Node (ll, lk, lr, _, _) -> (
            if height ll >= height lr then make ll lk (make lr k r)
            else
                match lr with
                | Node (lrl, lrk, lrr, _, _) ->
                    make (make ll lk lrl) lrk (make lrr k r)
                | Leaf ->
                    assert false )
        | Leaf ->
          assert false
    else if hr > hl + 2 then
      match r with
      | Node (rl, rk, rr, _, _) -> (
          if height rr >= height rl then make (make l k rl) rk rr
          else
            match rl with
            | Node (rll, rlk, rlr, _, _) ->
                make (make l k rll) rlk (make rlr rk rr)
            | Leaf ->
                assert false)
      | Leaf ->
          assert false
    else make l k r

(** Returns min element from the tree *)
let rec min_elt = function
    | Node (Leaf, k, _, _, _) -> k
    | Node (l, _, _, _, _) -> min_elt l
    | Leaf -> assert false

(** Return tree after removal of min element *)
let rec remove_min_elt = function
  | Node (Leaf, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Leaf -> invalid_arg "iSet.remove_min_elt"

(** Merges t1 t2 
    Assumes t1 and t2 are balanced and every element in t2 is bigger than in t1*)
let merge t1 t2 =
    match (t1, t2) with
    | (Leaf, _) -> t2
    | (_, Leaf) -> t1
    | _ ->
        let k = min_elt t2 in
            bal t1 k (remove_min_elt t2)

(** The empty set *)
let empty = Leaf

(** Returns true if the set is empty. *)
let is_empty x = (x = Leaf)

(** Raised when add_one is executed with tree not meeting the requirements *)
exception UnpreparedFailure

(** Helper for add function takes comparator, interval and tree returns tree
    after insertion of new element Assumes interval will not overlap with any
    element in the tree Throws UnpreparedTree otherwise *)
let rec add_one iVal = function
  | Node (l, k, r, h, _) -> (
      match cmp k iVal with
      | Overlapping -> raise UnpreparedFailure
      | Smaller ->
          let nl = add_one iVal l in
            bal nl k r
      | Bigger ->
          let nr = add_one iVal r in
            bal l k nr )
  | Leaf ->
      make Leaf iVal Leaf

(** Joins left,right tree and interval into balanced tree Assumes both are
    balanced and every element in left is smaller than interval and every in
    right bigger than interval *)
let rec join l iVal r =
  match (l, r) with
  | (Leaf, _) -> add_one iVal r
  | (_, Leaf) -> add_one iVal l
  | (Node (ll, lv, lr, lh, _), Node (rl, rv, rr, rh, _)) ->
      if lh > rh + 2 then bal ll lv (join lr iVal r) else 
      if rh > lh + 2 then bal (join l iVal rl) rv rr
      else make l iVal r

(** [split x s] returns a triple [(l, present, r)], where [l] is the set of
    elements of [s] that are strictly lesser than [x]; [r] is the set of
    elements of [s] that are strictly greater than [x]; [present] is [false] if
    [s] contains no element equal to [x], or [true] if [s] contains an element
    equal to [x]. *)
let split num set =
  let rec loop = function
    | Leaf -> (Leaf, false, Leaf)
    | Node (l, (x1, x2), r, _, _) ->
      match cmp (x1, x2) (num, num) with
      | Overlapping ->
            let new_l =
                if x1 < num then add_one (x1, min x2 (num - 1)) l
                else l
            and new_r =
                if x2 > num then add_one (max x1 (num + 1), x2) r
                else r
            in 
                (new_l, true, new_r)
      | Smaller ->
          let (ll, pres, rl) = loop l in
            (ll, pres, join rl (x1, x2) r)
      | Bigger ->
          let (lr, pres, rr) = loop r in
            (join l (x1, x2) lr, pres, rr)
  in
    loop set

(** [remove (x, y) s] returns a set containing the same elements as [s], except
    for all those which are included between [x] and [y]. Assumes [x <= y]. *)
let remove (min_val, max_val) set =
  let (l, _, temp_r) = split min_val set in
  let (_, _, r) = split max_val temp_r in
  match r with 
    | Leaf -> l 
    | Node _ -> join l (min_elt r) (remove_min_elt r)


(** Finds max value of interval after merging it with all possible to merge
    with intervals in the tree *)
let rec max_value_of_merged_interval ((min_ival, max_ival) as iVal) iSet = 
  match iSet with
  | Node (l, ((_, max_node) as k), r, _, _) -> (
      match cmp (min_ival -$ 1, max_ival +$ 1) k with
      | Bigger ->  max_value_of_merged_interval iVal l
      | Smaller -> max_value_of_merged_interval iVal r
      | Overlapping ->
          if max_ival <= max_node then max_node
          else max_value_of_merged_interval iVal r )
  | Leaf ->
      max_ival

(** Finds min value of interval after merging it with all possible to merge
    with intervals in the tree *)
let rec min_value_of_merged_interval ((min_ival, max_ival) as iVal) iSet =
    match iSet with 
    | Node (l, ((min_node, _) as k), r, _, _) -> (
        match cmp (min_ival -$ 1, max_ival +$ 1) k with
        | Bigger ->  min_value_of_merged_interval iVal l
        | Smaller -> min_value_of_merged_interval iVal r
        | Overlapping ->
            if min_node <= min_ival then min_node
            else min_value_of_merged_interval iVal l )
    | Leaf ->
        min_ival

(** Returns interval after merging it with every possible interval in the set *)
let merge_with_overlapping iVal set =
  let min_val = min_value_of_merged_interval iVal set
  and max_val = max_value_of_merged_interval iVal set in
    (min_val, max_val)


(** [add (x, y) s] returns a set containing the same elements as [s], plus all
    elements of the interval [[x,y]] including [x] and [y]. Assumes [x <= y]. *)
let add iVal set =
  let merged_iVal = merge_with_overlapping iVal set in
  let set_after_removal = remove merged_iVal set in
    add_one merged_iVal set_after_removal

(** [below n s] returns the number of elements of [s] that are lesser or equal
    to [n]. If there are more than max_int such elements, the result should be
    max_int. *)
let below num set =
  let rec pom num acc iSet =
    match iSet with
      | Leaf -> acc
      | Node (l, (min_val, max_val), r, _, (ints_in_l, _)) ->
          if min_val > num then pom num acc l
          else if max_val >= num then acc +$ iVal_size (min_val, min num max_val) +$ ints_in_l
          else
              let acc2 = acc + (ints_in_l +$ iVal_size (min_val, max_val)) in
              if acc2 < 0 then max_int else pom num acc2 r
  in 
    pom num 0 set

(** [mem x s] returns [true] if [s] contains [x], and [false] otherwise. *)
let mem x set =
    let rec loop iSet = 
        match iSet with
        | Node (l, k, r, _, _) ->(
            match cmp k (x, x) with
            | Overlapping -> true
            | Smaller -> loop l
            | Bigger -> loop r)
        | Leaf -> false
    in
        loop set

(** [iter f s] applies [f] to all continuous intervals in the set [s]. The
    intervals are passed to [f] in increasing order. *)
let iter f set =
  let rec loop = function
    | Leaf -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r
  in
    loop set


(** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)], where x1 ... xN are
    all continuous intervals of s, in increasing order. *)
let fold f set acc =
    let rec loop acc = function
        | Leaf -> acc
        | Node (l, k, r, _, _) -> loop (f k (loop acc l)) r
    in
        loop acc set


(** Return the list of all continuous intervals of the given set. The returned
    list is sorted in increasing order. *)
let elements set =
  let rec loop acc = function
    | Leaf -> acc
    | Node (l, k, r, _, _) -> loop (k :: loop acc r) l
  in
    loop [] set