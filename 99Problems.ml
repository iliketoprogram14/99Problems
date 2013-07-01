(* EXCEPTIONS *)
exception Empty_list;;
exception One_element;;
exception Not_in_bounds;;
exception Empty_branch;;
exception NotPossible;;
exception Need_to_implement;;

(* HELPFUL DATA TYPES *)

type comparable = Less | Greater | Equal
let compare a b =
  if a < b then Less
  else if a > b then Greater
  else Equal
;;


(* LIST HELPERS *)
let rec map (f: 'a -> 'b) lst : 'b list = 
  match lst with
  | [] -> []
  | h::t -> (f h)::(map f t)
;;

let rec reduce_right f lst b =
  match lst with
  | [] -> b
  | h::t -> f h (reduce_right f t b)
;;

let rec reduce f lst b =
  match lst with
  | [] -> b
  | h::t -> reduce f t (f h b)
;;


(* TREE TYPES AND HELPERS *)
type 'a tree = Empty | Branch of 'a * 'a tree * 'a tree

type color = Red | Black;;
type 'a rb_tree = Empty_rb | Branch_rb of 'a * color * 'a rb_tree * 'a rb_tree

type emptiness = Both_Empty | Left_Empty | Right_Empty | None_Empty;;

let left t =
  match t with
  | Empty -> Empty
  | Branch(v,l,r) -> l
;;

let right t =
  match t with
  | Empty -> Empty
  | Branch(v,l,r) -> r
;;

let value t =
  match t with
  | Empty -> raise Empty_branch
  | Branch(v,l,r) -> v
;;

let is_empty t = 
  match t with
  | Empty -> true
  | _ -> false
;;

let is_both_empty l r =
  match (is_empty l, is_empty r) with
  | (true, true) -> Both_Empty
  | (true, false) -> Left_Empty
  | (false, true) -> Right_Empty
  | (false, false) -> None_Empty
;;

(* INSERT *)
let rec insert value t =
  match t with
  | Empty -> Branch(value, Empty, Empty)
  | Branch(v,l,r) -> 
      match compare value v with
      | Less -> Branch(v, (insert value l), r)
      | Greater -> Branch(v, l, (insert value r))
      | Equal -> Branch(v,l,r)
;;

(* DELETE AND ITS HELPERS *)
let rec delete_min t =
  match t with
  | Empty -> raise NotPossible
  | Branch(v,Empty,r) -> r
  | Branch(v,l,r) -> Branch(v, (delete_min l), r)
;;

let find_min tree =
  let rec min t cur_min =
    match t with
    | Empty -> cur_min
    | Branch(v,l,r) -> min l v in
    min tree (value tree)
;;

let rec delete value t =
  match t with
  | Empty -> Empty
  | Branch(v,l,r) ->
      match compare value v with
      | Less -> delete value l
      | Greater -> delete value r
      | Equal -> 
          match (is_both_empty l r) with
          | Both_Empty -> Empty
          | Left_Empty -> r
          | Right_Empty -> l
          | None_Empty -> Branch((find_min r), l, (delete_min r))
;;

  (* RED BLACK TREES *)
let rec rbtree_to_tree rbt =
  match rbt with
  | Empty_rb -> Empty
  | Branch_rb(v,c,l,r) -> Branch(v, (rbtree_to_tree l), (rbtree_to_tree r))
;;

let tree_to_rbtree t =
  let rec t_to_rbt oldt newrbt =
    raise Need_to_implement in
raise Need_to_implement
;;    
            
let rec adjust value t =
  raise Need_to_implement
;;

(* TEST OBJECTS/DATA STRUCTURES *)
let testlist = [1;2;3;4;5;6;7;8;9;10];;
let palinlist = [1;2;3;4;5;4;3;2;1];;


(* 1. Find the last element of a list *)
let last_elem lst : 'a option = 
  match List.rev lst with
  | [] -> None
  | hd::tl -> Some hd
;;

let rec last_elem2 lst : 'a option=
  match lst with
  | [] -> None
  | hd::[] -> Some hd
  | hd::tl -> last_elem2 tl
;;


(* 2. Find the last but one element of a list (second to last) *)
let rec sec_to_last lst : 'a option =
  match lst with
  | [] -> None
  | hd::[] -> None
  | hd::(hd1::[]) -> Some hd
  | _::tl -> sec_to_last tl
;;


(* 3. Find the kth element of a list (not 0-indexed) *)
let rec kth lst k =
  match (k, lst) with
  | (_, []) -> None
  | (1, hd::tl) -> Some hd
  | (_, hd::tl) -> kth tl (k-1)
;;


(* 4. Find the number of elements of a list *)
let rec length lst =
  match lst with
  | [] -> 0
  | _::t -> 1 + (length t)
;;

let length2 lst = reduce (fun x y -> 1 + y) lst 0;;


(* 5. Reverse a list *)
let rev lst = reduce (fun x y -> x::y) lst [];;


(* 6. Find out whether a list is a palindrome *)
let palindrome lst =
  let rec palin_helper l1 l2 =
    match l1, l2 with
    | ([], []) -> true
    | (hd::tl, hd2::tl2) -> hd = hd2 && (palin_helper tl tl2)
    | _ -> raise NotPossible in
  palin_helper lst (rev lst)
;;


(* 7. Flatten a nested list structure *)
let flatten lst = reduce (fun x y -> y@x) lst [];;


(* 8. Eliminate consecutive duplicates of list elements *)
let rec elim_dup lst =
  match lst with
  | [] -> []
  | hd::[] -> [hd]
  | hd::(hd1::tl as tl2) -> 
      match hd=hd1 with
      | true -> elim_dup tl2
      | false -> hd::(elim_dup tl2)
;;


(* 9. Pack consecutive duplicates of list elements into sublists.  
 * If a list contains repeated elements, they should be placed in 
 * separate sublists *)
let dup (lst: 'a list) : 'a list list =
  let rec dup_helper (l: 'a list) (sublsts: 'a list list) : 'a list list =
    match l, sublsts with
    | [], _ -> sublsts
    | hd::tl, [] -> dup_helper tl [[hd]]
    | hd::tl, []::sublist_tl -> dup_helper tl ([hd]::sublist_tl)
    | hd::tl, (hd1::tl1)::sublist_tl ->
      if hd = hd1 then dup_helper tl ((hd::hd1::tl1)::sublist_tl)
      else dup_helper tl ([hd]::sublsts) in
  List.rev (dup_helper lst [])
;;


(* 10. Run-length encoding of a list *)
let run_len_encode (lst: 'a list) : (int * 'a) list =
  List.map (fun x -> (List.length x, List.hd x)) (dup lst)
;;


(* 11. Run-length encoding of a list, except those with no duplicates aren't in tuples *)

(* IMPOSSIBLE BECAUSE LISTS CAN ONLY HAVE ELEMENTS OF ONE TYPE *)


(* 12. Decode run-length encoded list *)
let run_len_decode lst : 'a list =
  let rec helper num char =
    match num with
    | 0 -> []
    | _ -> char::(helper (num - 1) char) in
  flatten (List.map (fun (a,b) -> helper a b) lst)
;;


(* 13. Direct Run Length Encoding *)
let direct_run_len_encode lst =
  let rec encode l cur_tuple last =
    let (a,b) = cur_tuple in
    match l with
    | [] -> [cur_tuple]
    | hd::tl ->
        match (hd = last) with
        | true -> encode tl (a+1, b) last
        | false -> cur_tuple::(encode tl (1, hd) hd) in
    match lst with
    | [] -> []
    | hd::tl -> encode tl (1,hd) hd
;;
        

(* 14. Duplicate the elements of a list *)
let duplicate lst = flatten (List.map (fun x -> [x]@[x]) lst);;


let duplicate2 lst = reduce (fun x y -> y@(x::[x])) lst [];;


(* 15. Replicate the elements of a list n times *)
let replicate lst num = 
  let rec repeater n char =
    match n with
    | 0 -> []
    | _ -> char::(repeater (n - 1) char) in
    reduce (fun x y -> y@(repeater num x)) lst []
;;


(* 16. Drop every nth element from a list *)
let drop lst n =
  let rec drop_helper l num =
    match l with
    | [] -> []
    | h::t ->
      match num with
      | 1 -> drop_helper t n
      | _ -> h::(drop_helper t (num - 1)) in
  drop_helper lst n
;;


(* 17. Split list into two parts, specified by length of first list given *)
let rec split lst n =
  match lst with
  | [] -> (lst,[])
  | h::t ->
    match n with
    | 1 -> ([h], t)
    | _ ->
      let (a,b) = split t (n - 1) in
      (h::a, b)
;;


(* 18. Extract a slice from a list (inclusive) *)
let extract lst start endn =
  let rec slice l en =
    match en with
    | 1 -> l
    | _ -> 
      match l with
      | [] -> []
      | h::t -> slice t (en - 1) in
  let rec extract_to_start l st en =
    match l with
    | [] -> l
    | h::t ->
      match st with
      | 1 -> slice l en
      | _ -> h::(extract_to_start t (st - 1) en) in
  extract_to_start lst start endn
;;
        

(* 31. Determine whether a given number is prime *)
let is_prime n =
  let rec prime n next factor_list =
    let new_factorlist = next::factor_list in
    if n/2 >= next then
      (match (n mod next) with
      | 0 -> 
             (match List.filter (fun x -> x * next == n) new_factorlist with
             | [] -> prime n (next + 1) new_factorlist
             | _ -> false )
      | _ -> prime n (next + 1) factor_list)
    else
      true in
  prime n 2 [1]
;;


(* 32. Determine GCF of two positive integers *)
let gcf n1 n2 =
  let rec gcf_helper factor curr =
    if curr > n1 || curr > n2 then factor
    else
      match (n1 mod curr = 0) && (n2 mod curr = 0) with
      | true -> gcf_helper curr (curr + 1)
      | false -> gcf_helper factor (curr + 1) in
  gcf_helper 1 2
;;


(* 33. Determien whether two positive integers are corprime (their GCF is 1) *)
let coprime n1 n2 = if gcf n1 n2 > 1 then false else true;;


(* 34.  Determine Euler's totient function phi(m) - basically it returns the number of coprime numbers less than m *)
let phi m =
  let rec phi_helper curr phi_num =
    match (curr >= m) with
    | true -> phi_num
    | false ->
        match (coprime m curr) with
        | true -> phi_helper (curr + 1) (phi_num + 1)
        | false -> phi_helper (curr + 1) phi_num in
    phi_helper 2 1
;;


(* 35/36. Determine the prime factors of a given positive integer *)
let prime_factors n =
  let rec prime_helper curr factor_list =
    if curr <= n then
      match (n mod curr = 0) && (is_prime curr) with
      | true -> prime_helper (curr + 1) (curr::factor_list)
      | false -> prime_helper (curr + 1) factor_list
    else factor_list in
  List.rev (prime_helper 2 [1])
;;


(* 39. A list of prime numbers *)
let rec prime_list up_to_n = 
  match up_to_n with
  | 0 -> []
  | _ ->
    if is_prime up_to_n then (prime_list (up_to_n - 1))@[up_to_n]
    else prime_list (up_to_n - 1)
;;


(* 40. Return two primes that add up to given even positive integer *)
let goldbach n =
  let lst = prime_list n in
  let (less, great) = split lst ((List.length lst) / 2) in
  let greater = List.rev great in
  let rec goldbach_helper less_lst great_lst =
    match less_lst with
    | [] -> (List.nth great_lst (List.length great_lst - 1), List.hd great_lst)
    | h::t ->
      let cur_less = List.hd less_lst in
      let cur_greater = List.hd great_lst in
      match (compare (cur_less + cur_greater) n) with
      | Less -> goldbach_helper (List.tl less_lst) great_lst
      | Greater -> goldbach_helper less_lst (great_lst)
      | Equal -> (cur_less, cur_greater) in
  goldbach_helper less greater
;;
  

(* 41. return a list of even positive integers and their Goldbach compositions given a range *) 
let rec evens lower upper =
  match (lower = upper + 1) with
  | true -> []
  | false ->
    if lower mod 2 = 0 then lower::(evens (lower + 1) upper)
    else evens (lower + 1) upper
;;


let goldbach_range lower upper =
  let even_lst = evens lower upper in
    List.map (fun x -> let (prime_l, prime_g) = goldbach x in
                (x, prime_l, prime_g))
      even_lst
;;


(* 54. determine whether a given tuple is a tree *)
(* not really possible with lists because lists must be homogenous... *)
let rec is_tree lst =
  match lst with
  | [] -> true
  | v::l::r::[] -> (is_tree l) && (is_tree r)
  | _ -> false
;;

let is_tree lst = 
  List.for_all (fun x -> List.length x = 3 || List.length x = 1) lst
;;


(* 55. Construct a completely balanced binary tree *)
let rec cbal_tree num_of_subtrees =
  match num_of_subtrees with
  | 0 -> Empty
  | _ -> 
    let split_num = num_of_subtrees/2 in
    if num_of_subtrees mod 2 = 0 then Branch(0, (cbal_tree (split_num - 1)), (cbal_tree split_num))
    else Branch(0, (cbal_tree split_num), (cbal_tree split_num))


(* 56. Determine whether a given binary tree is symmetric or not *)
let is_symmetric t = 
  let rec symmetric l r =
    (* figure out if the left subtree and right subtrees are the same via bools *)
    let l_is_empty = is_empty l in
    let r_is_empty = is_empty r in
      match (l_is_empty = r_is_empty) with
      | false -> false (* they are not symmetric *)
      | true -> (* they are symmetric *)
        if l_is_empty = true then true (* reached Empty, so no more calls *)
        else (symmetric (left l) (left r)) && (symmetric (right l) (right r))
  in
    match t with
    | Empty -> true
    | Branch(v, l, r) -> symmetric l r
;;


(* 57. Construct a BST from a list of integers *)
let lst_to_bst lst =
  reduce (fun x y -> insert x y) lst Empty
;;


(* 61. Count the leaves of a binary tree *)
let leaf_counter tree =
  let rec lc_rec t num : 'b = 
    match t with
    | Empty -> 0
    | Branch(v,l,r) -> (lc_rec l num + 1) + (lc_rec r num + 1) in
    lc_rec tree 0
;;


(* 62. Count the leaves of a binary tree in a list *)
let rec lc t =
    match t with
    | Empty -> []
    | Branch(v,l,r) -> (lc l)@[v]@(lc r)
;;


(* 62B. Collect internal nodes in a list *)
let rec lci t : 'a list =
  match t with
  | Empty -> []
  | Branch(v,l,r) ->
      if not(is_empty l && is_empty r) then
        (lci l)@[v]@(lci r)
      else []
;;
      
