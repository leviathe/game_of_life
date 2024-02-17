(* length *)

let rec length l =
match l with
|(_::[]) -> 1
|(_::l) -> 1+length l ;; (*Attention au cas []*)


(* i-ème *)

let nth n l =
if n < 0 then invalid_arg("nth: index must be a natural")
else
let rec rec_nth l n =
match (l,n) with
|([],n) when n > 0 -> failwith ("nth: list is too short")
|(e::l,n)  when n = 0 -> e
|(_::l,_) -> rec_nth l (n-1)
in rec_nth l n;;


(* is_positive *)

let rec is_pos l = 
match l with
[] -> true
|e::r when e >= 0 -> is_pos r
|_ -> false;;


(* get_max *)

let get_max l = (* vérification que l n'est pas vide *)
match l with
[] -> invalid_arg("get_max: empty list")
|e1::r ->
let rec rec_gm l emax =
match l with
[] -> emax
|e::r when e >= emax -> rec_gm r e
|_::r -> rec_gm r emax
in rec_gm r e1;;


(* inti_list *)

let init_list n e = (* vérification que n est inférieur 0 *)
if n < 0 then invalid_arg("init_list: n must be a natural")
else
  let rec repeat x e =
    match x with
    |0 -> []
    |x -> e :: repeat (x-1) e
  in repeat n e;;


(* append *)

let rec append l1 l2 =
  match l1 with
    [] -> l2
   |(e::r) -> e:: append r l2;;


(* put_list *)

let rec put_list v i l =
    match (i,l) with
    |(i,[]) when i > 0 -> []
    |(0,e::r) -> v :: r
    |(_,e::r) -> e:: put_list v (i-1) r ;;


(* init_board *)

let rec init_board (l,c) e =
  match l with
  | 0 -> []
  | _ -> (init_list c e) :: init_board((l-1),c) e;;


(* is_board *)

let is_board board =
  let rec rec_is_board board ref_val =
    match board with
    |[] -> true
    |(l::board) when length l = ref_val -> rec_is_board board ref_val
    |_ -> false
  in rec_is_board board (length (nth 1 board));;


(* print_board *)


(* fonction principale *)

let print_board board =
   let rec print_element l =
    match l with
      [] -> print_newline ()
     |(e::l) -> (print_char(e) ; print_element l) in
  let rec print_board board =
    match board with
      [] -> ()
     |(e::l) -> (print_element e;print_board l)
  in print_board board ;;


(* récupérer la valeur dans la matrice *)

let get_cell (x,y) board = nth y (nth x board);;


(* put_cell *)

let put_cell v (x,y) board =
  let old_element = nth x board in (* prendre l'élément l à la x place de la 'histogramme  *)
  let new_element = put_list v y old_element in (* changer l'élément e à la y place de l *)
put_list new_element x board;; (* remettre l dans la matrice *)
