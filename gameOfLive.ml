#use "topfind";;
#require "graphics";;

#use "list_tools.ml";;
#use "supply.ml";;

open Graphics;;


(* rules0 *)

let rules0 cell near =
  match (cell,near) with
  |(0,3) -> 1
  |(1,near) when near = 2 ||  near = 3 -> 1
  |(_,_) -> 0;;


(* count_neighbours *) (* Pb dans les coins et sur les bords avec nth*)

let count_neighbours (x,y) board  =
  let lu = if get_cell((x-1),(y-1)) board = 1 then 1
           else 0 in
  let mu = if get_cell((x-1),y) board = 1 then 1
           else 0 in
  let ru = if get_cell((x-1),(y+1)) board = 1 then 1
           else 0 in
  let lm = if get_cell((x),(y-1)) board = 1 then 1
           else 0 in
  let rm = if get_cell((x),(y+1)) board = 1 then 1
           else 0 in
  let lb = if get_cell((x+1),(y-1)) board = 1 then 1
           else 0 in
  let mb = if get_cell((x+1),(y)) board = 1 then 1
           else 0 in
  let rb = if get_cell((x+1),(y+1)) board = 1 then 1
           else 0 in
  lu + mu + ru + lm + rm + lb + mb + rb;;


(* seed_life *) (*NON TESTE*)
(* Pas sur de l'implementation de random *)

let rec seed_life board size nb_cell =
  if nb_cell = 0 then board
  else
    seed_life (put_cell 1 (Random.int(size), Random.int(size)) board) size (nb_cell - 1);;


(* new_life *) (* NON TESTE *)
let new_board board size nb_cell =
  seed_life
