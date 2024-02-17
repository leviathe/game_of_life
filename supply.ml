
# use "topfind";;
# require "graphics";;

# use "list_tools.ml";;

open Graphics;;
let open_window size = open_graph (" " ^ string_of_int size ^ "x" ^ string_of_int(size+20));;

(* Draw_cell *)

let draw_cell (x,y) size color =
  let x = x+1 and y = y+ 1 in
  fill_rect x y size size color;;


(* Draw_board *)

let cell_color e =
  match e with
  |0 -> white
  |_ -> black;;


let draw_board board cellsize =
  let max = lengh board in (* On recupere la longueur d'un cote *)
  let rec rdw max (x,y) =
    match (x,y) with
    |(max,_)-> () (* renvoyer type unit*)
    |(_,max) ->rdw max ((x+1),0) (* changer de colonne *)
    |(_,_) -> ( draw_cell (x,y) cellsize (cell_color (get_cell (x,y)) ; rdw max (x,y))
                in rdw max (0,0);;
