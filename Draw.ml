(* This file has been adapted from PSET 7 for use in this project *)

open Core.Std
(** A helper module for drawing graphics. *)

(** Draws a square at array position (x,y), taking into account the
    size of array squares. *)
let square ((x,y):int*int) (width:int) (color:Graphics.color): unit =
  Graphics.set_color color;
  Graphics.fill_rect (x*width) (y*width) width width
