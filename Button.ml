(* Referenced WorldObjectI.ml and Cell.ml while writing this module *)
(* Also referenced "The OCaml System Release 4.01 Documentation and User's Manual" 
   Graphics, Event, and List modules http://caml.inria.fr/pub/docs/manual-ocaml/ *)

open Core.Std
open WorldObjectI
open WorldObject
open ButtonI

class button p v c: button_i =
object(self)
  inherit world_object p

  val pos = p

  val b_value = v

  val mutable is_on : bool = false

  method! set_state (s:bool) = is_on <- s

  method get_state = is_on

  method get_value = b_value

  method get_category = c

  method! get_name = "button"

  method! get_pos = pos

  (* Draws the button in blue if its state is "on" or gray if its state is "off" *)
  method! draw = let draw_on_off (s:bool) =
                 (match s with
                 | true -> Draw.square pos World.button_size Graphics.blue;
		 | false -> Draw.square pos World.button_size 
                            (Graphics.rgb 100 100 100)) in
                 draw_on_off self#get_state;
		 let (x,y) = self#get_pos in
		 let (world_x,world_y)=
                     (x*World.button_size,y*World.button_size) in
                 Graphics.moveto (world_x+(World.button_size/2)) 
                                 (world_y+(World.button_size/4));
                 Graphics.set_color Graphics.white;
                 Graphics.draw_string (string_of_int v)


(***********************)
(***** Initializer *****)
(***********************)

  initializer
    self#register_handler UI.button_up self#button_click;
    self#register_handler UI.rules_updated self#update_buttons;

(*************************)
(***** Event Handler *****)
(*************************)

  method private button_click (coord:(int*int)) : unit =
    let (x,y) = coord in
    if (x > 0 && x <= World.(size*obj_width)) && 
       (y > 0 && y < World.console_height)
    then
      (let cx = x/World.button_size in
      let cy = y/World.button_size in
      let (bx,by) = self#get_pos in
      if cx = bx && cy = by then
        UI.rule_handler v c

      )

  method private update_buttons _ =
    if (c = "survive")
    then if (List.mem !UI.survival_rule b_value)
	 then is_on <- true
	 else is_on <- false;
    if (c = "birth")
    then if (List.mem !UI.birth_rule b_value)
	 then is_on <- true
	 else is_on <- false


end



