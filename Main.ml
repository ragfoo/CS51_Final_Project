(* This file has been adapted from PSET 7 for use in this project *)

open Core.Std
open Event51

(* Use of for loop from "Real World Ocaml" by Yaron Minsky, Anil Madhavapeddy 
   and Jason Hickey, pg 146 *)
let init_buttons pos cat =   
  let (x,y) = pos in
  let px = ref x in
  let py = ref y in
  for b_value = 0 to 8 do
    if (!px mod 2 = 0) then
      let button = (new Button.button (!px,!py) b_value cat) in
      button#draw;
      px := !px + 2
  done

let draw_buttons () =
  World.indices (fun pos ->
                 let buttons = 
                   List.filter ~f:(fun objs -> objs#get_name = "button") 
                               (World.get pos) in
                 List.iter ~f:(fun objs -> objs#draw) buttons)

let draw_console_label () =
  Graphics.set_color Graphics.black;
  Graphics.moveto (World.size*World.obj_width/3) 
                  (World.console_height - (World.button_size*3/4));
  Graphics.draw_string "Survival Settings";
  Graphics.moveto (World.size*World.obj_width/3) 
                  (World.console_height - (World.button_size*11/4));
  Graphics.draw_string "Birth Settings"

(* Initializer functions *)
let random_initializer () : unit =
  UI.draw_menu ();
  World.indices (fun x -> 
		 begin
                   let (_,py) = x in
                   if py > (World.console_pos_y) then      
		   (let cell = (new WorldObject.world_object x) in
		   if World.rand 2 = 0 then cell#resurrect;
		   cell#set_red (30 + World.rand 226);
		   cell#set_green (30 + World.rand 226);
		   cell#set_blue (30 + World.rand 226))
		 end
		);
  init_buttons World.survival_b_pos "survive";
  init_buttons World.birth_b_pos "birth";
  Event51.fire_event UI.rules_updated ()

let clean_initializer () : unit =
  UI.draw_menu ();
  World.indices (fun x -> let (_,py) = x in
                   if py > (World.console_pos_y) then      
                     ignore (new WorldObject.world_object x));
  init_buttons World.survival_b_pos "survive";
  init_buttons World.birth_b_pos "birth";
  Event51.fire_event UI.rules_updated ()

		   
(* Function that is called continuously while the simulation is running. *)
let event_loop part () : unit =
  Graphics.clear_graph () ;
  if part >= 1 
  then UI.step ();
  UI.draw_menu ();
  draw_buttons ();
  draw_console_label ()

(* Parse command-line arguments. Returns the appropriate initialization
  function to run and the current part. *)
let parse_args () : (unit -> unit) * int =
  if Array.length Sys.argv = 2 then
    match Sys.argv.(1) with
    | "rand" -> random_initializer, 1
    | _  -> clean_initializer, 2
  else clean_initializer, 2


let run () : unit =
  let initialize, part = parse_args () in
  UI.run_world initialize (event_loop part)
;;

run () ;;
