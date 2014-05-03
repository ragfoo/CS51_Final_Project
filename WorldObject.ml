(* This file has been adapted from PSET 7 for use in this project *)

open Core.Std
open WorldObjectI
open Event51

(** An abstract implementation of world_object_i that provides some helper
    functionality.

    All world_object objects add themselves to the world at point initial_p upon
    creation. *)

let color_of_life = Graphics.red
let color_of_death = Graphics.black

class world_object (initial_p:int*int) : world_object_i =
object (self)

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  val mutable pos : int*int = initial_p

  val mutable next_state = false

  val mutable next_color = color_of_death

  val mutable alive = false
  val mutable color = color_of_death

  val mutable red = 200
  val mutable blue = 200
  val mutable green = 200

  val mutable next_red = 0
  val mutable next_blue = 180
  val mutable next_green = 0

  val mutable next_birth_color = (Graphics.rgb 0 0 180)

  val mutable rule = "Game of Life"

  val mutable survival_rule = []

  val mutable birth_rule = []
 
  (***********************)
  (***** Initializer *****)
  (***********************)

  initializer
    World.add pos (self :> world_object_i) ;
    self#register_handler World.calculate_event self#calculate;
    self#register_handler World.action_event self#do_action;
    self#register_handler UI.rules_updated self#update_rules;
    self#register_handler UI.color_changed self#update_birth_color;
    self#register_handler UI.board_cleared self#clear_board;

  (**************************)
  (***** Event Handlers *****)
  (**************************)
  method private calculate _ =
    match self#get_rule with
    |_ -> self#game_of_life_rule

  method private do_action _ =
    match self#get_rule with
    |_ -> self#game_of_life_action

  method private update_rules _ =
    survival_rule <- !UI.survival_rule;
    birth_rule <- !UI.birth_rule

  method private clear_board _ =
    self#set_will_die;
    self#die;
    next_red <- !UI.draw_red;
    next_blue <- !UI.draw_blue;
    next_green <- !UI.draw_green;
    red <- !UI.draw_red;
    blue <- !UI.draw_blue;
    green <- !UI.draw_green

  method private update_birth_color _ =
    if not alive
    then
      begin
    	next_red <- !UI.draw_red;
	next_blue <- !UI.draw_blue;
	next_green <- !UI.draw_green;
	red <- !UI.draw_red;
	blue <- !UI.draw_blue;
	green <- !UI.draw_green
      end

  (**************************)
  (***** Helper Methods *****)
  (**************************)

  method private set_pos (p:int*int) : unit =
    if true then begin
      World.remove_must_exist pos (self :> world_object_i) ;
      World.add p (self :> world_object_i) ;
      pos <- p
    end else
      ()

  method set_rule r = rule <- r

  method get_rule = rule

  method set_birth_rule ls = birth_rule <- ls

  method set_survival_rule ls = survival_rule <- ls

  method get_birth_rule = birth_rule

  method get_survival_rule = survival_rule

  (*******************************)
  (***** WorldObject Methods *****)
  (*******************************)

  method private register_handler
    : 'a. 'a Event51.event -> ('a -> unit) -> unit =
    (fun e f -> ignore (Event51.add_listener e f))

  method private draw_square color =
    Draw.square pos World.obj_width color

  method get_name = "cell"

  method get_pos = pos

  method draw = self#draw_square self#get_color

  method draw_z_axis = 1

  method set_red r = red <- r
  method set_blue b = blue <- b
  method set_green g = green <- g

  method get_red = red
  method get_blue = blue
  method get_green = green

  method set_next_red r = next_red <- r
  method set_next_blue b = next_blue <- b
  method set_next_green g = next_green <- g

  method private get_color = color

  method die = alive <- false; 
	       color <- color_of_death;

  method resurrect = 
    if alive
    then
      begin
	red <- next_red;
	green <- next_green;
	blue <- next_blue
      end;
    alive <- true; 
    color <- Graphics.rgb next_red next_green next_blue;



  method is_alive = alive

  method set_state (s:bool) = alive <- s

  method set_color (c:Graphics.color) = color <- c

  method private set_will_live = 
    next_state <- true
  
  method set_will_die = 
    next_state <- false


  method private will_live = next_state
			    
  method neighbors = List.filter (World.objects_within_range self#get_pos 1) ~f:(fun x -> x#get_pos <> self#get_pos)
  
  method private living_neighbors = List.fold_right self#neighbors ~f:(fun x y -> if x#is_alive then y+1 else y) ~init:0


  (*******************************)
  (*****       RULES         *****)
  (*******************************)

  method private game_of_life_rule = 
    if self#is_alive
    then 
      begin
	if List.mem survival_rule self#living_neighbors
	then 
	  begin
	    self#set_will_live;
	    if red < 180 then self#set_next_red (min (red + 5) 120 );
	    if green < 180 then self#set_next_green (min (green + 5) 120);
	    if blue < 180 then self#set_next_blue (min (blue + 5) 120)
	  end
	else self#set_will_die
      end 
    else 
      begin
	if List.mem birth_rule self#living_neighbors  
	then 
	  begin
	    self#set_will_live;
	    next_red <- !UI.draw_red;
	    next_blue <- !UI.draw_blue;
	    next_green <- !UI.draw_green;
	    red <- !UI.draw_red;
	    blue <- !UI.draw_blue;
	    green <- !UI.draw_green
	  end
	else self#set_will_die
      end


  method private game_of_life_action =
    if self#will_live
    then self#resurrect
    else self#die

								   
end

