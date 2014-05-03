(* This file has been adapted from PSET 7 for use in this project *)

open Core.Std
open Event51

(** world_object_i is the super type of all objects in the world. Note that
    world_object_i is defined with "class type" making it an interface. *)
class type world_object_i =
object
  (** The name of the object. get_name is used for displaying purposes. *)
  method get_name : string

  (** The position of the object.  This is the primary way to tell if one
      object is on the same square as another *)
  method get_pos : int*int
			 
  (** set the name of the rule set you want the game to use **)
  method set_rule : string->unit

  (** get the name of the rule set **)
  method get_rule : string

  (* set the state of the cell *)
  method set_state : bool -> unit

  (* set the color of the cell *)
  method set_color : Graphics.color -> unit

  (** How to draw the object. *)
  method draw : unit

  (** The z-axis used for drawing the object. Objects with higher z-axis will be
   * drawn on top of objects with lower z-axis. *)
  method draw_z_axis : int

  method set_red : int->unit
  method set_blue : int->unit
  method set_green : int->unit

  method get_red : int
  method get_blue : int
  method get_green : int

  method set_next_red : int->unit
  method set_next_blue : int->unit
  method set_next_green : int->unit

  (* create a list of living neighbor ints that will induce birth *)
  method set_birth_rule : int list -> unit

  (* create a list of living neighbor ints that will maintain survival *)
  method set_survival_rule : int list -> unit

  method get_birth_rule : int list

  method get_survival_rule : int list

  (******************)
  (***** Events *****)
  (******************)
  (** Register a handler with an event.  This handler will be removed if this
      object dies. *)
  method private register_handler : 'a. 'a Event51.event -> ('a -> unit) -> unit

  method private draw_square :
    Graphics.color -> unit


  (** kill the object. *)
  method die : unit

  (** resurect the object. *)
  method resurrect : unit

  method is_alive : bool

  method private set_will_live : unit
  method set_will_die : unit
  method private will_live : bool

  method private living_neighbors : int

  method neighbors : world_object_i list

  method neighbors : world_object_i list
end
