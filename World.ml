(* This file has been adapted from PSET 7 for use in this project *)

open Core.Std
open WorldObjectI
open ButtonI
open Event51


(*************************)
(***** Configuration *****)
(*************************)

(** The icon width for each position. *)

let obj_width : int = 2

(** A random number generator for the world.  All random events happening in
   the world should use this random number generator. *)
let rand : int -> int = Random.self_init () ; Random.int

(** The world has size x size positions *)

let size : int = 250

(* Buton size and console height in pixels *)
let button_size : int = 20

let console_height : int = 5*button_size

(* Console height in terms of world positions *)
let console_pos_y : int = console_height/obj_width


(***********************)
(***** World State *****)
(***********************)
(* The y dimention of the world is now equal to size+(console_height/obj_width)
   to account for the number of world positions added for the console *)
(** The state of the world -- a matrix of positions, where each
   position contains a list of world objects. *)
let world : world_object_i list array array =
  Array.make_matrix ~dimx:size ~dimy:(size + (console_pos_y)) []

(****************************)
(***** World Operations *****)
(****************************)

(** Clear out all objects from the world. *)
let reset () : unit =
  Array.iter ~f:(fun row -> Helpers.array_map_modify (fun _ -> []) row)
             world

(** Get all objects associated with a location in the world. *)
let get ((x,y):int*int) : world_object_i list  =
  world.(x).(y) 

(** Set a location in the world to contain a new list of objects. *)
let set ((x,y):int*int) (wos:world_object_i list) : unit =
  world.(x).(y) <- wos

(** Modify a location in the world with value os to contain (f os). *)
let modify (p:int*int) (f:world_object_i list -> world_object_i list) : unit =
  set p (f (get p))

(** Add an object to the list of world objects at a location. *)
let add (p:int*int) (w:world_object_i) : unit =
  modify p (fun wos -> if List.mem wos w then wos else w::wos)

(** Remove an object from the list of world objects at a location. Does
    nothing if the object was not in the list. *)
let remove (p:int*int) (w:world_object_i) : unit =
  modify p (fun wos -> List.filter ~f:(fun w' -> w' <> w) wos)

(** Same as remove but fails if the object is not in the list. *)
let remove_must_exist (p:int*int) (w:world_object_i) : unit =
  assert (List.mem (get p) w) ;
  remove p w

(** Fold over all objects in the world. *)
let fold (f:world_object_i -> 'a -> 'a) (i:'a) : 'a =
  Array.fold_right
    ~f:(fun row accum ->
       Array.fold_right
         ~f:(fun os accum' -> List.fold_right ~f ~init:accum' os)
         ~init:accum
         row)
    ~init:i
    world

(** Call a function for all indices in the world. *)
let indices (f:int*int -> unit) : unit =
  Array.iteri ~f:(fun x -> Array.iteri ~f:(fun y _ -> f (x,y))) world

(** True if the world contains the point (x,y). *)
let check_bounds ((x,y):int*int) : bool =
  x >= 0 && x < size && y >= 0 && y < (size + (console_height/obj_width))

(** Iterate of all world objects along with their corresponding location. *)
let iteri (f:int*int -> world_object_i -> unit) : unit =
  indices (fun p -> List.iter ~f:(f p) (get p))

(** All objects within n spaces from location (x,y). *)
let objects_within_range ((x,y):int*int) (n:int) : world_object_i list =
  let xlow = max (x-n) 0 in
  let ylow = max (y-n) 0 in
  let xhigh = min (x+n) (size-1) in
  let yhigh = min (y+n) (size+(console_height/obj_width)-1) in
  let coords = Helpers.cross (Helpers.range xlow xhigh)
			     (Helpers.range ylow yhigh) in
  List.fold_right ~f:(fun p t -> get p @ t) ~init:[] coords

(* Call a function on all cells in the world *)
let cells_iter f = indices (fun p -> List.iter (get p) ~f:f)

let survival_b_pos = (2, 3)

let birth_b_pos = (2, 1)


(******************)
(***** EVENTS *****)
(******************)

(** Fires when objects should perform their action. *)
let action_event : unit Event51.event = Event51.new_event ()

let calculate_event : unit Event51.event = Event51.new_event ()
