(* This file has been adapted from PSET 7 for use in this project *)
(* Also referenced "The OCaml System Release 4.01 Documentation and User's Manual" 
   Graphics, Event, and List modules http://caml.inria.fr/pub/docs/manual-ocaml/ *)


open Core.Std
open Event51

(********************)
(***** UI State *****)
(********************)

let delay = ref 10
let counter = ref 0
let inc_counter () =
  incr counter ;
  if !counter >= !delay then (counter := 0 ; true) else false

let mouse_state = ref false
let mouse_pos = ref (0,0)

let paused = ref false

let draw_red = ref 0
let draw_blue = ref 180
let draw_green = ref 0

let brush_size = ref 1

(* Dimensions and settings for drawing the menu of keyboard commands *)
let menu_width = 200

let menu_height = (World.size*World.obj_width)+World.console_height

let menu_x = (World.size + 1)*World.obj_width

let menu_y = 0

let margin = 10

let rule_mode = ref "survive"

(* Number of pixes from the bottom of one line to the bottom of the one below *)
let linespace = 15

(* If an instruction string exceeds this number of characters,
   the program will look for a space betwwn words for a line break*)
let instr_max = 20

let instructions =[
  "Menu";
  "Key Inputs"; 
  "S or s: Slow the simulation"; 
  "F or f: Speed up the simulation";
  "C or c: clear the board";
  "Spacebar: Pause simulation";
  "Spacebar then N or n: Step forward";
  "Click and drag the mouse to paint new cells";
  "+ to increase the brush size";
  "- to decrease the brush size";
  "";
  "";
  "Numbers 0-8: toggle rule on/off";

  "";
  "'.' : Switch between Survival Rule and Birth Rule";

  "";

  "Survival Rule Numbers: the number of living neighbors a living cell must have to survive";

  "";

  "Birth Rule Numbers: the number of living neighbors a dead cell must have to be born";

  ]

let survival_rule : int list ref = ref [3;4;5;6;7]

let birth_rule : int list ref = ref [3;7;8]


(*********************)
(***** UI Events *****)
(*********************)

(** Fires when a key is pressed and returns the character corresponding
    to the key. *)
let key_pressed : char Event51.event = Event51.new_event ()

(** Fires when the color for cells has changed. *)
let color_changed : unit Event51.event = Event51.new_event ()

(** Fires when the color for cells has changed. *)
let board_cleared : unit Event51.event = Event51.new_event ()

(** Fires when the rules have been updated. *)
let rules_updated : unit Event51.event = Event51.new_event ()

(** Fires when the mouse button is pressed, indicating the coordinates
    where the mouse was when the event occurred. *)
let button_down : (int*int) Event51.event = Event51.new_event ()

(** Fires when the mouse button is released, indicating the coordinates
    where the mouse was when the event occurred. *)
let button_up : (int * int) Event51.event = Event51.new_event ()

(** Fires when the mouse moves, indicating the coordinates where the
    mouse was when the event occured. *)
let mouse_motion : (int * int) Event51.event = Event51.new_event ()

(** Fires each time the virtual clock ticks. *)
let clock : unit Event51.event = Event51.new_event ()

(*******************)
(***** Console *****)
(*******************)




(************************)
(***** Event System *****)
(************************)

exception Stop

let rule_handler i mode =
  if mode = "survive"
  then 
    if List.mem !survival_rule i
    then survival_rule := (List.filter !survival_rule ~f:(fun x -> x <> i))
    else survival_rule := i :: !survival_rule
  else
    if List.mem !birth_rule i
    then birth_rule := (List.filter !birth_rule ~f:(fun x -> x <> i))
    else birth_rule := i :: !birth_rule;

  Event51.fire_event rules_updated ()

(* Helper for mouse input. Redraws cells in range of cursor *)
let redraw p brush =
  let cells = World.objects_within_range p brush in
  List.iter cells ~f:(fun cell -> cell#resurrect; cell#draw)

(* Handles mouse input for painting on the board *)
let mouse_input p =
  let (x,y) = p in
  let cell_pos = (x/World.obj_width, y/World.obj_width) in
  let (cx,cy) = cell_pos in
  let in_world_x = (0<=cx) && (cx<World.size) in
  let in_world_y = ((World.console_pos_y<=cy) && 
                   (cy<World.(size+console_pos_y))) in
  if in_world_x && in_world_y
  then redraw cell_pos !brush_size

(* Functions for drawing the menu *)
(* Set the position to the beginning of a new line *)
(* Referenced "The OCaml System Release 4.01 Documentation and User's Manual" String 
   module http://caml.inria.fr/pub/docs/manual-ocaml/libref/String.html for use of 
   s.[i] notation, String.sub, and String.length *)
let new_line ln_start =
  Graphics.moveto (ln_start) ((Graphics.current_y()) - linespace)

let rec line_wrap str s_length s_max =
  if s_length > s_max then
     (let rec find_space s i = 
     (match s.[i] with
     | ' ' -> new_line(menu_x + margin); 
              Graphics.draw_string (String.sub str ~pos:0 ~len:i);
              line_wrap (String.sub str ~pos:(i+1) ~len:(s_length-(i+1))) 
                        (s_length-(i+1)) s_max;
     | _ -> if i < (s_length-1) then find_space s (i + 1)
            else (new_line(menu_x + margin); Graphics.draw_string s;))
     in find_space str s_max)
   else (new_line(menu_x + margin); Graphics.draw_string str)

(* Draw the list of instructions *)
let rec draw_instructions inst =
  match inst with
  | [] -> ()
  | hd::tl -> let strleng = String.length hd in
                            line_wrap hd strleng instr_max;
  draw_instructions tl
   
   

(* Draw the current brush size and erase before updating *)
let print_brush_size () =
  Graphics.set_color Graphics.white;
  Graphics.fill_rect (menu_x + margin) (2*margin) 
                     (menu_width - (2*margin)) margin;
  Graphics.moveto (menu_x + margin) (2*margin);
  Graphics.set_color Graphics.black;
  Graphics.draw_string "Brush size: "; 
    Graphics.draw_string (string_of_int !brush_size)

(* Draw a menu that includes the instructions and the brush size *)
let draw_menu () =
  Graphics.set_color Graphics.white;
  Graphics.fill_rect menu_x menu_y menu_width menu_height;
  Graphics.moveto (menu_x + margin) (menu_height - 2*margin);
  Graphics.set_color Graphics.black;
  draw_instructions instructions;
  print_brush_size ()

(* poll the Graphics module for the various events -- some care had to
   be taken to "de-bounce" the mouse. *)
let read_event () =
  let new_pos = Graphics.mouse_pos () in
  if new_pos <> !mouse_pos then begin
    mouse_pos := new_pos ;
    Event51.fire_event mouse_motion (Graphics.mouse_pos ())
  end ;
  if Graphics.key_pressed () then begin
    Event51.fire_event key_pressed (Graphics.read_key ())
  end ;
  if Graphics.button_down () then begin
    Event51.fire_event button_down new_pos
  end ;
  if not !mouse_state then begin
    let s = Graphics.wait_next_event [Graphics.Button_down ; Graphics.Poll] in
    if s.Graphics.button then begin
      mouse_state := true ;
      Event51.fire_event button_down new_pos
    end
  end ;
  if !mouse_state then begin
    let s = Graphics.wait_next_event [Graphics.Button_up ; Graphics.Poll] in
    if not s.Graphics.button then begin
      mouse_state := false ;
      Event51.fire_event button_up new_pos
    end
  end ;
  Event51.fire_event clock ()

(* Helper for restarting interrupted system calls (OY) *)
let rec restart f arg =
  try f arg
  with Unix.Unix_error (Unix.EINTR, _, _) -> restart f arg

(* The approximate frame rate (will actually be lower if we take non-trivial
   time to handle events) *)
let frame_rate = 30.0

(* Our basic event loop just calls read_event, which fires the appropriate
   events, then synchronizes the shadow graphics buffer with the screen,
   and then loops again. *)
let rec event_loop () =
  read_event ();
  Graphics.synchronize ();
  restart Thread.delay (1.0 /. frame_rate);
  event_loop ()

(* The code for step was extracted from PSET 7 Main.ml and was
 * originally part of the event_loop function in that file *)
let step () = 
    begin
      Event51.fire_event World.calculate_event ();
      Event51.fire_event World.action_event ();
    end;
      World.indices 
      begin 
	fun p ->
	let sorted = List.sort ~cmp:(fun x y -> 
                       compare x#draw_z_axis y#draw_z_axis) (World.get p) in
	List.iter ~f:(fun w -> w#draw) sorted;
      end

(** The command "run_ui x y init" starts up the graphical environment with a
    window size of x by y pixels, sets up the basic events such as the
    keyboard, mouse, etc. (see below), and then invokes the function init as
    an initializer, before entering an event polling loop which fires the
    appropriate event handlers whenever an action occurs. *)
let run_ui (x:int) (y:int) (init:unit->unit) : unit =
  try
    Graphics.open_graph "" ; Graphics.resize_window x y ;
    Graphics.auto_synchronize false ;
    init () ;
    event_loop ()
  with exn -> (Graphics.close_graph () ; raise exn)

(** only call the supplied function on every delay clock ticks and only if the
    simulation is not paused. *)
let clock_handler (f : unit -> unit) () : unit =
  if inc_counter () && not !paused then f ()

(** Press q or Q to stop the simulation.
    Press space to [un]pause the simulation.
    Press f or F to make the simulation go faster.
    Press s or S to make the simulation go slower. *)
(* Press + to increase th brush size and - to decrease it.
   Press pause the simulation and press n or N to step forward one iteration at
   a time.
   Press c or C to clesr the board.
   Press numbers 0 - 8 to set simulation rules. 
   Press . to toggle between survival an birth rule sets *)

let key_handler c =
  match c with
    | 'q' | 'Q' -> raise Stop
    | ' ' -> paused := not(!paused)
    | 'f' | 'F' -> delay := (!delay) - 5
    | 's' | 'S' -> delay := (!delay) + 5
    | 'r' | 'R' -> draw_red := 180; draw_green := 0; draw_blue := 0;
		   Event51.fire_event color_changed ()

    | 'g' | 'G' -> draw_green := 180; draw_red := 0; draw_blue := 0;
		   Event51.fire_event color_changed ()

    | 'b' | 'B' -> draw_blue := 180; draw_red := 0; draw_green := 0;
		   Event51.fire_event color_changed ()

    | '+' -> if !brush_size < 50 then brush_size := (!brush_size + 1); 
             print_brush_size ()
    | '-' -> if !brush_size > 0 then brush_size := (!brush_size - 1);
             print_brush_size ()
    | 'n' | 'N' -> if !paused then step ()
    | 'c' | 'C' -> Event51.fire_event board_cleared ()
    | '.' -> if !rule_mode = "survive" 
	     then rule_mode := "birth"
	     else rule_mode := "survive"
    | '0' -> rule_handler 0 !rule_mode
    | '1' -> rule_handler 1 !rule_mode
    | '2' -> rule_handler 2 !rule_mode
    | '3' -> rule_handler 3 !rule_mode
    | '4' -> rule_handler 4 !rule_mode
    | '5' -> rule_handler 5 !rule_mode
    | '6' -> rule_handler 6 !rule_mode
    | '7' -> rule_handler 7 !rule_mode
    | '8' -> rule_handler 8 !rule_mode
    | _ -> ()






(** Start the graphical environment initialized to the size of the world.
    Handle clock and input events necessary to run the simulation. *)
let run_world (init:unit -> unit) (clock_f:unit -> unit) : unit =
  run_ui ((World.size*World.obj_width)+menu_width) (* GUI width *)
         ((World.size*World.obj_width)+World.console_height) (* GUI height *)
         (* Event framework initializer *)
         begin fun () ->
           World.reset () ;
           ignore(Event51.add_listener clock (clock_handler clock_f)) ;
           ignore(Event51.add_listener key_pressed key_handler) ;
           ignore(Event51.add_listener button_down mouse_input) ;
           init ()
         end
