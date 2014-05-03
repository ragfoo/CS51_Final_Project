(* Signature for button objects *)
(* Referenced WorldObjectI.ml and Cell.ml while writing this module *)
open Core.Std
open WorldObjectI


class type button_i =
object
  inherit world_object_i

  method get_value : int

  method get_category : string

  method get_state : bool

end
