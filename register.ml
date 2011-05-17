(** The 'Loop Invariant' plugin.
    It contains one boolean state [Enabled] which can be set by the
    command line option "-loop-invariant".
    When this option is set it .... *)

open Loop_invariant

(** Register the new plug-in "Loop Invariant" and provide access to some plug-in
    dedicated features. *)
module Self =
  Plugin.Register
    (struct
       let name = "Loop Invariant"
       let shortname = "loopInvariant"
       let help = "my 'Loop Invariant' plugin"
     end)

(** Register the new Frama-C option "-loop-invariant". *)
module Enabled =
  Self.False
    (struct
       let option_name = "-loop-invariant"
       let help = "my loop invariant plugin"
       let kind = `Correctness
     end)

let compute_loop_invariant () = 
	Loop_invariant.theMain ()

let print =
  Dynamic.register
    ~plugin:"Loop Invariant"
    "run"
    ~journalize:true
    (Datatype.func Datatype.unit Datatype.unit)
    compute_loop_invariant

let main () =  if Enabled.get () then compute_loop_invariant ()

(** Register the function [run] as a main entry point. *)
let () = Db.Main.extend main
(*let () = Db.LoopInvariant.run <- run*)
