(** The 'Loop Invariant' plugin.
    It contains one boolean state [Enabled] which can be set by the
    command line option "-loop-invariant".
    When this option is set it .... *)

open GetCfg

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

let print () = 
	Self.result "my loop invariant plugin no theMain?\n";
	GetCfg.theMain ()

let print =
  Dynamic.register
    ~plugin:"Loop Invariant"
    "run"
    ~journalize:true
    (Datatype.func Datatype.unit Datatype.unit)
    print

let run () =  if Enabled.get () then print ()

(** Register the function [run] as a main entry point. *)
let () = Db.Main.extend run
(*let () = Db.LoopInvariant.run <- run*)
