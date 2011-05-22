(** The 'Loop Invariant' plugin.
    It contains one boolean state [Enabled] which can be set by the
    command line option "-loop-invariant".
    When this option is set it .... *)

open Loop_invariant
open Cil_types
open Cil_datatype
open Cil
open Visitor

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

class loopInvariant = object (self)

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (inplace_visit ()) as super

  initializer !Db.Value.compute ()

	val mutable decls = []

  method private current_ki =
    match self#current_stmt with None -> Kglobal | Some s -> Kstmt s

  method vvdec vi =
    let ki = self#current_ki in
    if Db.Value.is_accessible ki then begin
      let z =
	!Db.Value.lval_to_zone
	  ki ~with_alarms:CilE.warn_none_mode (Var vi, NoOffset)
      in
      decls <-  (vi, z) :: decls
    end;
    DoChildren
    
    method vterm_lval tlv =
    (try
       let lv = !Db.Properties.Interp.term_lval_to_lval ~result:None tlv in
       ignore (self#vlval lv)
     with Invalid_argument msg ->
       error "%s@." msg);
    DoChildren

  method vstmt_aux s =
    !Db.progress ();
    super#vstmt_aux s
end

let compute_loop_invariant () = 
	ignore (visitFramacFile (new loopInvariant) (Ast.get ()));
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