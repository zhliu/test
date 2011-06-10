open Cil_types
open Cmdline
open Function_analysis
open Db
open Db_types
open Ast_printer
open Globals

module F = Frontc
module C = Cil

type outfile = 
    { fname: string;
      fchan: out_channel } 
			
let outChannel : outfile option ref = ref None;;
let mergedChannel : outfile option ref = ref None;;

let parseOneFile (fname: string) : Cil_types.file * Cabs.file =
  (* PARSE and convert to CIL *)
  if !Cilutil.printStages then Format.print_string "printStages\n";(*ignore (E.log "Parsing %s\n" fname);*)
	F.parse fname ()
  (*let cil = F.parse fname () in
  
  if (not !Epicenter.doEpicenter) then (
    (* sm: remove unused temps to cut down on gcc warnings  *)
    (* (Stats.time "usedVar" Rmtmps.removeUnusedTemps cil);  *)
    (* (trace "sm" (dprintf "removing unused temporaries\n")); *)
    (Rmtmps.removeUnusedTemps cil)
  );
	if true then (
	(Rmtmps.removeUnusedTemps cil);)*)
	
let makeCFGFeature : C.featureDescr = 
  { C.fd_name = "makeCFG";
    C.fd_enabled = ref true;(*Cilutil.makeCFG;*)
    C.fd_description = "make the program look more like a CFG" ;
    C.fd_extraopt = [];
    C.fd_doit = (fun f -> 
      (*ignore (Partial.calls_end_basic_blocks f) ; 
      ignore (Partial.globally_unique_vids f) ; *)
      Cil.iterGlobals f (fun glob -> match glob with
        Cil_types.GFun(fd,_) -> Cfg.prepareCFG fd ;
                      (* jc: blockinggraph depends on this "true" arg 
                      ignore (Cil.computeCFGInfo fd true)*)
      | _ -> ()) 
    );
    C.fd_post_check = true;
  }
	
let features : C.featureDescr list = 
  [ (*Epicenter.feature;
    Simplify.feature;
    Canonicalize.feature;
    Callgraph.feature;
    Logwrites.feature;
    Heapify.feature1;
    Heapify.feature2;
    Oneret.feature;
    makeCFGFeature; (* ww: make CFG *must* come before Partial *) 
    Partial.feature;
    Simplemem.feature;
    Sfi.feature;
    Dataslicing.feature;
    Logcalls.feature;
    Ptranal.feature;
    Liveness.feature;*)
  ] 
  (*@ Feature_config.features*)