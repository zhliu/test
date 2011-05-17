open Cil_types
open Cmdline
open Function_analysis

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


let rec processOneFile (cil: Cil_types.file) =
  begin
		Printf.printf "--------开始处理的文件\n%s\n" cil.fileName;		
		Printf.printf "cil.globinitcalled=%b\n" cil.globinitcalled;
		(*Frontc.parse cil.fileName;
		Cfg.computeFileCFG cil;*)
		
		Printf.printf "length=cil.globals=%d\n" (List.length cil.globals);
		
		let fundec = Cil.getGlobInit cil in
		Printf.printf "length=fundec.sallstmts=%d\n" (List.length fundec.sallstmts);
		
		Cfg.clearCFGinfo  fundec;
		Cfg.clearFileCFG cil;
		
		Cfg.prepareCFG fundec;
		Cfg.computeCFGInfo fundec true;
		Printf.printf "before cil.globals fundec.name=%s\n" fundec.svar.vname;
		Printf.printf "cil.globinitcalled=%b\n" cil.globinitcalled;
		Printf.printf "length=fundec.sallstmts=%d\n" (List.length fundec.sallstmts);
		Printf.printf "fundec.smaxid=%d\n" fundec.smaxid;
		
		
		Printf.printf "%s\n" "----cil.globals";
		let loop_number = 0 in 
		
					(**let get_loc_str location=
						let loc=Cil.d_loc Format.std_formatter location in
						Pretty.sprint 80 doc;
					in*)
		List.iter (function g ->
			match g with
				|	(GText text) ->	
					Printf.printf "location.file=%s\n" text;
				| (GVarDecl (funspec,varinfo,location)) -> 
					(*Printf.printf "GVarDecl:location.file=%s\n" (get_loc_str location);*)
					Cil.d_var Format.err_formatter varinfo;
					Cil.d_loc Format.str_formatter location;
					Printf.printf "GVarDecl:varinfo.vname=%s\n" varinfo.vname;
					(*Cil.printType plainCilPrinter () varinfo.vtype;
					Format.print_string "\n";*)
				| (GType (typeinfo,location)) -> 
					Printf.printf "GType:typeinfo.tname=%s\n" typeinfo.tname;
					(*Printf.printf "GType:location.file=%s\n" (get_loc_str location);*)
				| (GCompTag (compinfo,location)) -> 
					Printf.printf "GCompTag:compinfo.cname=%s\n" compinfo.cname;
					(*Printf.printf "GCompTag:location.file=%s\n" (get_loc_str location);*)
				| (GCompTagDecl (compinfo,location)) -> 
					Printf.printf "GCompTagDecl:compinfo.cname=%s\n" compinfo.cname;
					(*Printf.printf "GCompTagDecl:location.file=%s\n" (get_loc_str location);*)
				| (GEnumTag (enuminfo,location)) -> 
					Printf.printf "GEnumTagDecl:enuminfo.ename=%s\n" enuminfo.ename;
					(*Printf.printf "GEnumTag:location.file=%s\n" (get_loc_str location);*)
				| (GEnumTagDecl (enuminfo,location)) -> 
					Printf.printf "GEnumTagDecl:enuminfo.ename=%s\n" enuminfo.ename;
					(*Printf.printf "GEnumTagDecl:location.file=%s\n" (get_loc_str location);*)
				| (GVarDecl (funspec,varinfo,location)) -> 
					Printf.printf "GVarDecl:varinfo.vname=%s\n" varinfo.vname; 
					(*Printf.printf "GVarDecl:location.file=%s\n" (get_loc_str location);*)
				| (GVar (varinfo,initinfo,location)) -> 
					Printf.printf "GVar:varinfo.vname=%s\n" varinfo.vname;
					(*Printf.printf "GVar:location.file=%s\n" (get_loc_str location);*)
					Printf.printf "Gvar:varinfo.vname=%s\n" varinfo.vname;
				| (GFun (fundec,location)) -> 
					Printf.printf "%s\n" "GFun:";
					(*Printf.printf "\tlocation.file=%s\n" (get_loc_str location);*)
					Printf.printf "fundec.name=%s\n" fundec.svar.vname;
					
					Cfg.cfgFun fundec;
					Function_analysis.count_loop_number fundec loop_number;
					Function_analysis.print_function_stmts fundec;
					(*let num = Cfg.cfgFun fundec in
					Printf.printf "\tCfg.cfgFun:num=%d\n" num;*)
					let dotName = "/home/lzh/"^fundec.svar.vname^".dot" in
					Cfg.printCfgFilename dotName fundec;
					
		
						
					(*Format.print_string "\n";
				| (GAsm (asm,location)) -> 
					Printf.printf "GAsm:location.file=%s\n" location.file;
				| (GPragma (attribute,location)) -> 
					Printf.printf "GPragma:location.file=%s\n" location.file;*)
				| _ -> Printf.printf "%s\n" "I donnot konw.";
			) cil.globals;
		Printf.printf "%s\n" "++++cil.globals";
		
		
		Printf.printf "fundec.svar.vname=%s\n" fundec.svar.vname;
		Printf.printf "%s\n" "----fundec.slocals";
		List.iter (fun varinfo ->
			Printf.printf "%s\n" varinfo.vname;
			) fundec.slocals;
		Printf.printf "%s\n" "++++fundec.slocals";
		
		Printf.printf "%s\n" "----fundec.sformals";
		List.iter (fun ele ->
			Printf.printf "%s\n" ele.vname;
			) fundec.sformals;
		Printf.printf "%s\n" "++++fundec.sformals";
		
		(*Function_analysis.visit_cilfile cil;
		let mem_functions = Loop_parameters.MemFunctions.get () in
	  if Loop_parameters.MemExecAll.get ()
	    || not (Datatype.String.Set.is_empty mem_functions)
	  then begin
	    Loop_parameters.feedback "====== MEMOIZING FUNCTIONS ======";
	    Ast.compute ();
		end;*)
		(*Ast.compute ();
		Db.Inputs.expr ();
		Ast.compute ();
		(*Printf.printf "%s\n" "----CFG";		
		List.iter (fun ele ->
			let num = Cfg.cfgFun fundec in
			d_cfgnodename () ele;
			) fundec.sallstmts;
		Printf.printf "%s\n" "++++CFG";*)*)

  end
	
let theMain () =
	Ast.compute ();
	processOneFile (Ast.get ());
	(*Function_analysis.print_proj_info;*)