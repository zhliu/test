open Cil_types
open Cmdline

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
		Printf.printf "fundec.name=%s\n" fundec.svar.vname;
		Printf.printf "cil.globinitcalled=%b\n" cil.globinitcalled;
		Printf.printf "length=fundec.sallstmts=%d\n" (List.length fundec.sallstmts);
		Printf.printf "fundec.smaxid=%d\n" fundec.smaxid;
		
		
		Printf.printf "%s\n" "----cil.globals";
		
		
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
					Format.print_string "GFun:\n";
					(*Printf.printf "\tlocation.file=%s\n" (get_loc_str location);*)
					Printf.printf "\tfundec.name=%s\n" fundec.svar.vname;
					
					Cfg.cfgFun fundec;
					(*let num = Cfg.cfgFun fundec in
					Printf.printf "\tCfg.cfgFun:num=%d\n" num;*)
					let dotName = "/home/lzh/"^fundec.svar.vname^".dot" in
					Cfg.printCfgFilename dotName fundec;
					
					
						(*let print_instr instr=
							let doc = Cil.d_instr Cil.defaultCilPrinter instr in
							let instrS= Pretty.sprint 80 doc in
							Printf.printf "\t\t%s\n" instrS;
						in
						
						let print_skind skind =
							match skind with
							| (Instr (instr)) ->
								Format.print_string "\t\tInstr:=";
								List.iter (fun ele ->
								print_instr ele;
								) instr;
							| (Return (expr,location)) -> 
								Printf.printf "\t\tReturn:location.file=%s\n" (get_loc_str location);
							|	(Break (location)) -> 
								Printf.printf "\t\tBreak:location.file=%s\n" (get_loc_str location);
							| _ -> 
								Printf.printf "\t\t%s\n" "I donnot konw.";
							in
							
					List.iter (fun stmt ->
						let doc = Cil.d_stmt () stmt in
						let stmtS=Pretty.sprint 80 doc in
						Printf.printf "\t\tsbody.bstmts:%s\n" stmtS;
						)	fundec.sbody.bstmts;
						
					List.iter (fun stmt ->
						let doc = Cil.d_stmt () stmt in
						let stmtS=Pretty.sprint 80 doc in
						Printf.printf "\t\tsbody.sallstmts:%s\n" stmtS;
						)	fundec.sallstmts;
						
					Format.print_string "\n";
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
		(*let mem_functions = Loop_parameters.MemFunctions.get () in
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
	(*let mem_functions = Loop_parameters.MemFunctions.get () in
  if Loop_parameters.MemExecAll.get ()
    || not (Datatype.String.Set.is_empty mem_functions)
  then begin
    Loop_parameters.feedback "====== MEMOIZING FUNCTIONS ======";
    Ast.compute ();
    Globals.Functions.iter
      (fun kf ->
	 let name = Kernel_function.get_name kf in
	 if Kernel_function.is_definition kf &&
	   (Loop_parameters.MemExecAll.get ()
	    || Datatype.String.Set.mem name mem_functions)
	 then begin
	   Loop_parameters.feedback "== function %a"
	     Kernel_function.pretty_name kf;
	   try
	     !Db.Value.memoize kf
           with Db.Value.Aborted ->
	     Loop_parameters.fatal "Cannot memoize %a: Analysis degenerated@."
	       Kernel_function.pretty_name kf
	 end)
  end;
  let usageMsg = "Usage: cilly [options] source-files" in
  (* Processign of output file arguments *)
  let openFile (what: string) (takeit: outfile -> unit) (fl: string) = 
		Format.print_string what;
		(*if !E.verboseFlag then
      ignore (Printf.printf "Setting %s to %s\n" what fl);*)
    (
			Format.print_string what;
			try takeit { fname = fl;
                  fchan = open_out fl }
    with _ ->
      raise (Arg.Bad ("Cannot open " ^ what ^ " file " ^ fl)))
  in
  let outName = ref "" in
    (* parse the command-line arguments *)
		Format.print_string "before parse cmd\n";
		Format.print_int !Arg.current;
		Format.print_string "\n";
    (*Arg.parse (Arg.align argDescr) Ciloptions.recordFile usageMsg;*)
		Format.print_string "after pare cmd\n";
    (*Cil.initCIL ();*)

    (*Ciloptions.fileNames := List.rev !Ciloptions.fileNames;*)
		
		Format.print_string "--------要处理的源文件如下:\n";
		List.iter (
			function name ->
			 	Format.print_string name;
				Format.print_string "\n";
			) Cmdline.use_cmdline_files; (*!Ciloptions.fileNames;*)
		Format.print_string "--------要处理的源文件--------\n";*)
		
		(*let one =
        match files with
          [one] -> one
        | [] -> E.s (E.error "No arguments for CIL")
        | _ ->
            let merged =
              Stats.time "merge" (Mergecil.merge files)
                (if !outName = "" then "stdout" else !outName) in
            if !E.hadErrors then
              E.s (E.error "There were errors during merging");
            (* See if we must save the merged file *)
            (match !mergedChannel with
              None -> ()
            | Some mc -> begin
                let oldpci = !C.print_CIL_Input in
                C.print_CIL_Input := true;
                Stats.time "printMerged"
                  (C.dumpFile !C.printerForMaincil mc.fchan mc.fname) merged;
                C.print_CIL_Input := oldpci
            end);
            merged
      in
			
    processOneFile one;*)
