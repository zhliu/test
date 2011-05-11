module F = Frontc
module C = Cil
module CType=Cil_types
(*module CK = Check
module E = Errormsg*)
open Pretty_source
open Cil
open Cil_types
open Abstract_interp
(*open Value_parameters*)
open Plugin
open Datatype
open Cabs

(*let file_name="/home/lzh/document/cil-1.3.7/test/small1/func.c";;
let print_files_name=
	Format.print_string "files name";;

Frontc.parse file_name;;

Cil.initCIL();;
print_files_name;;

let print_list  = 
	List.iter (
		fun p ->
		Format.print_string "a";
		)
		features;*)
		
type outfile = 
    { fname: string;
      fchan: out_channel } 
			
let outChannel : outfile option ref = ref None
let mergedChannel : outfile option ref = ref None

let parseOneFile (fname: string) : CType.file * Cabs.file =
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
        CType.GFun(fd,_) -> Cfg.prepareCFG fd ;
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


let rec processOneFile (cil: CType.file) =
  begin
		Printf.printf "--------开始处理的文件\n%s\n" cil.fileName;		
		Printf.printf "cil.globinitcalled=%b\n" cil.globinitcalled;
		(*Frontc.parse cil.fileName;
		Cfg.computeFileCFG cil;
		
		Printf.printf "length=cil.globals=%d\n" (List.length cil.globals);
		
		let fundec = getGlobInit cil in
		Printf.printf "length=fundec.sallstmts=%d\n" (List.length fundec.sallstmts);
		
		
		prepareCFG fundec;
		computeCFGInfo fundec true;
		Printf.printf "fundec.name=%s\n" fundec.svar.vname;
		Printf.printf "cil.globinitcalled=%b\n" cil.globinitcalled;
		Printf.printf "length=fundec.sallstmts=%d\n" (List.length fundec.sallstmts);
		Printf.printf "fundec.smaxid=%d\n" fundec.smaxid;
		
		
		Printf.printf "%s\n" "----cil.globals";
		List.iter (function g ->
			match g with
				|	(GText text) ->	
					Printf.printf "location.file=%s\n" text;
				| (GVarDecl (varinfo,location)) -> 
					Printf.printf "GVarDecl:location.file=%s\n" location.file;
					Printf.printf "GVarDecl:varinfo.vname=%s\n" varinfo.vname;
					printType plainCilPrinter () varinfo.vtype;
					Format.print_string "\n";
				| (GType (typeinfo,location)) -> 
					Printf.printf "GType:location.file=%s\n" location.file;
				| (GCompTag (compinfo,location)) -> 
					Printf.printf "GCompTag:location.file=%s\n" location.file;
				| (GCompTagDecl (compinfo,location)) -> 
					Printf.printf "GCompTagDecl:location.file=%s\n" location.file;
				| (GEnumTag (enuminfo,location)) -> 
					Printf.printf "GEnumTag:location.file=%s\n" location.file;
				| (GEnumTagDecl (enuminfo,location)) -> 
					Printf.printf "GEnumTagDecl:location.file=%s\n" location.file;
				| (GVarDecl (varinfo,location)) -> 
					Printf.printf "GVarDecl:location.file=%s\n" location.file;
				| (GVar (varinfo,initinfo,location)) -> 
					Printf.printf "GVar:location.file=%s\n" location.file;
					Printf.printf "Gvar:varinfo.vname=%s\n" varinfo.vname;
				| (GFun (fundec,location)) -> 
					Format.print_string "GFun:\n";
					Printf.printf "\tlocation.file=%s\n" location.file;
					Printf.printf "\tfundec.name=%s\n" fundec.svar.vname;
					let num = Cfg.cfgFun fundec in
					Printf.printf "\tCfg.cfgFun:num=%d\n" num;
					
					Cfg.printCfgFilename "/home/lzh/a.dot" fundec;
					
						let print_instr instr=
							let doc = Cil.d_instr () instr in
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
								Printf.printf "\t\tReturn:location.file=%s\n" location.file;
							|	(Break (location)) -> 
								Printf.printf "\t\tBreak:location.file=%s\n" location.file;
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
					Printf.printf "GPragma:location.file=%s\n" location.file;
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
		Printf.printf "%s\n" "++++fundec.sformals";*)
		
		(*let mem_functions = Value_parameters.MemFunctions.get () in
	  if Value_parameters.MemExecAll.get ()
	    || not (Datatype.String.Set.is_empty mem_functions)
	  then begin
	    Value_parameters.feedback "====== MEMOIZING FUNCTIONS ======";
	    Ast.compute ();
		end;*)
		(*Ast.compute ();
		Db.Inputs.expr ();*)
		Ast.compute ();
		(*Printf.printf "%s\n" "----CFG";		
		List.iter (fun ele ->
			let num = Cfg.cfgFun fundec in
			d_cfgnodename () ele;
			) fundec.sallstmts;
		Printf.printf "%s\n" "++++CFG";*)
		
		
		
    (*if !Cilutil.doCheck then begin
      ignore (E.log "First CIL check\n");
      if not (CK.checkFile [] cil) && !Cilutil.strictChecking then begin
        E.bug ("CIL's internal data structures are inconsistent "
               ^^"(see the warnings above).  This may be a bug "
               ^^"in CIL.\n")
      end
    end;*)

    (* Scan all the features configured from the Makefile and, if they are 
     * enabled then run them on the current file *)
    (*List.iter 
      (fun fdesc -> 
        if ! (fdesc.C.fd_enabled) then begin
          if !E.verboseFlag then 
            ignore (E.log "Running CIL feature %s (%s)\n" 
                      fdesc.C.fd_name fdesc.C.fd_description);
          (* Run the feature, and see how long it takes. *)
          Stats.time fdesc.C.fd_name
            fdesc.C.fd_doit cil;
          (* See if we need to do some checking *)
          if !Cilutil.doCheck && fdesc.C.fd_post_check then begin
            ignore (E.log "CIL check after %s\n" fdesc.C.fd_name);
            if not (CK.checkFile [] cil) && !Cilutil.strictChecking then begin
              E.error ("Feature \"%s\" left CIL's internal data "
                       ^^"structures in an inconsistent state. "
                       ^^"(See the warnings above)") fdesc.C.fd_name
            end
          end
        end)
      features;


    (match !outChannel with
      None -> ()
    | Some c -> Stats.time "printCIL" 
	(C.dumpFile (!C.printerForMaincil) c.fchan c.fname) cil);

    if !E.hadErrors then
      E.s (E.error "Error while processing file; see above for details.");*)

  end
	
(***** MAIN *****)  
let theMain () =
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
  (* sm: enabling this by default, since I think usually we
   * want 'cilly' transformations to preserve annotations; I
   * can easily add a command-line flag if someone sometimes
   * wants these suppressed *)
  
	(*C.print_CIL_Input := true;*)

  (*********** COMMAND LINE ARGUMENTS *****************)
  (* Construct the arguments for the features configured from the Makefile *)
  (*let blankLine = ("", Arg.Unit (fun _ -> ()), "") in
  let featureArgs = 
    List.fold_right 
      (fun fdesc acc ->
	if !(fdesc.C.fd_enabled) then
          (* The feature is enabled by default *)
          blankLine ::
          ("--dont" ^ fdesc.C.fd_name, Arg.Clear(fdesc.C.fd_enabled), 
           " Disable " ^ fdesc.C.fd_description) ::
          fdesc.C.fd_extraopt @ acc
	else
          (* Disabled by default *)
          blankLine ::
          ("--do" ^ fdesc.C.fd_name, Arg.Set(fdesc.C.fd_enabled), 
           " Enable " ^ fdesc.C.fd_description) ::
          fdesc.C.fd_extraopt @ acc
      ) features
      [blankLine]
  in
  let featureArgs = 
    ("", Arg.Unit (fun () -> ()), " \n\t\tCIL Features") :: featureArgs 
  in*)
    
  let argDescr = Ciloptions.options @ 
        [ 
          "--out", Arg.String (openFile "output" 
                                 (fun oc -> outChannel := Some oc)),
              " the name of the output CIL file.\n\t\t\t\tThe cilly script sets this for you.";
          "--mergedout", Arg.String (openFile "merged output"
                                       (fun oc -> mergedChannel := Some oc)),
              " specify the name of the merged file";
        ]
        @ F.args @ featureArgs in
  begin
    (* this point in the code is the program entry point *)

    Stats.reset Stats.HardwareIfAvail;

    (* parse the command-line arguments *)
		Format.print_string "before parse cmd\n";
		Format.print_int !Arg.current;
		Format.print_string "\n";
    Arg.parse (Arg.align argDescr) Ciloptions.recordFile usageMsg;
		Format.print_string "after pare cmd\n";
    Cil.initCIL ();

    Ciloptions.fileNames := List.rev !Ciloptions.fileNames;
		
		Format.print_string "--------要处理的源文件如下:\n";
		List.iter (
			function name ->
			 	Format.print_string name;
				Format.print_string "\n";
			) !Ciloptions.fileNames;
		Format.print_string "--------要处理的源文件--------\n";
		
    (*if !Cilutil.testcil <> "" then begin
      Testcil.doit !Cilutil.testcil
    end else
      (* parse each of the files named on the command line, to CIL *)
      let files = List.map parseOneFile !Ciloptions.fileNames in

      (* if there's more than one source file, merge them together; *)
      (* now we have just one CIL "file" to deal with *)
      let one =
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
            | 	Some mc -> begin
                let oldpci = !C.print_CIL_Input in
                C.print_CIL_Input := true;
                Stats.time "printMerged"
                  (C.dumpFile !C.printerForMaincil mc.fchan mc.fname) merged;
                C.print_CIL_Input := oldpci
            end);
            merged
      in
			Format.print_string "--------合并结束\n";
			
      if !E.hadErrors then
        E.s (E.error "Cabs2cil had some errors");*)

      (* process the CIL file (merged if necessary) *)
      processOneFile one
  end


Format.print_string "before main\n";;
theMain();;
Format.print_string "after main\n";;