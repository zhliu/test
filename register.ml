(** The 'LoopInvariant' plugin.
    It contains one boolean state [Enabled] which can be set by the
    command line option "-loop-invariant".
    When this option is set it .... *)

open LoopInvariant
open Project
open Cil_types
open Cil_datatype
open Cil
open Visitor
open Function_analysis
open Db
open Db_types
open Ast_printer
open Globals

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



let rec processOneFile (cil: Cil_types.file) =
  begin
		Printf.printf "--------the file is to be processed\n%s\n" cil.fileName;		
		Printf.printf "cil.globinitcalled=%b\n" cil.globinitcalled;
		
		(*create_syntactic_check_project ();*)
		let visitor = new non_zero_divisor in
		
		Printf.printf "Ast.is_computed=%b\n" (Ast.is_computed ());
		Printf.printf "anno.length=%d\n" (List.length (Globals.Annotations.get_all ()));
		List.iter(fun (annot,gene) ->
			Cil.d_annotation Format.std_formatter annot;
		)(Globals.Annotations.get_all ()); 
		
		Printf.printf "length=cil.globals=%d\n" (List.length cil.globals);
		
		(*let fundec = Cil.getGlobInit cil in*)
		(*Printf.printf "length=fundec.sallstmts=%d\n" (List.length fundec.sallstmts);
		
		Cfg.clearCFGinfo  fundec;*)
		(**before compute, must clear first. set clear_id to be false*)
		Cfg.clearFileCFG ~clear_id:false cil;
		Cfg.computeFileCFG cil;
		
		(*Cfg.prepareCFG fundec;
		Cfg.computeCFGInfo fundec true;*)
		(**
		Printf.printf "before cil.globals fundec.name=%s\n" fundec.svar.vname;
		Printf.printf "cil.globinitcalled=%b\n" cil.globinitcalled;
		Printf.printf "length=fundec.sallstmts=%d\n" (List.length fundec.sallstmts);
		Printf.printf "fundec.smaxid=%d\n" fundec.smaxid;*)
		
		
		Printf.printf "%s\n" "----cil.globals";
		
					(**let get_loc_str location=
						let loc=Cil.d_loc Format.std_formatter location in
						Pretty.sprint 80 doc;
					in*)
		
		(*!Db.Value.compute ();
		let visitor = new File.check_file cil.fileName in*)
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
					(*Printf.printf "%s" "loc:";
					Cil.d_loc Format.std_formatter location;
					Printf.printf "%s" "**\n";
					(*Printf.printf "\tlocation.file=%s\n" (get_loc_str location);*)
					Printf.printf "fundec.name=%s\n" fundec.svar.vname;
					
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
					
					Cfg.clearCFGinfo ~clear_id:false fundec;
					Cfg.cfgFun fundec;
					(*Function_analysis.get_loop_infor fundec;*)
					Function_analysis.count_loop_number fundec;
					(*Function_analysis.p_visitor visitor;
					Function_analysis.print_function_stmts fundec visitor;*)
					Function_analysis.print_function_body fundec visitor;
					(*let num = Cfg.cfgFun fundec in
					Printf.printf "\tCfg.cfgFun:num=%d\n" num;*)
					let dotName = "/home/lzh/"^fundec.svar.vname^".dot" in
					Cfg.printCfgFilename dotName fundec;
					let cmdstr = "dot /home/lzh/"^fundec.svar.vname^".dot -Tpng -o /home/lzh/"^fundec.svar.vname^".png" in
					Sys.command cmdstr;
					Printf.printf "%s\n" "";
						
					(*Format.print_string "\n";
				| (GAsm (asm,location)) -> 
					Printf.printf "GAsm:location.file=%s\n" location.file;
				| (GPragma (attribute,location)) -> 
					Printf.printf "GPragma:location.file=%s\n" location.file;*)
				| _ -> Printf.printf "%s\n" "I donnot konw.";
			) cil.globals;
			
			(*let mainname = ref "main" in
			let theFile : global list ref = ref [] in
			List.iter 
        begin
          fun g ->
            (match g with
              GFun(m, lm) when m.svar.vname = !mainname ->
                (* Prepend a prototype *)
                theFile := GVarDecl (gi.svar, lm) :: !theFile;
                m.sbody.bstmts <- 
                   compactStmts (mkStmt (Instr [Call(None, 
                                                     Lval(var gi.svar), 
                                                     [], locUnknown)]) 
                                 :: m.sbody.bstmts);
                file.globinitcalled <- true;
                if !E.verboseFlag then
                  ignore (E.log "Inserted the globinit\n")
            | _ -> ());
            theFile := g :: !theFile (* Now put the global back *)
        end
        cil.globals;*)
		Printf.printf "numbers of loops in the program=%n\n" !Function_analysis.loop_number;
		Printf.printf "%s\n" "++++cil.globals";
		
		(**get CallGraph and print*)
		let graph = Callgraph.computeGraph cil in
		Callgraph.printGraph Pervasives.stdout graph;
		
		
		(**Slicing*)
		(*let slicPro = !Db.Slicing.Project.mk_project "test_slice" in*)
		(*let slicPro = !Db.Slicing.Slice.create (Project.current ())  kfun in
		let kfuncion = !Db.Slicing.Slice.get_function (Project.current ()) in
		match kfuncion.fundec with
					| Definition (fundec , location) ->
						Printf.printf "%s\n" "Definition";
						Printf.printf "%s\n" fundec.svar.vname;
						Cil.d_loc Format.std_formatter location;
						Printf.printf "\n";
					| Declaration (funspec , varinfo , varinfolo , location) ->
						Printf.printf "%s\n" "Declaration";
						Cil.d_funspec Format.std_formatter funspec;
						Printf.printf "%s\n" varinfo.vname;
						Cil.d_loc Format.std_formatter location;
						Printf.printf "\n";
					| _ -> Printf.printf "%s\n" "i donnot konw";*)
						
		(**global kernel_function*)
		(*List.iter(fun s ->
			Printf.printf "----%s\n" s;
			List.iter(fun kfun ->
				match kfun.fundec with
					| Definition (fundec , location) ->
						Printf.printf "%s\n" "Definition";
						Printf.printf "%s\n" fundec.svar.vname;
						Cil.d_loc Format.std_formatter location;
						Printf.printf "\n";
					| Declaration (funspec , varinfo , varinfolo , location) ->
						Printf.printf "%s\n" "Declaration";
						Cil.d_funspec Format.std_formatter funspec;
						Printf.printf "%s\n" varinfo.vname;
						Cil.d_loc Format.std_formatter location;
						Printf.printf "\n";
					| _ -> Printf.printf "%s\n" "i donnot konw";
						
				) (Globals.FileIndex.get_functions s);
			Printf.printf "++++%s\n" s;
			) (Globals.FileIndex.get_files ());*)
		
		(**value compute*)
		(*let state= Db.Value.globals_state () in
		Printf.printf "Db.Value.is_reachable=%b\n" (Db.Value.is_reachable state);
		let visitor = new File.check_file cil.fileName in
		Db.Value.access visitor#current_kinstr lval;
		Printf.printf "%s\n" "begin visitFramacFile";
		Visitor.visitFramacFile visitor cil;
		Visitor.visitFramacFunction visitor fundec;
		Printf.printf "%s\n" "end visitFramacFile";*)
		
		let out_file = open_out "/home/lzh/result.c" in
		Cil.dumpFile Cil.defaultCilPrinter out_file "/home/lzh/new.c" cil;
		flush out_file;
		close_out out_file;
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
	Globals.Functions.iter
      (fun kf ->
				 let name = Kernel_function.get_name kf in
				Printf.printf "name=%s\n" name;
				 if Kernel_function.is_definition kf
				 then begin
				     Kernel_function.pretty_name Format.std_formatter kf;
				 end
	);
	Printf.printf "before Wp\n";
	let info = Dynamic.get ~plugin:"Wp" "run" (Datatype.func Datatype.unit Datatype.unit) in
	Printf.printf "after Wp\n";
	
	Ast.get ();
	Printf.printf "%s\n" "over";
	Printf.printf "Db.Value.is_computed=%b\n" (Db.Value.is_computed ());
	if not (Db.Value.is_computed ()) then !Db.Value.compute ();
	processOneFile (Ast.get ())
	(*Function_analysis.print_proj_info;*)
    
let compute_loop_invariant () = 
	Ast.compute ();
	ignore (visitFramacFile (new loopInvariant) (Ast.get ()));
	theMain ()
	
let print =
  Dynamic.register
    ~plugin:"Loop Invariant"
    "run"
    ~journalize:true
    (Datatype.func Datatype.unit Datatype.unit)
    compute_loop_invariant
	
let run () =  if Enabled.get () then compute_loop_invariant ()

let () = Db.Main.extend run
