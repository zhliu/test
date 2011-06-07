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


let rec processOneFile (cil: Cil_types.file) =
  begin
		Printf.printf "--------开始处理的文件\n%s\n" cil.fileName;		
		Printf.printf "cil.globinitcalled=%b\n" cil.globinitcalled;
		
		Printf.printf "length=cil.globals=%d\n" (List.length cil.globals);
		
		(**加上此句后会生成一个全局初始函数，挺烦的*)
		(*let fundec = Cil.getGlobInit cil in*)
		(*Printf.printf "length=fundec.sallstmts=%d\n" (List.length fundec.sallstmts);
		
		Cfg.clearCFGinfo  fundec;*)
		(**设为true代码会变成dead，不太明白,丢失了一些结构？这里设为false*)
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
		
		(*!Db.Value.compute ();*)
		let visitor = new File.check_file cil.fileName in
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
					(*Printf.printf "%s" "函数位置:";
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
					Function_analysis.print_function_body fundec;
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
		Printf.printf "程序中的循环个数=%n\n" !Function_analysis.loop_number;
		Printf.printf "%s\n" "++++cil.globals";
		
		(**调用关系图*)
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
						
		(**访问所有函数,得到kernel_function等*)
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
		
		(**value分析*)
		(*let state= Db.Value.globals_state () in
		Printf.printf "Db.Value.is_reachable=%b\n" (Db.Value.is_reachable state);
		let visitor = new File.check_file cil.fileName in
		Db.Value.access visitor#current_kinstr lval;
		Printf.printf "%s\n" "开始visitFramacFile";
		Visitor.visitFramacFile visitor cil;
		Visitor.visitFramacFunction visitor fundec;
		Printf.printf "%s\n" "结束visitFramacFile";*)
		
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
	(*Ast.compute ();*)
	Globals.Functions.iter
      (fun kf ->
				 let name = Kernel_function.get_name kf in
				Printf.printf "name=%s\n" name;
				 if Kernel_function.is_definition kf
				 then begin
				     Kernel_function.pretty_name Format.std_formatter kf;
				 end
	);
	let info = Dynamic.get ~plugin:"Wp" "run" (Datatype.func Datatype.unit Datatype.unit) in
	
	Ast.get ();
	Printf.printf "%s\n" "over";
	Printf.printf "Db.Value.is_computed=%b\n" (Db.Value.is_computed ());
	if not (Db.Value.is_computed ()) then !Db.Value.compute ();
	processOneFile (Ast.get ());
	(*Function_analysis.print_proj_info;*)