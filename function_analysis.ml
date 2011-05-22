open Cil
open Cil_types
open Visitor
open Project
open Callgraph

type sequence = stmt * lval list * lval list * lval list * stmt ref list

let loop_number = ref 0

(**统计函数中有多少个循环*)
let count_loop_number (funDec:Cil_types.fundec) = 
	List.iter (fun stmt ->
		match stmt.skind with
			| Loop(code_annotation , block , location , stmt1 , stmt2) -> 
				loop_number := !loop_number + 1;
			| _ -> loop_number := !loop_number;
		) funDec.sallstmts;
		!loop_number
		
let d_stmt_option stmt = 
		match stmt with
			| None -> Printf.printf "%s" "None"
			| Some s ->Cil.d_stmt Format.std_formatter s
			| _ -> Printf.printf "%s" "i donnot konw"

(**语句类型*)
let print_function_stmt_kind stmt visitor= 
	(*let loop_visitor = new Visitor.frama_c_inplace in
	Format.print_string "begin visit stmt\n";
	Visitor.visitFramacStmt loop_visitor stmt;
	Format.print_string "end visit stmt\n";*)
	match stmt.skind with
		| ( Instr ( instr ) ) ->
			Format.print_string "instr\n";
			(*Visitor.visitFramacInstr loop_visitor instr;
			Format.print_string "\n";*)
		| ( Return ( exp , location ) )->
			Cil.d_loc Format.std_formatter location;
			Format.print_string "return\n"
		| ( Goto ( stmt , location) ) ->
			Format.print_string "goto\n"
		| ( Break ( location ) ) ->
			Format.print_string "break\n"
		| ( Continue ( location ) ) ->
			Format.print_string "continue\n"
		| ( If ( expr , block1 , block2 , location ) ) ->
			Format.print_string "if\n";
			(match expr.enode with
				| Lval (lval) ->
					(
						match lval with
							| (Var (varinfo) , _) ->
								Printf.printf "%s\n" varinfo.vname;
							| (Mem (mem), _) ->
								Printf.printf "%s\n" "mem";
						);
					let value = !Db.Value.access visitor#current_kinstr lval in
					Db.Value.pretty Format.std_formatter value;
					Printf.printf "%s\n" "Lval";
				| _ ->
					Printf.printf "%s\n" "i donnot konw";);
		| ( Switch ( expr , block , stmtl , location ) ) ->
			Format.print_string "switch\n"
		| ( Loop ( code_annotation , block , location , stmt1 , stmt2 ) ) ->
			(*Printf.printf "new_loc.loc_plnum=%d\n" (fst location).Lexing.pos_lnum;
			let new_loc = location in
			let lnum = (fst location).Lexing.pos_lnum in
			(fst new_loc).Lexing.pos_lnum := !lnum+1;
			let exp = Cil.mkString new_loc "mkString" in*)
			Format.print_string "loop\n";
			let (p1,p2) = location in
			let mkPosition location : Lexing.position (*pos_fname pos_lnum pos_bol pos_cnum*) =
				{Lexing.pos_fname=(location).Lexing.pos_fname;
				pos_lnum=(location).Lexing.pos_lnum+2;
				pos_bol=(location).Lexing.pos_bol;
				pos_cnum=(location).Lexing.pos_cnum;} in
				
			let new_loc = mkPosition (fst location) in
			
			Printf.printf "new_loc.pos_fname=%s\n" (p1).Lexing.pos_fname;
			(p1).Lexing.pos_lnum=(p1).Lexing.pos_lnum+1;
			Printf.printf "new_loc.loc_lnum=%d\n" (new_loc).Lexing.pos_lnum;
			let guard = Cil.mkString (new_loc,p2) "mkString op" in
			
			let stmt = mkStmt ~ghost:false ~valid_sid:true stmt.skind in
			let stmtl=[stmt;] in
			let mywhilestmt = Cil.mkWhile guard stmtl in
			Printf.printf "%s\n" "我的语句begin";
			Cil.d_stmt Format.std_formatter stmt;
			List.iter(fun sm -> 
				Cil.d_stmt Format.std_formatter sm;
				) mywhilestmt;
			Printf.printf "\n%s\n" "我的语句end";
			
			(*Format.print_string "loop\n";
			Printf.printf "%s" "循环位置:";
			Cil.d_loc Format.std_formatter location;
			List.iter (fun codeAnnot ->
			Cil.d_code_annotation Format.std_formatter codeAnnot;
			) code_annotation;
			Cil.d_block Format.std_formatter block;
			Printf.printf "stmt1:%s" "\n";
			d_stmt_option stmt1;
			Printf.printf "stmt2:%s" "\n";
			d_stmt_option stmt2;
			Printf.printf "%s" "**\n";*)
		| ( Block ( block ) ) ->
			Format.print_string "block\n";
			(*Visitor.visitFramacBlock loop_visitor block;
			Format.print_string "\n";*)
		| ( UnspecifiedSequence (quence : sequence list) ) ->
			Format.print_string "unspecifiedSequence\n"
		| ( TryFinally ( block1 , block2 , location ) ) ->
			Format.print_string "TryFinally\n"
		| ( TryExcept ( block1 , ( instr , exp ) , block2 , location ) ) ->
			Format.print_string "TryExcept\n"
		| ( _ ) ->
			Format.print_string "other\n"
			
			
(**打印所有语句*)
let print_function_stmts fundec visitor= 
	List.iter (fun stmt ->
		(*Format.print_bool stmt.ghost;
		Format.print_int stmt.sid;
		Format.print_string "\n";*)
		Printf.printf "%s" "语句类型为:";
		print_function_stmt_kind stmt visitor;
		Printf.printf "%s" "语句的内容为:";
		Cil.d_stmt Format.std_formatter stmt;
		Format.print_string "\n";
		) fundec.sallstmts


let visit_cilfile file = 
	let loop_visitor = new Visitor.frama_c_inplace in
	Printf.printf "%s\n" "before visit";
	Visitor.visitFramacFile loop_visitor file
	
let print_proj_info = 
	Printf.printf "工程名称:%s\n" Project.name;(*
	Printf.printf "uname=%s\n" (Project.get_unique_name (Project.current()))*)