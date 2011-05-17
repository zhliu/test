open Cil
open Cil_types
open Visitor
open Project

	
type sequence = stmt * lval list * lval list * lval list * stmt ref list

(**统计函数中有多少个循环*)
let count_loop_number fundec loop_number = 
	List.iter (fun stmt ->
		match stmt.skind with
			| Loop(code_annotation , block , location , stmt1 , stmt2) -> 
				loop_number <- loop_number + 1;
			| _ -> loop_number;
		) fundec.sallstmts;
		loop_number
		
(**语句类型*)
let print_function_stmt_kind stmt = 
	let loop_visitor = new Visitor.frama_c_inplace in
	(*Format.print_string "begin visit stmt\n";
	Visitor.visitFramacStmt loop_visitor stmt;
	Format.print_string "end visit stmt\n";*)
	match stmt.skind with
		| ( Instr ( instr ) ) ->
			Format.print_string "instr\n";
			(*Visitor.visitFramacInstr loop_visitor instr;*)
			Format.print_string "\n";
		| ( Return ( exp , location ) )->
			Format.print_string "return\n"
		| ( Goto ( stmt , location) ) ->
			Format.print_string "goto\n"
		| ( Break ( location ) ) ->
			Format.print_string "break\n"
		| ( Continue ( location ) ) ->
			Format.print_string "continue\n"
		| ( If ( expr , block1 , block2 , location ) ) ->
			Format.print_string "if\n"
		| ( Switch ( expr , block , stmtl , location ) ) ->
			Format.print_string "switch\n"
		| ( Loop ( code_annotation , block , location , stmt1 , stmt2 ) ) ->
			Format.print_string "loop\n"
		| ( Block ( block ) ) ->
			Format.print_string "block\n";
			(*Visitor.visitFramacBlock loop_visitor block;*)
			Format.print_string "\n";
		| ( UnspecifiedSequence (quence : sequence list) ) ->
			Format.print_string "unspecifiedSequence\n"
		| ( TryFinally ( block1 , block2 , location ) ) ->
			Format.print_string "TryFinally\n"
		| ( TryExcept ( block1 , ( instr , exp ) , block2 , location ) ) ->
			Format.print_string "TryExcept\n"
		| ( _ ) ->
			Format.print_string "other\n"
			
			
(**打印所有语句*)
let print_function_stmts fundec = 
	List.iter (fun stmt ->
		(*Format.print_bool stmt.ghost;
		Format.print_int stmt.sid;
		Format.print_string "\n";*)
		Printf.printf "%s\n" "语句类型为:";
		print_function_stmt_kind stmt;
		Printf.printf "%s\n" "语句的内容为:";
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