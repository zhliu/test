
open Cil_types

type sequence = stmt * lval list * lval list * lval list * stmt ref list
(**语句类型*)
let print_function_stmt_kind stmt = 
	match stmt.skind with
		| ( Instr ( instr ) ) ->
			Format.print_string "instr\n"
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
			Format.print_string "block\n"
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
		(*match stmt.skind with
			| ( Instr (instr)) ->
				Format.print_string "instr\n";
			| _ ->
				Format.print_string "other\n";*)
		) fundec.sallstmts

		