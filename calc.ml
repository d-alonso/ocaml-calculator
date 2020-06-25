open Instruccion;;
open Diccionario;;
open Expr_arit;;
open Libreria;;

 let ejecutar = 
	let rec exec d =
		(print_string (">>");
		try exec (ejecutar_instruccion d ( Parser.s Scanner.token (Lexing.from_string (read_line ())) ) )
			with Variable_no_asignada s -> print_string("Variable " ^ s ^ " no asignada\n");exec d
			| Funcion_no_implementada s -> print_string("Funcion " ^ s ^ " no implementada\n");exec d
			| Comando_no_implementado s -> print_string("Comando " ^ s ^ " no implementado\n"); exec d
			| Scanner.Error_lexico -> print_string("Error lexico\n"); exec d
			| Parsing.Parse_error -> print_string("Error sintactico\n"); exec d
			| Fin_de_ejecucion -> print_string("... Adiós !!! :)\n") ;exit 0
		)
	in exec (diccionario_vacio)
;;

	










