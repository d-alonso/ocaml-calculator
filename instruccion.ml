open Expr_arit;;
open Diccionario;;

type instruccion =
  Asignacion of string * expresion_aritmetica
| Calculo of expresion_aritmetica
| Comando of string;;

exception Fin_de_ejecucion;;

exception Comando_no_implementado of string;;
	
let ejecutar_instruccion d i = match i with
	Asignacion (s,e) -> let ex=(evaluar_expresion d e) in 
			let _=print_string (s ^ " = " ^ (string_of_float ex)^ "\n") 
			in asignar_valor d s ex
	| Calculo e -> let ex=evaluar_expresion d e
			in let _=print_string (string_of_float (ex) ^ "\n") in d
	| Comando s -> match s with
			"var" -> let _=mostrar_diccionario d in d
			| "fun" -> let _=Libreria.mostrar_libreria () in d
			| "fin" -> raise (Fin_de_ejecucion)
			| _ -> raise (Comando_no_implementado s);;
