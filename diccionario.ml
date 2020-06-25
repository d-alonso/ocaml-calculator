
type diccionario = Diccionario of (string * float) list;;

exception Variable_no_asignada of string;;

let diccionario_vacio = Diccionario [];;

let rec consultar_valor d s = match d with
	Diccionario ((str,fl)::t) -> if str=s then fl
				else consultar_valor (Diccionario t) s
	| Diccionario [] -> raise (Variable_no_asignada s);;

let asignar_valor (Diccionario d) s fl = Diccionario ( List.rev_append (List.rev d) [(s,fl)] );; 

let rec mostrar_diccionario d = match d with
	Diccionario [] -> ()
	| Diccionario ((str,fl)::t) -> 
			print_string (str ^ " = " ^ (string_of_float fl) ^ "\n");
			mostrar_diccionario (Diccionario t);;
