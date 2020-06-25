exception Funcion_no_implementada of string;;

let listaF = [("abs",abs_float,"valor absoluto");
("sqrt",sqrt,"raíz cuadrada");("exp",exp,"exponencial");
("log",log,"logaritmo natural");("round",ceil,"redondeo")];;

let funcion s = 
	let rec aux s l = match l with
		(str,f,_)::t -> if str=s then f
				else aux s t
		| [] -> raise (Funcion_no_implementada s)
	in aux s listaF;;

let descripcion s = 
	let rec aux s l = match l with
		(str,_,d)::t -> if str=s then d
				else aux s t
		| [] -> raise (Funcion_no_implementada s)
	in aux s listaF;;


let rec mostrar_libreria () =
	let rec aux l = match l with
		[] -> ()
		| (str,f,d)::t -> print_string (str ^ " : " ^ d ^ "\n");
				aux t
	in aux listaF;;
