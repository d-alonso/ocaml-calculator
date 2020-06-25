open Diccionario;;

type operador_binario = Suma | Resta | Producto | Division;;

type operador_unario = Opuesto | Funcion of string;;

type expresion_aritmetica =
	  Constante of float
	| Variable of string
	| Operacion_unaria of operador_unario * expresion_aritmetica
	| Operacion_binaria of operador_binario * expresion_aritmetica * expresion_aritmetica;;



let operacion_binaria o = match o with
	Suma -> (+.)
	| Resta -> (-.)
	| Producto -> ( *. )
	| Division -> ( /. );;

let operacion_unaria o = match o with
	Opuesto -> (~-.)
	| Funcion s -> Libreria.funcion s;;




let rec evaluar_expresion d e = match e with
	Constante f -> f
	|Variable s -> consultar_valor d s
	| Operacion_unaria (o,se) -> (operacion_unaria o) (evaluar_expresion d se)
	| Operacion_binaria (o,se1,se2) -> (operacion_binaria o) (evaluar_expresion d se1) (evaluar_expresion d se2);;
