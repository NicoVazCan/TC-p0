(*1*)

let rec mapdoble fi fp = function
	| [] -> []
	| h::[] -> (fi h)::[]
	| hi::hp::t -> (fi hi)::(fp hp)::(mapdoble fi fp t);;

mapdoble (function x -> x) (function x -> -x) [1;1;1;1;1];;
(*
	- : int list = [1; -1; 1; -1; 1]
*)
mapdoble ((<) 4) ((>) 4) [];;
(*
	- : bool list = []
*)
mapdoble ((<) 4) ((>) 4) [1];;
(*
	- : bool list = [false]
*)
mapdoble ((<) 4) ((>) 4) [1;2;3;4;5;6;7];;
(*
	- : bool list = [false; true; false; false; true; false; true]
*)
mapdoble ((<) 4) ((>) 4) [1;2;3;4;5;6;7;8];;
(*
	- : bool list = [false; true; false; false; true; false; true; false]
*)


(*
	val mapdoble : ('a -> 'b) -> ('a -> 'b) -> 'a list -> 'b list = <fun>
*)

(*
	# mapdoble (function x -> x*2) (function x -> "x") [1;2;3;4;5];;                           
	Error: This expression has type string but an expression was expected of type int
*)

(*
	let y = function x -> 5 in mapdoble y;;

	- : ('_weak1 -> int) -> '_weak1 list -> int list = <fun>
*)


(*2*)

let rec primero_que_cumple f = function
	| [] -> raise Not_found
	| h::t -> if f h then h else primero_que_cumple f t;;

(*primero_que_cumple ((<) 4) [];;

	Exception: Not_found.
*)
(*primero_que_cumple ((<) 4) [1;2;3];;

	Exception: Not_found.
*)
primero_que_cumple ((<) 4) [1;2;3;4;5;6];;
(*
	- : int = 5
*)
primero_que_cumple ((<) 4) [5;1;2;3;4;];;
(*
	- : int = 5
*)
primero_que_cumple ((<) 4) [1;2;3;4;5;];;
(*
	- : int = 5
*)

(*
	val primero_que_cumple : ('a -> bool) -> 'a list -> 'a = <fun>
*)

let existe f l = try
	let _ = (primero_que_cumple f l) in true with
		| Not_found -> false;;

existe ((<) 2) [];;
(*
	- : bool = false
*)
existe ((<) 2) [1;2];;
(*
	- : bool = false
*)
existe ((<) 2) [1;2;3];;
(*
	- : bool = true
*)
existe ((<) 2) [1;2;3;4];;
(*
	- : bool = true
*)
existe ((<) 2) [4;1;2;3];;
(*
	- : bool = true
*)
let asociado l e = 
	let _, v = primero_que_cumple (fun (el, _) -> el = e) l in v;;

(*asociado [] 0;;

	Exception: Not_found.
*)
asociado [1,'a'] 1;;
(*
	- : char = 'a'
*)
(*asociado [1,'a'] 0;;

	Exception: Not_found.
*)
asociado [1,'a';2,'b';3,'c';4,'d'] 1;;
(*
	- : char = 'a'
*)
(*asociado [1,'a';2,'b';3,'c';4,'d'] 5;;

	Exception: Not_found.
*)
(*asociado [1,'a';2,'b';3,'c';4,'d'] 0;;

	Exception: Not_found.
*)

(*3*)

type 'a arbol_binario =
	  Vacio
	| Nodo of 'a * 'a arbol_binario * 'a arbol_binario;;

let test_tree0 =
	Nodo (3,
		Nodo (2, Vacio, Vacio),
		Nodo (5,
			Nodo (4, Vacio, Vacio),
			Nodo (1, Vacio, Vacio)))
and test_tree1 =
	Nodo (1,
		Nodo (2,
			Nodo (4, Vacio, Vacio),
			Nodo (5, Vacio, Vacio)),
		Nodo (3,
			Nodo (6, Vacio, Vacio),
			Nodo (7, Vacio, Vacio)));;

let rec in_orden = function
	| Vacio -> []
	| Nodo (v, ri, rd) -> ((in_orden ri)@(v::in_orden rd));;

in_orden test_tree0;;
(*
	- : int list = [2; 3; 4; 5; 1]
*)
in_orden test_tree1;;
(*
	- : int list = [4; 2; 5; 1; 6; 3; 7]
*)

let rec pre_orden = function
	| Vacio -> []
	| Nodo (v, ri, rd) -> v::((pre_orden ri)@(pre_orden rd));;

pre_orden test_tree0;;
(*
	- : int list = [3; 2; 5; 4; 1]
*)
pre_orden test_tree1;;
(*
	- : int list = [1; 2; 4; 5; 3; 6; 7
*)

let rec post_orden = function
	| Vacio -> []
	| Nodo (v, ri, rd) -> ((post_orden ri)@(post_orden rd))@[v];;

post_orden test_tree0;;
(*
	- : int list = [2; 4; 1; 5; 3]
*)
post_orden test_tree1;;
(*
	- : int list = [4; 5; 2; 6; 7; 3; 1]
*)

let anchura tree = 
	let rec aux = function
		| [] -> []
		| Vacio::t -> aux t
		| (Nodo (v, ri, rd))::t -> v::(aux (t@[ri; rd]))
	in aux [tree];;

anchura test_tree0;;
(*
	- : int list = [3; 2; 5; 4; 1]
*)
anchura test_tree1;;
(*
	- : int list = [1; 2; 3; 4; 5; 6; 7]
*)


(*4*)

type 'a conjunto = Conjunto of 'a list;;


let rec pertenece e = function
	| Conjunto [] -> false
	| Conjunto (h::t) -> h = e || pertenece e (Conjunto t);;

pertenece 1 (Conjunto []);;
(*
	- : bool = false
*)
pertenece 1 (Conjunto [1;2;3;4]);;
(*
	- : bool = true
*)
pertenece 0 (Conjunto [1;2;3;4]);;
(*
	- : bool = false
*)

let agregar e c =
	if pertenece e c then c
	else let Conjunto l = c in Conjunto (e::l);;

agregar 1 (Conjunto [1;2;3;4]);;
(*
	- : int conjunto = Conjunto [1; 2; 3; 4]
*)
agregar 0 (Conjunto [1;2;3;4]);;
(*
	- : int conjunto = Conjunto [0; 1; 2; 3; 4]
*)

let conjunto_of_list l =
	let rec aux c = function
		| [] ->  c
		| h::t -> aux (agregar h c) t
	in aux (Conjunto []) l;;

conjunto_of_list [];;
(*
	- : 'a conjunto = Conjunto []
*)
conjunto_of_list [1;2;3;4];;
(*
	- : int conjunto = Conjunto [4; 3; 2; 1]
*)

let suprimir e (Conjunto l) =
	let rec aux e = function
		| [] -> []
		| h::t -> if h = e 
			then t
			else h::(aux e t)
	in Conjunto (aux e l);;

suprimir 0 (Conjunto []);;
(*
	- : int conjunto = Conjunto []
*)
suprimir 0 (Conjunto [0]);;
(*
	- : int conjunto = Conjunto []
*)
suprimir 0 (Conjunto [0;1;3]);;
(*
	- : int conjunto = Conjunto [1; 3]
*)
suprimir 2 (Conjunto [1;2;3;4]);;
(*
	- : int conjunto = Conjunto [1; 3; 4]
*)
suprimir 0 (Conjunto [1;2;3;4]);;
(*
	- : int conjunto = Conjunto [1; 2; 3; 4]
*)

let cardinal (Conjunto l) =
	let rec aux n = function
		| [] -> n
		| _::t -> aux (n+1) t
	in aux 0 l;;

cardinal (Conjunto []);;
(*
	- : int = 0
*)
cardinal (Conjunto [1]);;
(*
	- : int = 1
*)
cardinal (Conjunto [1;2]);;
(*
	- : int = 2
*)
cardinal (Conjunto [1;2;3]);;
(*
	- : int = 3
*)
cardinal (Conjunto [1;2;3;4]);;
(*
	- : int = 4
*)

let rec union c = function
	| Conjunto [] -> c
	| Conjunto (h::t) -> union (agregar h c) (Conjunto t);;

union (Conjunto []) (Conjunto []);;
(*
	- : 'a conjunto = Conjunto []
*)
union (Conjunto []) (Conjunto [1;2;3;4]);;
(*
	- : int conjunto = Conjunto [4; 3; 2; 1]
*)
union (Conjunto [1;2;3;4]) (Conjunto []);;
(*
	- : int conjunto = Conjunto [1; 2; 3; 4]
*)
union (Conjunto [1;2;3;4]) (Conjunto [3;4;5;6]);;
(*
	- : int conjunto = Conjunto [6; 5; 1; 2; 3; 4]
*)

let interseccion c1 c2 =
	let rec aux c1 c2 lr = match c2 with  
		| Conjunto [] -> Conjunto lr
		| Conjunto (h::t) -> if pertenece h c1
			then aux c1 (Conjunto t) (h::lr)
			else aux c1 (Conjunto t) lr
	in aux c1 c2 [];;

interseccion (Conjunto []) (Conjunto []);;
(*
	- : 'a conjunto = Conjunto []
*)
interseccion (Conjunto []) (Conjunto [1;2;3;4]);;
(*
	- : int conjunto = Conjunto []
*)
interseccion (Conjunto [1;2;3;4]) (Conjunto []);;
(*
	- : int conjunto = Conjunto []
*)
interseccion (Conjunto [1;2;3;4]) (Conjunto [3;4;5;6]);;
(*
	- : int conjunto = Conjunto [4; 3]
*)

let rec diferencia c = function
	| Conjunto [] -> c
	| Conjunto (h::t) -> if pertenece h c
		then diferencia (suprimir h c) (Conjunto t)
		else diferencia c (Conjunto t);;

diferencia (Conjunto []) (Conjunto []);;
(*
	- : 'a conjunto = Conjunto []
*)
diferencia (Conjunto []) (Conjunto [1;2;3;4]);;
(*
	- : int conjunto = Conjunto []
*)
diferencia (Conjunto [1;2;3;4]) (Conjunto []);;
(*
	- : int conjunto = Conjunto [1; 2; 3; 4]
*)
diferencia (Conjunto [1;2;3;4]) (Conjunto [3;4;5;6]);;
(*
	- : int conjunto = Conjunto [1; 2]
*)

let rec incluido c1 c2 = match c1 with
	| Conjunto [] -> true
	| Conjunto (h::t) -> pertenece h c2 && incluido (Conjunto t) c2;;

incluido (Conjunto []) (Conjunto []);;
(*
	- : bool = true
*)
incluido (Conjunto []) (Conjunto [1;2;3;4]);;
(*
	- : bool = true
*)
incluido (Conjunto [1;2;3;4]) (Conjunto []);;
(*
	- : bool = false
*)
incluido (Conjunto [1;2;3;4]) (Conjunto [1;2;3;4]);;
(*
	- : bool = true
*)
incluido (Conjunto [1;2;3;4]) (Conjunto [1;2;3;4;5;6]);;
(*
	- : bool = true
*)
incluido (Conjunto [1;2;3;4;5;6]) (Conjunto [1;2;3;4]);;
(*
	- : bool = false
*)

let igual c1 c2 = incluido c1 c2 && incluido c2 c1;;

igual (Conjunto []) (Conjunto []);;
(*
	- : bool = true
*)
igual (Conjunto []) (Conjunto [1;2;3;4]);;
(*
	- : bool = false
*)
igual (Conjunto [1;2;3;4]) (Conjunto []);;
(*
	- : bool = false
*)
igual (Conjunto [1;2;3;4]) (Conjunto [1;2;3;4]);;
(*
	- : bool = true
*)
igual (Conjunto [1;2;3;4]) (Conjunto [1;2;3;4;5;6]);;
(*
	- : bool = false
*)
igual (Conjunto [1;2;3;4;5;6]) (Conjunto [1;2;3;4]);;
(*
	- : bool = false
*)

let rec producto_cartesiano (Conjunto l1) (Conjunto l2) =
	let rec aux1 v l = function
		| [] -> l
		| h::t -> (v, h)::(aux1 v l t)
	and aux2 = function
		| [] -> []
		| h::t -> (aux1 h (aux2 t) l2)
	in Conjunto (aux2 l1);;

producto_cartesiano (Conjunto []) (Conjunto [1;2;3]);;
(*
	- : ('a * int) conjunto = Conjunto []
*)
producto_cartesiano (Conjunto [1;2;3]) (Conjunto []);;
(*
	- : (int * 'a) conjunto = Conjunto []
*)
producto_cartesiano (Conjunto [1;2;3]) (Conjunto [1]);;
(*
	- : (int * int) conjunto = Conjunto [(1, 1); (2, 1); (3, 1)]
*)
producto_cartesiano (Conjunto [1]) (Conjunto [1;2;3]);;
(*
	- : (int * int) conjunto = Conjunto [(1, 1); (1, 2); (1, 3)]
*)
producto_cartesiano (Conjunto [1;2;3]) (Conjunto [1;2;3]);;
(*
	- : (int * int) conjunto =
	Conjunto
	 [(1, 1); (1, 2); (1, 3); (2, 1); (2, 2); (2, 3); (3, 1); (3, 2); (3, 3)]
*)

let list_of_conjunto (Conjunto l) = l;;

list_of_conjunto (Conjunto []);;
(*
	- : 'a list = []
*)
list_of_conjunto (Conjunto [1;2;3;4]);;
(*
	- : int list = [1; 2; 3; 4]
*)
