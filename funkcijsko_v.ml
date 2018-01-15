(* ===== Vaja 2: Funkcijsko Programiranje  ===== *)

(*Namig: Napiši si pomožno funkcijo za obračanje seznamov. *)
let reverse l =  
	let rec reverse_aux l  acc =
	match l with
	|[] -> acc
	|hd :: tl ->  reverse_aux tl (hd::acc)
	in
	reverse_aux l []
  
(* Funkcija "repeat x n" vrne seznam n ponovitev x. 
 Za neprimerne n funkcija vrne prazen seznam.
 ----------
 # repeat "A" 5;;
 - : string list = ["A"; "A"; "A"; "A"; "A"]
 # repeat "A" (-2);;
 - : string list = []
 ---------- *)

let rec repeat x n = 
	if n > 0 then x :: repeat x (n-1) else []

(* Funkcija "range n" sprejme številio n in vrne seznam vseh celih števil od 0
 do vključno n. Za neprimerne n funkcija vrne prazen seznam.
 Funkcija je repno rekurzivna.
 ----------
 # range 10;;
 - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
 ---------- *)

let range n = 
	let rec range_aux n acc = 
		if n > -1 then  range_aux(n-1) (n :: acc) else acc in
	range_aux n [] 

(* Funkcija "map f l" sprejme seznam l = [l0; l1; l2; ...] in funkcijo f
 in vrne seznam [f(l0); f(l1); f(l2); ...].
 ----------
 # let plus_two = (+)2 in
   map plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
 ---------- *)

let rec map f l = 
	match l with
	|[] -> []
	|hd::tl -> (f hd) :: map f tl

(* Funkcija "map_tlrec" je tail-recursive verzija funkcije map.
 ----------
 # let plus_two = (fun x -> x+2) in
   map_tlrec plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
 ---------- *)

let map_tlrec f l = 
	let rec map_aux f l acc =
		match l with 
		|[] -> reverse acc
		|hd :: tl -> map_aux f tl (f hd :: acc)
	in
	map_aux f l []

(* Funkcija "mapi f l" sprejme seznam l = [l0; l1; l2; ...] in funkcijo f
 ter vrne seznam [f 0 l0; f 1 l1; f 2 l2; ...].
 ----------
 # mapi (+) [0; 0; 0; 2; 2; 2];;
 - : int list = [0; 1; 2; 5; 6; 7]
 ---------- *)

let mapi f l =
	let rec mapi_aux f l n = 
		match l with 
		|[] -> []
		|hd :: tl -> (f n hd) :: mapi_aux f tl (n+1)
	in
	mapi_aux f l 0
	

(* Funkcija "zip l1 l2" sprejme seznama l1 = [l1_0; l1_1; l1_2; ...] in
 l2 = [l2_0; l2_1; l2_2; ...] in vrne seznam [(l1_0,l2_0); (l1_1,l2_1); ...].
 Če seznama nista enake dolžine vrne napako.
 ----------
 # zip [1; 1; 1; 1] [0; 1; 2; 3];;
 - : (int * int) list = [(1, 0); (1, 1); (1, 2); (1, 3)]
 # zip [1; 1; 1; 1] [1; 2; 3; 4; 5];;
 Exception: Failure "Seznama razlicnih dolzin.".
 ---------- *)

let rec zip l1 l2 = 
	match (l1,l2) with
	|([],[])-> []
	|([],_) -> failwith "Seznama razlicnih dolzin."
	|(_,[]) -> failwith "Seznama razlicnih dolzin."
	|(hd1::tl1, hd2::tl2) -> (hd1,hd2) :: zip tl1 tl2

(* Funkcija "zip_enum_tlrec l1 l2" sprejme seznama l1 = [l1_0; l1_1; l1_2; ...] 
 in l2 = [l2_0; l2_1; l2_2; ...] in vrne [(0, l1_0, l2_0); (1, l1_1, l2_1); ...].
 Funkcija je repno rekurzivna.
 Če seznama nista enake dolžine vrne napako.
 ----------
 # zip_enum_tlrec ["a"; "b"; "c"; "d"] [7; 3; 4; 2];;
 - : (int * string * int) list = [(0, "a", 7); (1, "b", 3); (2, "c", 4); (3, "d", 2)]
 ---------- *)

let zip_enum_tlrec l1 l2 = 
	let rec zip_aux l1 l2 n = 
		match (l1,l2) with
		|([],[])-> []
		|([],_) -> failwith "Seznama razlicnih dolzin."
		|(_,[]) -> failwith "Seznama razlicnih dolzin."
		|(hd1::tl1, hd2::tl2) -> (n, hd1, hd2) :: zip_aux tl1 tl2 (n+1)
	in
	zip_aux l1 l2 0

(* Funkcija "unzip l" sprejme seznam l = [(a0, b0); (a1, b2); ...]
 in vrne dvojico seznamov ([a0; a1; ...], [b0; b1; ...]).
 ----------
 # unzip [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
 ---------- *)

let unzip l = List.split l
	
	

(* Funkcija "unzip_tlrec l" je tail-recursive verzija funkcije unzip.
 ----------
 # unzip_tlrec [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
 ---------- *)

let unzip_tlrec l = 
	let rec unzip_aux l acc1 acc2 =
		match l with 
		|[] -> (reverse acc1, reverse acc2)
		|(x,y)::tl -> unzip_aux tl (x::acc1) (y::acc2)
	in
	unzip_aux l [] []

(* Funkcija "fold_left_no_acc f l" sprejme seznam l = [l0; l1; l2; ...; ln] in funkcijo f,
 vrne pa f(... (f (f (f l0 l1) l2) l3) ... ln).
 V primeru seznama z manj kot dvema elementoma vrne napako.
 ----------
 # fold_left_no_acc (^) ["F"; "I"; "K"; "U"; "S"];;
 - : string = "FIKUS"
 ---------- *)

let rec fold_left_no_acc f l = 
	match l with
	|[] -> failwith "premajh"
	|[_] -> failwith "premajh"
	|hd::tl::[] -> f hd tl 
	|hd :: tl -> f hd (fold_left_no_acc f tl)


(* Funkcija "apply_sequence f x n" vrne seznam zaporednih uporab funkcije f na x,
 [x; f x; f (f x); ...; f uporabljena n-krat na x].
 Funkcija je repno rekurzivna.
 ----------
 # apply_sequence (fun x -> x*x) 2 5;;
 - : int list = [2; 4; 16; 256; 65536; 4294967296]
 # apply_sequence (fun x -> x*x) 2 (-5);;
 - : int list = []
 ---------- *)

let apply_sequence f x n = 
	let rec aply_aux f n acc df =
		if n > 0 then aply_aux f (n-1) ((f df ) :: acc) (f df) else reverse acc
	in 
	aply_aux f n [] x
		

(* Funkcija "filter f l" sprejme seznam l in vrne seznam elementov l,
 za katere funkcija f vrne true.
 ----------
 # filter ((<)3) [0; 1; 2; 3; 4; 5];;
 - : int list = [4; 5]
 ---------- *)

let rec filter f l = 
	match l with
	|[] -> []
	|hd::tl -> if (f hd = true) then hd::filter f tl else filter f tl

(* Funkcija "exists f l" sprejme seznam l in vrne true če obstaja 
element
 seznama l, za katerega fukcija f vrne true in false sicer.
 Funkcija je repno rekurzivna.
 ----------
 # exists ((<)3) [0; 1; 2; 3; 4; 5];;
 - : bool = true
 # exists ((<)8) [0; 1; 2; 3; 4; 5];;
 - : bool = false
 ---------- *)

let rec exists f = function
	|[] -> false
	|hd::tl -> if (f hd =true) then true else exists f tl

(* Funkcija "first f none_value l" sprejme seznam l in vrne prvi element seznama,
 za katerega funkcija f vrne true, če takšnega elementa ni, pa vrne none_value.
 Funkcija je repno rekurzivna.
 ----------
 # first ((<)3) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 5
 # first ((<)8) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 0
 ---------- *)

let rec first f none_value l = 
	match l with
	|[] -> none_value
	|hd ::tl -> if (f hd = true) then hd else first f none_value tl
  
(* Severnjaki napadajo Medbrezje. Kot vrhovni čarodej poznaš zaporedje urokov s katerimi
 lahko Medbrezje zaščitiš pred napadom, zaporedje urokov pa je predstavljeno v seznamu oblike
 [("ime1", vrednost1); ("ime2", vrednost2); ...].
 Na razpolago imaš skupino čarodejov, ki so prav tako predstavljeni v seznamu oblike
 [("ime1", spretnost1); ("ime2", spretnost2); ...].
 Čarodej lahko izvede zaporedje urokov, če je njegova spretnost večja ali enaka skupni
 vrednosti vseh urokov v zaporedju.
 Funkcija "able_protectors spells wizards" vrne seznam imen vseh čarodejov, ki lahko
 samostojno zaščitijo Medbrezje.
 Funkcija "fails_on spells wizards" vrne seznam parov (čarodej, neuspešni urok), kjer
 je neuspešni urok prvi urok v zaporedju, za katerega čarodej nima več dovolj spretnosti.
 Če lahko čarodej zaporedje izvede v celoti, to predstavlja prazen niz.
 Namig: Dober čarodej uporablja svoje znanje in izkušnje, ki jih pridobi tekom učenja.
 ----------
 # let spells = [("Protect",51); ("Renounce", 17); ("Blaze", 420); ("Banish",103)] in
   let wizards = [("Merlin", 1832); ("Frodo", 53); ("Ajitam", 1337);
  ("Mr Duck", 7); ("Kylo Ren", 589); ("Snoop Dogg", 123)] in
   able_protectors spells wizards;;
 - : string list = ["Merlin"; "Ajitam"]
 ----------
 # let spells = [("Protect",51); ("Renounce", 17); ("Blaze", 420); ("Banish",103)] in
   let wizards = [("Merlin", 1832); ("Frodo", 53); ("Ajitam", 1337);
  ("Mr Duck", 7); ("Kylo Ren", 589); ("Snoop Dogg", 123)] in
   fails_on spells wizards;;
 - : (string * string) list = [("Merlin", ""); ("Frodo", "Renounce"); ("Ajitam", "");
  ("Mr Duck", "Protect"); ("Kylo Ren", "Banish"); ("Snoop Dogg", "Blaze")]
 ----------*)

let able_protectors spells wizards = 
	let (s1,s2) = unzip spells in 
	let (w1,w2) = unzip wizards in 
	let vsota = fold_left_no_acc (+) s2 in
	let rec dobri pogoj w1 w2 acc =
		match (w1,w2) with 
		|([],[]) -> reverse acc
		|(hd1 :: tl1, hd2 ::tl2) -> if hd2 > pogoj then dobri pogoj tl1 tl2 (hd1 :: acc) else dobri pogoj tl1 tl2 acc
	in
	dobri vsota w1 w2 []
		
	
let fails_on spells wizards = 
	let rec problem spells wiz_ab = 
		match spells with 
		|[] -> ""
		|(sp_name,sp_val)::tl -> 
			if wiz_ab >= sp_val
			then problem tl (wiz_ab - sp_val)
			else sp_name
	in
	let (wiz_name,wiz_ab) = unzip wizards in 
	let uroki = map (problem spells) wiz_ab in 
	zip wiz_name uroki












