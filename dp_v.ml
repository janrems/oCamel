
(* ===== Vaja: Dinamično programiranje  ===== *)

(* Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
   samo za eno polje navzdol ali za eno polje na desno in na koncu prispeti v
   desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
   različne (pozitivne) mase. Miška bi se rada kar se da nažrla, zato jo zanima,
   katero pot naj ubere.

   Napišite funkcijo "max_cheese cheese_matrix", ki dobi matriko z
   masami sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
   optimalni poti.

   ----------
   # max_cheese cheese_matrix;;
   - : int = 13
   ----------*)

let test_matrix = [| [| 1 ; 2 ; 0 |];
                     [| 2 ; 4 ; 5 |];
                     [| 7 ; 0 ; 1 |]  |]



			 
 let max_cheese cheese_matrix = 
	let n_vrstic = Array.length cheese_matrix in
	let n_stolpcev = Array.length cheese_matrix.(0) in
		let rec naj_pot x y = 
			let trenutna = cheese_matrix.(x).(y) in 
			let dol = if (x+1) >= n_vrstic then 0 else naj_pot (x+1) y in 
			let desno = if (y+1) >= n_stolpcev then 0 else naj_pot x (y+1) in 
			trenutna + max desno dol 
		in 
	naj_pot 0 0 

(* Rešujemo problem stolpov, ko smo ga spoznali na predavanjih.
   Imamo štiri različne tipe gradnikov, dva modra in dva rdeča.
   Modri gradniki so višin 2 in 3, rdeči pa višin 1 in 2.

   Napiši funkcijo "alternating_towers height", ki za podano višino "height"
   izpiše število različnih stolpov podane višine, kjer se barva gradnikov
   izmenjuje (rdeč na modrem, moder na rdečem itd.).

   Namig: Uporabi dve pomožni funkciji. Za medsebojno rekurzijo uporabi
          ukaz "and".
   ----------
   # alternating_towers 10;;
   - : int = 35
   ---------- *)

let alternating_towers height = 
	let rec rdeci height = 
		if height < 0 then 0 else
		match height with 
		|0 -> 0
		|1|2 -> 1
		|h -> modri(h-1) + modri(h-2) 
	and modri height = 
		if height < 0 then 0 else
		match height with 
		|0|1-> 0
		|2 -> 1
		|3 -> 2
		|h -> rdeci(h-2) + rdeci(h-3)
	in
	rdeci height + modri height 
	
	
