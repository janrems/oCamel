(* Zgodba, ki naj ostane neprevedena:
"Once upon a time, there was a university with a peculiar tenure policy. All
 faculty were tenured, and could only be dismissed for moral turpitude. What
 was peculiar was the definition of moral turpitude: making a false statement
 in class. Needless to say, the university did not teach computer science.
 However, it had a renowned department of mathematics.
 One Semester, there was such a large enrollment in complex variables that two
 sections were scheduled. In one section, Professor Descartes announced that a
 complex number was an ordered pair of reals, and that two complex numbers were
 equal when their corresponding components were equal. He went on to explain
 how to convert reals into complex numbers, what "i" was, how to add, multiply,
 and conjugate complex numbers, and how to find their magnitude.
 In the other section, Professor Bessel announced that a complex number was an
 ordered pair of reals the first of which was nonnegative, and that two complex
 numbers were equal if their first components were equal and either the first
 components were zero or the second components differed by a multiple of 2π. He
 then told an entirely different story about converting reals, "i", addition,
 multiplication, conjugation, and magnitude.
 Then, after their first classes, an unfortunate mistake in the registrar's
 office caused the two sections to be interchanged. Despite this, neither
 Descartes nor Bessel ever committed moral turpitude, even though each was
 judged by the other's definitions. The reason was that they both had an
 intuitive understanding of type. Having defined complex numbers and the
 primitive operations upon them, thereafter they spoke at a level of
 abstraction that encompassed both of their definitions.
 The moral of this fable is that:
   Type structure is a syntactic discipline for enforcing levels of
   abstraction."
 from:
 John C. Reynolds, "Types, Abstraction, and Parametric Polymorphism", IFIP83
 (optional) homework: read the rest of the introduction of the paper at
 https://people.mpi-sws.org/~dreyer/tor/papers/reynolds.pdf
 *)


(* Kompleksna števila so zahtevnejša. Pričnimo z Nat. *)


(* Definirajmo signaturo "NAT", ki določa strukturo naravnih števil. Ima
   osnovni tip, funkcijo enakosti, ničlo in enko, seštevanje, odštevanje in
   množenje. Hkrati naj vsebuje pretvorbe iz in v OCamlov "int" tip.*)

module type NAT = sig
    type t
    val eq   : t -> t -> bool
    val zero : t
	val one : t
	val add : t -> t -> t
	val sub : t -> t -> t
	val mult : t -> t -> t
	val of_int : int -> t
	val to_int : t -> int
	
  end


(* Napiši modul, ki zgradi modul s podpisom tipa NAT z uporabo OCamlovega 
   "int" tipa.
   Opozorilo: Dokler ni implementiranih vse funkcij v Nat_int se bo OCaml
   pritoževal. Temu se lahko izogneš tako, da funkcije, ki jih še niso napisane
   nadomestiš s 'failwith "later"'. *)
 
  
module Nat_int : NAT = struct
	type t = int
	let zero = 0
	let one = 1
	let eq x y = (x = y)
	let add x y = x+y
	let sub x y = max (x - y) 0
	let mult x y = x*y
	let of_int x = max x 0
	let to_int x = x
end


(* Sedaj naravna števila naredi s pomočjo Peanovih aksiomov
   https://en.wikipedia.org/wiki/Peano_axioms
   
   Osnovni tip modula podaj kot vsotni tip, kjer imaš ničlo in pa 
   naslednjika nekega naravnega števila [Zero in Successor n].
   Funkcije implementiraj s pomočjo rekurzije. Števili m in n sta enaki, če
   sta obe 0, ali pa sta naslednika k in l, kjer sta k in l enaki števili. *)



module Peano : NAT = struct
  type t = Zero| Successor of t
  let zero = Zero
  let one = Successor Zero
  let rec eq x y = 
	match x,y with
	|Zero,Zero -> true
	|Zero,_ -> false
	|_,Zero -> false
	|Successor  z, Successor  v -> eq z v 
  
  let rec add x y = 
	match x with
	|Zero -> y
	|Successor z -> add (z) (Successor y)
	
  let rec sub x y = 
	match x, y with 
	|Zero, _ -> Zero
	|x,Zero -> x
	|Successor z, Successor v -> sub z v
	
  let rec of_int x = 
	match x with
	|0 -> Zero
	|x -> if x < 0 then Zero 
			else Successor (of_int (x-1))

  let rec to_int x =
	match x with
	|Zero -> 0
	|Successor x -> (to_int x) + 1
	
  let mult x = failwith "sadsd"  
end
 

(* Definiraj signaturo modula kompleksnih števil.
   Potrebujemo osnovni tip, test enakosti, ničlo, enko, imaginarno konstanto i,
   negacijo števila, konjugacijo, seštevanje, množenje, deljenje in inverz. *)

(module type COMPLEX = sig
    type t = 
	val zero : t
	val one : t
	val i : t
    val eq : t -> t -> bool
    val neg : t
	val konj : t
	val add : t -> t -> t
	val mult : t -> t -> t
	val div : t -> t -> t
	val inv : t -> t
  end
 *)

(* Napiši kartezično implementacijo kompleksnih števil (torej z = x + iy).
   Deljenje je zahtevnejše, zato si ga lahko s 'failwith' trikom pustiš za kasneje.
 *)
 
(*
module Cartesian : COMPLEX = struct
  type t = {re : float; im : float}
  let eq x y = x.re = y.re && ...
  ...
end
 *)


(* Sedaj napiši še polarno implementacijo kompleksnih števil (torej z = r e^(i*fi) ).
   Seštevanje je v polarnih koordinatah zahtevnejše, zato najprej napiši druge reči. *)
   
(*
module Polar : COMPLEX = struct
  type t = {magn : float; arg : float}
  let pi = 2. *. acos 0.
  let rad deg = (deg /. 180.) *. pi
  let deg rad = (rad /. pi) *. 180.
  let eq x y =
  ...
end
 *)