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
    val eq : t -> t -> bool
    val zero : t
	val one : t
	val add : t -> t -> t 
	val sub : t -> t -> t
	val mult : t -> t -> t
	val int_of_nat : t -> int
	val nat_of_int : int -> t
  end


(* Napiši modul, ki zgradi modul s podpisom tipa NAT z uporabo OCamlovega 
   "int" tipa.
   Opozorilo: Dokler ni implementiranih vse funkcij v Nat_int se bo OCaml
   pritoževal. Temu se lahko izogneš tako, da funkcije, ki jih še niso napisane
   nadomestiš s 'failwith "later"'. *)
 
 
module Nat_int : NAT = struct
	type t = int
	let eq  = (=)
	let zero = 0
	let one = 1
	let add t1 t2 = t1 +t2
	let sub t1 t2 = max (t1 - t2) 0
	let mult t1 t2 = t1*t2
	let int_of_nat t = t
	let nat_of_int t = max t 0
end


(* Sedaj naravna števila naredi s pomočjo Peanovih aksiomov
   https://en.wikipedia.org/wiki/Peano_axioms
   
   Osnovni tip modula podaj kot vsotni tip, kjer imaš ničlo in pa 
   naslednjika nekega naravnega števila [Zero in Successor n].
   Funkcije implementiraj s pomočjo rekurzije. Števili m in n sta enaki, če
   sta obe 0, ali pa sta naslednika k in l, kjer sta k in l enaki števili. *)



module Nat_Pean :NAT = struct
  type t = Zero | Successor of t 
  let rec eq x y = 
	match (x,y) with 
	|(Zero,Zero) -> true
	|(Zero,_)|(_,Zero) -> false
	|(Successor a, Successor b) -> eq a b 
  let zero = Zero
  let one = Successor Zero
  let rec add t1 t2 =
	match (t1,t2) with
	|(Zero,Zero) -> Zero
	|(t1,Zero) -> t1
	|(Zero,t2) -> t2
	|(t1,Successor t2) -> add (Successor t1) t2
  let rec sub t1 t2 = 
	match (t1,t2) with
	|(Zero,Zero) -> Zero
	|(t1,Zero) -> t1
	|(Zero,t2) -> Zero
	|(Successor a,Successor b) -> sub a b 
  let rec mult t1 t2 = 
	match (t1,t2) with 
	|(_,Zero)|(Zero,_) -> Zero
	|(t1,Successor b) -> add t1 (mult t1 b)
  let rec nat_of_int t = 
	match t with 
	|0 -> Zero
	|x -> if x < 0 then Zero
			else Successor (nat_of_int (x-1))
  let rec int_of_nat t = 
	match t with
	|Zero -> 0
	|Successor x -> (int_of_nat x) +1
	
end


(* Definiraj signaturo modula kompleksnih števil.
   Potrebujemo osnovni tip, test enakosti, ničlo, enko, imaginarno konstanto i,
   negacijo števila, konjugacijo, seštevanje, množenje, deljenje in inverz. *)

module type COMPLEX = sig
    type t
    val eq : t -> t -> bool
    val zero : t
	val one : t
	val i : t
	val neg : t -> t
	val konj : t -> t
	val add : t -> t-> t
	val mult : t -> t -> t
	val div : t -> t -> t
	val inv : t -> t
  end


(* Napiši kartezično implementacijo kompleksnih števil (torej z = x + iy).
   Deljenje je zahtevnejše, zato si ga lahko s 'failwith' trikom pustiš za kasneje.
 *)
 

module Cartesian : COMPLEX = struct
  type t = {re : float; im : float}
  let eq x y = (x.re = y.re) && (x.im = y.im)
  let zero = {re = 0; im = 0}
  let one = {re = 1 ; im = 0}
  let i = {re = 0;im = 1}
  let neg z = {re = - z.re; im = - z.im}
  let konj z = {re = z.re;im = -z.im}
  let add z1 z2 = {re = z1.re + z2.re; im = z1.im + z2.im}
  let mult z1 z2 = {re = z1.re*z2.re - z1.im*z2.im; im = z1.re*z2.im + z1.im*z2.re}
  let div z1 z2 = {re = (z1.re*z2.re + z1.im*z2.im)/z2.re*z2.re 
end
 


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