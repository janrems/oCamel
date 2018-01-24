type izraz = Samo of int|Plus of izraz*izraz|Krat of izraz*izraz

let rec izracunaj izraz = 
	match izraz with
	|Samo a -> a
	|Plus(a, b) -> izracunaj a + izracunaj b
	|Krat(a,b) -> izracunaj a * izracunaj b
	
let vsota = (<)