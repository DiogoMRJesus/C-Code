open Z

(* Creation of the array that will store the already calculated values for optimization *)
let array = Array.make 10001 Z.zero

(* Recursive function that will calculate the Motzkin number *)
let rec motzkin n = 
  if n < Z.of_int 2 then Z.one
  else if array.(Z.to_int n) > Z.zero then array.(Z.to_int n)
  else
    let dn1 = Z.mul (Z.of_int 3) (Z.sub n (Z.of_int 1)) in
    let dn2 = Z.add (Z.mul (Z.of_int 2) n) (Z.of_int 1) in
    let dn3 = Z.add n (Z.of_int 2) in
    let mn1 = Z.sub n (Z.of_int 2) in
    let mn2 = Z.sub n (Z.of_int 1) in
    let resultado = 
      Z.div 
        (Z.add 
          (Z.mul dn1 (motzkin mn1)) 
          (Z.mul dn2 (motzkin mn2))
        ) 
        dn3 
    in
    array.(Z.to_int n) <- resultado; (* Assign the calculated value to its respective slot in the Array *)
    resultado

(* Main function where the input and output reading and writing will occur *)
let () =
  let input = Z.of_int (read_int ()) in 
  if input <= Z.of_int 10000 && input >= Z.zero then 
    let output = motzkin input in 
    print_string (Z.to_string output);
    print_endline ""

(* 
Diogo Jesus nº 49676
Gonçalo Caixeiro nº 49758
Sources: https://v2.ocaml.org/manual/index.html
         https://stackoverflow.com/questions/71889378/how-should-i-implement-this-algorithm-with-zarith-because-it-only-works-till-26
         Material provided by the professor

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////   

 This program is divided into three parts:

- Calculation of the Motzkin number;
  The value is calculated by the recursive expression «motzkin» which is divided into two blocks through a conditional instruction. If the requested number is less than 2, the output value will automatically be 1 as defined in the expression.
  Otherwise (greater than or equal to 2), we will move to the second block, where the Motzkin number will be calculated.
  In this block, an expression was placed that translates the calculation of the Motzkin number.
  This expression was segmented into several subfunctions to facilitate the reading of the code in the transition to Zarith
  (library used to "deal" with large numbers), thus preventing errors.
  These subfunctions will be called by the Motzkin function, which will recursively call the Motzkin functions
  of the lower values until it reaches a known value, in this case 1, and proceeds with the calculations.

- Memorization of the value;
  Since we are working with very large numbers, it was necessary to memorize the calculated values
  to reduce the program execution time, as most of the program's execution time is due to
  the calculation of the Motzkin number.
  Therefore, it was necessary to add an Array to the program, called "array", which was used to store the values previously calculated 
  by the function, given that for the calculation of a very large Motzkin number, it will be necessary to calculate 
  the same value several times (Example: Motzkin 4 - Needs to calculate Motzkin of 2 twice) and the larger the value,
  the more repetitions will occur, so whenever it is necessary to calculate a value, it will first be checked in that specific position
  of the array if that number is greater than zero (that is, if there is a stored value in that position), if there is a stored value, the 
  program uses the previously calculated value, otherwise it proceeds to the function where it is forced to calculate it.
  Finally, this value is stored in the corresponding "n" position of the array to be used if necessary.

- Reading (numbers from 0 to 10000) and data presentation.
  In this program, an input is received, of the integer type, which is subsequently converted to the "Zarith" library type.
  It is then tested whether the input value is within the desired range. If false, the program ends; otherwise, 
  the program proceeds, where it will introduce the "input" value into the recursive "motzkin" function, and its value 
  will be stored in the "output" variable.
  Finally, this value will be printed using "print_string" and a line break will be made to place the program's "output"
  on the next line.

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

Motzkin number calculation demonstration

«Input»

5

«Calculation of the Motzkin number and its presentation»

21     

«End of Program»

Motzkin number calculation demonstration if the entered number is out of the imposed range 

«Input»

-3

«No output as the number is less than 0»

«End of Program»
*)
