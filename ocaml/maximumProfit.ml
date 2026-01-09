let rec read_numbers prices m =
  match m with
  | 0 -> ()
  | _ -> let tamanho, preco = Scanf.scanf "%d %d\n" (fun x y -> x, y) in
         prices.(tamanho) <- preco;
         read_numbers prices (m-1)

  (* Recursive function to read slice sizes and prices *)

let maiorvalor array array2 m n =
  array.(1) <- array2.(1);
  let rec for_i i =
    if i <= n then
      let rec for_j j =
        if j <= m then begin   

          (* Check if the current slice size (j) is <= the total number of slices (i), to verify if we can sell the slice (j) *) 

          if j <= i then begin

            (* Calculate the current profit by adding the max profit obtained from the remaining slices "i-j" *)

            let lucro = array.(i-j) + array2.(j) in

            (* If this calculated profit is greater than the max profit previously calculated for the slice number "i", update the max profit *)

            if lucro > array.(i) then array.(i) <- lucro
          end;
          for_j (j+1)
        end
      in for_j 1;
      for_i (i+1)
  in for_i 2

  (* Recursive function to calculate the maximum profit *)

let () = 
  let n = read_int () in
  let m = read_int () in

  (* Variable initialization *)

  if (m<=10000 && m>0) && (n<=10000 && n>0) && (m<=n) then

    (* Check if the variables are within the requested limits *)
    
    let prices = Array.make 10001 0 in
    read_numbers prices m;
    let array3 = Array.make 10001 0 in (* This array will be used to store the maximum profit for each number of cake slices *)
    array3.(0) <- 0;
    maiorvalor array3 prices m n;
    Printf.printf "%d\n" array3.(n)

    (* Sources: https://v2.ocaml.org/manual/index.html

            https://stackoverflow.com/questions/71889378/how-should-i-implement-this-algorithm-with-zarith-because-it-only-works-till-26

            Material provided by the professor

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////  


    The user is asked to enter two values that will be stored in the variables m = number of slice sizes & 
    n = Maximum number of parts/slices in which a cake can be cut. In turn, the slice size values (x) and their
    corresponding price (y) will be read by a recursive function that will transform these two values into tuples. 
    This reading of values through the recursive function will fill the positions of the array from the last position to the first (Bottom-Top)
    and will be used together with a match structure that will successively compare the value of "k" ("m-1x" with 0<x<=m) and if the value of k = 0 it concludes 
    that the entire array has been filled and then it will exit the cycle and proceed to the continuation of the program, otherwise, that is, if it finds
    another value different from 0 it will read again another 2 numbers (later transformed into tuples) to fill the array.

    Next, another function was created to calculate the highest profit, which uses dynamic programming.
      Initially, the initial position is assigned the value 0, because if no cake slices are sold, the profit will be 0.
      The "array" array will have to start at position +1 since the previous value has already been assigned to 0 (if no slices are sold) 
      The «array3» array will store the maximum values and the «prices» array will store the slice prices.
      This function has two iterations, with the first starting with the outer loop iterating from 1 to the total number of slices "n"
      In turn, in the inner loop, it will also iterate from 1 to the maximum size of slices that can be sold "m".
      Then in line 23, we will calculate the profit from selling a certain number of cake slices, that is, each "j" corresponds to the slice currently being 
      sold. To calculate the highest profit, the function will recursively check the previous sizes to calculate the maximum value for that
      slice size, and then as it progresses in the array containing the maximum value for that same size, it will check if it is more
      advantageous to sell the slice alone/isolated or to sell the slice as a "combination of previous slices", and after making this calculation
      it will compare the profits obtained with the value in that index of the array (corresponding to the size) and will place the higher value in the array,
      that is, the most advantageous value (highest profit).
      Finally, it will print the value corresponding to the maximum profit that can be obtained by selling n -> number of slices into which a cake can be cut, having
      m -> slice sizes into which the cake can be cut.

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

      Program Demonstration for calculating the highest profit:

      n = 8
      m = 7 

      Check if m and n values are within the desired range 
      (7<=10000 && 7>0) && (8<=10000 && 8>0) && (7<=8) --> true
      proceeds to the continuation of the program (calculation of the highest profit)...

      tuples (slice size/price relation)

      1 2
      2 4
      3 8
      5 12
      6 17
      7 17
      8 20

      Calculation of the highest profit

      21

      Presentation of the value (in this case 21)

      *)
