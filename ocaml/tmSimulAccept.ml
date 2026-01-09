type estado = string [@@deriving yaml]

type simbolo = string

let simbolo_to_yaml s = `String s

let simbolo_of_yaml = function
  | `String s -> Ok s
  | `Float f -> Ok (string_of_int (int_of_float f))
  | `Bool b -> Ok (if b then "y" else "n")
  | v -> Error (`Msg ("Invalid simbolo in YAML: " ^ Yaml.to_string_exn v))

type direction = L | R | S

(* Custom serialization/deserialization for direction *)
let direction_to_yaml = function
  | L -> `String "L"
  | R -> `String "R"
  | S -> `String "S"

let direction_of_yaml = function
  | `String "L" -> Ok L
  | `String "R" -> Ok R
  | `String "S" -> Ok S
  | v -> Error (`Msg ("Invalid direction in YAML: " ^ Yaml.to_string_exn v))

type transition = estado * simbolo * direction [@@deriving yaml]

type delta = (string, (string, transition) Hashtbl.t) Hashtbl.t

(* Custom serialization/deserialization for delta *)
let delta_to_yaml delta =
  `O
    (Hashtbl.fold
       (fun key inner acc ->
         let inner_lst =
           Hashtbl.fold
             (fun k v acc2 -> (k, transition_to_yaml v) :: acc2)
             inner []
         in
         (key, `O inner_lst) :: acc )
       delta [] )

let delta_of_yaml = function
  | `O lst ->
      let outer_tbl = Hashtbl.create (List.length lst) in
      List.iter
        (fun (key, value) ->
          match value with
          | `O inner_lst ->
              let inner_tbl = Hashtbl.create (List.length inner_lst) in
              List.iter
                (fun (k, v) ->
                  let transition =
                    match transition_of_yaml v with
                    | Ok t -> t
                    | Error (`Msg m) -> failwith m
                  in
                  Hashtbl.add inner_tbl k transition )
                inner_lst ;
              Hashtbl.add outer_tbl key inner_tbl
          | v ->
              failwith ("Invalid inner delta YAML: " ^ Yaml.to_string_exn v) )
        lst ;
      Ok outer_tbl
  | v -> failwith ("Invalid delta YAML: " ^ Yaml.to_string_exn v)

type tm =
  { states: estado list
  ; input_alphabet: simbolo list
  ; tape_alphabet_extra: simbolo list
  ; start_state: estado
  ; accept_state: estado
  ; reject_state: estado
  ; delta: delta }
[@@deriving yaml]

type tm_w = {m: tm [@key "M"]; w: string} [@@deriving yaml]

let rec read_multiplelines () =
  try
    let line = read_line () in
    line ^ "\n" ^ read_multiplelines ()
  with End_of_file -> ""

let yaml_val = function
  | Ok v -> v (* v has type Yaml.value *)
  | Error (`Msg m) -> failwith m

(* string para tm_w *)
let tm_w_de_string s = s |> Yaml.of_string_exn |> tm_w_of_yaml |> yaml_val

let wordLista word = 
  let first = String.sub word 0 1 in
  let resto = String.sub word 1 (String.length word -1) in  
    let rec list resto =
      match String.length resto with
      | 0 -> []
      | _ -> 
        let a = String.sub resto 0 1 in
        let b = String.sub resto 1 (String.length resto - 1) in 
        a :: list b in 
        let v = list resto in
  ([], first, v)

let listaWord fita = 
  let (esq, cabeca, dir) = fita in
  let esqString = String.concat "" (List.rev esq) in 
  let dirString = String.concat "" dir in 
  let strFinal = esqString ^ cabeca ^ dirString in
  (strFinal)

let julg boolz = 
  match boolz with
  | Some true -> "YES"
  | Some false -> "NO"
  | None -> "DON'T KNOW"

let find est simb delta = 
    match Hashtbl.find_opt delta est with
    | None -> None
    | Some a ->  
      match Hashtbl.find_opt a simb with 
      | None -> None
      | Some b -> Some b 

let switch fita nextT = 
  let (estD, simbolo, direcao) = nextT in
  let (esq, _, dir) = fita in
  if direcao = R then 
    let esq2 = simbolo :: esq in 
    if dir <> [] then 
      let cabeca = List.hd dir in
      let dir2 = List.tl dir in
      let fita2 = (esq2, cabeca, dir2) in
      (estD, fita2)
    else                        
      if simbolo = "_" then       
      (estD, (esq, simbolo, dir))
    else 
      (estD, (simbolo :: esq, "_", dir))
  else 
    if direcao = L then
    let dir2 = simbolo :: dir in 
    if esq <> [] then
    let cabeca = List.hd esq in 
    let esq2 = List.tl esq in 
    let fita2 = (esq2, cabeca, dir2) in
    (estD, fita2)
    else
    (estD, (esq, simbolo, dir))
  else 
    (estD, fita)



let f1 tm word = 
  let {states = _; input_alphabet = _; tape_alphabet_extra = _; start_state = estInicial; accept_state = estAccept; reject_state = estRej; delta} = tm in
  let fita = wordLista word in 
  let estadoAtual = estInicial in
  let count = 0 in
  let rec tmSimul fita estAtual count = 
  let (_, cabeca, _) = fita in
  if estAtual = estAccept then (Some true, fita)
  else
    if estAtual = estRej then (Some false, fita)
    else
      let nextT = find estAtual cabeca delta in
      if nextT = None then
        (Some false, fita)
      else
        let count = count + 1 in
        if (count >= 200) then
          (None, fita)
        else
        let nextX = Option.get nextT in
        let (est, fila) = switch fita nextX in
        tmSimul fila est count in 
        let finale = tmSimul fita estadoAtual count in
        (finale)
        
     
let adicionarF fita = 
let a = String.length fita in
if a = 0 then 
    "_"
else if (fita.[a-1] = '_') then
  fita 
else
  fita ^ "_"

let pertence est list = 
  if List.mem est list then 
    true 
else
  false


let wordIsAlf word alfInput = 
  let ( _ , cabeca, resto) = wordLista word in
  let word = cabeca :: resto in
  let v = 
    List.fold_left(fun acc x -> 
      if List.mem x alfInput then
        x :: acc 
      else 
        acc) [] word in
        if (List.rev v) = word then 
          true
      else false
  

let verificarTM tm word= 
  let {states = estados; input_alphabet = alfIn; tape_alphabet_extra = _; start_state = estInicial; accept_state = estAccept; reject_state = estRej; _} = tm in
  if pertence estInicial estados && pertence estAccept estados && pertence estRej estados && not (estados = []) && not (alfIn = []) && (wordIsAlf word alfIn) then
    true
      else false

        
let _ =
  let s = read_multiplelines () |> String.trim in
  (*enable print_endline s ;*)
  let tw = s |> tm_w_de_string in
  let tm = tw.m in
  let word = tw.w in


  if not (verificarTM tm word) then (
    print_endline "INVALID";
    exit 0
  );

  let (bool1,fita) = f1 tm word in 
  let fit = listaWord fita in
  let fit2 = adicionarF fit in

  let veredict = julg bool1 in

  if veredict = "YES" then
  (Printf.printf "%s\n%s\n" veredict fit2)
  else if veredict = "NO" then 
    (Printf.printf "%s\n%s\n" veredict fit2)
  else
  (Printf.printf "%s\n" veredict)

