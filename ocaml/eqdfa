open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type estado = int [@@deriving yojson]

type simbolo = char option [@@deriving yojson]

type transicao = (estado * simbolo) * estado [@@deriving yojson]

(* o nosso nfa *)
type nfa =
  estado list * char list * transicao list * estado list * estado list
[@@deriving yojson]

type nfaList = nfa list [@@deriving yojson]

let ordemDuplicados lista = List.sort_uniq compare lista

let rec read_multiplelines () =
  try
    let line = read_line () in
    line ^ " " ^ read_multiplelines ()
  with End_of_file -> ""

let nfa_de_string s = s |> Yojson.Safe.from_string |> nfa_of_yojson

let nfaListString s = s |> Yojson.Safe.from_string |> nfaList_of_yojson

let nfa_para_string nfa = yojson_of_nfa nfa |> Yojson.Safe.to_string

let ordenar_nfa nfa =
  let (estados, alfabeto, transicoes, iniciais, finais) = nfa in
  let estados_ordenados = List.sort_uniq compare estados in
  let alfabeto_ordenado = List.sort_uniq compare alfabeto in
  let transicoes_ordenadas = List.sort_uniq compare transicoes in
  let iniciais_ordenados = List.sort_uniq compare iniciais in
  let finais_ordenados = List.sort_uniq compare finais in
  (estados_ordenados, alfabeto_ordenado, transicoes_ordenadas, iniciais_ordenados, finais_ordenados)

  
(*Função para fazer o produto cartesiano de dois dfas*)
let produto dfa1 dfa2 = 
  let (estados1, _, _, _, _) = dfa1 in
  let (estados2, _, _, _, _) = dfa2 in
    let estadosProntos = 
      List.fold_left(fun acc estadoA -> 
        let estadosNovos =
        List.fold_left(fun acc estadoB -> 
          let novosEstados = (estadoA, estadoB) in
          novosEstados :: acc 
          ) acc estados2 in
          estadosNovos
        ) [] estados1 in
        (estadosProntos) 

(*Função para os estadosIniciais, porém apenas iremos ter um estado (e1,e2), uma vez que todos os DFAS apenas são dados 
com um estado inicial e o novo estado inicial é o seu tuplo *)
let estadosIniciais dfa1 dfa2 =
  let (_, _, _, inicial1, _) = dfa1 in 
  let (_, _, _, inicial2, _) = dfa2 in
  let in1 = List.hd inicial1 in
  let in2 = List.hd inicial2 in
  let estadoInicial = (in1, in2) in
  ([estadoInicial]) 


let estadosFinaisUniao dfa1 dfa2 novosEstados = 
  let (_, _, _, _, finais1) = dfa1 in
  let (_, _, _, _, finais2) = dfa2 in
  let estFinais =
        List.fold_left(fun acc2 (estX,estY) ->
          if List.mem estX finais1 then 
             (estX,estY) :: acc2
          else if List.mem estY finais2 then
            (estX,estY) :: acc2
          else
            acc2
              ) [] novosEstados in 
            (estFinais)

let estadosFinaisIntersecao dfa1 dfa2 novosEstados = 
  let (_, _, _, _, finais1) = dfa1 in
  let (_, _, _, _, finais2) = dfa2 in
  let estFinais =
        List.fold_left(fun acc2 (estX,estY) ->
          if List.mem estX finais1 && List.mem estY finais2 then
           (estX,estY) :: acc2
          else
            acc2
              ) [] novosEstados in 
            (estFinais)

(*Estas transições são usadas para construir as transições tanto da união como da interseção, uma vez que o 
 que distingue as duas são os estados finais *)

let transicoes novosEstados alfabeto transicoes1 transicoes2 = 
  let novasTransicoes =
  List.fold_left( fun acc est -> 
    let acc =
      List.fold_left(fun acc (estX, estY) -> 
        let acc2 =
        List.fold_left(fun acc s ->
          let x = List.filter (fun ((q1, simb), _) -> q1 = estX &&  simb = Some s ) transicoes1 in
          let y = List.filter (fun ((q2, simb), _) -> q2 = estY &&  simb = Some s ) transicoes2 in
          let ((_,_), valx) = List.hd x in
          let ((_,_), valy) = List.hd y in
          let novaTransicao = (((estX, estY), s), (valx, valy)) in
          (novaTransicao :: acc)
          ) acc alfabeto in
           acc2
        ) acc est in
        acc
    ) [] novosEstados in 
    (List.rev novasTransicoes)
  

(*Função União*)
let uniao dfa1 dfa2 =
  let (_, alfabeto1, transicoes1, _, _) = dfa1 in
  let (_, alfabeto2, transicoes2, _, _) = dfa2 in

  let novoAlfabeto = alfabeto1 @ alfabeto2 in
  let novoAfbDup = ordemDuplicados novoAlfabeto in

  let novosEst = produto dfa1 dfa2 in
  let iniciales = estadosIniciais dfa1 dfa2 in
  let finales = estadosFinaisUniao dfa1 dfa2 novosEst in
  let trans = transicoes [List.rev novosEst] novoAfbDup transicoes1 transicoes2 in 
  (List.rev novosEst, novoAfbDup, trans, iniciales, finales)

(*Função interseção*)
let intersecao dfa1 dfa2 =
  let (_, alfabeto1, transicoes1, _, _) = dfa1 in
  let (_, alfabeto2, transicoes2, _, _) = dfa2 in

  let novoAlfabeto = alfabeto1 @ alfabeto2 in
  let novoAfbDup = ordemDuplicados novoAlfabeto in

  let novosEst = produto dfa1 dfa2 in
  let iniciales = estadosIniciais dfa1 dfa2 in
  let finales = estadosFinaisIntersecao dfa1 dfa2 novosEst in
  let trans = transicoes [List.rev novosEst] novoAfbDup transicoes1 transicoes2 in 
  (List.rev novosEst, novoAfbDup, trans, iniciales, finales)

(*Função complemento*)
let complemento dfa1 =
  let (estados, alfabeto, transicoes, iniciais, finales) = dfa1 in
  let novosEstFinais =
  List.fold_left(fun acc x -> 
    if List.mem x finales then
      acc 
    else
       x :: acc
    ) [] estados in
    (estados, alfabeto, transicoes, iniciais, List.rev novosEstFinais) 

(*Função de renumeração de estados - como estamos a trabalhar com o produto cartesiano, temos de passar do *)
(*Tipo (e1,e2) para apenas um (e3) *)
let renumerar cmpl = 
  let (estados, alfabeto, transicoes, iniciais, finais) = cmpl in

  let rec listaTuplos i ests =
    match ests with 
    | [] -> []
    | (a,b) :: tl -> ((a,b), i) :: listaTuplos (i + 1) tl in
    let l = listaTuplos 1 estados in

    let newTras = 
      List.fold_left(fun acc (((e1,e2),s),(e3,e4)) -> 
        let transE1 = List.filter(fun ((eX,eY),_) -> (compare (eX,eY) (e1,e2)) = 0) l in
        let (_,estadoNumE1) = List.hd transE1 in
        let transE2 = List.filter(fun ((eX,eY),_) -> (compare (eX,eY) (e3,e4)) = 0) l in
        let (_,estadoNumE2) = List.hd transE2 in
        let transUpdate = ((estadoNumE1, Some s), estadoNumE2) in
        transUpdate :: acc 
        ) [] transicoes in

    let newInicial = 
        let (a,b) = List.hd iniciais in
        let x = List.filter(fun ((eX,eY),_) -> (compare (a,b) (eX,eY)) = 0) l in
        let ((_,_), estadoNum) = List.hd x in
        (estadoNum) in


    let newFinais = 
      List.fold_left(fun acc ((est1,est2), n) ->
        if List.mem (est1,est2) finais then
          n :: acc
        else 
          acc
        ) [] l in

    let (_ , est2) = List.hd (List.rev l) in
    let ests = 
    let rec criarLista max acc =
    if max < 1 then acc else criarLista (max-1) (max::acc) in
      criarLista est2 [] in  
      
    (ests, alfabeto, List.rev newTras, [newInicial], List.rev newFinais)

  (*Procura por todos os estados que conseguimos chegar através de um estado anterior *)
  (*O símbolo por onde chegamos é indiferente pois interessa apenas onde consegue chegar *)
  let estDest estado trans = 
  let states =
  List.fold_left(fun acc ((es1, _),es2) ->
    if estado = es1 then  
      es2 :: acc
    else
    acc) [] trans in
    (states)
  
    (*Esta função irá verificar se a os estados já alcançados são finais *)
  let espelhoMeu lst finais =
    List.exists (fun x -> List.mem x lst) finais

(*Função que vai procurar pela aceitação *)
let cacador inicial trans finais = 
  let destInicial = estDest inicial trans in
  let checked = [inicial] in
  let porVer = destInicial in
  
  let rec hunt checked porVer = 
    if espelhoMeu checked finais then
        true
    else
    match porVer with
    | [] -> (espelhoMeu checked finais) 
    | x :: xs -> 
      if List.mem x checked then
        hunt checked xs 
      else
    let newDest = estDest x trans in 
    let newPorver = ordemDuplicados (porVer @ newDest) in
    let newChecked = x :: checked in
    hunt newChecked newPorver in
    hunt checked porVer 
 
(*Função devolve bool se conseguiu alcançar um estado final a partir do estado inicial*)
(*Aqui são chamadas as funções para resolver o problema da equivalência dos slides da aula 7 *)
let eqdfa dfa1 dfa2 = 
  let lb = complemento dfa2 in
  let int1 = intersecao dfa1 lb in
  let part1 = renumerar int1 in
  let la = complemento dfa1 in
  let int2 = intersecao la dfa2 in
  let part2 = renumerar int2 in
  let total = uniao part1 part2 in
  
  let doneTotal = renumerar total in
  let (_, _, transi, ins, fi) = doneTotal in
  let i = List.hd ins in
  let z = cacador i transi fi in
  (z)

  let verificacaoInvalid dfa = 
      let (_, _, transicoes, _, _) = dfa in
    List.fold_left(fun acc ((e1, s), _) ->
      let a = List.filter(fun ((a, b), _) -> a = e1 && b = s) transicoes in
      if List.length a > 1 then
        acc + 1 
      else 
        acc
    ) 0 transicoes  

let _ =
  let s = read_multiplelines () |> String.trim in
  
  (*Alterar para uma lista de DFAS*)
  let tirarP = String.sub s 1 (String.length s -1) in
  let tirarU = String.sub tirarP 0 (String.length tirarP -1) in
  let addP = "[" ^ tirarU in
  let addU = addP ^ "]" in 


  (* deve editar as chamadas aqui ... *)

  let nfa = addU |> nfaListString in

  (*Separação dos DFAS*)
  let d1 = List.hd nfa in
  let nfa2List = List.tl nfa in
  let d2 = List.hd nfa2List in

  (*Verificação se são DFAS*)
  if (verificacaoInvalid d1 > 1) || (verificacaoInvalid d2 > 1)then (
    print_endline "INVALID";
  exit 0
  );

  (*Função eqdfa que irá chamar todas as anteriores *)
  let a = eqdfa d1 d2 in
  if(not a = true) then
    print_endline ("TRUE")
    else
  print_endline ("FALSE")

