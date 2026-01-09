open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type estado = int [@@deriving yojson]
type simbolo = char option [@@deriving yojson]
type transicao = (estado * simbolo) * estado [@@deriving yojson]
type nfa =
  estado list * char list * transicao list * estado list * estado list
[@@deriving yojson]

let rec read_multiplelines () =
  try 
    let line = read_line () in
    line ^ " " ^ read_multiplelines ()
  with End_of_file -> ""

let nfa_de_string s = s |> Yojson.Safe.from_string |> nfa_of_yojson

let nfa_para_string nfa = yojson_of_nfa nfa |> Yojson.Safe.to_string

let ordenar_nfa nfa =
  let (estados, alfabeto, transicoes, iniciais, finais) = nfa in
  let estados_ordenados = List.sort_uniq compare estados in
  let alfabeto_ordenado = List.sort_uniq compare alfabeto in
  let transicoes_ordenadas = List.sort_uniq compare transicoes in
  let iniciais_ordenados = List.sort_uniq compare iniciais in
  let finais_ordenados = List.sort_uniq compare finais in
  (estados_ordenados, alfabeto_ordenado, transicoes_ordenadas, iniciais_ordenados, finais_ordenados)

(* Função para organizar e tirar duplicados *)
let ordemDuplicados lista = List.sort_uniq compare lista


(*reverter - esta função irá reverter todas as transições, estados origem passam a destino e vice-versa, também troca os iniciais com os finais *)
let reverter (estados, alfabeto, transicoes, iniciais, finais) =
  let transicoesRev =
    List.map (fun ((q1, s), q2) -> ((q2, s), q1)) transicoes in
  (estados, alfabeto, transicoesRev, finais, iniciais)

(* unificar - criar um estado inicial (novoEstado) para ligar a todos os estados iniciais anteriores (origem)
todas as transições que saem estAnterior agora irão sair do novo estado inicial para o mesmo sítio *)
let unificar nfa = 
  let (estados, alfabeto, transicoes, iniciais, finais) = nfa in
  
  let max_estado = 
    if estados = [] then 0 
    else List.fold_left max min_int estados in
  let novoEstado = max_estado + 1 in
  
  let novasTransicoes =
    List.fold_left (fun acc ((origem, simbolo), destino) ->
      if List.mem origem iniciais then
        ((novoEstado, simbolo), destino) :: acc
      else
        acc
    ) [] transicoes in
  
  let novosFinais = 
    if List.exists (fun i -> List.mem i finais) iniciais then
      ordemDuplicados (novoEstado :: finais)
    else
      finais in
  
  (novoEstado :: estados, alfabeto, transicoes @ novasTransicoes, [novoEstado], novosFinais)

(* Nesta função irei renumerar os estados, através de um contador auxiliar (x=1 até n) em (estado, x), bem como
chamar a função auxiliar da construção do dfa e realizar toda a renumeração dos estados*)
let converter nfa =
  let (_, alfabeto, transicoes, iniciais, finais) = nfa in
  
  (* Função onde será feito o powerset para as transições e estados seguintes 
  Aqui iremos percorrer todo o alfabeto e para cada letra, encontrar quais os estados que vão para lá do estado atual pela letra (x), exemplo
  letra = a; estadoAtual = [2,3], então vamos ver às transições do NFA original, onde é que [2,3], (primeiro o 2 e o 3 em seguida) chegarão por a,
  após isso iremos ter um estado ou conjunto de estados, ex. [4,5], verificamos se ainda não o vimos e caso contrário adicionamos à pilha para ver e fazemos 
  uma transição do estadoAtual para esse estado de destino [4,5] por a: [2,3] -> a -> [4,5] *)

  let rec auxConverter listaVistos listaPilha novasTransicoes =
  match listaPilha with
  | [] -> (List.rev listaVistos, List.rev novasTransicoes)
  | estadoAtual :: pilhaTail ->
      if List.mem estadoAtual listaVistos then
        auxConverter listaVistos pilhaTail novasTransicoes
      else
        let transicoesAtual =
          List.fold_left (fun acc simbolo ->
            let simboloAtual = Some simbolo in
            let estadosDestino =
              List.fold_left (fun estDFinal estado ->
                let estXdest = 
                  List.fold_left (fun acc ((estado1, s), estado2) ->
                    if estado1 = estado && s = simboloAtual then 
                      estado2 :: acc
                    else acc
                      ) [] transicoes in
                  let estado2 = (estDFinal @ estXdest)  in
                  ordemDuplicados estado2) [] estadoAtual in
            if estadosDestino <> [] then
              ((estadoAtual, simbolo), estadosDestino) :: acc
            else acc
          ) [] alfabeto in
        let estadoAddPilha =
          List.fold_left (fun acc (_, destino) -> 
            if List.mem destino (listaVistos @ listaPilha) then 
              acc
            else destino :: acc
          ) [] transicoesAtual in
        auxConverter (estadoAtual :: listaVistos) (pilhaTail @ estadoAddPilha)(transicoesAtual @ novasTransicoes) in

let (listaVistos, transicoes_conjuntos) = auxConverter [] [iniciais] [] in

  let listaVistos2 = listaVistos in 
  let novoInicial = iniciais in
   
  (*A lista l será a lista que contém os pares (estado,x)*)
  let rec listaTuplos contador listaVistos2 = 
  match listaVistos2 with
  | [] -> []
  | hd :: tl ->(hd,contador):: listaTuplos (contador +1) tl in
  let l = listaTuplos 1 listaVistos2 in

  (*Iremos percorrer todos os estados do DFA resultante e relacionar com os finais anteriores *)
  let estadosFinais = 
  List.fold_left(fun acc (es1, es2) ->
      if List.exists (fun estf -> List.mem estf es1) finais  then 
        es2 :: acc 
      else
        acc) [] l in

  (* Esta função irá fazer o mesmo processo para o estadoInicial *)
  let estadoInicial2 = 
  List.fold_left(fun acc (es1, es2) ->
      if List.exists (fun estf -> List.mem estf es1) novoInicial  then 
        es2 :: acc 
      else
        acc) [] l in

  (* Aqui as transições serão atualizadas, o estado origem e o estado destino, tendo em conta a associação com o x correspondente do par (estado,x) *)
  let newTra =
  List.fold_left (fun acc ((e1,smb),e2) ->
    let lista = List.filter(fun (a, _) -> (compare (ordemDuplicados a) (ordemDuplicados e1)) = 0) l in
    let (_, estadoNum) = List.hd lista in
    let lista2 = List.filter(fun (a, _) -> (compare (ordemDuplicados a) (ordemDuplicados e2)) = 0) l in
    let (_, estadoNum2) = List.hd lista2 in
      ((estadoNum,(Some smb)),estadoNum2) :: acc ) [] transicoes_conjuntos in

  (*Para renumerar os estados, iremos reverter a lista e ver o indice "n" mais alto (valor mais alto de x) e em seguida atribuir todos os valores de 1-n 
  É relevante ter em conta que como a lista l foi criada tendo em conta a listaVazios (estados do DFA) onde foi verificado que não existem duplicados e os valores
  existem todos entre 1-n *)
  let (_ , est2) = List.hd (List.rev l) in
  let estFinal = 
    let rec criarLista max acc =
    if max < 1 then acc else criarLista (max-1) (max::acc) in
      criarLista est2 [] in  
  (estFinal , alfabeto, newTra, estadoInicial2, estadosFinais)

let _ =
  let s = read_multiplelines () |> String.trim in

  let nfa = s |> nfa_de_string in

  let (_,_, t, _,_) = nfa in
let epsi = 
  let rec ep t =
    match t with 
    | [] -> false
    | ((_, s), _) :: tail ->
      if s = None then true
      else ep tail 
      in ep t 
      in
  
  if epsi then (
    print_endline "EPSILON";
    exit 0 
  );

  (* imprimir nfa *)
 (*print_endline (nfa_para_string (ordenar_nfa nfa));
  print_endline (nfa_para_string (reverter nfa));
  print_endline (nfa_para_string (unificar nfa));
  print_endline (nfa_para_string (ordenar_nfa (unificar (reverter nfa)))); *)
  print_endline (nfa_para_string (ordenar_nfa (converter(unificar(reverter(converter(unificar (reverter nfa))))))));
