type regExp =
| Epsilon
| All of int
| Lettre of char*int
| Concat of regExp*regExp
| Altern of regExp*regExp
| Etoile of regExp

exception Invalid_RegExp

(* FONCTIONS AUXILIAIRES POUR STRING_TO_REGEXP*)

let creer_liste_lettre () =
  let rec aux i l =
    if i<256 then 
      aux (i+1) ( (Char.chr i, 0)::l )
    else l 
  in
  aux 0 []

let rec lettre_dans_liste c l = match l with
| [] -> -1
| (a,i)::q -> if a = c then i else lettre_dans_liste c q

let rec incr_lettre_liste c l = match l with
| [] -> (c,0)::[]
| (a,i)::q -> if a = c then (a,i+1)::q else (a,i)::(incr_lettre_liste c q)

(*LECTURE D'UN STRING, TRANSFORMATION EN REGEXP LINÉARISÉE*)

let string_to_regexp s = (*création de la regexp à partir du str rentré par l'utilisateur*)
  let n = String.length s in
  if n = 0 then raise Invalid_RegExp else (*une regexp vide est invalide*)
  let pile = Stack.create () in (*La pile est indispensable, car on utilise dans le str l'odre postfixe*)
  let all_cmp = ref 0 in (*Compteur de "ALL"*)
  let lettre_cmp = ref (creer_liste_lettre ()) in (*Liste contenant les compteurs de lettres*)
  for i=0 to (n-1) do
    match s.[i] with
    | '.' -> (Stack.push (All (!all_cmp)) pile ; all_cmp := 1 + !all_cmp )
    | '@' -> Stack.push (Concat (Stack.pop pile, Stack.pop pile)) pile
    | '|' -> Stack.push (Altern (Stack.pop pile, Stack.pop pile)) pile
    | '*' -> Stack.push (Etoile (Stack.pop pile)) pile
    | '?' -> Stack.push (Altern (Stack.pop pile, Epsilon)) pile
    | a   -> begin
      let i = lettre_dans_liste a !lettre_cmp in (*On récupère le prochain indice à mettre, ou -1 si la lettre est non reconnue*)
      if i = -1 then (
        raise Invalid_RegExp )
      else (
        Stack.push (Lettre (a,i)) pile;
        lettre_cmp := incr_lettre_liste a !lettre_cmp ) (*On incrémente lecompteur pour la prochaine fois*)
    end
  done;
  if (Stack.length pile) <> 1 then raise Invalid_RegExp (*Il n'est censé rester qu'un élément dans la pile : notre regexp*)
  else Stack.pop pile

(*CRÉATION DES ENSEMBLES PREF, SUFF ET FACT*)
(*On les implémente avec des listes regexp de longueur 1 (des lettres, epsilon ou des ALL)*)

let pref_regexp e = 
  let pref = ref [] in (*Le définir ici permet à la variable d'être globale.*)
  let rec aux regexp = match regexp with
  | Epsilon -> ()                                     (*On a déjà ajouté Epsilon*)
  | All (i) -> pref := (All (i))::(!pref)             (*Ici on ajoute simplement les lettres\all à pref*)
  | Lettre (a,i) -> pref := (Lettre (a,i))::(!pref)
  | Concat (e1,e2) -> aux e1                          (*On appelle les cas inductifs*)
  | Altern (e1,e2) -> (aux e1; aux e2)
  | Etoile (e1) -> aux e1
in
(aux e; !pref)

let suff_regexp e = 
  let suff = ref [] in (*Le définir ici permet à la variable d'être globale.*)
  let rec aux regexp = match regexp with
  | Epsilon -> ()                                     (*On a déjà ajouté Epsilon*)
  | All (i) -> suff := (All (i))::(!suff)             (*Ici on ajoute simplement les lettres\all à suff*)
  | Lettre (a,i) -> suff := (Lettre (a,i))::(!suff)
  | Concat (e1,e2) -> aux e2                          (*On appelle les cas inductifs*)
  | Altern (e1,e2) -> (aux e1; aux e2)
  | Etoile (e1) -> aux e1
in
(aux e; !suff)

let rec prod_elt_ensemble a l = match l with        (*Ici on aura besoin de 3 fonction auxiliaire : celle-ci est*)
| [] -> []                                          (*l'auxiliaire d'une auxiliaire, pour réaliser un produit ensembliste*)
| e::q -> (a,e)::(prod_elt_ensemble a q)

let rec prod_ensembliste l1 l2 = match l1 with
| [] -> []
| e::q -> (prod_elt_ensemble e l2)@(prod_ensembliste q l2)

let rec fact_regexp e = match e with
| Epsilon | Lettre _ | All _ -> [] (*Dans ces cas-là, il n'y a qu'un seul caractère. On se permet de les ignorer.*)
| Concat (e1,e2) -> (fact_regexp e1)@(fact_regexp e2)@(prod_ensembliste (suff_regexp e1) (pref_regexp e2)) (*On unit 3 ensembles ici, dont le produit ensembliste définit plus haut.*)
| Altern (e1,e2) -> (fact_regexp e1)@(fact_regexp e2)
| Etoile (e1) -> (fact_regexp e1)@(fact_regexp e1)@(prod_ensembliste (suff_regexp e1) (pref_regexp e1)) (* e1* admet les mêmes facteurs que e1.e1*)
                    
type automate = {start: regExp list; finish: regExp list; delta: (regExp,regExp) Hashtbl.t}
                
let rec len_regexp e = match e with
  | Epsilon -> 0
  | Lettre _ | All _ -> 1
  | Concat (e1, e2) | Altern (e1, e2) -> len_regexp e1 + len_regexp e2
  | Etoile (e1) -> len_regexp e1
                    
let regexp_to_automate e =
  let f = fact_regexp e in
  let a = {start=pref_regexp e; finish=suff_regexp e; delta=Hashtbl.create (len_regexp e)} in
  let q = ref a.start in
  while !q <> [] do begin
    let h = List.hd !q in
    q := List.tl !q;
    List.iter (fun (r1,r2) ->
        if r1 = h then begin
          Hashtbl.add a.delta h r2;
          if not (Hashtbl.mem a.delta r2) then q := r2::!q
        end
      ) f
  end; done;
  a
  
let delta a q c = 
  let res = ref [] in
  List.iter (fun e -> match e with 
      | All _ -> res := e::!res
      | Lettre (l, _) -> if l = c then res := e::!res
    ) (if q = Epsilon then a.start else Hashtbl.find_all a.delta q);
  !res
    
let delta_star a s = 
  let n = String.length s in
  let res = ref [] in
  let rec aux lq i = if i<n then List.iter (fun q -> aux (delta a q s.[i]) (i+1)) lq else res := lq@(!res) in
  aux [Epsilon] 0;
  !res
    
let is_recognised a s = 
  let res = ref false in
  List.iter (fun q -> if not (!res) && List.mem q a.finish then res := true) (delta_star a s);
  !res






  let process_line line a =
    if is_recognised a line then
      Printf.printf "%s\n%!" line
  
  (* Lecture de l'entrée, ligne par ligne *)
  let process input a =
    try
      while true do
        let line = Stdlib.input_line input in
        process_line line a
      done
    with End_of_file -> ()
  
  let main () =
    (* Vérifie que l'expression régulière est bien présente en premier
       argument. Sinon, on affiche un message indiquant comment utiliser
       ce programme et on quitte avec un code d'erreur de `1`. *)
    let argc = Array.length Sys.argv in
    if argc <> 3 then begin
      Printf.printf "usage : %s regex [file]\n%!" Sys.argv.(0);
      exit 1
    end;
    (* S'il y a un deuxième argument c'est qu'il faut lire dans ce
       fichier, sinon, on utilise l'entrée standard. *)
    let input =
      if (argc = 3) then begin
        Stdlib.open_in Sys.argv.(2)
      end else
        Stdlib.stdin
    in
    Printf.printf
      "* Regexp you entered is '%s'\n* Reading from %s\n\n%!"
      Sys.argv.(1)
      (if argc = 3 then Sys.argv.(2) else "stdin");
    let a = regexp_to_automate (string_to_regexp (Sys.argv.(1))) in
    process input a;
    if argc = 3 then Stdlib.close_in input
  
  let () = main ()