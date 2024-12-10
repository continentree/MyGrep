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
  pref := Epsilon::(!pref); (*Epsilon est toujours préfixe, mais pas toujours dans la regexp : on l'ajoute donc ici*)
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
  suff := Epsilon::(!suff); (*Epsilon est toujours suffixe, mais pas toujours dans la regexp : on l'ajoute donc ici*)
  let rec aux regexp = match regexp with
  | Epsilon -> ()                                     (*On a déjà ajouté Epsilon*)
  | All (i) -> suff := (All (i))::(!suff)             (*Ici on ajoute simplement les lettres\all à suff*)
  | Lettre (a,i) -> suff := (Lettre (a,i))::(!suff)
  | Concat (e1,e2) -> aux e2                          (*On appelle les cas inductifs*)
  | Altern (e1,e2) -> (aux e1; aux e2)
  | Etoile (e1) -> aux e1
in
(aux e; !suff)

